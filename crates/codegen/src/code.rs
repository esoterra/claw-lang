use std::collections::HashMap;

use ast::{ExpressionId, FunctionId, NameId, StatementId};
use claw_ast as ast;

use crate::{
    builders::module::{ModuleBuilder, ModuleDataIndex, ModuleFunctionIndex},
    expression::EncodeExpression,
    function::{self, EncodedFuncs, EncodedFunction},
    imports::{self, EncodedImports},
    statement::EncodeStatement,
    types::{EncodeType, FieldInfo, Signedness},
    GenerationError,
};
use claw_resolver::{
    types::ResolvedType, ImportFuncId, ImportType, ImportTypeId, ItemId, LocalId, ParamId,
    ResolvedComponent, ResolvedFunction,
};
use cranelift_entity::EntityRef;
use wasm_encoder as enc;

pub struct CodeGenerator<'gen> {
    // Parent output (currently just for data segments)
    mod_builder: &'gen mut ModuleBuilder,

    // Context
    comp: &'gen ResolvedComponent,
    imports: &'gen EncodedImports,
    functions: &'gen EncodedFuncs,
    func_idx_for_import: &'gen HashMap<ImportFuncId, ModuleFunctionIndex>,
    func_idx_for_func: &'gen HashMap<FunctionId, ModuleFunctionIndex>,

    realloc: ModuleFunctionIndex,
    // Function structs
    function: &'gen ast::Function,
    resolved_func: &'gen ResolvedFunction,
    encoded_func: &'gen EncodedFunction,

    // Internal output
    builder: enc::Function,

    // Tracking state
    #[allow(dead_code)]
    local_space: Vec<enc::ValType>,
    return_index: Option<u32>,
    call_params_index: u32,
    call_results_index: u32,
    index_for_local: HashMap<LocalId, CoreLocalId>,
    index_for_expr: HashMap<ExpressionId, CoreLocalId>,
}
pub struct CoreLocalId(u32);

impl From<u32> for CoreLocalId {
    fn from(value: u32) -> Self {
        CoreLocalId(value)
    }
}

impl<'gen> CodeGenerator<'gen> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        mod_builder: &'gen mut ModuleBuilder,
        comp: &'gen ResolvedComponent,
        imports: &'gen EncodedImports,
        functions: &'gen EncodedFuncs,
        func_idx_for_import: &'gen HashMap<ImportFuncId, ModuleFunctionIndex>,
        func_idx_for_func: &'gen HashMap<FunctionId, ModuleFunctionIndex>,
        encoded_func: &'gen EncodedFunction,
        id: FunctionId,
        realloc: ModuleFunctionIndex,
    ) -> Result<Self, GenerationError> {
        let function = &comp.component.functions[id];
        let resolved_func = &comp.funcs[&id];

        let mut local_space = encoded_func.flat_params.clone();
        let locals_start = local_space.len();

        // If the result spills, allocate it a local right after the flat params
        let return_index = encoded_func.results.as_ref().and_then(|info| {
            if info.spill.spill() {
                let index = local_space.len();
                local_space.push(enc::ValType::I32);
                Some(index as u32)
            } else {
                None
            }
        });

        let call_params_index = local_space.len() as u32;
        local_space.push(enc::ValType::I32);
        let call_results_index = local_space.len() as u32;
        local_space.push(enc::ValType::I32);

        // Layout locals
        let mut index_for_local = HashMap::new();
        let mut locals = Vec::with_capacity(resolved_func.locals.len());
        for (id, _local) in resolved_func.locals.iter() {
            let rtype = resolved_func.local_type(id, &comp.component)?;
            let local_id = CoreLocalId((local_space.len() + locals.len()) as u32);
            index_for_local.insert(id, local_id);
            rtype.append_flattened(comp, &mut locals);
        }
        local_space.extend(locals);

        // Layout expressions
        let mut index_for_expr = HashMap::new();
        let mut allocator =
            ExpressionAllocator::new(comp, resolved_func, &mut local_space, &mut index_for_expr);
        for statement in function.body.iter() {
            let statement = comp.component.get_statement(*statement);
            statement.alloc_expr_locals(&mut allocator)?;
        }

        let locals = &local_space[locals_start..];
        let locals = locals.iter().map(|l| (1, *l));
        let mut builder = enc::Function::new(locals);

        if let Some(return_index) = return_index {
            // old ptr, old size
            builder.instruction(&enc::Instruction::I32Const(0));
            builder.instruction(&enc::Instruction::I32Const(0));

            let result_type = comp.component.functions[id].results.unwrap();
            // align
            let align = result_type.align(comp);
            let align = 2u32.pow(align);
            builder.instruction(&enc::Instruction::I32Const(align as i32));
            // new size
            let size = result_type.mem_size(comp);
            builder.instruction(&enc::Instruction::I32Const(size as i32));
            // call allocator
            builder.instruction(&enc::Instruction::Call(realloc.into()));
            // store address
            builder.instruction(&enc::Instruction::LocalSet(return_index));
        }

        Ok(Self {
            mod_builder,
            comp,
            imports,
            functions,
            realloc,
            func_idx_for_import,
            func_idx_for_func,
            function,
            resolved_func,
            encoded_func,
            builder,
            local_space,
            return_index,
            call_params_index,
            call_results_index,
            index_for_local,
            index_for_expr,
        })
    }

    pub fn encode_statement(&mut self, statement: StatementId) -> Result<(), GenerationError> {
        let stmt = self.comp.component.get_statement(statement);
        stmt.encode(self)
    }

    pub fn encode_child(&mut self, expression: ExpressionId) -> Result<(), GenerationError> {
        let expr = self.comp.component.expr().get_exp(expression);
        expr.encode(expression, self)
    }

    pub fn instruction(&mut self, instruction: &enc::Instruction) {
        self.builder.instruction(instruction);
    }

    pub fn const_i32(&mut self, constant: i32) {
        self.builder
            .instruction(&enc::Instruction::I32Const(constant));
    }

    pub fn expression_type(
        &self,
        expression: ExpressionId,
    ) -> Result<ResolvedType, GenerationError> {
        let type_id = self
            .resolved_func
            .expression_type(expression, &self.comp.component)?;
        Ok(type_id)
    }

    pub fn get_ptype(
        &self,
        expression: ExpressionId,
    ) -> Result<Option<ast::PrimitiveType>, GenerationError> {
        let rtype = self.expression_type(expression)?;
        let ptype = match rtype {
            ResolvedType::Primitive(ptype) => Some(ptype),
            ResolvedType::Import(_) => todo!(),
            ResolvedType::Defined(type_id) => {
                let valtype = self.comp.component.get_type(type_id);
                match valtype {
                    ast::ValType::Result(_) => None,
                    ast::ValType::Primitive(ptype) => Some(*ptype),
                }
            }
        };
        Ok(ptype)
    }

    pub fn one_field(&self, expression: ExpressionId) -> Result<FieldInfo, GenerationError> {
        let rtype = self.expression_type(expression)?;
        let mut fields = rtype.fields(self.comp);
        assert_eq!(
            fields.len(),
            1,
            "Expected expression to only have one field"
        );
        Ok(fields.remove(0))
    }

    pub fn fields(&self, expression: ExpressionId) -> Result<Vec<FieldInfo>, GenerationError> {
        let rtype = self.expression_type(expression)?;
        Ok(rtype.fields(self.comp))
    }

    pub fn lookup_name(&self, ident: NameId) -> ItemId {
        self.resolved_func.bindings[&ident]
    }

    pub fn lookup_name_str(&self, ident: NameId) -> &str {
        self.comp.component.get_name(ident)
    }

    pub fn lookup_import_type(&self, id: ImportTypeId) -> &ImportType {
        &self.comp.imports.types[id]
    }

    pub fn spill_return(&self) -> bool {
        self.encoded_func
            .results
            .as_ref()
            .map(|r| r.spill())
            .unwrap_or(false)
    }

    pub fn allocate(&mut self) {
        self.instruction(&enc::Instruction::Call(self.realloc.into()))
    }

    pub fn encode_call(
        &mut self,
        item: ItemId,
        args: &[ExpressionId],
        expression: Option<ExpressionId>,
    ) -> Result<(), GenerationError> {
        match item {
            ItemId::ImportFunc(id) => self.encode_import_call(id, args, expression),
            ItemId::Function(id) => self.encode_func_call(id, args, expression),
            _ => panic!(""),
        }
    }

    fn encode_import_call(
        &mut self,
        id: ImportFuncId,
        args: &[ExpressionId],
        expression: Option<ExpressionId>,
    ) -> Result<(), GenerationError> {
        let enc_import_func = self.imports.funcs.get(&id).unwrap();
        // Prepare arguments
        if let Some(spilled_params) = &enc_import_func.spill_params {
            self.prepare_import_spilled_args(spilled_params, args)?;
        } else {
            // Push all the field values onto the stack
            for arg in args.iter().copied() {
                let fields = self.fields(arg)?;
                for field in fields.iter() {
                    self.read_expr_field(arg, field);
                }
            }
        }
        // Prepare return area
        if let Some(spilled_results) = &enc_import_func.spill_results {
            self.prepare_import_return_area(spilled_results);
        }
        // Encode call instruction
        let index = self.func_idx_for_import.get(&id);
        let index = *index.unwrap();
        self.instruction(&enc::Instruction::Call(index.into()));
        // Write expression output if needed
        if let Some(expression) = expression {
            let fields = self.fields(expression)?;
            for field in fields.iter() {
                if enc_import_func.spill_results.is_some() {
                    // spilled value is read from return area
                    self.read_return_area(field);
                } else {
                    // value is already on stack
                }
                self.write_expr_field(expression, field);
            }
        }
        Ok(())
    }

    fn prepare_import_spilled_args(
        &mut self,
        spilled_params: &imports::SpilledParams,
        args: &[ExpressionId],
    ) -> Result<(), GenerationError> {
        // Allocate spilled parameters
        self.const_i32(0);
        self.const_i32(0);
        self.const_i32(2i32.pow(spilled_params.align));
        self.const_i32(spilled_params.size as i32);
        self.allocate();
        self.local_set(self.call_params_index);
        // Write params into memory
        assert_eq!(spilled_params.params.len(), args.len());
        let args_iter = args.iter().copied();
        let params_iter = spilled_params.params.iter();
        for (arg, param_info) in args_iter.zip(params_iter) {
            let fields = self.fields(arg)?;
            for field in fields.iter() {
                self.local_get(self.call_params_index);
                let mem_offset = param_info.mem_offset + field.mem_offset;
                self.const_i32(mem_offset as i32);
                self.instruction(&enc::Instruction::I32Add);
                self.read_expr_field(arg, field);
                self.write_mem(field);
            }
        }
        // Push param pointer onto stack
        self.local_get(self.call_params_index);
        Ok(())
    }

    fn prepare_import_return_area(&mut self, spilled_results: &imports::SpilledResults) {
        // Allocate spilled results
        self.const_i32(0);
        self.const_i32(0);
        self.const_i32(2i32.pow(spilled_results.align));
        self.const_i32(spilled_results.size as i32);
        self.allocate();
        self.local_set(self.call_results_index);
        // Push result pointer onto stack
        self.local_get(self.call_results_index);
    }

    fn read_return_area(&mut self, field: &FieldInfo) {
        self.local_get(self.call_results_index);
        self.read_mem_field(field);
    }

    fn encode_func_call(
        &mut self,
        id: FunctionId,
        args: &[ExpressionId],
        expression: Option<ExpressionId>,
    ) -> Result<(), GenerationError> {
        let encoded_func = self.functions.funcs.get(&id).unwrap();
        // Prepare arguments
        if let Some(spilled_params) = &encoded_func.spill_params {
            self.prepare_function_spilled_args(spilled_params, &encoded_func.params, args)?;
        } else {
            // Push all the field values onto the stack
            for arg in args.iter().copied() {
                let fields = self.fields(arg)?;
                for field in fields.iter() {
                    self.read_expr_field(arg, field);
                }
            }
        }

        // Encode call instruction
        let index = self.func_idx_for_func.get(&id);
        let index = *index.unwrap();
        self.instruction(&enc::Instruction::Call(index.into()));
        // Write expression output if needed
        if let Some(expression) = expression {
            let fields = self.fields(expression)?;
            if let Some(results) = &encoded_func.results {
                if results.spill() {
                    // Save the results pointer
                    self.local_set(self.call_results_index);
                    // Write the fields from return area
                    for field in fields.iter() {
                        self.read_return_area(field);
                        self.write_expr_field(expression, field);
                    }
                } else {
                    // Write the fields from the stack
                    for field in fields.iter() {
                        self.write_expr_field(expression, field);
                    }
                }
            }
        }
        Ok(())
    }

    fn prepare_function_spilled_args(
        &mut self,
        spilled_params: &function::SpilledParams,
        params: &[function::ParamInfo],
        args: &[ExpressionId],
    ) -> Result<(), GenerationError> {
        // Allocate spilled parameters
        self.const_i32(0);
        self.const_i32(0);
        self.const_i32(2i32.pow(spilled_params.align));
        self.const_i32(spilled_params.size as i32);
        self.allocate();
        self.local_set(self.call_params_index);
        // Write params into memory
        assert_eq!(params.len(), args.len());
        let args_iter = args.iter().copied();
        let params_iter = params.iter();
        for (arg, param_info) in args_iter.zip(params_iter) {
            let fields = self.fields(arg)?;
            for field in fields.iter() {
                self.local_get(self.call_params_index);
                let mem_offset = param_info.mem_offset + field.mem_offset;
                self.const_i32(mem_offset as i32);
                self.instruction(&enc::Instruction::I32Add);
                self.read_expr_field(arg, field);
                self.write_mem(field);
            }
        }
        // Push param pointer onto stack
        self.local_get(self.call_params_index);
        Ok(())
    }

    pub fn read_param_field(&mut self, param: ParamId, field: &FieldInfo) {
        let param_info = &self.encoded_func.params[param.index()];
        if self.encoded_func.spill_params.is_some() {
            let mem_index = param_info.mem_offset + field.mem_offset;
            self.builder.instruction(&enc::Instruction::LocalGet(0));
            self.const_i32(mem_index as i32);
            self.builder.instruction(&enc::Instruction::I32Add);
            self.load_field(field);
        } else {
            let local_index = param_info.index_offset + field.index_offset;
            self.local_get(local_index);
        }
    }

    pub fn read_local_field(&mut self, local: LocalId, field: &FieldInfo) {
        let local_index = &self.index_for_local[&local];
        let local_index = local_index.0 + field.index_offset;
        self.local_get(local_index);
    }

    pub fn write_local_field(&mut self, local: LocalId, field: &FieldInfo) {
        let local_index = &self.index_for_local[&local];
        let local_index = local_index.0 + field.index_offset;
        self.local_set(local_index);
    }

    pub fn read_expr_field(&mut self, expression: ExpressionId, field: &FieldInfo) {
        let local_index = &self.index_for_expr[&expression];
        let local_index = local_index.0 + field.index_offset;
        self.local_get(local_index);
    }

    pub fn write_expr_field(&mut self, expression: ExpressionId, field: &FieldInfo) {
        let local_index = &self.index_for_expr[&expression];
        let local_index = local_index.0 + field.index_offset;
        self.local_set(local_index);
    }

    pub fn read_return_ptr(&mut self) -> Result<(), GenerationError> {
        let return_ptr_index = self.return_index.unwrap();
        self.local_get(return_ptr_index);
        Ok(())
    }

    /// The value's base memory offset MUST be on the stack before calling this
    pub fn field_address(&mut self, field: &FieldInfo) {
        self.const_i32(field.mem_offset as i32);
        self.instruction(&enc::Instruction::I32Add);
    }

    /// The value's base memory offset MUST be on the stack before calling this
    pub fn read_mem_field(&mut self, field: &FieldInfo) {
        self.field_address(field);
        self.load_field(field);
    }

    /// Fields absolute offset in memory MUST be on the stack underneath the value before calling this
    pub fn write_mem(&mut self, field: &FieldInfo) {
        self.store_field(field);
    }

    pub fn encode_const_bytes(&mut self, data: &[u8]) -> ModuleDataIndex {
        self.mod_builder.data(data)
    }

    pub fn encode_const_int(&mut self, int: u64, field: &FieldInfo) {
        let instruction = match field.stack_type {
            enc::ValType::I32 => enc::Instruction::I32Const(int as i32),
            enc::ValType::I64 => enc::Instruction::I64Const(int as i64),
            _ => panic!("Not an integer"),
        };
        self.instruction(&instruction);
    }

    pub fn encode_const_float(&mut self, float: f64, field: &FieldInfo) {
        let instruction = match field.stack_type {
            enc::ValType::F32 => enc::Instruction::F32Const(float as f32),
            enc::ValType::F64 => enc::Instruction::F64Const(float),
            _ => panic!("Not a float!"),
        };
        self.instruction(&instruction);
    }

    fn local_get(&mut self, local_index: u32) {
        self.builder
            .instruction(&enc::Instruction::LocalGet(local_index));
    }

    fn local_set(&mut self, local_index: u32) {
        self.builder
            .instruction(&enc::Instruction::LocalSet(local_index));
    }

    fn load_field(&mut self, field: &FieldInfo) {
        let mem_arg = field.mem_arg();
        let instruction = match (field.stack_type, field.signedness, field.mems_size) {
            // Small types with sign-extending
            (enc::ValType::I32, Signedness::Unsigned, 1) => enc::Instruction::I32Load8U(mem_arg),
            (enc::ValType::I32, Signedness::Signed, 1) => enc::Instruction::I32Load8S(mem_arg),
            (enc::ValType::I32, Signedness::Unsigned, 2) => enc::Instruction::I32Load16U(mem_arg),
            (enc::ValType::I32, Signedness::Signed, 2) => enc::Instruction::I32Load16S(mem_arg),
            // 32 and 64 bit values don't need sign-extending
            (enc::ValType::I32, _, 4) => enc::Instruction::I32Load(mem_arg),
            (enc::ValType::I64, _, 8) => enc::Instruction::I64Load(mem_arg),
            // Floats
            (enc::ValType::F32, _, 4) => enc::Instruction::F32Load(mem_arg),
            (enc::ValType::F64, _, 8) => enc::Instruction::F64Load(mem_arg),
            // Fallback error
            (valtype, s, size) => panic!(
                "Cannot load value type {:?} with signedness {:?} and size {}",
                valtype, s, size
            ),
        };
        self.builder.instruction(&instruction);
    }

    fn store_field(&mut self, field: &FieldInfo) {
        let mem_arg = field.mem_arg();
        let instruction = match field.stack_type {
            enc::ValType::I32 => enc::Instruction::I32Store(mem_arg),
            enc::ValType::I64 => enc::Instruction::I64Store(mem_arg),
            enc::ValType::F32 => enc::Instruction::F32Store(mem_arg),
            enc::ValType::F64 => enc::Instruction::F64Store(mem_arg),
            valtype => panic!("Cannot store value type {:?}", valtype),
        };
        self.builder.instruction(&instruction);
    }

    pub fn finalize(mut self) -> Result<enc::Function, GenerationError> {
        for statement in self.function.body.iter() {
            self.encode_statement(*statement)?;
        }
        self.builder.instruction(&enc::Instruction::End);
        Ok(self.builder)
    }
}

pub struct ExpressionAllocator<'a> {
    // Context
    comp: &'a ResolvedComponent,
    func: &'a ResolvedFunction,
    // State
    local_space: &'a mut Vec<enc::ValType>,
    index_for_expr: &'a mut HashMap<ExpressionId, CoreLocalId>,
}

impl<'a> ExpressionAllocator<'a> {
    pub fn new(
        comp: &'a ResolvedComponent,
        func: &'a ResolvedFunction,
        local_space: &'a mut Vec<enc::ValType>,
        index_for_expr: &'a mut HashMap<ExpressionId, CoreLocalId>,
    ) -> Self {
        Self {
            comp,
            func,
            local_space,
            index_for_expr,
        }
    }

    pub fn alloc(&mut self, expression: ExpressionId) -> Result<(), GenerationError> {
        // Record index
        let index = self.local_space.len() as u32;
        let index = CoreLocalId(index);
        self.index_for_expr.insert(expression, index);
        // Allocate locals
        let rtype = self
            .func
            .expression_type(expression, &self.comp.component)?;
        rtype.append_flattened(self.comp, self.local_space);
        Ok(())
    }

    #[allow(dead_code)]
    pub fn alloc_extra(&mut self, valtype: enc::ValType) -> Result<(), GenerationError> {
        self.local_space.push(valtype);
        Ok(())
    }

    pub fn alloc_child(&mut self, expression: ExpressionId) -> Result<(), GenerationError> {
        let expr = self.comp.component.expr().get_exp(expression);
        expr.alloc_expr_locals(expression, self)
    }

    pub fn alloc_statement(&mut self, statement: StatementId) -> Result<(), GenerationError> {
        let statement = self.comp.component.get_statement(statement);
        statement.alloc_expr_locals(self)
    }
}
