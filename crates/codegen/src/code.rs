use std::collections::HashMap;

use ast::{ExpressionId, FunctionId, NameId, StatementId};
use claw_ast as ast;

use crate::{
    builders::module::{ModuleDataIndex, ModuleFunctionIndex},
    expression::EncodeExpression,
    function::{FunctionGenerator, ParamInfo, ResultsInfo},
    module::ModuleGenerator,
    statement::EncodeStatement,
    types::{EncodeType, FieldInfo, Signedness},
    GenerationError,
};
use claw_resolver::{
    types::ResolvedType, ImportType, ImportTypeId, ItemId, LocalId, ParamId, ResolvedComponent, ResolvedFunction
};
use cranelift_entity::EntityRef;
use wasm_encoder as enc;

pub struct CodeGenerator<'gen> {
    parent: &'gen mut ModuleGenerator,
    comp: &'gen ResolvedComponent,
    func: &'gen ResolvedFunction,
    func_ast: &'gen ast::Function,
    realloc: ModuleFunctionIndex,

    builder: enc::Function,

    spill_params: bool,
    params: Vec<ParamInfo>,

    results: Option<ResultsInfo>,
    return_index: Option<u32>,
    index_for_local: HashMap<LocalId, CoreLocalId>,
    index_for_expr: HashMap<ExpressionId, CoreLocalId>,
    #[allow(dead_code)]
    local_space: Vec<enc::ValType>,
}
pub struct CoreLocalId(u32);

impl From<u32> for CoreLocalId {
    fn from(value: u32) -> Self {
        CoreLocalId(value)
    }
}

impl<'gen> CodeGenerator<'gen> {
    pub fn new(
        parent: &'gen mut ModuleGenerator,
        comp: &'gen ResolvedComponent,
        func_gen: FunctionGenerator,
        id: FunctionId,
        realloc: ModuleFunctionIndex,
    ) -> Result<Self, GenerationError> {
        let func = &comp.funcs[&id];
        let func_ast = &comp.component.functions[id];

        // Layout parameters
        let FunctionGenerator {
            spill_params,
            params,
            flat_params,
            results,
        } = func_gen;
        let mut local_space = flat_params;

        let locals_start = local_space.len();

        let return_index = results
            .as_ref()
            .map(|info| {
                if info.spill.spill() {
                    let index = local_space.len();
                    local_space.push(enc::ValType::I32);
                    Some(index as u32)
                } else {
                    None
                }
            })
            .flatten();

        // Layout locals
        let mut index_for_local = HashMap::new();
        let mut locals = Vec::with_capacity(func.locals.len());
        for (id, _local) in func.locals.iter() {
            let rtype = func.local_type(id, &comp.component)?;
            let local_id = CoreLocalId((local_space.len() + locals.len()) as u32);
            index_for_local.insert(id, local_id);
            rtype.append_flattened(&comp.component, &mut locals);
        }
        local_space.extend(locals);

        // Layout expressions
        let mut index_for_expr = HashMap::new();
        let mut allocator =
            ExpressionAllocator::new(comp, func, &mut local_space, &mut index_for_expr);
        let function = &comp.component.functions[id];
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
            let align = result_type.align(&comp.component);
            let align = 2u32.pow(align);
            builder.instruction(&enc::Instruction::I32Const(align as i32));
            // new size
            let size = result_type.mem_size(&comp.component);
            builder.instruction(&enc::Instruction::I32Const(size as i32));
            // call allocator
            builder.instruction(&enc::Instruction::Call(realloc.into()));
            // store address
            builder.instruction(&enc::Instruction::LocalSet(return_index));
        }

        Ok(Self {
            parent,
            comp,
            func,
            func_ast,
            builder,
            realloc,
            spill_params,
            params,
            results,
            return_index,
            index_for_local,
            index_for_expr,
            local_space,
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
            .func
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
        let mut fields = rtype.fields(&self.comp.component);
        assert_eq!(
            fields.len(),
            1,
            "Expected expression to only have one field"
        );
        Ok(fields.remove(0))
    }

    pub fn fields(&self, expression: ExpressionId) -> Result<Vec<FieldInfo>, GenerationError> {
        let rtype = self.expression_type(expression)?;
        Ok(rtype.fields(&self.comp.component))
    }

    pub fn lookup_name(&self, ident: NameId) -> ItemId {
        self.func.bindings[&ident]
    }

    pub fn lookup_name_str(&self, ident: NameId) -> &str {
        self.comp.component.get_name(ident)
    }

    pub fn lookup_import_type(&self, id: ImportTypeId) -> &ImportType {
        &self.comp.imports.types[id]
    }

    pub fn spill_return(&self) -> bool {
        self.results.as_ref().map(|r| r.spill()).unwrap_or(false)
    }

    pub fn allocate(&mut self) -> Result<(), GenerationError> {
        self.instruction(&enc::Instruction::Call(self.realloc.into()));
        Ok(())
    }

    pub fn encode_call(&mut self, item: ItemId) -> Result<(), GenerationError> {
        let index = match item {
            ItemId::ImportFunc(import) => self.parent.import_func_idx(import),
            ItemId::Function(function) => self.parent.func_func_idx(function),
            _ => panic!(""),
        };
        self.instruction(&enc::Instruction::Call(index.into()));
        Ok(())
    }

    pub fn read_param_field(&mut self, param: ParamId, field: &FieldInfo) {
        let param_info = &self.params[param.index()];
        if self.spill_params {
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
        self.parent.module.data(data)
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
        for statement in self.func_ast.body.iter() {
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
        rtype.append_flattened(&self.comp.component, self.local_space);
        Ok(())
    }

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
