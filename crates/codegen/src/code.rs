use std::collections::HashMap;

use ast::{ExpressionId, FunctionId, NameId, StatementId};
use claw_ast as ast;

use crate::{
    function::{FunctionGenerator, ParamInfo, ReturnInfo},
    types::{ptype_mem_arg, EncodeType, FieldInfo},
    ComponentGenerator, EncodeExpression, EncodeStatement, GenerationError, ModuleFunctionIndex,
};
use claw_resolver::{FunctionResolver, ItemId, LocalId, ParamId, ResolvedComponent, ResolvedType};
use cranelift_entity::EntityRef;
use wasm_encoder as enc;

pub struct CodeGenerator<'gen> {
    id: FunctionId,
    comp: &'gen ResolvedComponent,
    parent: &'gen mut ComponentGenerator,

    builder: enc::Function,

    params: Vec<ParamInfo>,
    return_info: ReturnInfo,
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
        code_gen: &'gen mut ComponentGenerator,
        comp: &'gen ResolvedComponent,
        func_gen: FunctionGenerator,
        realloc: ModuleFunctionIndex,
        id: FunctionId,
    ) -> Result<Self, GenerationError> {
        let resolver = &comp.resolved_funcs[&id];

        // Layout parameters
        let FunctionGenerator {
            params,
            param_types,
            return_type,
        } = func_gen;
        let mut local_space = param_types;

        let locals_start = local_space.len();

        let return_index = if return_type.spill() {
            let index = local_space.len();
            local_space.push(enc::ValType::I32);
            Some(index as u32)
        } else {
            None
        };

        // Layout locals
        let mut index_for_local = HashMap::new();
        let mut locals = Vec::with_capacity(resolver.locals.len());
        for (id, _local) in resolver.locals.iter() {
            let rtype = resolver.get_resolved_local_type(id, &comp.component)?;
            let local_id = CoreLocalId((local_space.len() + locals.len()) as u32);
            index_for_local.insert(id, local_id);
            rtype.append_flattened(&comp.component, &mut locals);
        }
        local_space.extend(locals);

        // Layout expressions
        let resolver = &comp.resolved_funcs[&id];
        let mut index_for_expr = HashMap::new();
        let mut allocator =
            ExpressionAllocator::new(comp, resolver, &mut local_space, &mut index_for_expr);
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

            let result_type = comp.component.functions[id].return_type.unwrap();
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
            id,
            parent: code_gen,
            comp,
            builder,
            params,
            return_info: return_type,
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

    pub fn get_resolved_type(
        &self,
        expression: ExpressionId,
    ) -> Result<ResolvedType, GenerationError> {
        let resolver = &self.comp.resolved_funcs[&self.id];
        let type_id = resolver.get_resolved_type(expression, &self.comp.component)?;
        Ok(type_id)
    }

    pub fn fields(&self, expression: ExpressionId) -> Result<Vec<FieldInfo>, GenerationError> {
        let rtype = self.get_resolved_type(expression)?;
        Ok(rtype.fields(&self.comp.component))
    }

    pub fn lookup_name(&self, ident: NameId) -> ItemId {
        let resolver = &self.comp.resolved_funcs[&self.id];
        resolver.bindings[&ident]
    }

    pub fn spill_return(&self) -> bool {
        self.return_info.spill()
    }

    pub fn encode_call(&mut self, item: ItemId) -> Result<(), GenerationError> {
        let index = match item {
            ItemId::Import(import) => self.parent.func_idx_for_import[&import],
            ItemId::Function(function) => self.parent.func_idx_for_func[&function],
            _ => panic!(""),
        };
        self.instruction(&enc::Instruction::Call(index.into()));
        Ok(())
    }

    pub fn read_param_field(&mut self, param: ParamId, field: &FieldInfo) {
        let param_info = &self.params[param.index()];
        match param_info {
            ParamInfo::Local(local_info) => {
                let local_index = local_info.index_offset + field.index_offset;
                self.local_get(local_index);
            }
            ParamInfo::Spilled(spilled_info) => {
                let mem_index = spilled_info.mem_offset + field.mem_offset;
                self.builder.instruction(&enc::Instruction::LocalGet(0));
                self.builder
                    .instruction(&enc::Instruction::I32Const(mem_index as i32));
                self.builder.instruction(&enc::Instruction::I32Add);
                self.load_ptype(field.ptype);
            }
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
        self.instruction(&enc::Instruction::I32Const(field.mem_offset as i32));
        self.instruction(&enc::Instruction::I32Add);
    }

    /// The value's base memory offset MUST be on the stack before calling this
    pub fn read_mem_field(&mut self, field: &FieldInfo) {
        self.field_address(field);
        self.load_ptype(field.ptype);
    }

    /// Fields absolute offset in memory MUST be on the stack underneath the value before calling this
    pub fn write_mem(&mut self, field: &FieldInfo) {
        self.store_ptype(field.ptype);
    }

    pub fn encode_const_int(&mut self, int: u64, field: &FieldInfo) {
        let instruction = match field.ptype {
            ast::PrimitiveType::U8
            | ast::PrimitiveType::S8
            | ast::PrimitiveType::U16
            | ast::PrimitiveType::S16
            | ast::PrimitiveType::U32
            | ast::PrimitiveType::S32 => enc::Instruction::I32Const(int as i32),

            ast::PrimitiveType::U64 | ast::PrimitiveType::S64 => {
                enc::Instruction::I64Const(int as i64)
            }

            _ => panic!("Not an integer"),
        };
        self.instruction(&instruction);
    }

    pub fn encode_const_float(&mut self, float: f64, field: &FieldInfo) {
        let instruction = match field.ptype {
            ast::PrimitiveType::F32 => enc::Instruction::F32Const(float as f32),
            ast::PrimitiveType::F64 => enc::Instruction::F64Const(float),
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

    fn load_ptype(&mut self, ptype: ast::PrimitiveType) {
        let mem_arg = ptype_mem_arg(ptype);
        let instruction = match ptype {
            // Small types with sign-extending
            ast::PrimitiveType::U8 => enc::Instruction::I32Load8U(mem_arg),
            ast::PrimitiveType::S8 => enc::Instruction::I32Load8S(mem_arg),
            ast::PrimitiveType::U16 => enc::Instruction::I32Load16U(mem_arg),
            ast::PrimitiveType::S16 => enc::Instruction::I32Load16S(mem_arg),
            // 32 and 64 bit values don't need sign-extending
            ast::PrimitiveType::U32 | ast::PrimitiveType::S32 => enc::Instruction::I32Load(mem_arg),
            ast::PrimitiveType::U64 | ast::PrimitiveType::S64 => enc::Instruction::I64Load(mem_arg),
            // Floats
            ast::PrimitiveType::F32 => enc::Instruction::F32Load(mem_arg),
            ast::PrimitiveType::F64 => enc::Instruction::F64Load(mem_arg),
            // Booleans are treated as 8-bit unsigned values
            ast::PrimitiveType::Bool => enc::Instruction::I32Load8U(mem_arg),
        };
        self.builder.instruction(&instruction);
    }

    fn store_ptype(&mut self, ptype: ast::PrimitiveType) {
        let mem_arg = ptype_mem_arg(ptype);
        let instruction = match ptype {
            // All types which fit in i32
            ast::PrimitiveType::Bool
            | ast::PrimitiveType::U8
            | ast::PrimitiveType::S8
            | ast::PrimitiveType::U16
            | ast::PrimitiveType::S16
            | ast::PrimitiveType::U32
            | ast::PrimitiveType::S32 => enc::Instruction::I32Store(mem_arg),
            // Types that use i64
            ast::PrimitiveType::U64 | ast::PrimitiveType::S64 => {
                enc::Instruction::I64Store(mem_arg)
            }
            // Floats
            ast::PrimitiveType::F32 => enc::Instruction::F32Store(mem_arg),
            ast::PrimitiveType::F64 => enc::Instruction::F64Store(mem_arg),
        };
        self.builder.instruction(&instruction);
    }

    pub fn finalize(mut self) -> Result<(), GenerationError> {
        let function = &self.comp.component.functions[self.id];
        for statement in function.body.iter() {
            self.encode_statement(*statement)?;
        }
        self.builder.instruction(&enc::Instruction::End);

        let mod_func_idx = self.parent.func_idx_for_func[&self.id];
        self.parent.module.code(mod_func_idx, self.builder);
        Ok(())
    }
}

pub struct ExpressionAllocator<'a> {
    // Context
    comp: &'a ResolvedComponent,
    resolver: &'a FunctionResolver,
    // State
    local_space: &'a mut Vec<enc::ValType>,
    index_for_expr: &'a mut HashMap<ExpressionId, CoreLocalId>,
}

impl<'a> ExpressionAllocator<'a> {
    pub fn new(
        comp: &'a ResolvedComponent,
        resolver: &'a FunctionResolver,
        local_space: &'a mut Vec<enc::ValType>,
        index_for_expr: &'a mut HashMap<ExpressionId, CoreLocalId>,
    ) -> Self {
        Self {
            comp,
            resolver,
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
            .resolver
            .get_resolved_type(expression, &self.comp.component)?;
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
