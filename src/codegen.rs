use crate::{
    ast::{self, ExpressionId, FunctionId, ImportId, StatementId, TypeId}, context::{WithContext, C}, resolver::{FunctionResolver, ItemId, ResolvedComponent, ResolvedType, ResolverError}
};
use miette::Diagnostic;
use thiserror::Error;

use cranelift_entity::EntityRef;
use enc::ModuleArg;
use wasm_encoder as enc;
use wasm_encoder::Instruction;

const MODULE_ALLOCATOR_IDX: u32 = 0;
const MODULE_CODE_IDX: u32 = 1;

const MODULE_INSTANCE_IMPORTS_IDX: u32 = 0;
const MODULE_INSTANCE_ALLOCATOR_IDX: u32 = 1;
const MODULE_INSTANCE_CODE_IDX: u32 = 2;

const MODULE_FUNC_REALLOC: u32 = 0;

#[derive(Default)]
pub struct CodeGenerator {
    module: ModuleBuilder,
    component: ComponentBuilder,
}

#[derive(Error, Debug, Diagnostic)]
pub enum GenerationError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Resolver(#[from] ResolverError)
}

/// Module Index Spaces
/// - types: imports, funcs
/// - functions: imports, funcs
#[derive(Default)]
pub struct ModuleBuilder {
    types: enc::TypeSection,

    // The module function index and  for an import is that imports index
    imports: enc::ImportSection,
    // The module function index for a function is that functions index
    // plus the number of total imports
    funcs: enc::FunctionSection,

    globals: enc::GlobalSection,
    exports: enc::ExportSection,
    code: enc::CodeSection,
}

#[derive(Default)]
pub struct ComponentBuilder {
    alias: enc::ComponentAliasSection,
    types: enc::ComponentTypeSection,
    imports: enc::ComponentImportSection,
    lower_funcs: enc::CanonicalFunctionSection,
    lift_funcs: enc::CanonicalFunctionSection,
    exports: enc::ComponentExportSection,
    instantiate_args: Vec<(String, enc::ExportKind, u32)>,
}

impl CodeGenerator {
    pub fn generate(mut self, resolved_comp: &ResolvedComponent) -> Result<Vec<u8>, GenerationError> {
        self.encode_globals(resolved_comp);

        self.encode_import_allocator();

        for (id, import) in resolved_comp.component.imports.iter() {
            self.encode_import(id, import, resolved_comp);
        }

        for (id, function) in resolved_comp.component.functions.iter() {
            self.encode_func(id, function, resolved_comp)?;
        }

        Ok(self.emit_bytes())
    }

    fn encode_import_allocator(&mut self) {
        let mem_type = enc::MemoryType { minimum: 1, maximum: None, memory64: false, shared: false };
        let mem_ty = enc::EntityType::Memory(mem_type);
        self.module.imports.import("alloc", "memory", mem_ty);

        self.module.types.function(vec![enc::ValType::I32; 4], vec![enc::ValType::I32; 1]);
        let fn_type = enc::EntityType::Function(MODULE_FUNC_REALLOC);
        self.module.imports.import("alloc", "realloc", fn_type);
    }

    fn encode_import(
        &mut self,
        id: ImportId,
        import: &ast::Import,
        resolved_comp: &ResolvedComponent,
    ) {
        let import_name = resolved_comp.component.get_name(import.ident);

        let comp = &resolved_comp.component;
        match &import.external_type {
            ast::ExternalType::Function(fn_type) => {
                // Encode Module Type and Import
                self.encode_mod_import_type(fn_type, comp);
                let module_ty = enc::EntityType::Function(id.to_inner_core_idx());
                self.module.imports.import("claw", import_name, module_ty);

                // Encode Component Type and Import
                self.encode_comp_import_type(fn_type, comp);
                let component_ty = enc::ComponentTypeRef::Func(id.to_comp_idx());
                self.component.imports.import(import_name, component_ty);

                // Lower the Import
                self.component.lower_funcs.lower(id.to_outer_core_idx(), []);

                self.component.instantiate_args.push((
                    import_name.to_owned(),
                    enc::ExportKind::Func,
                    id.to_outer_core_idx(),
                ));
            }
        }
    }

    fn encode_globals(&mut self, component: &ResolvedComponent) {
        let comp = &component.component;

        for (id, global) in component.component.globals.iter() {
            let global_type = enc::GlobalType {
                mutable: global.mutable,
                val_type: global.type_id.with(comp).to_valtype(),
            };

            let init_expr = if let Some(init_value) = component.global_vals.get(&id) {
                let valtype = component.component.get_type(global.type_id);
                match valtype {
                    ast::ValType::Result { .. } => todo!(),
                    ast::ValType::String => todo!(),
                    ast::ValType::Primitive(p) => init_value.to_const_expr(*p),
                }
            } else {
                panic!("Cannot generate WASM for unresolved global")
            };

            self.module.globals.global(global_type, &init_expr);
        }
    }

    fn encode_func(
        &mut self,
        id: FunctionId,
        function: &ast::Function,
        context: &ResolvedComponent,
    ) -> Result<(), GenerationError> {
        let comp = &context.component;
        // Encode module and component type sections
        self.encode_mod_func_type(&function.signature.fn_type, comp);

        // Encode module function
        self.module.funcs.function(id.with(context).to_inner_core_idx());

        // Encode module code
        let resolver = context.resolved_funcs.get(&id).unwrap();
        let locals = encode_locals(resolver, context);
        let mut builder = enc::Function::new(locals);

        for statement in function.body.iter() {
            encode_statement(&context, *statement, id, &mut builder)?;
        }
        builder.instruction(&Instruction::End);

        self.module.code.function(&builder);

        if function.exported {
            self.encode_func_export(id, function, context);
        }
        Ok(())
    }

    fn encode_func_export(
        &mut self,
        id: FunctionId,
        function: &ast::Function,
        context: &ResolvedComponent,
    ) {
        let comp = &context.component;
        let ident = function.signature.ident;
        let name = context.component.get_name(ident);

        // Export function from module
        self.module
            .exports
            .export(name, enc::ExportKind::Func, id.with(context).to_inner_core_idx());
        // Alias module instance export into component
        self.component.alias.alias(enc::Alias::CoreInstanceExport {
            instance: MODULE_INSTANCE_CODE_IDX,
            kind: enc::ExportKind::Func,
            name,
        });
        // Encode component func type
        self.encode_comp_func_type(&function.signature.fn_type, comp);
        // Lift aliased function to component function
        const NO_CANON_OPTS: [enc::CanonicalOption; 0] = [];
        self.component
            .lift_funcs
            .lift(
                id.with(context).to_outer_core_idx(),
                id.with(context).to_outer_core_idx(),
                NO_CANON_OPTS
            );
        // Export component function
        self.component.exports.export(
            name,
            enc::ComponentExportKind::Func,
            id.with(context).to_comp_idx(),
            Some(enc::ComponentTypeRef::Func(id.with(context).to_comp_idx())),
        );
    }

    fn emit_bytes(self) -> Vec<u8> {
        let mut code_module = enc::Module::new();

        // Combine module sections in order
        code_module.section(&self.module.types);
        code_module.section(&self.module.imports);
        code_module.section(&self.module.funcs);
        code_module.section(&self.module.globals);
        code_module.section(&self.module.exports);
        code_module.section(&self.module.code);

        let mut component = enc::Component::new();

        // Component Types
        component.section(&self.component.types);
        // Component Imports
        component.section(&self.component.imports);
        // Lower Imported Functions
        component.section(&self.component.lower_funcs);

        // Modules
        component.section(&enc::RawSection {
            id: enc::ComponentSectionId::CoreModule.into(),
            data: gen_allocator().as_slice()
        });
        component.section(&enc::ModuleSection(&code_module));

        // Module Instances
        let mut module_instances = enc::InstanceSection::new();
        // Imports Module Instance
        module_instances.export_items(self.component.instantiate_args);
        // Allocator Module Instance
        let args: [(String, _); 0] = [];
        module_instances.instantiate(MODULE_ALLOCATOR_IDX, args);
        // Code Module Instance
        let args = [
            ("claw", ModuleArg::Instance(MODULE_INSTANCE_IMPORTS_IDX)),
            ("alloc", ModuleArg::Instance(MODULE_INSTANCE_ALLOCATOR_IDX)),
        ];
        module_instances.instantiate(MODULE_CODE_IDX, args);
        component.section(&module_instances);

        // Alias module exports
        component.section(&self.component.alias);
        // Lift component functions
        component.section(&self.component.lift_funcs);
        // Export component functions
        component.section(&self.component.exports);

        component.finish()
    }

    fn encode_mod_import_type(&mut self, fn_type: &ast::FnType, comp: &ast::Component) {
        let params = fn_type
            .arguments
            .iter()
            .map(|(_name, valtype)| valtype.with(comp).to_valtype());

        let result_type = fn_type.return_type.with(comp).to_valtype();
        self.module.types.function(params, [result_type]);
    }

    fn encode_comp_import_type(&mut self, fn_type: &ast::FnType, comp: &ast::Component) {
        let params = fn_type.arguments.iter().map(|(name, type_id)| {
            let name = comp.get_name(*name);
            let valtype = comp.get_type(*type_id);
            (name, valtype.with(comp).to_comp_valtype())
        });
        let valtype = comp.get_type(fn_type.return_type);
        let result_type = valtype.with(comp).to_comp_valtype();
        self.component
            .types
            .function()
            .params(params)
            .result(result_type);
    }

    fn encode_mod_func_type(&mut self, fn_type: &ast::FnType, comp: &ast::Component) {
        let params = fn_type
            .arguments
            .iter()
            .map(|(_name, type_id)| type_id.with(comp).to_valtype());

        let result_type = fn_type.return_type.with(comp).to_valtype();
        self.module.types.function(params, [result_type]);
    }

    fn encode_comp_func_type(&mut self, fn_type: &ast::FnType, comp: &ast::Component) {
        let params = fn_type.arguments.iter().map(|(name, type_id)| {
            let name = comp.get_name(*name);
            let valtype = comp.get_type(*type_id);
            (name, valtype.with(comp).to_comp_valtype())
        });
        let valtype = comp.get_type(fn_type.return_type);
        let result_type = valtype.with(comp).to_comp_valtype();
        self.component
            .types
            .function()
            .params(params)
            .result(result_type);
    }
}

impl ImportId {
    fn to_inner_core_idx(&self) -> u32 {
        let alloc_offset = 1 as u32;
        let func_offset = self.index() as u32;
        alloc_offset + func_offset
    }

    fn to_outer_core_idx(&self) -> u32 {
        let func_offset = self.index() as u32;
        func_offset
    }

    fn to_comp_idx(&self) -> u32 {
        let func_offset = self.index() as u32;
        func_offset
    }
}

impl<'ctx> C<'ctx, FunctionId, ResolvedComponent> {
    fn to_inner_core_idx(&self) -> u32 {
        let alloc_offset = 1 as u32;
        let import_offset = self.context.component.imports.len() as u32;
        let func_offset = self.value.index() as u32;
        alloc_offset + import_offset + func_offset
    }

    fn to_outer_core_idx(&self) -> u32 {
        let import_offset = self.context.component.imports.len() as u32;
        let func_offset = self.value.index() as u32;
        import_offset + func_offset
    }

    fn to_comp_idx(&self) -> u32 {
        let import_offset = self.context.component.imports.len() as u32;
        let func_offset = self.value.index() as u32;
        import_offset + func_offset
    }
}

fn encode_locals(
    resolver: &FunctionResolver,
    resolved_comp: &ResolvedComponent,
) -> Vec<(u32, enc::ValType)> {
    resolver
        .locals
        .iter()
        .map(|(id, _local)| {
            let rtype = *resolver.local_types.get(&id).unwrap();
            (1, rtype.with(&resolved_comp.component).to_valtype())
        })
        .collect()
}

fn encode_statement(
    component: &ResolvedComponent,
    statement: StatementId,
    func: FunctionId,
    builder: &mut enc::Function,
) -> Result<(), GenerationError> {
    let resolver = component.resolved_funcs.get(&func).unwrap();

    match component.component.get_statement(statement) {
        ast::Statement::Let(ast::Let {
            ident, expression, ..
        })
        | ast::Statement::Assign(ast::Assign {
            ident, expression, ..
        }) => {
            encode_expression(component, *expression, func, builder)?;
            match resolver.bindings.get(&ident).unwrap() {
                ItemId::Import(_) => unimplemented!(),
                ItemId::Global(global) => {
                    builder.instruction(&Instruction::GlobalSet(global.index() as u32));
                }
                ItemId::Param(param) => {
                    let local_index = param.index() as u32;
                    builder.instruction(&Instruction::LocalSet(local_index));
                }
                ItemId::Local(local) => {
                    let func = component.component.functions.get(func).unwrap();
                    let local_index = local.index() + func.signature.fn_type.arguments.len();
                    let local_index = local_index as u32;
                    builder.instruction(&Instruction::LocalSet(local_index));
                }
                ItemId::Function(_) => unimplemented!(),
            }
        }
        ast::Statement::Call(call) => {
            for arg in call.args.iter() {
                encode_expression(component, *arg, func, builder)?;
            }
            let index = match resolver.bindings.get(&call.ident).unwrap() {
                ItemId::Import(import) => import.to_inner_core_idx(),
                ItemId::Function(function) => function.with(component).to_inner_core_idx(),
                _ => panic!(""),
            };
            builder.instruction(&Instruction::Call(index as u32));
        }
        ast::Statement::If(ast::If { condition, block }) => {
            encode_expression(component, *condition, func, builder)?;
            builder.instruction(&Instruction::If(enc::BlockType::Empty));
            for statement in block.iter() {
                encode_statement(component, *statement, func, builder)?;
            }
            builder.instruction(&Instruction::End);
        }
        ast::Statement::Return(ast::Return { expression }) => {
            encode_expression(component, *expression, func, builder)?;
            builder.instruction(&Instruction::Return);
        }
    };
    Ok(())
}

/// A simple helper that calls EncodeExpression::encode
fn encode_expression(
    component: &ResolvedComponent,
    expression: ExpressionId,
    func: FunctionId,
    builder: &mut enc::Function,
) -> Result<(), GenerationError> {
    let expr = component.component.expr().get_exp(expression);
    expr.encode(component, expression, func, builder)?;
    Ok(())
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum Signedness {
    Unsigned,
    Signed,
    NotApplicable,
}

const S: Signedness = Signedness::Signed;
const U: Signedness = Signedness::Unsigned;

impl ast::PrimitiveType {
    fn core_type(&self) -> enc::ValType {
        use ast::PrimitiveType;

        match self {
            PrimitiveType::Bool => enc::ValType::I32,

            PrimitiveType::U64 | PrimitiveType::S64 => enc::ValType::I64,

            PrimitiveType::U32
            | PrimitiveType::U16
            | PrimitiveType::U8
            | PrimitiveType::S32
            | PrimitiveType::S16
            | PrimitiveType::S8 => enc::ValType::I32,

            PrimitiveType::F32 => enc::ValType::F32,
            PrimitiveType::F64 => enc::ValType::F64,
        }
    }

    fn signedness(&self) -> Signedness {
        use ast::PrimitiveType;

        match self {
            PrimitiveType::U64 | PrimitiveType::U32 | PrimitiveType::U16 | PrimitiveType::U8 => {
                Signedness::Unsigned
            }
            PrimitiveType::S64 | PrimitiveType::S32 | PrimitiveType::S16 | PrimitiveType::S8 => {
                Signedness::Signed
            }
            _ => Signedness::NotApplicable,
        }
    }
}

// Literal

impl ast::Literal {
    fn to_const_expr(&self, ptype: ast::PrimitiveType) -> enc::ConstExpr {
        use ast::{Literal, PrimitiveType};
        match (ptype, self) {
            (PrimitiveType::S32 | PrimitiveType::U32, Literal::Integer(value)) => {
                enc::ConstExpr::i32_const(*value as i32)
            }
            (PrimitiveType::S64 | PrimitiveType::U64, Literal::Integer(value)) => {
                enc::ConstExpr::i64_const(*value as i64)
            }
            (PrimitiveType::F32, Literal::Float(value)) => enc::ConstExpr::f32_const(*value as f32),
            (PrimitiveType::F64, Literal::Float(value)) => enc::ConstExpr::f64_const(*value),
            _ => todo!(),
        }
    }
}

// ResolvedType

#[allow(dead_code)]
impl<'ctx> C<'ctx, ResolvedType, ast::Component> {
    fn to_valtype(&self) -> enc::ValType {
        match self.value {
            ResolvedType::Primitive(p) => p.to_valtype(),
            ResolvedType::ValType(type_id) => type_id.with(self.context).to_valtype(),
        }
    }

    fn to_comp_valtype(&self) -> enc::ComponentValType {
        match self.value {
            ResolvedType::Primitive(p) => p.to_comp_valtype(),
            ResolvedType::ValType(t) => t.with(self.context).to_comp_valtype(),
        }
    }
}

// TypeId

#[allow(dead_code)]
impl<'ctx> C<'ctx, TypeId, ast::Component> {
    fn to_valtype(&self) -> enc::ValType {
        let valtype = self.context.get_type(*self.value);
        valtype.with(self.context).to_valtype()
    }

    fn to_comp_valtype(&self) -> enc::ComponentValType {
        let valtype = self.context.get_type(*self.value);
        valtype.with(self.context).to_comp_valtype()
    }
}

// ast::ValType

impl<'ctx> C<'ctx, ast::ValType, ast::Component> {
    fn to_valtype(&self) -> enc::ValType {
        match self.value {
            ast::ValType::Primitive(p) => p.to_valtype(),
            _ => panic!("Cannot encode non-primitive as a valtype"),
        }
    }

    fn to_comp_valtype(&self) -> enc::ComponentValType {
        match self.value {
            ast::ValType::Result { .. } => todo!(),
            ast::ValType::String => todo!(),
            ast::ValType::Primitive(p) => p.to_comp_valtype(),
        }
    }
}

// PrimitiveType

impl ast::PrimitiveType {
    fn to_valtype(&self) -> enc::ValType {
        use ast::PrimitiveType;
        match self {
            PrimitiveType::U32
            | PrimitiveType::S32
            | PrimitiveType::U16
            | PrimitiveType::S16
            | PrimitiveType::U8
            | PrimitiveType::S8
            | PrimitiveType::Bool => enc::ValType::I32,

            PrimitiveType::U64 | PrimitiveType::S64 => enc::ValType::I64,

            PrimitiveType::F32 => enc::ValType::F32,
            PrimitiveType::F64 => enc::ValType::F64,
        }
    }

    fn to_primitive_valtype(&self) -> enc::PrimitiveValType {
        use ast::PrimitiveType;
        match self {
            PrimitiveType::U64 => enc::PrimitiveValType::U64,
            PrimitiveType::U32 => enc::PrimitiveValType::U32,
            PrimitiveType::U16 => enc::PrimitiveValType::U16,
            PrimitiveType::U8 => enc::PrimitiveValType::U8,
            PrimitiveType::S64 => enc::PrimitiveValType::S64,
            PrimitiveType::S32 => enc::PrimitiveValType::S32,
            PrimitiveType::S16 => enc::PrimitiveValType::S16,
            PrimitiveType::S8 => enc::PrimitiveValType::S8,
            PrimitiveType::F32 => enc::PrimitiveValType::Float32,
            PrimitiveType::F64 => enc::PrimitiveValType::Float64,
            PrimitiveType::Bool => enc::PrimitiveValType::Bool,
        }
    }

    fn to_comp_valtype(&self) -> enc::ComponentValType {
        enc::ComponentValType::Primitive(self.to_primitive_valtype())
    }
}

// ValType

#[allow(dead_code)]
impl ast::ValType {
    fn to_comp_valtype(&self) -> enc::ComponentValType {
        match self {
            ast::ValType::Result { .. } => todo!(),
            ast::ValType::String => todo!(),
            ast::ValType::Primitive(p) => p.to_comp_valtype(),
        }
    }
}

//

trait EncodeExpression {
    fn encode(
        &self,
        component: &ResolvedComponent,
        expression: ExpressionId,
        func: FunctionId,
        builder: &mut enc::Function,
    ) -> Result<(), GenerationError>;
}

impl EncodeExpression for ast::Expression {
    fn encode(
        &self,
        component: &ResolvedComponent,
        expression: ExpressionId,
        func: FunctionId,
        builder: &mut enc::Function,
    ) -> Result<(), GenerationError> {
        let expr: &dyn EncodeExpression = match self {
            ast::Expression::Identifier(expr) => expr,
            ast::Expression::Literal(expr) => expr,
            ast::Expression::Call(expr) => expr,
            ast::Expression::Unary(expr) => expr,
            ast::Expression::Binary(expr) => expr,
        };
        expr.encode(component, expression, func, builder)?;
        Ok(())
    }
}

impl EncodeExpression for ast::Identifier {
    fn encode(
        &self,
        component: &ResolvedComponent,
        _expression: ExpressionId,
        func: FunctionId,
        builder: &mut enc::Function,
    ) -> Result<(), GenerationError> {
        let resolver = component.resolved_funcs.get(&func).unwrap();
        match resolver.bindings.get(&self.ident).unwrap() {
            ItemId::Import(_) => unimplemented!(),
            ItemId::Global(global) => {
                builder.instruction(&Instruction::GlobalGet(global.index() as u32));
            }
            ItemId::Param(param) => {
                let local_index = param.index();
                builder.instruction(&Instruction::LocalGet(local_index as u32));
            }
            ItemId::Local(local) => {
                let func = component.component.functions.get(func).unwrap();
                let local_index = local.index() + func.signature.fn_type.arguments.len();
                builder.instruction(&Instruction::LocalGet(local_index as u32));
            }
            ItemId::Function(_) => unimplemented!(),
        }
        Ok(())
    }
}

impl EncodeExpression for ast::Literal {
    fn encode(
        &self,
        component: &ResolvedComponent,
        expression: ExpressionId,
        func: FunctionId,
        builder: &mut enc::Function,
    ) -> Result<(), GenerationError> {
        let comp = &component.component;
        let resolver = component.resolved_funcs.get(&func).unwrap();

        let rtype = resolver.get_resolved_type(expression, comp)?;
        let valtype = rtype.with(comp).to_valtype();

        use ast::Literal;
        let instruction = match (valtype, self) {
            (enc::ValType::I32, Literal::Integer(value)) => Instruction::I32Const(*value as i32),
            (enc::ValType::I64, Literal::Integer(value)) => Instruction::I64Const(*value as i64),
            (enc::ValType::F32, Literal::Float(value)) => Instruction::F32Const(*value as f32),
            (enc::ValType::F64, Literal::Float(value)) => Instruction::F64Const(*value),
            _ => todo!(),
        };
        builder.instruction(&instruction);
        Ok(())
    }
}

impl EncodeExpression for ast::Call {
    fn encode(
        &self,
        component: &ResolvedComponent,
        _expression: ExpressionId,
        func: FunctionId,
        builder: &mut enc::Function,
    ) -> Result<(), GenerationError> {
        for arg in self.args.iter() {
            encode_expression(component, *arg, func, builder)?;
        }
        let resolver = component.resolved_funcs.get(&func).unwrap();
        let index = match resolver.bindings.get(&self.ident).unwrap() {
            ItemId::Import(import) => import.to_inner_core_idx(),
            ItemId::Function(function) => function.with(component).to_inner_core_idx(),
            _ => panic!(""),
        };
        builder.instruction(&Instruction::Call(index as u32));
        Ok(())
    }
}

impl EncodeExpression for ast::UnaryExpression {
    fn encode(
        &self,
        component: &ResolvedComponent,
        _expression: ExpressionId,
        func: FunctionId,
        builder: &mut enc::Function,
    ) -> Result<(), GenerationError> {
        builder.instruction(&enc::Instruction::I32Const(0));
        encode_expression(component, self.inner, func, builder)?;
        builder.instruction(&enc::Instruction::I32Sub);
        Ok(())
    }
}


impl EncodeExpression for ast::BinaryExpression {
    fn encode(
        &self,
        component: &ResolvedComponent,
        _expression: ExpressionId,
        func: FunctionId,
        builder: &mut enc::Function,
    ) -> Result<(), GenerationError> {
        let comp = &component.component;
        encode_expression(component, self.left, func, builder)?;
        encode_expression(component, self.right, func, builder)?;

        let resolver = component.resolved_funcs.get(&func).unwrap();
        let rtype = resolver.get_resolved_type(self.left, comp)?;

        let p = rtype.with(&component.component).as_primitive().unwrap();

        let instruction = match (self.op, p.core_type(), p.signedness()) {
            // Multiply
            (ast::BinaryOp::Multiply, enc::ValType::I32, _) => enc::Instruction::I32Mul,
            (ast::BinaryOp::Multiply, enc::ValType::I64, _) => enc::Instruction::I64Mul,
            (ast::BinaryOp::Multiply, enc::ValType::F32, _) => enc::Instruction::F32Mul,
            (ast::BinaryOp::Multiply, enc::ValType::F64, _) => enc::Instruction::F64Mul,
            // Divide
            (ast::BinaryOp::Divide, enc::ValType::I32, S) => enc::Instruction::I32DivS,
            (ast::BinaryOp::Divide, enc::ValType::I32, U) => enc::Instruction::I32DivU,
            (ast::BinaryOp::Divide, enc::ValType::I64, S) => enc::Instruction::I64DivS,
            (ast::BinaryOp::Divide, enc::ValType::I64, U) => enc::Instruction::I64DivU,
            (ast::BinaryOp::Divide, enc::ValType::F32, _) => enc::Instruction::F32Div,
            (ast::BinaryOp::Divide, enc::ValType::F64, _) => enc::Instruction::F64Div,
            // Modulo
            (ast::BinaryOp::Modulo, enc::ValType::I32, S) => enc::Instruction::I32RemS,
            (ast::BinaryOp::Modulo, enc::ValType::I32, U) => enc::Instruction::I32RemU,
            (ast::BinaryOp::Modulo, enc::ValType::I64, S) => enc::Instruction::I64RemS,
            (ast::BinaryOp::Modulo, enc::ValType::I64, U) => enc::Instruction::I64RemU,
            // Addition
            (ast::BinaryOp::Add, enc::ValType::I32, _) => enc::Instruction::I32Add,
            (ast::BinaryOp::Add, enc::ValType::I64, _) => enc::Instruction::I64Add,
            (ast::BinaryOp::Add, enc::ValType::F32, _) => enc::Instruction::F32Add,
            (ast::BinaryOp::Add, enc::ValType::F64, _) => enc::Instruction::F64Add,
            // Subtraction
            (ast::BinaryOp::Subtract, enc::ValType::I32, _) => enc::Instruction::I32Sub,
            (ast::BinaryOp::Subtract, enc::ValType::I64, _) => enc::Instruction::I64Sub,
            (ast::BinaryOp::Subtract, enc::ValType::F32, _) => enc::Instruction::F32Sub,
            (ast::BinaryOp::Subtract, enc::ValType::F64, _) => enc::Instruction::F64Sub,
            // Logical Bit Shifting
            (ast::BinaryOp::BitShiftL, enc::ValType::I32, _) => enc::Instruction::I32Shl,
            (ast::BinaryOp::BitShiftL, enc::ValType::I64, _) => enc::Instruction::I64Shl,
            (ast::BinaryOp::BitShiftR, enc::ValType::I32, _) => enc::Instruction::I32ShrU,
            (ast::BinaryOp::BitShiftR, enc::ValType::I64, _) => enc::Instruction::I64ShrU,
            // Arithmetic Bit Shifting
            (ast::BinaryOp::ArithShiftR, enc::ValType::I32, S) => enc::Instruction::I32ShrS,
            (ast::BinaryOp::ArithShiftR, enc::ValType::I32, U) => enc::Instruction::I32ShrU,
            (ast::BinaryOp::ArithShiftR, enc::ValType::I64, S) => enc::Instruction::I64ShrS,
            (ast::BinaryOp::ArithShiftR, enc::ValType::I64, U) => enc::Instruction::I64ShrU,
            // Less than
            (ast::BinaryOp::LessThan, enc::ValType::I32, S) => enc::Instruction::I32LtS,
            (ast::BinaryOp::LessThan, enc::ValType::I32, U) => enc::Instruction::I32LtU,
            (ast::BinaryOp::LessThan, enc::ValType::I64, S) => enc::Instruction::I64LtS,
            (ast::BinaryOp::LessThan, enc::ValType::I64, U) => enc::Instruction::I64LtU,
            (ast::BinaryOp::LessThan, enc::ValType::F32, _) => enc::Instruction::F32Lt,
            (ast::BinaryOp::LessThan, enc::ValType::F64, _) => enc::Instruction::F64Lt,
            (ast::BinaryOp::LessThan, valtype, s) => panic!("Failed to encode '<' for type {:?} and signedness {:?}", valtype, s),
            // Less than equal
            (ast::BinaryOp::LessThanEqual, enc::ValType::I32, S) => enc::Instruction::I32LeS,
            (ast::BinaryOp::LessThanEqual, enc::ValType::I32, U) => enc::Instruction::I32LeU,
            (ast::BinaryOp::LessThanEqual, enc::ValType::I64, S) => enc::Instruction::I64LeS,
            (ast::BinaryOp::LessThanEqual, enc::ValType::I64, U) => enc::Instruction::I64LeU,
            (ast::BinaryOp::LessThanEqual, enc::ValType::F32, _) => enc::Instruction::F32Le,
            (ast::BinaryOp::LessThanEqual, enc::ValType::F64, _) => enc::Instruction::F64Le,
            // Greater than
            (ast::BinaryOp::GreaterThan, enc::ValType::I32, S) => enc::Instruction::I32GtS,
            (ast::BinaryOp::GreaterThan, enc::ValType::I32, U) => enc::Instruction::I32GtU,
            (ast::BinaryOp::GreaterThan, enc::ValType::I64, S) => enc::Instruction::I64GtS,
            (ast::BinaryOp::GreaterThan, enc::ValType::I64, U) => enc::Instruction::I64GtU,
            (ast::BinaryOp::GreaterThan, enc::ValType::F32, _) => enc::Instruction::F32Gt,
            (ast::BinaryOp::GreaterThan, enc::ValType::F64, _) => enc::Instruction::F64Gt,
            // Greater than or equal
            (ast::BinaryOp::GreaterThanEqual, enc::ValType::I32, S) => enc::Instruction::I32GeS,
            (ast::BinaryOp::GreaterThanEqual, enc::ValType::I32, U) => enc::Instruction::I32GeU,
            (ast::BinaryOp::GreaterThanEqual, enc::ValType::I64, S) => enc::Instruction::I64GeS,
            (ast::BinaryOp::GreaterThanEqual, enc::ValType::I64, U) => enc::Instruction::I64GeU,
            (ast::BinaryOp::GreaterThanEqual, enc::ValType::F32, _) => enc::Instruction::F32Ge,
            (ast::BinaryOp::GreaterThanEqual, enc::ValType::F64, _) => enc::Instruction::F64Ge,
            // Equal
            (ast::BinaryOp::Equals, enc::ValType::I32, _) => enc::Instruction::I32Eq,
            (ast::BinaryOp::Equals, enc::ValType::I64, _) => enc::Instruction::I64Eq,
            (ast::BinaryOp::Equals, enc::ValType::F32, _) => enc::Instruction::F32Eq,
            (ast::BinaryOp::Equals, enc::ValType::F64, _) => enc::Instruction::F64Eq,
            // Not equal
            (ast::BinaryOp::NotEquals, enc::ValType::I32, _) => enc::Instruction::I32Eq,
            (ast::BinaryOp::NotEquals, enc::ValType::I64, _) => enc::Instruction::I64Eq,
            (ast::BinaryOp::NotEquals, enc::ValType::F32, _) => enc::Instruction::F32Eq,
            (ast::BinaryOp::NotEquals, enc::ValType::F64, _) => enc::Instruction::F64Eq,
            // Bitwise and
            (ast::BinaryOp::BitAnd, enc::ValType::I32, _) => enc::Instruction::I32And,
            (ast::BinaryOp::BitAnd, enc::ValType::I64, _) => enc::Instruction::I64And,
            // Bitwise xor
            (ast::BinaryOp::BitXor, enc::ValType::I32, _) => enc::Instruction::I32Xor,
            (ast::BinaryOp::BitXor, enc::ValType::I64, _) => enc::Instruction::I64Xor,
            // Bitwise or
            (ast::BinaryOp::BitOr, enc::ValType::I32, _) => enc::Instruction::I32Or,
            (ast::BinaryOp::BitOr, enc::ValType::I64, _) => enc::Instruction::I64Or,
            // Logical and/or
            (ast::BinaryOp::LogicalAnd, enc::ValType::I32, _) => enc::Instruction::I32And,
            (ast::BinaryOp::LogicalOr, enc::ValType::I32, _) => enc::Instruction::I32Or,
            // Fallback
            (operator, valtype, _) => panic!("Cannot apply binary operator {:?} to type {:?}", operator, valtype)
        };
        builder.instruction(&instruction);
        Ok(())
    }
}

pub fn gen_allocator() -> Vec<u8> {
    let wat = include_str!("../allocator.wat");
    wat::parse_str(wat).unwrap()
}