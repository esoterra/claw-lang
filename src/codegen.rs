use crate::{
    ast::{
        self, expressions::Literal, types::ValType, ExpressionId, FnType, FunctionId, Import, ImportId
    },
    resolver::{FunctionResolver, ItemId, ResolvedComponent},
};

use cranelift_entity::EntityRef;
use enc::ModuleArg;
use wasm_encoder as enc;

const MODULE_IDX: u32 = 0;

const INLINE_EXPORT_INSTANCE_IDX: u32 = 0;
const MODULE_INSTANCE_IDX: u32 = 1;

#[derive(Default)]
pub struct CodeGenerator {
    module: ModuleBuilder,
    component: ComponentBuilder,
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
    pub fn generate(mut self, resolved_comp: &ResolvedComponent) -> Vec<u8> {
        self.encode_globals(resolved_comp);

        for (id, import) in resolved_comp.component.imports.iter() {
            self.encode_import(id, import);
        }

        for (id, function) in resolved_comp.component.functions.iter() {
            self.encode_func(id, function, resolved_comp);
        }

        self.emit_bytes()
    }

    fn encode_import(&mut self, id: ImportId, import: &Import) {
        let import_func_idx = id.index() as u32;
        let import_name = import.name.as_ref().as_str();

        match &import.external_type {
            ast::ExternalType::Function(fn_type) => {
                // Encode Module Type and Import
                self.encode_mod_import_type(fn_type);
                let module_ty = enc::EntityType::Function(import_func_idx);
                self.module.imports.import("claw", import_name, module_ty);

                // Encode Component Type and Import
                self.encode_comp_import_type(fn_type);
                let component_ty = enc::ComponentTypeRef::Func(import_func_idx);
                self.component.imports.import(import_name, component_ty);

                // Lower the Import
                self.component.lower_funcs.lower(import_func_idx, []);

                self.component.instantiate_args.push((
                    import_name.to_owned(),
                    enc::ExportKind::Func,
                    import_func_idx,
                ));
            }
        }
    }

    fn encode_globals(&mut self, component: &ResolvedComponent) {
        for (id, global) in component.component.globals.iter() {
            let valtype = global.valtype.as_ref();

            let global_type = enc::GlobalType {
                mutable: global.mut_kwd.is_some(),
                val_type: encode_valtype(valtype),
            };

            let init_expr = if let Some(init_value) = component.global_vals.get(&id) {
                init_value.to_constexpr(valtype)
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
        resolved_comp: &ResolvedComponent,
    ) {
        // Encode module and component type sections
        self.encode_mod_func_type(&function.signature.fn_type);

        let func_idx = resolved_comp.component.imports.len() + id.index();
        let func_idx = func_idx as u32;

        // Encode module function
        self.module.funcs.function(func_idx);

        // Encode module code
        let resolver = resolved_comp.resolved_funcs.get(&id).unwrap();
        let locals = encode_locals(resolver);
        let mut builder = enc::Function::new(locals);

        for statement in function.body.as_ref().statements.iter() {
            encode_statement(
                &resolved_comp,
                resolver,
                function,
                statement.as_ref(),
                &mut builder,
            );
        }
        builder.instruction(&enc::Instruction::End);

        self.module.code.function(&builder);

        if function.export_kwd.is_some() {
            self.encode_func_export(func_idx, function);
        }
    }

    fn encode_func_export(&mut self, func_idx: u32, function: &ast::Function) {
        // Export function from module
        self.module.exports.export(
            function.signature.name.as_ref(),
            enc::ExportKind::Func,
            func_idx,
        );
        // Alias module instance export into component
        self.component.alias.alias(enc::Alias::CoreInstanceExport {
            instance: MODULE_INSTANCE_IDX,
            kind: enc::ExportKind::Func,
            name: function.signature.name.as_ref(),
        });
        // Encode component func type
        self.encode_comp_func_type(&function.signature.fn_type);
        // Lift aliased function to component function
        const NO_CANON_OPTS: [enc::CanonicalOption; 0] = [];
        self.component
            .lift_funcs
            .lift(func_idx, func_idx, NO_CANON_OPTS);
        // Export component function
        self.component.exports.export(
            function.signature.name.as_ref(),
            enc::ComponentExportKind::Func,
            func_idx,
            Some(enc::ComponentTypeRef::Func(func_idx)),
        );
    }

    fn emit_bytes(self) -> Vec<u8> {
        let mut module = enc::Module::new();
        let mut component = enc::Component::new();

        // Combine module sections in order
        module.section(&self.module.types);
        module.section(&self.module.imports);
        module.section(&self.module.funcs);
        module.section(&self.module.globals);
        module.section(&self.module.exports);
        module.section(&self.module.code);

        // Build up component
        // Embed module
        component.section(&enc::ModuleSection(&module));
        // Encode function types and definitions
        component.section(&self.component.types);
        // Import component functions
        component.section(&self.component.imports);
        // Lower them to module level
        component.section(&self.component.lower_funcs);
        // Instantiate module
        let mut comp_instantiate = enc::InstanceSection::new();
        comp_instantiate.export_items(self.component.instantiate_args);
        comp_instantiate.instantiate(
            MODULE_IDX,
            [("claw", ModuleArg::Instance(INLINE_EXPORT_INSTANCE_IDX))],
        );
        component.section(&comp_instantiate);
        // Alias module exports
        component.section(&self.component.alias);
        // Lift component functions
        component.section(&self.component.lift_funcs);
        // Export component functions
        component.section(&self.component.exports);

        component.finish()
    }

    fn encode_mod_import_type(&mut self, fn_type: &FnType) {
        let params = fn_type
            .arguments
            .iter()
            .map(|(_name, valtype)| encode_valtype(valtype.as_ref()));

        let result_type = encode_valtype(fn_type.return_type.as_ref());
        self.module.types.function(params, [result_type]);
    }

    fn encode_comp_import_type(&mut self, fn_type: &FnType) {
        let params = fn_type.arguments.iter().map(|(name, valtype)| {
            (
                name.as_ref().as_str(),
                encode_comp_valtype(valtype.as_ref()),
            )
        });
        let result_type = encode_comp_valtype(fn_type.return_type.as_ref());
        self.component
            .types
            .function()
            .params(params)
            .result(result_type);
    }

    fn encode_mod_func_type(&mut self, fn_type: &FnType) {
        let params = fn_type
            .arguments
            .iter()
            .map(|(_name, valtype)| encode_valtype(valtype.as_ref()));

        let result_type = encode_valtype(fn_type.return_type.as_ref());
        self.module.types.function(params, [result_type]);
    }

    fn encode_comp_func_type(&mut self, fn_type: &FnType) {
        let params = fn_type.arguments.iter().map(|(name, valtype)| {
            (
                name.as_ref().as_str(),
                encode_comp_valtype(valtype.as_ref()),
            )
        });
        let result_type = encode_comp_valtype(fn_type.return_type.as_ref());
        self.component
            .types
            .function()
            .params(params)
            .result(result_type);
    }
}

fn encode_locals(resolver: &FunctionResolver) -> Vec<(u32, enc::ValType)> {
    resolver
        .locals
        .iter()
        .map(|(id, _local)| {
            let valtype = resolver.local_types.get(&id).unwrap();
            (1, encode_valtype(&valtype))
        })
        .collect()
}

fn encode_statement(
    component: &ResolvedComponent,
    resolver: &FunctionResolver,
    func: &ast::Function,
    statement: &ast::Statement,
    builder: &mut enc::Function,
) {
    match statement {
        ast::Statement::Let {
            ident: _,
            name_id,
            expression,
            ..
        }
        | ast::Statement::Assign {
            ident: _,
            name_id,
            expression,
            ..
        } => {
            encode_expression(resolver, func, *expression, builder);
            match resolver.bindings.get(name_id).unwrap() {
                ItemId::Import(_) => unimplemented!(),
                ItemId::Global(global) => {
                    builder.instruction(&enc::Instruction::GlobalSet(global.index() as u32));
                }
                ItemId::Param(param) => {
                    let local_index = param.index() as u32;
                    builder.instruction(&enc::Instruction::LocalSet(local_index));
                }
                ItemId::Local(local) => {
                    let local_index = local.index() + func.signature.fn_type.arguments.len();
                    let local_index = local_index as u32;
                    builder.instruction(&enc::Instruction::LocalSet(local_index));
                }
                ItemId::Function(_) => unimplemented!(),
            }
        }
        ast::Statement::Call { call } => {
            for arg in call.args.iter() {
                encode_expression(resolver, func, *arg, builder);
            }
            let index = match resolver.bindings.get(&call.name_id).unwrap() {
                ItemId::Import(import) => import.index(),
                ItemId::Function(function) => function.index(),
                _ => panic!(""),
            };
            builder.instruction(&enc::Instruction::Call(index as u32));
        }
        ast::Statement::If {
            if_kwd: _,
            condition,
            block,
        } => {
            encode_expression(resolver, func, *condition, builder);
            builder.instruction(&enc::Instruction::If(enc::BlockType::Empty));
            for statement in block.value.statements.iter() {
                encode_statement(component, resolver, func, statement.as_ref(), builder);
            }
            builder.instruction(&enc::Instruction::End);
        }
        ast::Statement::Return {
            return_kwd: _,
            expression,
        } => {
            encode_expression(resolver, func, *expression, builder);
            builder.instruction(&enc::Instruction::Return);
        }
    };
}

/// A simple helper that calls EncodeExpression::encode
fn encode_expression(
    resolver: &FunctionResolver,
    func: &ast::Function,
    expression: ExpressionId,
    builder: &mut enc::Function,
) {
    let valtype = resolver.expression_types.get(&expression).unwrap();
    let expr = func.expressions.get_exp(expression);
    expr.encode(valtype, resolver, func, builder);
}

// fn encode_bin_op(bin_op: &BinaryOp, valtype: &ValType, builder: &mut enc::Function) {
//     let core_valtype = core_type_of(&valtype);
//     let signedness = signedness_of(&valtype);

//     let instruction = match (bin_op, core_valtype, signedness) {
//         // Multiply
//         (ast::BinaryOp::Mult, enc::ValType::I32, _) => enc::Instruction::I32Mul,
//         (ast::BinaryOp::Mult, enc::ValType::I64, _) => enc::Instruction::I64Mul,
//         (ast::BinaryOp::Mult, enc::ValType::F32, _) => enc::Instruction::F32Mul,
//         (ast::BinaryOp::Mult, enc::ValType::F64, _) => enc::Instruction::F64Mul,
//         (ast::BinaryOp::Mult, vtype, _) => panic!("Cannot multiply type {:?}", vtype),
//         // Divide
//         (ast::BinaryOp::Div, enc::ValType::I32, S) => enc::Instruction::I32DivS,
//         (ast::BinaryOp::Div, enc::ValType::I32, U) => enc::Instruction::I32DivU,
//         (ast::BinaryOp::Div, enc::ValType::I64, S) => enc::Instruction::I64DivS,
//         (ast::BinaryOp::Div, enc::ValType::I64, U) => enc::Instruction::I64DivU,
//         (ast::BinaryOp::Div, enc::ValType::F32, _) => enc::Instruction::F32Div,
//         (ast::BinaryOp::Div, enc::ValType::F64, _) => enc::Instruction::F64Div,
//         (ast::BinaryOp::Div, vtype, _) => panic!("Cannot divide type {:?}", vtype),
//         // Modulo
//         (ast::BinaryOp::Mod, _, _) => todo!(),
//         // Addition
//         (ast::BinaryOp::Add, enc::ValType::I32, _) => enc::Instruction::I32Add,
//         (ast::BinaryOp::Add, enc::ValType::I64, _) => enc::Instruction::I64Add,
//         (ast::BinaryOp::Add, enc::ValType::F32, _) => enc::Instruction::F32Add,
//         (ast::BinaryOp::Add, enc::ValType::F64, _) => enc::Instruction::F64Add,
//         // Subtraction
//         (ast::BinaryOp::Sub, enc::ValType::I32, _) => enc::Instruction::I32Sub,
//         (ast::BinaryOp::Sub, enc::ValType::I64, _) => enc::Instruction::I64Sub,
//         (ast::BinaryOp::Sub, enc::ValType::F32, _) => enc::Instruction::F32Sub,
//         (ast::BinaryOp::Sub, enc::ValType::F64, _) => enc::Instruction::F64Sub,
//         (ast::BinaryOp::BitShiftL, _, _) => todo!(),
//         (ast::BinaryOp::BitShiftR, _, _) => todo!(),
//         (ast::BinaryOp::ArithShiftR, _, _) => todo!(),
//         // Less than
//         (ast::BinaryOp::LT, enc::ValType::I32, S) => enc::Instruction::I32LtS,
//         (ast::BinaryOp::LT, enc::ValType::I32, U) => enc::Instruction::I32LtU,
//         (ast::BinaryOp::LT, enc::ValType::I64, S) => enc::Instruction::I64LtS,
//         (ast::BinaryOp::LT, enc::ValType::I64, U) => enc::Instruction::I64LtU,
//         (ast::BinaryOp::LT, enc::ValType::F32, _) => enc::Instruction::F32Lt,
//         (ast::BinaryOp::LT, enc::ValType::F64, _) => enc::Instruction::F64Lt,
//         // Less than equal
//         (ast::BinaryOp::LTE, enc::ValType::I32, S) => enc::Instruction::I32LeS,
//         (ast::BinaryOp::LTE, enc::ValType::I32, U) => enc::Instruction::I32LeU,
//         (ast::BinaryOp::LTE, enc::ValType::I64, S) => enc::Instruction::I64LeS,
//         (ast::BinaryOp::LTE, enc::ValType::I64, U) => enc::Instruction::I64LeU,
//         (ast::BinaryOp::LTE, enc::ValType::F32, _) => enc::Instruction::F32Le,
//         (ast::BinaryOp::LTE, enc::ValType::F64, _) => enc::Instruction::F64Le,
//         // Greater than
//         (ast::BinaryOp::GT, enc::ValType::I32, S) => enc::Instruction::I32GtS,
//         (ast::BinaryOp::GT, enc::ValType::I32, U) => enc::Instruction::I32GtU,
//         (ast::BinaryOp::GT, enc::ValType::I64, S) => enc::Instruction::I64GtS,
//         (ast::BinaryOp::GT, enc::ValType::I64, U) => enc::Instruction::I64GtU,
//         (ast::BinaryOp::GT, enc::ValType::F32, _) => enc::Instruction::F32Gt,
//         (ast::BinaryOp::GT, enc::ValType::F64, _) => enc::Instruction::F64Gt,
//         // Greater than or equal
//         (ast::BinaryOp::GTE, enc::ValType::I32, S) => enc::Instruction::I32GeS,
//         (ast::BinaryOp::GTE, enc::ValType::I32, U) => enc::Instruction::I32GeU,
//         (ast::BinaryOp::GTE, enc::ValType::I64, S) => enc::Instruction::I64GeS,
//         (ast::BinaryOp::GTE, enc::ValType::I64, U) => enc::Instruction::I64GeU,
//         (ast::BinaryOp::GTE, enc::ValType::F32, _) => enc::Instruction::F32Ge,
//         (ast::BinaryOp::GTE, enc::ValType::F64, _) => enc::Instruction::F64Ge,
//         // Equal
//         (ast::BinaryOp::EQ, enc::ValType::I32, _) => enc::Instruction::I32Eq,
//         (ast::BinaryOp::EQ, enc::ValType::I64, _) => enc::Instruction::I64Eq,
//         (ast::BinaryOp::EQ, enc::ValType::F32, _) => enc::Instruction::F32Eq,
//         (ast::BinaryOp::EQ, enc::ValType::F64, _) => enc::Instruction::F64Eq,
//         // Not equal
//         (ast::BinaryOp::NEQ, enc::ValType::I32, _) => enc::Instruction::I32Eq,
//         (ast::BinaryOp::NEQ, enc::ValType::I64, _) => enc::Instruction::I64Eq,
//         (ast::BinaryOp::NEQ, enc::ValType::F32, _) => enc::Instruction::F32Eq,
//         (ast::BinaryOp::NEQ, enc::ValType::F64, _) => enc::Instruction::F64Eq,
//         // Bitwise and
//         (ast::BinaryOp::BitAnd, _, _) => todo!(),
//         (ast::BinaryOp::BitXor, _, _) => todo!(),
//         (ast::BinaryOp::BitOr, _, _) => todo!(),
//         (ast::BinaryOp::LogicalAnd, _, _) => todo!(),
//         (ast::BinaryOp::LogicalOr, _, _) => todo!(),
//         _ => todo!(),
//     };
//     builder.instruction(&instruction);
// }

fn core_type_of(valtype: &ValType) -> enc::ValType {
    match valtype {
        ValType::U64 | ValType::S64 => enc::ValType::I64,

        ValType::U32 | ValType::U16 | ValType::U8 | ValType::S32 | ValType::S16 | ValType::S8 => {
            enc::ValType::I32
        }

        ValType::F32 => enc::ValType::F32,
        ValType::F64 => enc::ValType::F64,

        vtype => unimplemented!("Core type of {:?}", vtype),
    }
}

#[derive(PartialEq, Eq)]
enum Signedness {
    Unsigned,
    Signed,
    NotApplicable,
}

const S: Signedness = Signedness::Signed;
const U: Signedness = Signedness::Unsigned;

fn signedness_of(valtype: &ValType) -> Signedness {
    match valtype {
        ValType::U64 | ValType::U32 | ValType::U16 | ValType::U8 => Signedness::Unsigned,
        ValType::S64 | ValType::S32 | ValType::S16 | ValType::S8 => Signedness::Signed,
        _ => Signedness::NotApplicable,
    }
}

impl ast::Literal {
    fn to_constexpr(&self, valtype: &ValType) -> enc::ConstExpr {
        match (valtype, self) {
            (ValType::S32 | ValType::U32, Literal::Integer(value)) => {
                enc::ConstExpr::i32_const(*value as i32)
            }
            (ValType::S64 | ValType::U64, Literal::Integer(value)) => {
                enc::ConstExpr::i64_const(*value as i64)
            }
            (ValType::F32, Literal::Float(value)) => enc::ConstExpr::f32_const(*value as f32),
            (ValType::F64, Literal::Float(value)) => enc::ConstExpr::f64_const(*value),
            _ => todo!(),
        }
    }
}

fn encode_valtype(valtype: &ValType) -> enc::ValType {
    match valtype {
        ValType::U32 | ValType::S32 => enc::ValType::I32,
        ValType::U64 | ValType::S64 => enc::ValType::I64,
        ValType::F32 => enc::ValType::F32,
        ValType::F64 => enc::ValType::F64,
        ValType::Bool => enc::ValType::I32,
        _ => panic!("Unsupported type for WAT output {:?}", valtype),
    }
}

fn encode_comp_valtype(valtype: &ValType) -> enc::ComponentValType {
    use enc::PrimitiveValType;
    let primitive = match valtype {
        ValType::Result { ok: _, err: _ } => todo!(),
        ValType::String => PrimitiveValType::String,
        ValType::U64 => PrimitiveValType::U64,
        ValType::U32 => PrimitiveValType::U32,
        ValType::U16 => PrimitiveValType::U16,
        ValType::U8 => PrimitiveValType::U8,
        ValType::S64 => PrimitiveValType::S64,
        ValType::S32 => PrimitiveValType::S32,
        ValType::S16 => PrimitiveValType::S16,
        ValType::S8 => PrimitiveValType::S8,
        ValType::F32 => PrimitiveValType::Float32,
        ValType::F64 => PrimitiveValType::Float64,
        ValType::Bool => PrimitiveValType::Bool,
    };
    enc::ComponentValType::Primitive(primitive)
}

// 

trait EncodeExpression {
    fn encode(
        &self,
        valtype: &ValType,
        resolver: &FunctionResolver,
        func: &ast::Function,
        builder: &mut enc::Function,
    ) -> ();
}

impl EncodeExpression for ast::Expression {
    fn encode(
        &self,
        valtype: &ValType,
        resolver: &FunctionResolver,
        func: &ast::Function,
        builder: &mut enc::Function,
    ) -> () {
        let expr: &dyn EncodeExpression = match self {
            ast::Expression::Identifier(expr) => expr,
            ast::Expression::Literal(expr) => expr,
            ast::Expression::Call(expr) => expr,

            _ => todo!()
        };
        expr.encode(valtype, resolver, func, builder);
    }
}

impl EncodeExpression for ast::Identifier {
    fn encode(
        &self,
        _valtype: &ValType,
        resolver: &FunctionResolver,
        func: &ast::Function,
        builder: &mut enc::Function,
    ) -> () {
        match resolver.bindings.get(&self.name_id).unwrap() {
            ItemId::Import(_) => unimplemented!(),
            ItemId::Global(global) => {
                builder.instruction(&enc::Instruction::GlobalGet(global.index() as u32));
            }
            ItemId::Param(param) => {
                let local_index = param.index();
                builder.instruction(&enc::Instruction::LocalGet(local_index as u32));
            }
            ItemId::Local(local) => {
                let local_index = local.index() + func.signature.fn_type.arguments.len();
                builder.instruction(&enc::Instruction::LocalGet(local_index as u32));
            }
            ItemId::Function(_) => unimplemented!(),
        }
    }
}

impl EncodeExpression for ast::Literal {
    fn encode(
        &self,
        valtype: &ValType,
        _resolver: &FunctionResolver,
        _func: &ast::Function,
        builder: &mut enc::Function,
    ) -> () {
        let instruction = match (valtype, self) {
            (ValType::S32 | ValType::U32, Literal::Integer(value)) => {
                enc::Instruction::I32Const(*value as i32)
            }
            (ValType::S64 | ValType::U64, Literal::Integer(value)) => {
                enc::Instruction::I64Const(*value as i64)
            }
            (ValType::F32, Literal::Float(value)) => enc::Instruction::F32Const(*value as f32),
            (ValType::F64, Literal::Float(value)) => enc::Instruction::F64Const(*value),
            _ => todo!(),
        };
        builder.instruction(&instruction);
    }
}

impl EncodeExpression for ast::Call {
    fn encode(
        &self,
        _valtype: &ValType,
        resolver: &FunctionResolver,
        func: &ast::Function,
        builder: &mut enc::Function,
    ) -> () {
        for arg in self.args.iter() {
            encode_expression(resolver, func, *arg, builder);
        }
        let index = match resolver.bindings.get(&self.name_id).unwrap() {
            ItemId::Import(import) => import.index(),
            ItemId::Function(function) => function.index(),
            _ => panic!(""),
        };
        builder.instruction(&enc::Instruction::Call(index as u32));
    }
}

impl EncodeExpression for ast::Multiply {
    fn encode(
        &self,
        valtype: &ValType,
        resolver: &FunctionResolver,
        func: &ast::Function,
        builder: &mut enc::Function,
    ) -> () {
        let left = func.expressions.get_exp(self.left);
        left.encode(valtype, resolver, func, builder);

        let right = func.expressions.get_exp(self.right);
        right.encode(valtype, resolver, func, builder);

        let instruction = match core_type_of(valtype) {
            enc::ValType::I32 => enc::Instruction::I32Mul,
            enc::ValType::I64 => enc::Instruction::I64Mul,
            enc::ValType::F32 => enc::Instruction::F32Mul,
            enc::ValType::F64 => enc::Instruction::F64Mul,
            vtype => panic!("Cannot multiply type {:?}", vtype),
        };
        builder.instruction(&instruction);
    }
}

impl EncodeExpression for ast::Divide {
    fn encode(
        &self,
        valtype: &ValType,
        resolver: &FunctionResolver,
        func: &ast::Function,
        builder: &mut enc::Function,
    ) -> () {
        let left = func.expressions.get_exp(self.left);
        left.encode(valtype, resolver, func, builder);

        let right = func.expressions.get_exp(self.right);
        right.encode(valtype, resolver, func, builder);

        let instruction = match (core_type_of(valtype), signedness_of(valtype)) {
            (enc::ValType::I32, S) => enc::Instruction::I32DivS,
            (enc::ValType::I32, U) => enc::Instruction::I32DivU,
            (enc::ValType::I64, S) => enc::Instruction::I64DivS,
            (enc::ValType::I64, U) => enc::Instruction::I64DivU,
            (enc::ValType::F32, _) => enc::Instruction::F32Div,
            (enc::ValType::F64, _) => enc::Instruction::F64Div,
            _ => panic!("Cannot divide type {:?}", valtype),
        };
        builder.instruction(&instruction);
    }
}