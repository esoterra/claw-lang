use crate::{
    ast::{self, component::FunctionSignature, expressions::Literal, types::ValType, ExpressionId},
    resolver::{FunctionResolver, ItemId, ResolvedComponent},
};

use wasm_encoder as enc;

pub fn generate(resolved_comp: ResolvedComponent) -> Vec<u8> {
    let mut module = enc::Module::new();
    let mut component = enc::Component::new();

    let mut module_globals = enc::GlobalSection::new();
    let mut module_types = enc::TypeSection::new();
    let mut module_funcs = enc::FunctionSection::new();
    let mut module_code = enc::CodeSection::new();
    let mut module_exports = enc::ExportSection::new();

    let mut comp_alias = enc::ComponentAliasSection::new();
    let mut comp_types = enc::ComponentTypeSection::new();
    let mut comp_funcs = enc::CanonicalFunctionSection::new();
    let mut comp_exports = enc::ComponentExportSection::new();

    let mut mod_func_index = 0;
    let mut comp_func_index = 0;

    encode_globals(&resolved_comp, &mut module_globals);

    for (id, function) in resolved_comp.component.functions.iter() {
        // Encode module and component type sections
        encode_mod_func_type(&function.signature, &mut module_types);

        // Encode module function
        module_funcs.function(mod_func_index);

        // Encode module code
        let resolver = resolved_comp.resolved_funcs.get(id).unwrap();
        let locals = encode_locals(resolver);
        let mut builder = enc::Function::new(locals);

        for statement in function.body.as_ref().statements.iter() {
            encode_statement(resolver, function, statement.as_ref(), &mut builder);
        }
        builder.instruction(&enc::Instruction::End);

        module_code.function(&builder);

        if function.export_kwd.is_some() {
            // Export function from module
            module_exports.export(
                function.signature.name.as_ref(),
                enc::ExportKind::Func,
                id.index() as u32,
            );
            // Alias module instance export into component
            const MODULE_INSTANCE_INDEX: u32 = 0;
            comp_alias.alias(enc::Alias::CoreInstanceExport {
                instance: MODULE_INSTANCE_INDEX,
                kind: enc::ExportKind::Func,
                name: function.signature.name.as_ref().as_str(),
            });
            // Encode component func type
            encode_comp_func_type(&function.signature, &mut comp_types);
            // Lift aliased function to component function
            const NO_CANON_OPTS: [enc::CanonicalOption; 0] = [];
            comp_funcs.lift(mod_func_index, comp_func_index, NO_CANON_OPTS);
            // Export component function
            comp_exports.export(
                function.signature.name.as_ref().as_str(),
                enc::ComponentExportKind::Func,
                comp_func_index,
                Some(enc::ComponentTypeRef::Func(comp_func_index)),
            );
            comp_func_index += 1;
        }

        mod_func_index += 1;
    }

    // Combine module sections in order
    module.section(&module_types);
    module.section(&module_funcs);
    module.section(&module_globals);
    module.section(&module_exports);
    module.section(&module_code);

    // Build up component
    // Embed module
    component.section(&enc::ModuleSection(&module));
    // Instantiate module
    let mut comp_instantiate = enc::InstanceSection::new();
    const NO_ARGS: [(String, enc::ModuleArg); 0] = [];
    comp_instantiate.instantiate(0, NO_ARGS);
    component.section(&comp_instantiate);
    // Alias exports
    component.section(&comp_alias);
    // Encode function types and definitions
    component.section(&comp_types);
    component.section(&comp_funcs);
    // Export component functions
    component.section(&comp_exports);

    // Produce final binary
    component.finish()
}

fn encode_globals(component: &ResolvedComponent, module_globals: &mut enc::GlobalSection) {
    for (id, global) in component.component.globals.iter() {
        let valtype = global.valtype.as_ref();

        let global_type = enc::GlobalType {
            mutable: global.mut_kwd.is_some(),
            val_type: encode_valtype(valtype),
        };

        let init_expr = if let Some(init_value) = component.global_vals.get(id) {
            literal_to_constexpr(valtype, init_value)
        } else {
            panic!("Cannot generate WASM for unresolved global")
        };

        module_globals.global(global_type, &init_expr);
    }
}

fn encode_mod_func_type(signature: &FunctionSignature, module_types: &mut enc::TypeSection) {
    let params = signature
        .arguments
        .iter()
        .map(|(_name, valtype)| encode_valtype(valtype.as_ref()));

    let result_type = encode_valtype(signature.return_type.as_ref());
    module_types.function(params, [result_type]);
}

fn encode_comp_func_type(
    signature: &FunctionSignature,
    comp_types: &mut enc::ComponentTypeSection,
) {
    let params = signature.arguments.iter().map(|(name, valtype)| {
        (
            name.as_ref().as_str(),
            encode_comp_valtype(valtype.as_ref()),
        )
    });
    let result_type = encode_comp_valtype(signature.return_type.as_ref());
    comp_types.function().params(params).result(result_type);
}

fn encode_locals(resolver: &FunctionResolver) -> Vec<(u32, enc::ValType)> {
    resolver
        .locals
        .iter()
        .map(|(id, local)| {
            dbg!(id, local);
            let valtype = resolver.local_types.get(id).unwrap();
            (1, encode_valtype(&valtype))
        })
        .collect()
}

fn encode_statement(
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
                    let local_index = param.0;
                    dbg!(param, local_index);
                    builder.instruction(&enc::Instruction::LocalSet(param.0 as u32));
                }
                ItemId::Local(local) => {
                    let local_index = local.index() + func.signature.arguments.len();
                    dbg!(local, local_index);
                    builder.instruction(&enc::Instruction::LocalSet(local_index as u32));
                }
            }
        }
        ast::Statement::If {
            if_kwd: _,
            condition,
            block,
        } => {
            encode_expression(resolver, func, *condition, builder);
            builder.instruction(&enc::Instruction::If(enc::BlockType::Empty));
            for statement in block.value.statements.iter() {
                encode_statement(resolver, func, statement.as_ref(), builder);
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

fn encode_expression(
    resolver: &FunctionResolver,
    func: &ast::Function,
    expression: ExpressionId,
    builder: &mut enc::Function,
) {
    match func.expressions.get_exp(expression) {
        ast::Expression::Unary { .. } => {
            todo!()
        }

        ast::Expression::Binary {
            left,
            operator,
            right,
        } => {
            encode_expression(resolver, func, *left, builder);
            encode_expression(resolver, func, *right, builder);

            let valtype = resolver.expression_types.get(expression).unwrap();
            let inner_valtype = resolver.expression_types.get(*left).unwrap();
            match operator.value {
                ast::BinaryOp::Mult => encode_mul(valtype, builder),
                ast::BinaryOp::Div => todo!(),
                ast::BinaryOp::Mod => todo!(),
                ast::BinaryOp::Add => encode_add(valtype, builder),
                ast::BinaryOp::Sub => encode_sub(valtype, builder),
                ast::BinaryOp::BitShiftL => todo!(),
                ast::BinaryOp::BitShiftR => todo!(),
                ast::BinaryOp::ArithShiftR => todo!(),
                ast::BinaryOp::LT => encode_lt(inner_valtype, builder),
                ast::BinaryOp::LTE => todo!(),
                ast::BinaryOp::GT => todo!(),
                ast::BinaryOp::GTE => todo!(),
                ast::BinaryOp::EQ => encode_eq(valtype, builder),
                ast::BinaryOp::NEQ => encode_ne(valtype, builder),
                ast::BinaryOp::BitAnd => todo!(),
                ast::BinaryOp::BitXor => todo!(),
                ast::BinaryOp::BitOr => todo!(),
                ast::BinaryOp::LogicalAnd => todo!(),
                ast::BinaryOp::LogicalOr => todo!(),
            };
        }

        ast::Expression::Invocation { .. } => todo!(),

        ast::Expression::Identifier { ident: _, name_id } => {
            match resolver.bindings.get(name_id).unwrap() {
                ItemId::Import(_) => unimplemented!(),
                ItemId::Global(global) => {
                    builder.instruction(&enc::Instruction::GlobalGet(global.index() as u32));
                }
                ItemId::Param(param) => {
                    let local_index = param.0;
                    dbg!(param, local_index);
                    builder.instruction(&enc::Instruction::LocalGet(local_index as u32));
                }
                ItemId::Local(local) => {
                    let local_index = local.index() + func.signature.arguments.len();
                    dbg!(local, local_index);
                    builder.instruction(&enc::Instruction::LocalGet(local_index as u32));
                }
            }
        }

        ast::Expression::Literal { value } => {
            let valtype = resolver.expression_types.get(expression).unwrap();
            let instruction = match (valtype, &value.value) {
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
}

fn encode_add(valtype: &ValType, builder: &mut enc::Function) {
    let instruction = match core_type_of(&valtype) {
        enc::ValType::I32 => enc::Instruction::I32Add,
        enc::ValType::I64 => enc::Instruction::I64Add,
        enc::ValType::F32 => enc::Instruction::F32Add,
        enc::ValType::F64 => enc::Instruction::F64Add,
        _ => unimplemented!(),
    };
    builder.instruction(&instruction);
}

fn encode_sub(valtype: &ValType, builder: &mut enc::Function) {
    let instruction = match core_type_of(&valtype) {
        enc::ValType::I32 => enc::Instruction::I32Sub,
        enc::ValType::I64 => enc::Instruction::I64Sub,
        enc::ValType::F32 => enc::Instruction::F32Sub,
        enc::ValType::F64 => enc::Instruction::F64Sub,
        _ => unimplemented!(),
    };
    builder.instruction(&instruction);
}

fn encode_mul(valtype: &ValType, builder: &mut enc::Function) {
    let instruction = match core_type_of(&valtype) {
        enc::ValType::I32 => enc::Instruction::I32Mul,
        enc::ValType::I64 => enc::Instruction::I64Mul,
        enc::ValType::F32 => enc::Instruction::F32Mul,
        enc::ValType::F64 => enc::Instruction::F64Mul,
        _ => unimplemented!(),
    };
    builder.instruction(&instruction);
}

fn encode_eq(valtype: &ValType, builder: &mut enc::Function) {
    let instruction = match core_type_of(&valtype) {
        enc::ValType::I32 => enc::Instruction::I32Eq,
        enc::ValType::I64 => enc::Instruction::I64Eq,
        enc::ValType::F32 => enc::Instruction::F32Eq,
        enc::ValType::F64 => enc::Instruction::F64Eq,
        _ => unimplemented!(),
    };
    builder.instruction(&instruction);
}

fn encode_ne(valtype: &ValType, builder: &mut enc::Function) {
    let instruction = match core_type_of(&valtype) {
        enc::ValType::I32 => enc::Instruction::I32Ne,
        enc::ValType::I64 => enc::Instruction::I64Ne,
        enc::ValType::F32 => enc::Instruction::F32Ne,
        enc::ValType::F64 => enc::Instruction::F64Ne,
        _ => unimplemented!(),
    };
    builder.instruction(&instruction);
}

fn encode_lt(valtype: &ValType, builder: &mut enc::Function) {
    let instruction = match valtype {
        ValType::U64 => enc::Instruction::I64LtU,
        ValType::S64 => enc::Instruction::I64LtS,

        ValType::U32 | ValType::U16 | ValType::U8 => enc::Instruction::I32LtU,
        ValType::S32 | ValType::S16 | ValType::S8 => enc::Instruction::I32LtS,

        ValType::F32 => enc::Instruction::F32Lt,
        ValType::F64 => enc::Instruction::F64Lt,

        vtype => unimplemented!("comparison '<' of type {:?}", vtype),
    };
    builder.instruction(&instruction);
}

fn core_type_of(valtype: &ValType) -> enc::ValType {
    match valtype {
        ValType::U64 | ValType::S64 => enc::ValType::I64,

        ValType::U32 | ValType::U16 | ValType::U8 | ValType::S32 | ValType::S16 | ValType::S8 => {
            enc::ValType::I32
        }

        ValType::F32 => enc::ValType::F32,
        ValType::F64 => enc::ValType::F64,

        _ => unimplemented!(),
    }
}

fn literal_to_constexpr(valtype: &ValType, literal: &Literal) -> enc::ConstExpr {
    match (valtype, &literal) {
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
