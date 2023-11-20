use crate::{
    ast::{self, component::FunctionSignature, expressions::Literal, types::ValType, ExpressionId},
    resolver::{FunctionResolver, ResolvedComponent, ItemId},
};

use wasm_encoder as enc;

pub fn generate(component: ResolvedComponent) -> Vec<u8> {
    let mut module = enc::Module::new();
    let globals = encode_globals(&component);
    let (types, functions, codes) = encode_functions(&component);
    let exports = encode_exports(&component);
    module.section(&types);
    module.section(&functions);
    module.section(&globals);
    module.section(&exports);
    module.section(&codes);
    module.finish()
}

fn encode_globals(component: &ResolvedComponent) -> enc::GlobalSection {
    let mut section = enc::GlobalSection::new();

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

        section.global(global_type, &init_expr);
    }

    section
}

fn encode_functions(
    component: &ResolvedComponent,
) -> (enc::TypeSection, enc::FunctionSection, enc::CodeSection) {
    let mut type_section = enc::TypeSection::new();
    let mut func_section = enc::FunctionSection::new();
    let mut code_section = enc::CodeSection::new();

    let mut next_type_index = 0;

    for (id, function) in component.component.functions.iter() {
        // Encode Type
        encode_func_signature(&function.signature, &mut type_section);

        func_section.function(next_type_index);
        next_type_index += 1;

        let resolver = component.resolved_funcs.get(id).unwrap();

        let locals = encode_locals(resolver);
        let mut builder = enc::Function::new(locals);

        for statement in function.body.as_ref().statements.iter() {
            encode_statement(resolver, function, statement.as_ref(), &mut builder);
        }
        builder.instruction(&enc::Instruction::End);

        code_section.function(&builder);
    }

    (type_section, func_section, code_section)
}

fn encode_func_signature(signature: &FunctionSignature, type_section: &mut enc::TypeSection) {
    let params = signature
        .arguments
        .iter()
        .map(|(_name, valtype)| encode_valtype(valtype.as_ref()));

    let result_type = encode_valtype(signature.return_type.as_ref());
    let results = [result_type];

    type_section.function(params, results);
}

fn encode_locals(
    resolver: &FunctionResolver,
) -> Vec<(u32, enc::ValType)> {
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

fn encode_exports(component: &ResolvedComponent) -> enc::ExportSection {
    let mut section = enc::ExportSection::new();

    for (id, func) in component.component.functions.iter() {
        if func.export_kwd.is_some() {
            section.export(
                func.signature.name.as_ref(),
                enc::ExportKind::Func,
                id.index() as u32,
            );
        }
    }

    section
}

fn encode_statement(
    resolver: &FunctionResolver,
    func: &ast::Function,
    statement: &ast::Statement,
    builder: &mut enc::Function,
) {
    match statement {
        ast::Statement::Let {
            ident: _, name_id, expression, ..
        } | ast::Statement::Assign {
            ident: _, name_id,
            expression, ..
        }=> {
            encode_expression(resolver, func, *expression, builder);
            match resolver.bindings.get(name_id).unwrap() {
                ItemId::Import(_) => unimplemented!(),
                ItemId::Global(global) => {
                    builder.instruction(&enc::Instruction::GlobalSet(global.index() as u32));
                },
                ItemId::Param(param) => {
                    let local_index = param.0;
                    dbg!(param, local_index);
                    builder.instruction(&enc::Instruction::LocalSet(param.0 as u32));
                },
                ItemId::Local(local) => {
                    let local_index = local.index() + func.signature.arguments.len();
                    dbg!(local, local_index);
                    builder.instruction(&enc::Instruction::LocalSet(local_index as u32));
                },
            }
        },
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
        },
        ast::Statement::Return {
            return_kwd: _,
            expression,
        } => {
            encode_expression(resolver, func, *expression, builder);
            builder.instruction(&enc::Instruction::Return);
        },
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
        },

        ast::Expression::Binary { left, operator, right } => {
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
                ast::BinaryOp::NEQ => todo!(),
                ast::BinaryOp::BitAnd => todo!(),
                ast::BinaryOp::BitXor => todo!(),
                ast::BinaryOp::BitOr => todo!(),
                ast::BinaryOp::LogicalAnd => todo!(),
                ast::BinaryOp::LogicalOr => todo!(),
            };
        },

        ast::Expression::Invocation { .. } => todo!(),

        ast::Expression::Identifier { ident: _, name_id } => {
            match resolver.bindings.get(name_id).unwrap() {
                ItemId::Import(_) => unimplemented!(),
                ItemId::Global(global) => {
                    builder.instruction(&enc::Instruction::GlobalGet(global.index() as u32));
                },
                ItemId::Param(param) => {
                    let local_index = param.0;
                    dbg!(param, local_index);
                    builder.instruction(&enc::Instruction::LocalGet(local_index as u32));
                },
                ItemId::Local(local) => {
                    let local_index = local.index() + func.signature.arguments.len();
                    dbg!(local, local_index);
                    builder.instruction(&enc::Instruction::LocalGet(local_index as u32));
                },
            }
        },

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
        },
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
