use crate::ast::{
    module::FunctionSignature,
    expressions::Literal,
    types::ValType
};
use crate::resolver::ModuleItem;
use crate::ir::{self, NeedsResolve, type_graph::TypeGraph};

use wasm_encoder as enc;

pub fn generate(ir: ir::Module) -> Vec<u8> {
    let mut module = enc::Module::new();
    let globals = encode_globals(&ir.globals);
    let (types, functions, codes) = encode_functions(&ir.functions);
    let exports = encode_exports(&ir.exports);
    module.section(&types);
    module.section(&functions);
    module.section(&globals);
    module.section(&exports);
    module.section(&codes);
    module.finish()
}

fn encode_globals(globals: &Vec<ir::Global>) -> enc::GlobalSection {
    let mut section = enc::GlobalSection::new();

    for global in globals.iter() {
        let valtype = global.type_.value.clone();
        
        let global_type = enc::GlobalType {
            mutable: global.mutable,
            val_type: encode_valtype(global.type_.as_ref()),
        };
        
        let init_expr = if let NeedsResolve::Resolved(init_value) = &global.initial_value {
            literal_to_constexpr(valtype, init_value)
        } else { panic!("Cannot generate WASM for unresolved global") };

        section.global(global_type, &init_expr);
    }

    section
}

fn encode_functions(functions: &Vec<ir::Function>) -> (enc::TypeSection, enc::FunctionSection, enc::CodeSection) {
    let mut type_section = enc::TypeSection::new();
    let mut func_section = enc::FunctionSection::new();
    let mut code_section = enc::CodeSection::new();

    let mut next_type_index = 0;

    for function in functions.iter() {
        // Encode Type
        encode_func_signature(&function.signature, &mut type_section);

        func_section.function(next_type_index);
        next_type_index += 1;

        let type_graph = match &function.type_graph {
            NeedsResolve::Resolved(type_graph) => type_graph,
            _ => panic!("Cannot generate WASM for unresolved function")
        };

        let locals = encode_locals(type_graph, function);
        let mut func = enc::Function::new(locals);

        if let NeedsResolve::Resolved(instructions) = &function.body {
            for instruction in instructions.iter() {
                encode_instruction(&type_graph, instruction, &mut func);
            }
            func.instruction(&enc::Instruction::End);
        } else { panic!("Cannot generate WASM for unresolved function") }
        
        code_section.function(&func);
    }

    (type_section, func_section, code_section)
}

fn encode_func_signature(signature: &FunctionSignature, type_section: &mut enc::TypeSection) {
    let params = signature.arguments
        .iter()
        .map(|(_name, valtype)|
            encode_valtype(valtype.as_ref())
        );

    let result_type = encode_valtype(signature.return_type.as_ref());
    let results = [result_type];

    type_section.function(params, results);
}

fn encode_locals(type_graph: &TypeGraph, function: &ir::Function) -> Vec<(u32, enc::ValType)> {
    if let NeedsResolve::Resolved(locals) = &function.locals {
        if locals.len() == function.signature.arguments.len() {
            return vec![];
        }

        locals.iter()
            .skip(function.signature.arguments.len())
            .map(|local| {
                let valtype = type_graph.type_of(*local).unwrap();
                (1, encode_valtype(&valtype))
            })
            .collect()
    } else {
        panic!("RIP!!!!")
    }
}

fn encode_exports(exports: &Vec<ir::Export>) -> enc::ExportSection {
    let mut section = enc::ExportSection::new();

    for export in exports.iter() {
        match &export.id {
            ModuleItem::Function(index) => {
                section.export(export.ident.as_ref(), enc::ExportKind::Func, *index as u32);
            },
            _ => panic!("Only function exports supported")
        }
    }

    section
}

fn encode_instruction(
    type_graph: &TypeGraph,
    instruction: &ir::Instruction,
    func: &mut enc::Function
) {
    match &instruction {
        ir::Instruction::Constant {
            node,
            value
        } => {
            let valtype = type_graph.type_of(*node).expect("Constant type not known");
            let instruction = match (valtype, value) {
                (ValType::S32 | ValType::U32, Literal::Integer(value)) => enc::Instruction::I32Const(*value as i32),
                (ValType::S64 | ValType::U64, Literal::Integer(value)) => enc::Instruction::I64Const(*value as i64),
                (ValType::F32, Literal::Float(value)) => enc::Instruction::F32Const(*value as f32),
                (ValType::F64, Literal::Float(value)) => enc::Instruction::F64Const(*value),
                _ => todo!()
            };
            func.instruction(&instruction);
        },
        ir::Instruction::GlobalGet { index } => {
            func.instruction(&enc::Instruction::GlobalGet(*index as u32));
        },
        ir::Instruction::GlobalSet {
            index,
            value
        } => {
            encode_instruction(type_graph, &value, func);
            func.instruction(&enc::Instruction::GlobalSet(*index as u32));
        },
        ir::Instruction::LocalGet { node: _, index } => {
            func.instruction(&enc::Instruction::LocalGet(*index as u32));
        },
        ir::Instruction::LocalSet {
            index,
            value
        } => {
            encode_instruction(type_graph, &value, func);
            func.instruction(&enc::Instruction::LocalSet(*index as u32));
        },
        ir::Instruction::BinaryArith {
            node,
            op,
            left,
            right
        } => {
            let valtype = type_graph.type_of(*node).expect("");
            encode_instruction(type_graph, left, func);
            encode_instruction(type_graph, right, func);
            match op {
                ir::BinArithOp::Add => encode_add(valtype, func),
                ir::BinArithOp::Sub => encode_sub(valtype, func),
                ir::BinArithOp::Mul => encode_mul(valtype, func),
            };
        },
        ir::Instruction::BinaryRel {
            node: _,
            op,
            left,
            right
        } => {
            let node = match left.as_ref() {
                ir::Instruction::Constant { node, .. } => Some(node),
                ir::Instruction::GlobalGet { .. } => todo!(),
                ir::Instruction::GlobalSet { .. } => todo!(),
                ir::Instruction::LocalGet { node, .. } => Some(node),
                ir::Instruction::LocalSet { .. } => todo!(),
                ir::Instruction::BinaryArith { node, .. } => Some(node),
                ir::Instruction::BinaryRel { node, .. } => Some(node),
                ir::Instruction::BinaryLog { .. } => todo!(),
                ir::Instruction::If { .. } => todo!(),
                ir::Instruction::Return { .. } => todo!(),
            };
            let node = node.unwrap();
            let valtype = type_graph.type_of(*node).expect("");
            encode_instruction(type_graph, left, func);
            encode_instruction(type_graph, right, func);
            match op {
                ir::BinRelOp::EQ => encode_eq(valtype, func),
                ir::BinRelOp::LT => encode_lt(valtype, func),
            }
        },
        ir::Instruction::If {
            body
        } => {
            func.instruction(&enc::Instruction::If(enc::BlockType::Empty));
            for instruction in body {
                encode_instruction(type_graph, instruction, func);
            }
            func.instruction(&enc::Instruction::End);
        },
        ir::Instruction::Return { value } => {
            encode_instruction(type_graph, &value, func);
            func.instruction(&enc::Instruction::Return);
        },
        _ => todo!()
    };
}

fn encode_add(
    valtype: ValType,
    func: &mut enc::Function
) {
    let instruction = match core_type_of(&valtype) {
        enc::ValType::I32 => enc::Instruction::I32Add,
        enc::ValType::I64 => enc::Instruction::I64Add,
        enc::ValType::F32 => enc::Instruction::F32Add,
        enc::ValType::F64 => enc::Instruction::F64Add,
        _ => unimplemented!()
    };
    func.instruction(&instruction);
}

fn encode_sub(
    valtype: ValType,
    func: &mut enc::Function
) {
    let instruction = match core_type_of(&valtype) {
        enc::ValType::I32 => enc::Instruction::I32Sub,
        enc::ValType::I64 => enc::Instruction::I64Sub,
        enc::ValType::F32 => enc::Instruction::F32Sub,
        enc::ValType::F64 => enc::Instruction::F64Sub,
        _ => unimplemented!()
    };
    func.instruction(&instruction);
}

fn encode_mul(
    valtype: ValType,
    func: &mut enc::Function
) {
    let instruction = match core_type_of(&valtype) {
        enc::ValType::I32 => enc::Instruction::I32Mul,
        enc::ValType::I64 => enc::Instruction::I64Mul,
        enc::ValType::F32 => enc::Instruction::F32Mul,
        enc::ValType::F64 => enc::Instruction::F64Mul,
        _ => unimplemented!()
    };
    func.instruction(&instruction);
}

fn encode_eq(
    valtype: ValType,
    func: &mut enc::Function
) {
    let instruction = match core_type_of(&valtype) {
        enc::ValType::I32 => enc::Instruction::I32Eq,
        enc::ValType::I64 => enc::Instruction::I64Eq,
        enc::ValType::F32 => enc::Instruction::F32Eq,
        enc::ValType::F64 => enc::Instruction::F64Eq,
        _ => unimplemented!()
    };
    func.instruction(&instruction);
}

fn encode_lt(
    valtype: ValType,
    func: &mut enc::Function
) {
    let instruction = match valtype {
        ValType::U64 => enc::Instruction::I64LtU,
        ValType::S64 => enc::Instruction::I64LtS,

        ValType::U32 | ValType::U16 | ValType::U8 => enc::Instruction::I32LtU,
        ValType::S32 | ValType::S16 | ValType::S8 => enc::Instruction::I32LtS,

        ValType::F32 => enc::Instruction::F32Lt,
        ValType::F64 => enc::Instruction::F64Lt,

        vtype => unimplemented!("comparison '<' of type {:?}", vtype)
    };
    func.instruction(&instruction);
}

fn core_type_of(valtype: &ValType) -> enc::ValType {
    match valtype {
        ValType::U64 | ValType::S64 => enc::ValType::I64,

        ValType::U32 | ValType::U16 | ValType::U8 |
        ValType::S32 | ValType::S16 | ValType::S8 => enc::ValType::I32,

        ValType::F32 => enc::ValType::F32,
        ValType::F64 => enc::ValType::F64,

        _ => unimplemented!()
    }   
}

fn literal_to_constexpr(valtype: ValType, literal: &Literal) -> enc::ConstExpr {
    match (valtype, &literal) {
        (ValType::S32 | ValType::U32, Literal::Integer(value)) => enc::ConstExpr::i32_const(*value as i32),
        (ValType::S64 | ValType::U64, Literal::Integer(value)) => enc::ConstExpr::i64_const(*value as i64),
        (ValType::F32, Literal::Float(value)) => enc::ConstExpr::f32_const(*value as f32),
        (ValType::F64, Literal::Float(value)) => enc::ConstExpr::f64_const(*value),
        _ => todo!()
    }
}

fn encode_valtype(valtype: &ValType) -> enc::ValType {
    match valtype {
        ValType::U32 => enc::ValType::I32,
        ValType::S32 => enc::ValType::I32,
        ValType::U64 => enc::ValType::I64,
        ValType::S64 => enc::ValType::I64,
        ValType::Bool => enc::ValType::I32,
        _ => panic!("Unsupported type for WAT output {:?}", valtype)
    }
}
