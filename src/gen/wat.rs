use std::fmt::Write;

use crate::ast::expressions::Literal;
use crate::ast::types::{ValType, BasicVal};
use crate::resolver::ModuleItem;
use crate::ir::{self, NeedsResolve, type_graph::TypeGraph};

pub fn generate(ir: ir::Module) -> String {
    let mut result = String::new();
    let _ = write!(result, "(module\n");
    globals_to_wat(&ir.globals, &mut result);
    functions_to_wat(&ir.functions, &mut result);
    exports_to_wat(&ir.exports, &mut result);
    let _ = write!(result, ")");
    result
}

fn globals_to_wat(globals: &Vec<ir::Global>, result: &mut String) {
    for (index, global) in globals.iter().enumerate() {
        let valtype = global.type_.value.clone();

        let valtype_s = valtype_to_wat(&global.type_.value);
        let valtype_s = if global.mutable {
            format!("(mut {})", valtype_s)
        } else { valtype_s };

        let init_val = if let NeedsResolve::Resolved(init_value) = &global.initial_value {
            literal_to_wat(valtype, init_value)
        } else { panic!("Cannot generate WASM for unresolved global") };

        let _ = write!(result, "   (global $G{} {} {})\n", index, valtype_s, init_val);
    }
}

fn functions_to_wat(functions: &Vec<ir::Function>, result: &mut String) {
    for (index, function) in functions.iter().enumerate() {
        let valtype = valtype_to_wat(&function.signature.return_type.value);
        let _ = write!(result, "   (func $F{} (result {})\n", index, valtype);

        let type_graph = match &function.type_graph {
            NeedsResolve::Resolved(type_graph) => type_graph,
            _ => panic!("Cannot generate WASM for unresolved function")
        };

        if let NeedsResolve::Resolved(instructions) = &function.body {
            for instruction in instructions.iter() {
                let _ = write!(result, "      {}\n", instruction_to_wat(&type_graph, instruction));
            }
        } else { panic!("Cannot generate WASM for unresolved function") }
        let _ = write!(result, "   )\n");
    }
}

fn exports_to_wat(exports: &Vec<ir::Export>, result: &mut String) {
    for export in exports.iter() {
        match &export.id {
            ModuleItem::Function(index) => {
                let _ = write!(result, "   (export {:?} (func $F{}))\n", export.ident.value, index);
            },
            _ => panic!("Only function exports supported")
        }
    }
}

fn instruction_to_wat(type_graph: &TypeGraph, instruction: &ir::Instruction) -> String {
    match &instruction {
        ir::Instruction::Constant { node, value } => {
            let valtype = type_graph.type_of(*node).expect("Constant type not known");
            literal_to_wat(valtype, value)
        },
        ir::Instruction::GlobalGet { index } => format!("(global.get $G{})", index),
        ir::Instruction::GlobalSet { index, value } => {
            format!("(global.set $G{} {})", index, instruction_to_wat(type_graph, &value))
        },
        ir::Instruction::Add { node, left, right } => {
            let valtype = type_graph.type_of(*node).expect("Addition type not known");
            if let ValType::Basic(basic_type) = valtype {
                match basic_type {
                    BasicVal::U32 | BasicVal::S32 | BasicVal::I32 => {
                        format!("(i32.add {} {})",
                            instruction_to_wat(type_graph, &left),
                            instruction_to_wat(type_graph, &right)
                        )
                    },
                    BasicVal::U64 | BasicVal::S64 | BasicVal::I64 => {
                        format!("(i64.add {} {})",
                            instruction_to_wat(type_graph, &left),
                            instruction_to_wat(type_graph, &right)
                        )
                    },
                    _ => panic!("Unsupported addition return type for WAT output {:?}", basic_type)
                }
            } else { panic!("Only basic types supported for addition") }
        },
        ir::Instruction::Return { value } => {
            format!("(return {})", instruction_to_wat(type_graph, &value))
        }
    }
}

fn literal_to_wat(valtype: ValType, literal: &Literal) -> String {
    match (valtype, &literal) {
        (ValType::Basic(BasicVal::I32), Literal::Integer(value)) => format!("(i32.const {})", value),
        (ValType::Basic(BasicVal::I64), Literal::Integer(value)) => format!("(i64.const {})", value),
        (ValType::Basic(BasicVal::U32), Literal::Integer(value)) => format!("(i32.const {})", value),
        (ValType::Basic(BasicVal::U64), Literal::Integer(value)) => format!("(i64.const {})", value),
        (ValType::Basic(BasicVal::F32), Literal::Float(value)) => format!("(f32.const {})", value),
        (ValType::Basic(BasicVal::F64), Literal::Float(value)) => format!("(f64.const {})", value),
        _ => todo!()
    }
}

fn valtype_to_wat(valtype: &ValType) -> String {
    match valtype {
        ValType::Basic(BasicVal::U32) => "i32".to_string(),
        ValType::Basic(BasicVal::S32) => "i32".to_string(),
        ValType::Basic(BasicVal::I32) => "i32".to_string(),
        ValType::Basic(BasicVal::U64) => "i64".to_string(),
        ValType::Basic(BasicVal::S64) => "i64".to_string(),
        ValType::Basic(BasicVal::I64) => "i64".to_string(),
        _ => panic!("Unsupported type for WAT output {:?}", valtype)
    }
}