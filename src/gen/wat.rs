use std::fmt::Write;

use crate::ast::{
    module::FunctionSignature,
    expressions::Literal,
    types::ValType
};
use crate::resolver::ModuleItem;
use crate::ir::type_graph::TypeNode;
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

        let valtype_s = valtype_to_wat(global.type_.as_ref());
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
        let signature = fn_signature_to_wat(&function.signature);
        let _ = write!(result, "   (func $F{} {}\n", index, signature);

        let type_graph = match &function.type_graph {
            NeedsResolve::Resolved(type_graph) => type_graph,
            _ => panic!("Cannot generate WASM for unresolved function")
        };

        if let Some(local_defs) = fn_locals_to_wat(type_graph, function) {
            let _ = write!(result, "      {}\n", local_defs);
        }

        if let NeedsResolve::Resolved(instructions) = &function.body {
            let base_context = Context::Indented { amount: 2 };
            for instruction in instructions.iter() {
                instruction_to_wat(&type_graph, instruction, result, base_context);
            }
        } else { panic!("Cannot generate WASM for unresolved function") }
        let _ = write!(result, "   )\n");
    }
}

fn fn_signature_to_wat(signature: &FunctionSignature) -> String {
    let mut parts: Vec<String> = signature.arguments
        .iter()
        .enumerate()
        .map(|(i, (_name, valtype))|
            format!("(param $L{} {})", i, valtype_to_wat(valtype.as_ref()))
        )
        .collect();

    let result_type = valtype_to_wat(signature.return_type.as_ref());
    parts.push(format!("(result {})", result_type));
    parts.join(" ")
}

fn fn_locals_to_wat(type_graph: &TypeGraph, function: &ir::Function) -> Option<String> {
    if let NeedsResolve::Resolved(locals) = &function.locals {
        if locals.len() == function.signature.arguments.len() {
            return None;
        }

        let local_str = locals.iter()
            .enumerate()
            .skip(function.signature.arguments.len())
            .map(|(i, local)| {
                let valtype = type_graph.type_of(*local).unwrap();
                format!("(local $L{} {})", i, valtype_to_wat(&valtype))
            })
            .collect::<Vec<String>>()
            .join(" ");
        Some(local_str)
    } else {
        panic!("RIP!!!!")
    }
}

fn exports_to_wat(exports: &Vec<ir::Export>, result: &mut String) {
    for export in exports.iter() {
        match &export.id {
            ModuleItem::Function(index) => {
                let _ = write!(result, "   (export {:?} (func $F{}))\n", export.ident.as_ref(), index);
            },
            _ => panic!("Only function exports supported")
        }
    }
}

fn instruction_to_wat(
    type_graph: &TypeGraph, instruction: &ir::Instruction,
    result: &mut String, line: Context
) {
    line.emit_prefix(result);
    match &instruction {
        ir::Instruction::Constant {
            node,
            value
        } => {
            let valtype = type_graph.type_of(*node).expect("Constant type not known");
            let const_expr = literal_to_wat(valtype, value);
            let _ = write!(result, "{}", const_expr);
        },
        ir::Instruction::GlobalGet { index } => {
            let _ = write!(result, "(global.get $G{})", index);
        },
        ir::Instruction::GlobalSet {
            index,
            value
        } => {
            let _ = write!(result, "(global.set $G{} ", index);
            instruction_to_wat(type_graph, &value, result, Context::Inline);
            let _ = write!(result, ")");
        },
        ir::Instruction::LocalGet { node: _, index } => {
            let _ = write!(result, "(local.get $L{})", index);
        },
        ir::Instruction::LocalSet {
            index,
            value
        } => {
            let _ = write!(result, "(local.set $L{} ", index);
            instruction_to_wat(type_graph, &value, result, Context::Inline);
            let _ = write!(result, ")");
        },
        ir::Instruction::BinaryArith {
            node,
            op,
            left,
            right
        } => {
            let mnemonic = match op {
                ir::BinArithOp::Add => "add",
                ir::BinArithOp::Sub => "sub",
                ir::BinArithOp::Mul => "mul",
            };
            binary_expr_to_wat(
                type_graph,
                mnemonic, *node,
                left, right, result
            );
        },
        ir::Instruction::BinaryRel {
            node,
            op,
            left,
            right
        } => {
            let mnemonic = match op {
                ir::BinRelOp::EQ => "eq",
                ir::BinRelOp::LT => "lt",
            };
            binary_expr_to_wat(
                type_graph,
                mnemonic, *node,
                left, right, result
            );
        },
        ir::Instruction::If {
            body
        } => {
            let _ = write!(result, "(if\n");
            for instruction in body {
                instruction_to_wat(type_graph, instruction, result, line.indent());
            }
            line.emit_prefix(result);
            let _ = write!(result, "end)");
        },
        ir::Instruction::Return { value } => {
            let _ = write!(result, "(return ");
            instruction_to_wat(type_graph, &value, result, Context::Inline);
            let _ = write!(result, ")");
        },
        _ => todo!()
    };
    line.emit_suffix(result);
}

fn binary_expr_to_wat(
    type_graph: &TypeGraph,
    label: &str,
    node: TypeNode,
    left: &Box<ir::Instruction>,
    right: &Box<ir::Instruction>,
    result: &mut String
) {
    let valtype = type_graph.type_of(node)
        .expect("Binary expr type unknown");
    let _ = write!(result, "({}.{} ", valtype_to_wat(&valtype), label);
    instruction_to_wat(type_graph, &left, result, Context::Inline);
    let _ = write!(result, " ");
    instruction_to_wat(type_graph, &right, result, Context::Inline);
    let _ = write!(result, ")");
}

fn literal_to_wat(valtype: ValType, literal: &Literal) -> String {
    match (valtype, &literal) {
        (ValType::S32, Literal::Integer(value)) => format!("(i32.const {})", value),
        (ValType::S64, Literal::Integer(value)) => format!("(i64.const {})", value),
        (ValType::U32, Literal::Integer(value)) => format!("(i32.const {})", value),
        (ValType::U64, Literal::Integer(value)) => format!("(i64.const {})", value),
        (ValType::F32, Literal::Float(value)) => format!("(f32.const {})", value),
        (ValType::F64, Literal::Float(value)) => format!("(f64.const {})", value),
        _ => todo!()
    }
}

fn valtype_to_wat(valtype: &ValType) -> String {
    match valtype {
        ValType::U32 => "i32".to_string(),
        ValType::S32 => "i32".to_string(),
        ValType::U64 => "i64".to_string(),
        ValType::S64 => "i64".to_string(),
        ValType::Bool => "i32".to_string(),
        _ => panic!("Unsupported type for WAT output {:?}", valtype)
    }
}

#[derive(Debug, Clone, Copy)]
enum Context {
    Inline,
    Indented { amount: usize }
}

impl Context {
    fn indent(&self) -> Self {
        match *self {
            Context::Inline => Context::Indented { amount: 1 },
            Context::Indented { amount } => Context::Indented { amount: amount + 1 }
        }
    }

    fn emit_prefix(&self, result: &mut String) {
        match *self {
            Context::Inline => {},
            Context::Indented { amount } => {
                let _ = write!(result, "{}", "   ".repeat(amount));
            }
        }
    }

    fn emit_suffix(&self, result: &mut String) {
        match *self {
            Context::Inline => {},
            Context::Indented { amount: _ } => {
                let _ = write!(result, "\n");
            }
        }
    }
}