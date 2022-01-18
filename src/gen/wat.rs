use std::fmt::Write;

use crate::ast::types::{ValType, BasicVal};
use crate::ir::{self, NeedsResolve};
use crate::resolver::ItemID;

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
        let valtype = valtype_to_wat(&global.type_.value);
        if let NeedsResolve::Resolved(init_value) = &global.initial_value {
            let _ = write!(result, "   (global $G{} {} {})\n", index, valtype, constant_to_wat(&init_value));
        }
    }
}

fn functions_to_wat(functions: &Vec<ir::Function>, result: &mut String) {
    for (index, function) in functions.iter().enumerate() {
        let valtype = valtype_to_wat(&function.signature.return_type.value);
        let _ = write!(result, "   (func $F{} (result {})\n", index, valtype);
        if let NeedsResolve::Resolved(instructions) = &function.body {
            for instruction in instructions.iter() {
                let _ = write!(result, "      {}\n", instruction_to_wat(instruction));
            }
        } else { panic!("Cannot generate WASM for unresolved function")}
        let _ = write!(result, "   )\n");
    }
}

fn exports_to_wat(exports: &Vec<ir::Export>, result: &mut String) {
    for export in exports.iter() {
        match &export.id {
            ItemID::Function(index) => {
                let _ = write!(result, "   (export {:?} (func $F{}))\n", export.ident.value, index);
            },
            _ => panic!("Only function exports supported")
        }
    }
}

fn constant_to_wat(constant: &ir::Constant) -> String {
    match &constant {
        ir::Constant::I32 { value } => format!("{}", value)
    }
}

fn valtype_to_wat(valtype: &ValType) -> String {
    match valtype {
        ValType::Basic(BasicVal::U32) => "i32".to_string(),
        ValType::Basic(BasicVal::S32) => "i32".to_string(),
        ValType::Basic(BasicVal::I32) => "i32".to_string(),
        _ => panic!("Unsupported type for WAT output {:?}", valtype)
    }
}

fn instruction_to_wat(instruction: &ir::Instruction) -> String {
    match &instruction {
        ir::Instruction::Constant { value } => constant_to_wat(value),
        ir::Instruction::GlobalGet { index } => format!("global.get $G{}", index),
        ir::Instruction::GlobalSet { index } => format!("global.set $G{}", index),
        ir::Instruction::Add { result_type } => {
            match &result_type {
                BasicVal::U32 => format!("i32.add"),
                BasicVal::S32 => format!("i32.add"),
                BasicVal::I32 => format!("i32.add"),
                _ => panic!("Unsupported addition return type for WAT output {:?}", result_type)
            }
        },
        ir::Instruction::Return => "return".to_string()
    }
}