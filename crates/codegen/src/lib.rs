#![allow(clippy::single_match)]

mod builders;
mod code;
mod expression;
mod function;
mod statement;
mod types;

use std::collections::HashMap;

use builders::component::*;
use builders::module::*;
use code::CodeGenerator;
pub use expression::*;
use function::FunctionGenerator;
pub use statement::*;

use ast::{FunctionId, ImportId, PrimitiveType};
use claw_ast as ast;
use claw_resolver::{ResolvedComponent, ResolverError};
use miette::Diagnostic;
use thiserror::Error;

use types::EncodeType;
use wasm_encoder as enc;

pub fn generate(resolved_comp: &ResolvedComponent) -> Result<Vec<u8>, GenerationError> {
    let component = generate_component(resolved_comp)?;
    Ok(component.finalize().finish())
}

fn generate_component(
    resolved_comp: &ResolvedComponent,
) -> Result<ComponentBuilder, GenerationError> {
    let comp = &resolved_comp.component;
    let mut component = ComponentBuilder::default();

    let alloc_module = component.module_bytes(gen_allocator());
    let code_module = component.module(generate_module(resolved_comp)?);

    let mut inline_export_args = Vec::new();
    for import in comp.imports.values() {
        let import_name = comp.get_name(import.ident);

        match &import.external_type {
            ast::ExternalType::Function(fn_type) => {
                // Encode Component Type and Import
                let type_idx = encode_comp_func_type(fn_type, comp, &mut component);
                let func_idx = component.import_func(import_name, type_idx);

                // Lower the Import
                let core_func_idx = component.lower_func(func_idx);

                inline_export_args.push((
                    import_name.to_owned(),
                    InlineExportItem::Func(core_func_idx),
                ));
            }
        }
    }

    let imports_instance = component.inline_export(inline_export_args);

    let alloc_instance = component.instantiate(alloc_module, vec![]);

    let args = vec![
        (
            "claw".to_string(),
            ModuleInstiateArgs::Instance(imports_instance),
        ),
        (
            "alloc".to_string(),
            ModuleInstiateArgs::Instance(alloc_instance),
        ),
    ];
    let code_instance = component.instantiate(code_module, args);

    let memory = component.alias_memory(alloc_instance, "memory");
    let realloc = component.alias_func(alloc_instance, "realloc");

    for function in resolved_comp.component.functions.values() {
        if function.exported {
            let name = comp.get_name(function.ident);
            // Alias module instance export into component
            let core_func_idx = component.alias_func(code_instance, name);
            // Encode component func type
            let type_idx = encode_comp_func_type(function, comp, &mut component);
            // Lift aliased function to component function
            let func_idx = component.lift_func(core_func_idx, type_idx, memory, realloc);
            // Export component function
            component.export_func(name, func_idx, type_idx);
        }
    }

    Ok(component)
}

fn encode_comp_func_type(
    fn_type: &dyn ast::FnTypeInfo,
    comp: &ast::Component,
    builder: &mut ComponentBuilder,
) -> ComponentTypeIndex {
    let params = fn_type.get_args().iter().map(|(name, type_id)| {
        let name = comp.get_name(*name);
        let valtype = comp.get_type(*type_id);
        (name, valtype.to_comp_valtype(comp))
    });

    let result = fn_type.get_return_type().map(|return_type| {
        let valtype = comp.get_type(return_type);
        valtype.to_comp_valtype(comp)
    });
    builder.func_type(params, result)
}

fn generate_module(resolved_comp: &ResolvedComponent) -> Result<enc::Module, GenerationError> {
    let generator = ComponentGenerator::default();
    generator.generate(resolved_comp)
}

#[derive(Default)]
pub struct ComponentGenerator {
    module: ModuleBuilder,

    func_idx_for_import: HashMap<ImportId, ModuleFunctionIndex>,
    func_idx_for_func: HashMap<FunctionId, ModuleFunctionIndex>,
}

#[derive(Error, Debug, Diagnostic)]
pub enum GenerationError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Resolver(#[from] ResolverError),
}

impl ComponentGenerator {
    pub fn generate(
        mut self,
        resolved_comp: &ResolvedComponent,
    ) -> Result<enc::Module, GenerationError> {
        let comp = &resolved_comp.component;
        // There is only ever one memory, memory zero
        let (_memory, realloc) = self.encode_import_allocator();

        for (id, import) in comp.imports.iter() {
            self.encode_import(id, import, comp);
        }

        self.encode_globals(resolved_comp)?;

        let mut functions = Vec::new();
        for (id, function) in resolved_comp.component.functions.iter() {
            let func_gen = FunctionGenerator::new(function, comp);
            functions.push((id, func_gen));
            self.encode_func(id, function, comp)?;
        }

        for (id, function) in functions {
            let func_gen = CodeGenerator::new(&mut self, resolved_comp, function, realloc, id)?;
            func_gen.finalize()?;
        }

        Ok(self.module.finalize())
    }

    fn encode_import_allocator(&mut self) -> (ModuleMemoryIndex, ModuleFunctionIndex) {
        let memory: ModuleMemoryIndex = self.module.import_memory("alloc", "memory");
        let realloc_type = self
            .module
            .func_type(vec![enc::ValType::I32; 4], vec![enc::ValType::I32; 1]);
        let realloc = self.module.import_func("alloc", "realloc", realloc_type);
        (memory, realloc)
    }

    fn encode_import(&mut self, id: ImportId, import: &ast::Import, comp: &ast::Component) {
        let import_name = comp.get_name(import.ident);

        let comp = &comp;
        match &import.external_type {
            ast::ExternalType::Function(fn_type) => {
                let func_gen = FunctionGenerator::new(fn_type, comp);
                let type_idx = func_gen.encode_func_type(&mut self.module);
                let func_idx = self.module.import_func("claw", import_name, type_idx);
                self.func_idx_for_import.insert(id, func_idx);
            }
        }
    }

    fn encode_globals(&mut self, resolved_comp: &ResolvedComponent) -> Result<(), GenerationError> {
        let comp = &resolved_comp.component;
        for (id, global) in comp.globals.iter() {
            let valtypes = global.type_id.flatten(comp);
            assert_eq!(valtypes.len(), 1, "Cannot use non-primitive globals");
            let valtype = valtypes[0];

            let init_expr = if let Some(init_value) = resolved_comp.global_vals.get(&id) {
                let valtype = comp.get_type(global.type_id);
                match valtype {
                    ast::ValType::Result { .. } => todo!(),
                    ast::ValType::String => todo!(),
                    ast::ValType::Primitive(ptype) => literal_to_const_expr(init_value, *ptype),
                }
            } else {
                panic!("Cannot generate WASM for unresolved global")
            };

            self.module.global(global.mutable, valtype, &init_expr);
        }
        Ok(())
    }

    fn encode_func(
        &mut self,
        id: FunctionId,
        function: &ast::Function,
        comp: &ast::Component,
    ) -> Result<(), GenerationError> {
        let func_gen = FunctionGenerator::new(function, comp);
        let type_idx = func_gen.encode_func_type(&mut self.module);
        let func_idx = self.module.function(type_idx);

        self.func_idx_for_func.insert(id, func_idx);

        if function.exported {
            let ident = function.ident;
            let name = comp.get_name(ident);
            // Export function from module
            self.module.export_func(name, func_idx);
        }

        Ok(())
    }
}

// Literal

fn literal_to_const_expr(literal: &ast::Literal, ptype: ast::PrimitiveType) -> enc::ConstExpr {
    use ast::Literal;
    match (ptype, literal) {
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

// ValType

pub fn gen_allocator() -> Vec<u8> {
    let wat = include_str!("../allocator.wat");
    wat::parse_str(wat).unwrap()
}
