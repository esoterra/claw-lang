#![allow(clippy::single_match)]

mod builders;
mod code;
mod expression;
mod function;
mod imports;
mod module;
mod statement;
mod types;

use builders::component::*;

use claw_ast as ast;
use claw_resolver::{ResolvedComponent, ResolverError};
use miette::Diagnostic;
use thiserror::Error;
use types::EncodeType;

#[derive(Error, Debug, Diagnostic)]
pub enum GenerationError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Resolver(#[from] ResolverError),
}

pub fn generate(resolved_comp: &ResolvedComponent) -> Result<Vec<u8>, GenerationError> {
    let component = generate_component(resolved_comp)?;
    Ok(component.finalize().finish())
}

fn generate_component(
    resolved_comp: &ResolvedComponent,
) -> Result<ComponentBuilder, GenerationError> {
    let mut builder = ComponentBuilder::default();

    let alloc_module = builder.module_bytes(gen_allocator());

    let args: Vec<(&str, ModuleInstantiateArgs)> = vec![];
    let alloc_instance = builder.instantiate(alloc_module, args);

    let memory = builder.alias_memory(alloc_instance, "memory");
    let realloc = builder.alias_core_func(alloc_instance, "realloc");

    let import_encoder = imports::ImportEncoder::new(&mut builder, resolved_comp, memory, realloc);
    let imports = import_encoder.encode()?;

    let function_encoder = function::FunctionEncoder::new(resolved_comp);
    let functions = function_encoder.encode()?;

    let code_module = builder.module(module::generate(resolved_comp, &imports, &functions)?);

    let args = vec![
        ("alloc", ModuleInstantiateArgs::Instance(alloc_instance)),
        ("claw", ModuleInstantiateArgs::Instance(imports.imports_instance)),
    ];
    let code_instance = builder.instantiate(code_module, args);

    generate_exports(&mut builder, resolved_comp, code_instance, memory, realloc)?;

    Ok(builder)
}

fn generate_exports(
    component: &mut ComponentBuilder,
    resolved_comp: &ResolvedComponent,
    code_instance: ComponentModuleInstanceIndex,
    memory: ComponentCoreMemoryIndex,
    realloc: ComponentCoreFunctionIndex,
) -> Result<(), GenerationError> {
    let comp = &resolved_comp.component;

    for function in resolved_comp.component.functions.values() {
        if function.exported {
            let name = comp.get_name(function.ident);
            // Alias module instance export into component
            let core_func_idx = component.alias_core_func(code_instance, name);
            // Alias the post return
            let post_return_idx =
                component.alias_core_func(code_instance, format!("{}_post_return", name).as_str());

            // Encode component func type
            let params = function.params.iter().map(|(param_name, param_type)| {
                let param_name = resolved_comp.component.get_name(*param_name);
                let param_type = comp.get_type(*param_type);
                let param_type = match param_type {
                    ast::ValType::Result(_) => todo!(),
                    ast::ValType::Primitive(ptype) => ptype.to_comp_valtype(&resolved_comp),
                };
                (param_name, param_type)
            });
            let results = function.results.map(|result_type| {
                let result_type = comp.get_type(result_type);
                match result_type {
                    ast::ValType::Result(_) => todo!(),
                    ast::ValType::Primitive(ptype) => ptype.to_comp_valtype(&resolved_comp),
                }
            });
            let type_idx = component.func_type(params, results);

            // Lift aliased function to component function
            let func_idx =
                component.lift_func(core_func_idx, type_idx, memory, realloc, post_return_idx);
            // Export component function
            component.export_func(name, func_idx, type_idx);
        }
    }
    Ok(())
}

// ValType

pub fn gen_allocator() -> Vec<u8> {
    let wat = include_str!("../allocator.wat");
    wat::parse_str(wat).unwrap()
}
