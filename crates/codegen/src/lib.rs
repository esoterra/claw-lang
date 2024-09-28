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

pub const MAX_FLAT_PARAMS: u8 = 16;
pub const MAX_FLAT_RESULTS: u8 = 1;

pub fn generate(
    comp: &ast::Component,
    rcomp: &ResolvedComponent,
) -> Result<Vec<u8>, GenerationError> {
    let builder = generate_component(comp, rcomp)?;
    Ok(builder.finalize().finish())
}

fn generate_component(
    comp: &ast::Component,
    rcomp: &ResolvedComponent,
) -> Result<ComponentBuilder, GenerationError> {
    let mut builder = ComponentBuilder::default();

    let alloc_module = builder.module_bytes(gen_allocator());

    let args: Vec<(&str, ModuleInstantiateArgs)> = vec![];
    let alloc_instance = builder.instantiate(alloc_module, args);

    let memory = builder.alias_memory(alloc_instance, "memory");
    let realloc = builder.alias_core_func(alloc_instance, "realloc");

    let import_encoder = imports::ImportEncoder::new(&mut builder, comp, rcomp, memory, realloc);
    let imports = import_encoder.encode()?;

    let function_encoder = function::FunctionEncoder::new(comp, rcomp);
    let functions = function_encoder.encode()?;

    let code_module = builder.module(module::generate(comp, rcomp, &imports, &functions)?);

    let args = vec![
        ("alloc", ModuleInstantiateArgs::Instance(alloc_instance)),
        (
            "claw",
            ModuleInstantiateArgs::Instance(imports.imports_instance),
        ),
    ];
    let code_instance = builder.instantiate(code_module, args);

    generate_exports(comp, rcomp, code_instance, memory, realloc, &mut builder)?;

    Ok(builder)
}

struct ExportGenerator<'ctx> {
    comp: &'ctx ast::Component,
    rcomp: &'ctx ResolvedComponent,

    code_instance: ComponentModuleInstanceIndex,
    memory: ComponentCoreMemoryIndex,
    realloc: ComponentCoreFunctionIndex,
}

impl<'ctx> ExportGenerator<'ctx> {
    fn generate(&mut self, builder: &mut ComponentBuilder) -> Result<(), GenerationError> {
        for (_, function) in self.comp.iter_functions() {
            if function.exported {
                self.generate_function_export(function, builder)?;
            }
        }

        Ok(())
    }

    fn generate_function_export(
        &mut self,
        function: &ast::Function,
        builder: &mut ComponentBuilder,
    ) -> Result<(), GenerationError> {
        let name = self.comp.get_name(function.ident);
        // Alias module instance export into component
        let core_func_idx = builder.alias_core_func(self.code_instance, name);
        // Alias the post return
        let post_return_idx =
            builder.alias_core_func(self.code_instance, format!("{}_post_return", name).as_str());

        // Encode component func type
        let params = function.params.iter().map(|(param_name, param_type)| {
            let param_name = self.comp.get_name(*param_name);
            let param_type = self.comp.get_type(*param_type);
            let param_type = match param_type {
                ast::ValType::Result(_) => todo!(),
                ast::ValType::Primitive(ptype) => ptype.to_comp_valtype(self.comp, self.rcomp),
            };
            (param_name, param_type)
        });
        let results = function.results.map(|result_type| {
            let result_type = self.comp.get_type(result_type);
            match result_type {
                ast::ValType::Result(_) => todo!(),
                ast::ValType::Primitive(ptype) => ptype.to_comp_valtype(self.comp, self.rcomp),
            }
        });
        let type_idx = builder.func_type(params, results);

        // Lift aliased function to component function
        let func_idx = builder.lift_func(
            core_func_idx,
            type_idx,
            self.memory,
            self.realloc,
            post_return_idx,
        );
        // Export component function
        builder.export_func(name, func_idx, type_idx);

        Ok(())
    }
}

fn generate_exports(
    comp: &ast::Component,
    rcomp: &ResolvedComponent,
    code_instance: ComponentModuleInstanceIndex,
    memory: ComponentCoreMemoryIndex,
    realloc: ComponentCoreFunctionIndex,
    builder: &mut ComponentBuilder,
) -> Result<(), GenerationError> {
    let mut gen = ExportGenerator {
        comp,
        rcomp,
        code_instance,
        memory,
        realloc,
    };
    gen.generate(builder)
}

// ValType

pub fn gen_allocator() -> &'static [u8] {
    let allocator_wasm = include_bytes!(concat!(env!("OUT_DIR"), "/allocator.wasm"));
    allocator_wasm
}
