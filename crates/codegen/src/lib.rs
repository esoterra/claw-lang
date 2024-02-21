#![allow(clippy::single_match)]

mod component_builder;
mod expression;
mod module_builder;
mod statement;

use std::collections::HashMap;

use component_builder::*;
pub use expression::*;
use module_builder::*;
pub use statement::*;

use ast::{FunctionId, ImportId, PrimitiveType, TypeId};
use claw_ast as ast;
use claw_resolver::{FunctionResolver, ResolvedComponent, ResolvedType, ResolverError};
use miette::Diagnostic;
use thiserror::Error;

use wasm_encoder as enc;
use wasm_encoder::Instruction;

pub struct CodeGenerator {
    module: ModuleBuilder,
    component: ComponentBuilder,

    imports_instance: ComponentModuleInstanceIndex,
    code_module: ComponentModuleIndex,
    code_instance: ComponentModuleInstanceIndex,

    func_idx_for_import: HashMap<ImportId, ModuleFunctionIndex>,
    func_idx_for_func: HashMap<FunctionId, ModuleFunctionIndex>,

    inline_export_args: Vec<(String, InlineExportItem)>,
}

#[derive(Error, Debug, Diagnostic)]
pub enum GenerationError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Resolver(#[from] ResolverError),
}

impl Default for CodeGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl CodeGenerator {
    pub fn new() -> Self {
        let mut component = ComponentBuilder::default();

        let alloc_module = component.module_bytes(gen_allocator());
        let code_module = component.reserve_module();

        let imports_instance = component.reserve_inline_export();
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

        Self {
            module: ModuleBuilder::default(),
            component,
            imports_instance,
            code_module,
            code_instance,
            func_idx_for_import: Default::default(),
            func_idx_for_func: Default::default(),
            inline_export_args: Default::default(),
        }
    }

    pub fn generate(
        mut self,
        resolved_comp: &ResolvedComponent,
    ) -> Result<Vec<u8>, GenerationError> {
        self.encode_globals(resolved_comp);

        self.encode_import_allocator();

        for (id, import) in resolved_comp.component.imports.iter() {
            self.encode_import(id, import, resolved_comp);
        }

        for (id, function) in resolved_comp.component.functions.iter() {
            self.encode_func(id, function, resolved_comp)?;
        }

        for (id, function) in resolved_comp.component.functions.iter() {
            self.encode_code(id, function, resolved_comp)?;
        }

        Ok(self.emit_bytes())
    }

    fn encode_import_allocator(&mut self) {
        let _memory = self.module.import_memory("alloc", "memory");
        let realloc_type = self
            .module
            .func_type(vec![enc::ValType::I32; 4], vec![enc::ValType::I32; 1]);
        self.module.import_func("alloc", "realloc", realloc_type);
    }

    fn encode_import(
        &mut self,
        id: ImportId,
        import: &ast::Import,
        resolved_comp: &ResolvedComponent,
    ) {
        let import_name = resolved_comp.component.get_name(import.ident);

        let comp = &resolved_comp.component;
        match &import.external_type {
            ast::ExternalType::Function(fn_type) => {
                // Encode Module Type and Import
                let mod_type_idx = self.encode_mod_func_type(fn_type, comp);
                let mod_func_idx = self.module.import_func("claw", import_name, mod_type_idx);

                self.func_idx_for_import.insert(id, mod_func_idx);

                // Encode Component Type and Import
                let comp_type_idx = self.encode_comp_func_type(fn_type, comp);
                let comp_func_idx = self.component.import_func(import_name, comp_type_idx);

                // Lower the Import
                let comp_core_func_idx = self.component.lower_func(comp_func_idx);

                self.inline_export_args.push((
                    import_name.to_owned(),
                    InlineExportItem::Func(comp_core_func_idx),
                ));
            }
        }
    }

    fn encode_globals(&mut self, component: &ResolvedComponent) {
        for (id, global) in component.component.globals.iter() {
            let valtype = type_id_to_core_valtype(global.type_id, &component.component);

            let init_expr = if let Some(init_value) = component.global_vals.get(&id) {
                let valtype = component.component.get_type(global.type_id);
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
    }

    fn encode_func(
        &mut self,
        id: FunctionId,
        function: &ast::Function,
        context: &ResolvedComponent,
    ) -> Result<(), GenerationError> {
        let comp = &context.component;

        let mod_type_idx = self.encode_mod_func_type(function, comp);
        let mod_func_idx = self.module.function(mod_type_idx);

        self.func_idx_for_func.insert(id, mod_func_idx);

        if function.exported {
            self.encode_func_export(mod_func_idx, function, context);
        }

        Ok(())
    }

    fn encode_code(
        &mut self,
        id: FunctionId,
        function: &ast::Function,
        context: &ResolvedComponent,
    ) -> Result<(), GenerationError> {
        let resolver = context.resolved_funcs.get(&id).unwrap();
        let locals = encode_locals(resolver, context)?;
        let mut builder = enc::Function::new(locals);

        for statement in function.body.iter() {
            encode_statement(self, context, *statement, id, &mut builder)?;
        }
        builder.instruction(&Instruction::End);

        let mod_func_idx = *self.func_idx_for_func.get(&id).unwrap();
        self.module.code(mod_func_idx, builder);
        Ok(())
    }

    fn encode_func_export(
        &mut self,
        mod_func_idx: ModuleFunctionIndex,
        function: &ast::Function,
        context: &ResolvedComponent,
    ) {
        let comp = &context.component;
        let ident = function.ident;
        let name = context.component.get_name(ident);

        // Export function from module
        self.module.export_func(name, mod_func_idx);
        // Alias module instance export into component
        let comp_core_func_idx = self.component.alias_func(self.code_instance, name);
        // Encode component func type
        let comp_type_idx = self.encode_comp_func_type(function, comp);
        // Lift aliased function to component function
        let comp_func_idx = self.component.lift_func(comp_core_func_idx, comp_type_idx);
        // Export component function
        self.component
            .export_func(name, comp_func_idx, comp_type_idx);
    }

    fn emit_bytes(mut self) -> Vec<u8> {
        // Fill in imports instance
        self.component
            .fill_inline_export_args(self.imports_instance, self.inline_export_args);

        // Fill in code module & instance
        let module = self.module.finalize();
        self.component.fill_module(self.code_module, module);

        self.component.finalize().finish()
    }

    fn encode_mod_func_type(
        &mut self,
        fn_type: &dyn ast::FnTypeInfo,
        comp: &ast::Component,
    ) -> ModuleTypeIndex {
        let params = fn_type
            .get_args()
            .iter()
            .map(|(_name, type_id)| type_id_to_core_valtype(*type_id, comp));

        match fn_type.get_return_type() {
            Some(return_type) => {
                let result_type = type_id_to_core_valtype(return_type, comp);
                self.module.func_type(params, [result_type])
            }
            None => self.module.func_type(params, []),
        }
    }

    fn encode_comp_func_type(
        &mut self,
        fn_type: &dyn ast::FnTypeInfo,
        comp: &ast::Component,
    ) -> ComponentTypeIndex {
        let params = fn_type.get_args().iter().map(|(name, type_id)| {
            let name = comp.get_name(*name);
            let valtype = comp.get_type(*type_id);
            (name, valtype_to_comp_valtype(valtype))
        });

        let result = fn_type.get_return_type().map(|return_type| {
            let valtype = comp.get_type(return_type);
            valtype_to_comp_valtype(valtype)
        });
        self.component.func_type(params, result)
    }
}

fn encode_locals(
    resolver: &FunctionResolver,
    resolved_comp: &ResolvedComponent,
) -> Result<Vec<(u32, enc::ValType)>, GenerationError> {
    let mut locals = Vec::with_capacity(resolver.locals.len());
    for (id, _local) in resolver.locals.iter() {
        let rtype = resolver.get_resolved_local_type(id, &resolved_comp.component)?;
        locals.push((1, rtype_to_core_valtype(rtype, &resolved_comp.component)));
    }
    Ok(locals)
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

// ResolvedType

fn rtype_to_core_valtype(rtype: ResolvedType, component: &ast::Component) -> enc::ValType {
    match rtype {
        ResolvedType::Unit => panic!("Not able to encode as valtype"),
        ResolvedType::Primitive(ptype) => ptype_to_valtype(ptype),
        ResolvedType::ValType(type_id) => type_id_to_core_valtype(type_id, component),
    }
}

pub fn rtype_to_ptype(rtype: ResolvedType, component: &ast::Component) -> Option<PrimitiveType> {
    match rtype {
        ResolvedType::Unit => None,
        ResolvedType::Primitive(ptype) => Some(ptype),
        ResolvedType::ValType(type_id) => match component.get_type(type_id) {
            ast::ValType::Result { .. } => None,
            ast::ValType::String => None,
            ast::ValType::Primitive(ptype) => Some(*ptype),
        },
    }
}

// TypeId

fn type_id_to_core_valtype(type_id: TypeId, component: &ast::Component) -> enc::ValType {
    let valtype = component.get_type(type_id);
    valtype_to_core_valtype(valtype)
}

// ast::ValType

fn valtype_to_core_valtype(valtype: &ast::ValType) -> enc::ValType {
    match valtype {
        ast::ValType::Primitive(ptype) => ptype_to_valtype(*ptype),
        _ => panic!("Cannot encode non-primitive as a valtype"),
    }
}

fn valtype_to_comp_valtype(valtype: &ast::ValType) -> enc::ComponentValType {
    match valtype {
        ast::ValType::Result { .. } => todo!(),
        ast::ValType::String => todo!(),
        ast::ValType::Primitive(ptype) => ptype_to_comp_valtype(*ptype),
    }
}

// PrimitiveType

fn ptype_to_valtype(ptype: PrimitiveType) -> enc::ValType {
    use ast::PrimitiveType as PType;
    match ptype {
        PType::U32 | PType::S32 | PType::U16 | PType::S16 | PType::U8 | PType::S8 | PType::Bool => {
            enc::ValType::I32
        }

        PType::U64 | PType::S64 => enc::ValType::I64,

        PType::F32 => enc::ValType::F32,
        PType::F64 => enc::ValType::F64,
    }
}

fn ptype_to_ptype_valtype(ptype: PrimitiveType) -> enc::PrimitiveValType {
    use ast::PrimitiveType as PType;
    match ptype {
        PType::U64 => enc::PrimitiveValType::U64,
        PType::U32 => enc::PrimitiveValType::U32,
        PType::U16 => enc::PrimitiveValType::U16,
        PType::U8 => enc::PrimitiveValType::U8,
        PType::S64 => enc::PrimitiveValType::S64,
        PType::S32 => enc::PrimitiveValType::S32,
        PType::S16 => enc::PrimitiveValType::S16,
        PType::S8 => enc::PrimitiveValType::S8,
        PType::F32 => enc::PrimitiveValType::Float32,
        PType::F64 => enc::PrimitiveValType::Float64,
        PType::Bool => enc::PrimitiveValType::Bool,
    }
}

fn ptype_to_comp_valtype(ptype: PrimitiveType) -> enc::ComponentValType {
    enc::ComponentValType::Primitive(ptype_to_ptype_valtype(ptype))
}

fn ptype_to_core_valtype(ptype: PrimitiveType) -> enc::ValType {
    use ast::PrimitiveType as PType;

    match ptype {
        PType::Bool => enc::ValType::I32,

        PType::U64 | PType::S64 => enc::ValType::I64,

        PType::U32 | PType::U16 | PType::U8 | PType::S32 | PType::S16 | PType::S8 => {
            enc::ValType::I32
        }

        PType::F32 => enc::ValType::F32,
        PType::F64 => enc::ValType::F64,
    }
}

// ValType

pub fn gen_allocator() -> Vec<u8> {
    let wat = include_str!("../allocator.wat");
    wat::parse_str(wat).unwrap()
}
