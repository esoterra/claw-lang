mod component_builder;
mod expression;
mod module_builder;
mod statement;

use std::collections::HashMap;

use component_builder::*;
pub use expression::*;
use module_builder::*;
pub use statement::*;

use crate::{
    ast::{self, FunctionId, ImportId, TypeId},
    context::{WithContext, C},
    resolver::{FunctionResolver, ResolvedComponent, ResolvedType, ResolverError},
};
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
        let comp = &component.component;

        for (id, global) in component.component.globals.iter() {
            let valtype = global.type_id.with(comp).as_valtype();

            let init_expr = if let Some(init_value) = component.global_vals.get(&id) {
                let valtype = component.component.get_type(global.type_id);
                match valtype {
                    ast::ValType::Result { .. } => todo!(),
                    ast::ValType::String => todo!(),
                    ast::ValType::Primitive(p) => init_value.to_const_expr(*p),
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
            .map(|(_name, valtype)| valtype.with(comp).as_valtype());

        match fn_type.get_return_type() {
            Some(return_type) => {
                let result_type = return_type.with(comp).as_valtype();
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
            (name, valtype.with(comp).as_comp_valtype())
        });

        let result = fn_type.get_return_type().map(|return_type| {
            let valtype = comp.get_type(return_type);
            valtype.with(comp).as_comp_valtype()
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
        locals.push((1, rtype.with(&resolved_comp.component).as_valtype()));
    }
    Ok(locals)
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum Signedness {
    Unsigned,
    Signed,
    NotApplicable,
}

impl ast::PrimitiveType {
    fn core_type(&self) -> enc::ValType {
        use ast::PrimitiveType;

        match self {
            PrimitiveType::Bool => enc::ValType::I32,

            PrimitiveType::U64 | PrimitiveType::S64 => enc::ValType::I64,

            PrimitiveType::U32
            | PrimitiveType::U16
            | PrimitiveType::U8
            | PrimitiveType::S32
            | PrimitiveType::S16
            | PrimitiveType::S8 => enc::ValType::I32,

            PrimitiveType::F32 => enc::ValType::F32,
            PrimitiveType::F64 => enc::ValType::F64,
        }
    }

    fn signedness(&self) -> Signedness {
        use ast::PrimitiveType;

        match self {
            PrimitiveType::U64 | PrimitiveType::U32 | PrimitiveType::U16 | PrimitiveType::U8 => {
                Signedness::Unsigned
            }
            PrimitiveType::S64 | PrimitiveType::S32 | PrimitiveType::S16 | PrimitiveType::S8 => {
                Signedness::Signed
            }
            _ => Signedness::NotApplicable,
        }
    }
}

// Literal

impl ast::Literal {
    fn to_const_expr(&self, ptype: ast::PrimitiveType) -> enc::ConstExpr {
        use ast::{Literal, PrimitiveType};
        match (ptype, self) {
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
}

// ResolvedType

#[allow(dead_code)]
impl<'ctx> C<'ctx, ResolvedType, ast::Component> {
    fn as_valtype(&self) -> enc::ValType {
        match self.value {
            ResolvedType::Unit => panic!("Not able to encode as valtype"),
            ResolvedType::Primitive(p) => p.as_valtype(),
            ResolvedType::ValType(type_id) => type_id.with(self.context).as_valtype(),
        }
    }

    fn as_comp_valtype(&self) -> enc::ComponentValType {
        match self.value {
            ResolvedType::Unit => panic!("Not able to encode as valtype"),
            ResolvedType::Primitive(p) => p.as_comp_valtype(),
            ResolvedType::ValType(t) => t.with(self.context).as_comp_valtype(),
        }
    }
}

// TypeId

#[allow(dead_code)]
impl<'ctx> C<'ctx, TypeId, ast::Component> {
    fn as_valtype(&self) -> enc::ValType {
        let valtype = self.context.get_type(*self.value);
        valtype.with(self.context).as_valtype()
    }

    fn as_comp_valtype(&self) -> enc::ComponentValType {
        let valtype = self.context.get_type(*self.value);
        valtype.with(self.context).as_comp_valtype()
    }
}

// ast::ValType

impl<'ctx> C<'ctx, ast::ValType, ast::Component> {
    fn as_valtype(&self) -> enc::ValType {
        match self.value {
            ast::ValType::Primitive(p) => p.as_valtype(),
            _ => panic!("Cannot encode non-primitive as a valtype"),
        }
    }

    fn as_comp_valtype(&self) -> enc::ComponentValType {
        match self.value {
            ast::ValType::Result { .. } => todo!(),
            ast::ValType::String => todo!(),
            ast::ValType::Primitive(p) => p.as_comp_valtype(),
        }
    }
}

// PrimitiveType

impl ast::PrimitiveType {
    fn as_valtype(&self) -> enc::ValType {
        use ast::PrimitiveType;
        match self {
            PrimitiveType::U32
            | PrimitiveType::S32
            | PrimitiveType::U16
            | PrimitiveType::S16
            | PrimitiveType::U8
            | PrimitiveType::S8
            | PrimitiveType::Bool => enc::ValType::I32,

            PrimitiveType::U64 | PrimitiveType::S64 => enc::ValType::I64,

            PrimitiveType::F32 => enc::ValType::F32,
            PrimitiveType::F64 => enc::ValType::F64,
        }
    }

    fn as_primitive_valtype(&self) -> enc::PrimitiveValType {
        use ast::PrimitiveType;
        match self {
            PrimitiveType::U64 => enc::PrimitiveValType::U64,
            PrimitiveType::U32 => enc::PrimitiveValType::U32,
            PrimitiveType::U16 => enc::PrimitiveValType::U16,
            PrimitiveType::U8 => enc::PrimitiveValType::U8,
            PrimitiveType::S64 => enc::PrimitiveValType::S64,
            PrimitiveType::S32 => enc::PrimitiveValType::S32,
            PrimitiveType::S16 => enc::PrimitiveValType::S16,
            PrimitiveType::S8 => enc::PrimitiveValType::S8,
            PrimitiveType::F32 => enc::PrimitiveValType::Float32,
            PrimitiveType::F64 => enc::PrimitiveValType::Float64,
            PrimitiveType::Bool => enc::PrimitiveValType::Bool,
        }
    }

    fn as_comp_valtype(&self) -> enc::ComponentValType {
        enc::ComponentValType::Primitive(self.as_primitive_valtype())
    }
}

// ValType

#[allow(dead_code)]
impl ast::ValType {
    fn as_comp_valtype(&self) -> enc::ComponentValType {
        match self {
            ast::ValType::Result { .. } => todo!(),
            ast::ValType::String => todo!(),
            ast::ValType::Primitive(p) => p.as_comp_valtype(),
        }
    }
}

pub fn gen_allocator() -> Vec<u8> {
    let wat = include_str!("../../allocator.wat");
    wat::parse_str(wat).unwrap()
}
