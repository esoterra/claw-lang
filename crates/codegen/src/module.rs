use std::collections::HashMap;

use ast::{FunctionId, NameId};
use claw_ast as ast;
use claw_resolver::{ImportFuncId, ImportFunction, ResolvedComponent, ResolvedType};
use wasm_encoder as enc;

use crate::{
    builders::module::*, code::CodeGenerator, function::FunctionGenerator, types::EncodeType,
    GenerationError,
};

pub(crate) fn generate(resolved_comp: &ResolvedComponent) -> Result<enc::Module, GenerationError> {
    let generator = ModuleGenerator::default();
    generator.generate(resolved_comp)
}

#[derive(Default)]
pub struct ModuleGenerator {
    pub module: ModuleBuilder,

    func_idx_for_import: HashMap<ImportFuncId, ModuleFunctionIndex>,
    func_idx_for_func: HashMap<FunctionId, ModuleFunctionIndex>,
}

impl ModuleGenerator {
    pub fn generate(
        mut self,
        resolved_comp: &ResolvedComponent,
    ) -> Result<enc::Module, GenerationError> {
        let comp = &resolved_comp.component;
        // There is only ever one memory, memory zero
        let (_memory, realloc, clear) = self.encode_import_allocator();

        for (id, import_func) in resolved_comp.imports.funcs.iter() {
            self.encode_import_func(id, import_func, comp);
        }

        self.encode_globals(resolved_comp)?;

        let mut functions = Vec::new();
        for (id, func) in resolved_comp.component.functions.iter() {
            // Encode function
            let func_gen = self.encode_func(id, func, comp)?;
            let ident = func.ident;
            // Encode post-return
            let post_return = self.encode_post_return_func(ident, &func_gen, comp)?;
            functions.push((id, post_return, func_gen));
        }

        for (id, post_return, func_gen) in functions {
            // Encode function code
            let code_gen = CodeGenerator::new(&mut self, resolved_comp, func_gen, id, realloc)?;
            let builder = code_gen.finalize()?;
            let mod_func_idx = self.func_idx_for_func[&id];
            self.module.code(mod_func_idx, builder);

            // Encode post-return code
            let mut builder = enc::Function::new(vec![]);
            builder.instruction(&enc::Instruction::Call(clear.into()));
            builder.instruction(&enc::Instruction::End);
            self.module.code(post_return, builder);
        }

        Ok(self.module.finalize())
    }

    pub fn func_func_idx(&self, id: FunctionId) -> ModuleFunctionIndex {
        *self.func_idx_for_func.get(&id).unwrap()
    }

    pub fn import_func_idx(&self, id: ImportFuncId) -> ModuleFunctionIndex {
        *self.func_idx_for_import.get(&id).unwrap()
    }

    fn encode_import_allocator(
        &mut self,
    ) -> (ModuleMemoryIndex, ModuleFunctionIndex, ModuleFunctionIndex) {
        let memory: ModuleMemoryIndex = self.module.import_memory("alloc", "memory");

        let realloc_type = self
            .module
            .func_type(vec![enc::ValType::I32; 4], vec![enc::ValType::I32; 1]);
        let realloc = self.module.import_func("alloc", "realloc", realloc_type);

        let clear_type = self.module.func_type(vec![], vec![]);
        let clear = self.module.import_func("alloc", "clear", clear_type);

        (memory, realloc, clear)
    }

    fn encode_import_func(
        &mut self,
        id: ImportFuncId,
        import_func: &ImportFunction,
        comp: &ast::Component,
    ) {

        let params = import_func
            .params
            .iter()
            .map(|(name, rtype)| {
                (name.to_owned(), *rtype)
            })
            .collect();
        let results = import_func.results;
        let func_gen = FunctionGenerator::new(params, results, comp);
        let type_idx = func_gen.encode_mod_type(&mut self.module);
        let import_name = import_func.name.as_str();
        let func_idx = self.module.import_func("claw", import_name, type_idx);
        self.func_idx_for_import.insert(id, func_idx);
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
                    ast::ValType::Result(_) => todo!(),
                    ast::ValType::Primitive(ptype) => literal_to_const_expr(init_value, *ptype),
                }
            } else {
                panic!("Cannot generate WASM for unresolved global")
            };

            self.module.global(global.mutable, valtype, &init_expr);
        }
        Ok(())
    }

    fn encode_post_return_func(
        &mut self,
        ident: NameId,
        func_gen: &FunctionGenerator,
        comp: &ast::Component,
    ) -> Result<ModuleFunctionIndex, GenerationError> {
        let return_type = &func_gen.results;
        let type_idx = match return_type {
            Some(info) => self.module.func_type([info.spill.valtype()], []),
            None => self.module.func_type([], []),
        };
        let func_idx = self.module.function(type_idx);

        let name = comp.get_name(ident);
        let name = format!("{}_post_return", name);
        self.module.export_func(name.as_str(), func_idx);

        Ok(func_idx)
    }

    fn encode_func(
        &mut self,
        id: FunctionId,
        function: &ast::Function,
        comp: &ast::Component,
    ) -> Result<FunctionGenerator, GenerationError> {
        let params = function
            .params
            .iter()
            .map(|(name, type_id)| {
                let name = comp.get_name(*name).to_owned();
                let rtype = ResolvedType::Defined(*type_id);
                (name, rtype)
            })
            .collect();
        let results = function.results.map(|type_id| ResolvedType::Defined(type_id));
        let func_gen = FunctionGenerator::new(params, results, comp);
        let type_idx = func_gen.encode_mod_type(&mut self.module);
        let func_idx = self.module.function(type_idx);

        self.func_idx_for_func.insert(id, func_idx);

        if function.exported {
            let ident = function.ident;
            let name = comp.get_name(ident);
            // Export function from module
            self.module.export_func(name, func_idx);
        }

        Ok(func_gen)
    }
}

// Literal

fn literal_to_const_expr(literal: &ast::Literal, ptype: ast::PrimitiveType) -> enc::ConstExpr {
    use ast::{Literal, PrimitiveType};
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
