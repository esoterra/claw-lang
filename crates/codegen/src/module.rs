use std::collections::HashMap;

use ast::{FunctionId, NameId};
use claw_ast as ast;
use claw_resolver::{ImportFuncId, ImportFunction, ResolvedComponent};
use wasm_encoder as enc;

use crate::{
    builders::module::*,
    code::CodeGenerator,
    function::{EncodedFuncs, EncodedFunction},
    imports::{EncodedImportFunc, EncodedImports},
    types::EncodeType,
    GenerationError,
};

pub(crate) fn generate(
    resolved_comp: &ResolvedComponent,
    imports: &EncodedImports,
    functions: &EncodedFuncs,
) -> Result<enc::Module, GenerationError> {
    ModuleGenerator::new(resolved_comp, imports, functions).generate()
}

pub struct ModuleGenerator<'gen> {
    pub resolved_comp: &'gen ResolvedComponent,
    pub comp: &'gen ast::Component,
    imports: &'gen EncodedImports,
    functions: &'gen EncodedFuncs,
    pub module: ModuleBuilder,

    func_idx_for_import: HashMap<ImportFuncId, ModuleFunctionIndex>,
    func_idx_for_func: HashMap<FunctionId, ModuleFunctionIndex>,
}

impl<'gen> ModuleGenerator<'gen> {
    fn new(
        resolved_comp: &'gen ResolvedComponent,
        imports: &'gen EncodedImports,
        functions: &'gen EncodedFuncs,
    ) -> Self {
        Self {
            resolved_comp,
            comp: &resolved_comp.component,
            imports,
            functions,
            module: Default::default(),
            func_idx_for_import: Default::default(),
            func_idx_for_func: Default::default(),
        }
    }

    pub fn generate(mut self) -> Result<enc::Module, GenerationError> {
        // There is only ever one memory, memory zero
        let (_memory, realloc, clear) = self.encode_import_allocator();

        for (id, import_func) in self.resolved_comp.imports.funcs.iter() {
            let encoded_import_func = self.imports.funcs.get(&id).unwrap();
            let func_idx = self.encode_import_func(import_func, encoded_import_func);
            self.func_idx_for_import.insert(id, func_idx);
        }

        self.encode_globals()?;

        // Encode functions
        for (id, function) in self.resolved_comp.component.functions.iter() {
            let encoded_func = self.functions.funcs.get(&id).unwrap();
            let func_idx = self.encode_func(function, encoded_func)?;
            self.func_idx_for_func.insert(id, func_idx);
        }
        // Encode function code
        for (id, encoded_func) in self.functions.funcs.iter() {
            let id = *id;
            let code_gen = CodeGenerator::new(
                &mut self.module,
                self.resolved_comp,
                &self.imports,
                &self.functions,
                &self.func_idx_for_import,
                &self.func_idx_for_func,
                encoded_func,
                id,
                realloc,
            )?;
            let builder = code_gen.finalize()?;
            let mod_func_idx = self.func_idx_for_func[&id];
            self.module.code(mod_func_idx, builder);
        }

        // Encode post returns
        for (id, function) in self.resolved_comp.component.functions.iter() {
            // Encode function
            let ident = function.ident;
            let encoded_func = self.functions.funcs.get(&id).unwrap();
            let post_return = self.encode_post_return_func(ident, &encoded_func)?;
            // Encode code
            let mut builder = enc::Function::new(vec![]);
            builder.instruction(&enc::Instruction::Call(clear.into()));
            builder.instruction(&enc::Instruction::End);
            self.module.code(post_return, builder);
        }

        Ok(self.module.finalize())
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
        import_func: &ImportFunction,
        encoded_import_func: &EncodedImportFunc,
    ) -> ModuleFunctionIndex {
        let type_idx = encoded_import_func.encode_mod_type(&mut self.module);
        let import_name = import_func.name.as_str();
        self.module.import_func("claw", import_name, type_idx)
    }

    fn encode_globals(&mut self) -> Result<(), GenerationError> {
        for (id, global) in self.comp.globals.iter() {
            let valtypes = global.type_id.flatten(self.resolved_comp);
            assert_eq!(valtypes.len(), 1, "Cannot use non-primitive globals");
            let valtype = valtypes[0];

            let init_expr = if let Some(init_value) = self.resolved_comp.global_vals.get(&id) {
                let valtype = self.comp.get_type(global.type_id);
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
        encoded_func: &EncodedFunction,
    ) -> Result<ModuleFunctionIndex, GenerationError> {
        let return_type = &encoded_func.results;
        let type_idx = match return_type {
            Some(info) => self.module.func_type([info.spill.valtype()], []),
            None => self.module.func_type([], []),
        };
        let func_idx = self.module.function(type_idx);

        let name = self.comp.get_name(ident);
        let name = format!("{}_post_return", name);
        self.module.export_func(name.as_str(), func_idx);

        Ok(func_idx)
    }

    fn encode_func(
        &mut self,
        function: &ast::Function,
        encoded_func: &EncodedFunction,
    ) -> Result<ModuleFunctionIndex, GenerationError> {
        let comp = &self.resolved_comp.component;
        let type_idx = encoded_func.encode_mod_type(&mut self.module);
        let func_idx = self.module.function(type_idx);

        if function.exported {
            let ident = function.ident;
            let name = comp.get_name(ident);
            // Export function from module
            self.module.export_func(name, func_idx);
        }

        Ok(func_idx)
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
