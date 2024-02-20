use wasm_encoder as enc;

#[derive(Default)]
pub struct ModuleBuilder {
    types: enc::TypeSection,

    // The module function index and  for an import is that imports index
    imports: enc::ImportSection,
    // The module function index for a function is that functions index
    // plus the number of total imports
    funcs: enc::FunctionSection,

    globals: enc::GlobalSection,
    exports: enc::ExportSection,

    code: Vec<Option<enc::Function>>,

    num_types: u32,
    num_funcs: u32,
    num_memories: u32,
    num_globals: u32,
}

#[derive(Clone, Copy, Debug)]
pub struct ModuleTypeIndex(u32);


#[derive(Clone, Copy, Debug)]
pub struct ModuleFunctionIndex(u32);


#[allow(dead_code)]
#[derive(Clone, Copy, Debug)]
pub struct ModuleMemoryIndex(u32);


#[allow(dead_code)]
#[derive(Clone, Copy, Debug)]
pub struct ModuleGlobalIndex(u32);

impl From<ModuleFunctionIndex> for u32 {
    fn from(value: ModuleFunctionIndex) -> Self {
        value.0
    }
}

impl ModuleBuilder {
    pub fn func_type<P, R>(&mut self, params: P, results: R) -> ModuleTypeIndex
    where
        P: IntoIterator<Item = enc::ValType>,
        P::IntoIter: ExactSizeIterator,
        R: IntoIterator<Item = enc::ValType>,
        R::IntoIter: ExactSizeIterator,
    {
        self.types.function(params, results);
        self.next_type_idx()
    }

    pub fn import_func(
        &mut self,
        module: &str,
        field: &str,
        fn_type: ModuleTypeIndex,
    ) -> ModuleFunctionIndex {
        let fn_type = enc::EntityType::Function(fn_type.0);
        self.imports.import(module, field, fn_type);
        self.code.push(None);
        self.next_func_idx()
    }

    pub fn function(&mut self, fn_type: ModuleTypeIndex) -> ModuleFunctionIndex {
        self.funcs.function(fn_type.0);
        self.code.push(None);
        self.next_func_idx()
    }

    pub fn code(&mut self, func: ModuleFunctionIndex, code: enc::Function) {
        let index = func.0 as usize;
        match self.code[index] {
            Some(_) => panic!("Code for function {} already provided", index),
            None => {
                self.code[index] = Some(code);
            }
        }
    }

    pub fn export_func(&mut self, name: &str, func: ModuleFunctionIndex) {
        self.exports.export(name, enc::ExportKind::Func, func.0);
    }

    pub fn import_memory(&mut self, module: &str, field: &str) -> ModuleMemoryIndex {
        let mem_type = enc::MemoryType {
            minimum: 1,
            maximum: None,
            memory64: false,
            shared: false,
        };
        let mem_ty = enc::EntityType::Memory(mem_type);
        self.imports.import(module, field, mem_ty);

        self.next_memory_idx()
    }

    pub fn global(
        &mut self,
        mutable: bool,
        valtype: enc::ValType,
        init_expr: &enc::ConstExpr,
    ) -> ModuleGlobalIndex {
        let global_type = enc::GlobalType {
            mutable,
            val_type: valtype,
        };
        self.globals.global(global_type, init_expr);
        self.next_global_idx()
    }

    pub fn finalize(self) -> enc::Module {
        let mut module = enc::Module::new();
        module.section(&self.types);
        module.section(&self.imports);
        module.section(&self.funcs);
        module.section(&self.globals);
        module.section(&self.exports);

        // Encode code sections
        let mut code = enc::CodeSection::new();
        for func in self.code.into_iter() {
            match func {
                Some(func) => {
                    code.function(&func);
                }
                None => {}
            }
        }
        module.section(&code);

        module
    }

    fn next_type_idx(&mut self) -> ModuleTypeIndex {
        let index = ModuleTypeIndex(self.num_types);
        self.num_types += 1;
        index
    }

    fn next_func_idx(&mut self) -> ModuleFunctionIndex {
        let index = ModuleFunctionIndex(self.num_funcs);
        self.num_funcs += 1;
        index
    }

    fn next_memory_idx(&mut self) -> ModuleMemoryIndex {
        let index = ModuleMemoryIndex(self.num_memories);
        self.num_memories += 1;
        index
    }

    fn next_global_idx(&mut self) -> ModuleGlobalIndex {
        let index = ModuleGlobalIndex(self.num_globals);
        self.num_globals += 1;
        index
    }
}
