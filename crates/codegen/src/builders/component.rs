use wasm_encoder as enc;

#[derive(Default)]
pub struct ComponentBuilder {
    component: enc::Component,

    num_types: u32,
    num_funcs: u32,
    num_core_funcs: u32,
    num_core_mems: u32,
    num_modules: u32,
    num_module_instances: u32,
}

#[derive(Clone, Copy, Debug)]
pub struct ComponentModuleIndex(u32);

#[derive(Clone, Copy, Debug)]
pub struct ComponentModuleInstanceIndex(u32);

#[derive(Clone, Copy, Debug)]
pub struct ComponentTypeIndex(u32);

#[derive(Clone, Copy, Debug)]
pub struct ComponentFunctionIndex(u32);

#[derive(Clone, Copy, Debug)]
pub struct ComponentCoreFunctionIndex(u32);

#[derive(Clone, Copy, Debug)]
pub struct ComponentCoreMemoryIndex(u32);

pub enum InlineExportItem {
    Func(ComponentCoreFunctionIndex),
}

pub enum ModuleInstiateArgs {
    Instance(ComponentModuleInstanceIndex),
}

impl ComponentBuilder {
    pub fn module(&mut self, module: enc::Module) -> ComponentModuleIndex {
        self.component.section(&enc::ModuleSection(&module));
        self.next_mod_idx()
    }

    pub fn module_bytes(&mut self, bytes: Vec<u8>) -> ComponentModuleIndex {
        self.component.section(&enc::RawSection {
            id: enc::ComponentSectionId::CoreModule.into(),
            data: bytes.as_slice(),
        });
        self.next_mod_idx()
    }

    #[allow(dead_code)]
    pub fn inline_export(
        &mut self,
        exports: Vec<(String, InlineExportItem)>,
    ) -> ComponentModuleInstanceIndex {
        let exports: Vec<(String, enc::ExportKind, u32)> = exports
            .into_iter()
            .map(|(name, arg)| match arg {
                InlineExportItem::Func(func) => (name, enc::ExportKind::Func, func.0),
            })
            .collect();
        let mut section = enc::InstanceSection::new();
        section.export_items(exports);
        self.component.section(&section);
        self.next_mod_instance_idx()
    }

    pub fn instantiate(
        &mut self,
        module: ComponentModuleIndex,
        args: Vec<(String, ModuleInstiateArgs)>,
    ) -> ComponentModuleInstanceIndex {
        let args: Vec<_> = args
            .into_iter()
            .map(|(name, arg)| match arg {
                ModuleInstiateArgs::Instance(instance) => {
                    (name, enc::ModuleArg::Instance(instance.0))
                }
            })
            .collect();
        let mut section = enc::InstanceSection::new();
        section.instantiate(module.0, args);
        self.component.section(&section);
        self.next_mod_instance_idx()
    }

    pub fn func_type<'b, P>(
        &mut self,
        params: P,
        result: Option<enc::ComponentValType>,
    ) -> ComponentTypeIndex
    where
        P: IntoIterator<Item = (&'b str, enc::ComponentValType)>,
        P::IntoIter: ExactSizeIterator,
    {
        let mut section = enc::ComponentTypeSection::new();
        let mut builder = section.function();
        builder.params(params);
        match result {
            Some(return_type) => {
                builder.result(return_type);
            }
            None => {
                builder.results([] as [(&str, enc::ComponentValType); 0]);
            }
        }
        self.component.section(&section);
        self.next_type_idx()
    }

    pub fn import_func(
        &mut self,
        name: &str,
        fn_type: ComponentTypeIndex,
    ) -> ComponentFunctionIndex {
        let mut section = enc::ComponentImportSection::new();
        let ty = enc::ComponentTypeRef::Func(fn_type.0);
        section.import(name, ty);
        self.component.section(&section);
        self.next_func_idx()
    }

    pub fn lower_func(&mut self, func: ComponentFunctionIndex) -> ComponentCoreFunctionIndex {
        let mut section = enc::CanonicalFunctionSection::new();
        section.lower(func.0, []);
        self.component.section(&section);
        self.next_core_func_idx()
    }

    pub fn alias_memory(
        &mut self,
        instance: ComponentModuleInstanceIndex,
        name: &str,
    ) -> ComponentCoreMemoryIndex {
        let mut section = enc::ComponentAliasSection::new();
        section.alias(enc::Alias::CoreInstanceExport {
            instance: instance.0,
            kind: enc::ExportKind::Memory,
            name,
        });
        self.component.section(&section);
        self.next_core_memory_idx()
    }

    pub fn alias_func(
        &mut self,
        instance: ComponentModuleInstanceIndex,
        name: &str,
    ) -> ComponentCoreFunctionIndex {
        let mut section = enc::ComponentAliasSection::new();
        section.alias(enc::Alias::CoreInstanceExport {
            instance: instance.0,
            kind: enc::ExportKind::Func,
            name,
        });
        self.component.section(&section);
        self.next_core_func_idx()
    }

    pub fn lift_func(
        &mut self,
        func: ComponentCoreFunctionIndex,
        fn_type: ComponentTypeIndex,
        memory: ComponentCoreMemoryIndex,
        realloc: ComponentCoreFunctionIndex,
        post_return: ComponentCoreFunctionIndex,
    ) -> ComponentFunctionIndex {
        let mut section = enc::CanonicalFunctionSection::new();
        let canon_opts: [enc::CanonicalOption; 3] = [
            enc::CanonicalOption::Memory(memory.0),
            enc::CanonicalOption::Realloc(realloc.0),
            enc::CanonicalOption::PostReturn(post_return.0)
        ];
        section.lift(func.0, fn_type.0, canon_opts);
        self.component.section(&section);
        self.next_func_idx()
    }

    pub fn export_func(
        &mut self,
        name: &str,
        func: ComponentFunctionIndex,
        fn_type: ComponentTypeIndex,
    ) -> ComponentFunctionIndex {
        let mut section = enc::ComponentExportSection::new();
        section.export(
            name,
            enc::ComponentExportKind::Func,
            func.0,
            Some(enc::ComponentTypeRef::Func(fn_type.0)),
        );
        self.component.section(&section);
        self.next_func_idx()
    }

    pub fn finalize(self) -> enc::Component {
        self.component
    }

    fn next_mod_idx(&mut self) -> ComponentModuleIndex {
        let index = ComponentModuleIndex(self.num_modules);
        self.num_modules += 1;
        index
    }

    fn next_mod_instance_idx(&mut self) -> ComponentModuleInstanceIndex {
        let index = ComponentModuleInstanceIndex(self.num_module_instances);
        self.num_module_instances += 1;
        index
    }

    fn next_type_idx(&mut self) -> ComponentTypeIndex {
        let index = ComponentTypeIndex(self.num_types);
        self.num_types += 1;
        index
    }

    fn next_func_idx(&mut self) -> ComponentFunctionIndex {
        let index = ComponentFunctionIndex(self.num_funcs);
        self.num_funcs += 1;
        index
    }

    fn next_core_func_idx(&mut self) -> ComponentCoreFunctionIndex {
        let index = ComponentCoreFunctionIndex(self.num_core_funcs);
        self.num_core_funcs += 1;
        index
    }

    fn next_core_memory_idx(&mut self) -> ComponentCoreMemoryIndex {
        let index = ComponentCoreMemoryIndex(self.num_core_mems);
        self.num_core_mems += 1;
        index
    }
}
