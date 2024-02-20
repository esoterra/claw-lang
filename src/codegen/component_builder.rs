use wasm_encoder as enc;

#[derive(Default)]
pub struct ComponentBuilder {
    alias: enc::ComponentAliasSection,
    types: enc::ComponentTypeSection,
    imports: enc::ComponentImportSection,
    lower_funcs: enc::CanonicalFunctionSection,
    lift_funcs: enc::CanonicalFunctionSection,
    exports: enc::ComponentExportSection,

    modules: Vec<ModuleSlot>,
    module_instances: Vec<ModuleInstanceSlots>,

    num_types: u32,
    num_funcs: u32,
    num_core_funcs: u32,
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

enum ModuleSlot {
    Empty,
    Filled(enc::Module),
    Bytes(Vec<u8>),
}

enum ModuleInstanceSlots {
    EmptyInlineExport,
    FilledInlineExport {
        exports: Vec<(String, enc::ExportKind, u32)>,
    },
    ModuleInstance {
        module_index: ComponentModuleIndex,
        args: Vec<(String, enc::ModuleArg)>,
    },
}

pub enum InlineExportItem {
    Func(ComponentCoreFunctionIndex),
}

pub enum ModuleInstiateArgs {
    Instance(ComponentModuleInstanceIndex),
}

impl ComponentBuilder {
    #[allow(dead_code)]
    pub fn module(&mut self, module: enc::Module) -> ComponentModuleIndex {
        let index = self.modules.len() as u32;
        self.modules.push(ModuleSlot::Filled(module));
        ComponentModuleIndex(index)
    }

    pub fn module_bytes(&mut self, bytes: Vec<u8>) -> ComponentModuleIndex {
        let index = self.modules.len() as u32;
        self.modules.push(ModuleSlot::Bytes(bytes));
        ComponentModuleIndex(index)
    }

    pub fn reserve_module(&mut self) -> ComponentModuleIndex {
        let index = self.modules.len() as u32;
        self.modules.push(ModuleSlot::Empty);
        ComponentModuleIndex(index)
    }

    pub fn fill_module(&mut self, index: ComponentModuleIndex, module: enc::Module) {
        let index = index.0 as usize;
        assert!(matches!(self.modules[index], ModuleSlot::Empty));
        self.modules[index] = ModuleSlot::Filled(module);
    }

    #[allow(dead_code)]
    pub fn inline_export(
        &mut self,
        exports: Vec<(String, enc::ExportKind, u32)>,
    ) -> ComponentModuleInstanceIndex {
        self.module_instances
            .push(ModuleInstanceSlots::FilledInlineExport { exports });
        self.next_mod_instance_idx()
    }

    pub fn reserve_inline_export(&mut self) -> ComponentModuleInstanceIndex {
        self.module_instances
            .push(ModuleInstanceSlots::EmptyInlineExport);
        self.next_mod_instance_idx()
    }

    pub fn fill_inline_export_args(
        &mut self,
        instance: ComponentModuleInstanceIndex,
        exports: Vec<(String, InlineExportItem)>,
    ) {
        let index = instance.0 as usize;
        match &self.module_instances[index] {
            ModuleInstanceSlots::EmptyInlineExport => {
                let exports = exports
                    .into_iter()
                    .map(|(name, arg)| match arg {
                        InlineExportItem::Func(func) => (name, enc::ExportKind::Func, func.0),
                    })
                    .collect();
                self.module_instances[index] = ModuleInstanceSlots::FilledInlineExport { exports };
            }
            ModuleInstanceSlots::FilledInlineExport { .. } => {
                panic!("Slot for instance {} already filled", index)
            }
            ModuleInstanceSlots::ModuleInstance { .. } => {
                panic!("Slot for instance {} already filled", index)
            }
        }
    }

    pub fn instantiate(
        &mut self,
        module: ComponentModuleIndex,
        args: Vec<(String, ModuleInstiateArgs)>,
    ) -> ComponentModuleInstanceIndex {
        let args = args
            .into_iter()
            .map(|(name, arg)| match arg {
                ModuleInstiateArgs::Instance(instance) => {
                    (name, enc::ModuleArg::Instance(instance.0))
                }
            })
            .collect();
        self.module_instances
            .push(ModuleInstanceSlots::ModuleInstance {
                module_index: module,
                args,
            });
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
        let mut builder = self.types.function();
        builder.params(params);
        match result {
            Some(return_type) => {
                builder.result(return_type);
            }
            None => {
                builder.results([] as [(&str, enc::ComponentValType); 0]);
            }
        }
        self.next_type_idx()
    }

    pub fn import_func(
        &mut self,
        name: &str,
        fn_type: ComponentTypeIndex,
    ) -> ComponentFunctionIndex {
        let ty = enc::ComponentTypeRef::Func(fn_type.0);
        self.imports.import(name, ty);
        self.next_func_idx()
    }

    pub fn lower_func(&mut self, func: ComponentFunctionIndex) -> ComponentCoreFunctionIndex {
        self.lower_funcs.lower(func.0, []);
        self.next_core_func_idx()
    }

    pub fn alias_func(
        &mut self,
        instance: ComponentModuleInstanceIndex,
        name: &str,
    ) -> ComponentCoreFunctionIndex {
        self.alias.alias(enc::Alias::CoreInstanceExport {
            instance: instance.0,
            kind: enc::ExportKind::Func,
            name,
        });
        self.next_core_func_idx()
    }

    pub fn lift_func(
        &mut self,
        func: ComponentCoreFunctionIndex,
        fn_type: ComponentTypeIndex,
    ) -> ComponentFunctionIndex {
        const NO_CANON_OPTS: [enc::CanonicalOption; 0] = [];
        self.lift_funcs.lift(func.0, fn_type.0, NO_CANON_OPTS);
        self.next_func_idx()
    }

    pub fn export_func(
        &mut self,
        name: &str,
        func: ComponentFunctionIndex,
        fn_type: ComponentTypeIndex,
    ) {
        self.exports.export(
            name,
            enc::ComponentExportKind::Func,
            func.0,
            Some(enc::ComponentTypeRef::Func(fn_type.0)),
        );
    }

    pub fn finalize(self) -> enc::Component {
        let mut component = enc::Component::new();

        // Component Types
        component.section(&self.types);
        // Component Imports
        component.section(&self.imports);
        // Lower Imported Functions
        component.section(&self.lower_funcs);

        for module in self.modules {
            match module {
                ModuleSlot::Empty => panic!("Module slot not filled before component finalized"),
                ModuleSlot::Filled(module) => {
                    component.section(&enc::ModuleSection(&module));
                }
                ModuleSlot::Bytes(bytes) => {
                    component.section(&enc::RawSection {
                        id: enc::ComponentSectionId::CoreModule.into(),
                        data: bytes.as_slice(),
                    });
                }
            };
        }

        let mut instantiations = enc::InstanceSection::new();
        for instance in self.module_instances {
            match instance {
                ModuleInstanceSlots::EmptyInlineExport => {
                    panic!("Inline export instantiation slot not filled before component finalized")
                }
                ModuleInstanceSlots::FilledInlineExport { exports } => {
                    instantiations.export_items(exports);
                }
                ModuleInstanceSlots::ModuleInstance { module_index, args } => {
                    instantiations.instantiate(module_index.0, args);
                }
            }
        }
        component.section(&instantiations);

        // Alias module exports
        component.section(&self.alias);
        // Lift component functions
        component.section(&self.lift_funcs);
        // Export component functions
        component.section(&self.exports);

        component
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
}
