use std::collections::HashMap;

use crate::builders::module::{ModuleBuilder, ModuleTypeIndex};
use crate::{builders::component::*, types::EncodeType, GenerationError};
use claw_resolver::{ImportFuncId, ImportFunction, ImportItemId, ImportType, ImportTypeId, ResolvedInterface};

use claw_resolver::{ResolvedComponent, ResolvedType};
use wasm_encoder as enc;

pub struct ImportEncoder<'gen> {
    builder: &'gen mut ComponentBuilder,
    resolved_comp: &'gen ResolvedComponent,
    memory: ComponentCoreMemoryIndex,
    realloc: ComponentCoreFunctionIndex,

    funcs: HashMap<ImportFuncId, EncodedImportFunc>,

    inline_export_args: Vec<(String, InlineExportItem)>,
}

pub struct EncodedImports {
    pub imports_instance: ComponentModuleInstanceIndex,
    pub funcs: HashMap<ImportFuncId, EncodedImportFunc>
}

pub struct EncodedImportFunc {
    pub spill_params: bool,
    pub spill_results: bool,
    pub core_params: Vec<enc::ValType>,
    pub core_results: Vec<enc::ValType>,
}

impl EncodedImportFunc {
    pub fn encode_mod_type(&self, builder: &mut ModuleBuilder) -> ModuleTypeIndex {
        let params = self.core_params.iter().copied();
        let results = self.core_results.iter().copied();
        builder.func_type(params, results)
    }
}


impl<'gen> ImportEncoder<'gen> {
    pub fn new(
        builder: &'gen mut ComponentBuilder,
        resolved_comp: &'gen ResolvedComponent,
        memory: ComponentCoreMemoryIndex,
        realloc: ComponentCoreFunctionIndex,
    ) -> Self {
        let funcs = HashMap::new();
        let inline_export_args = Vec::new();

        Self {
            builder,
            resolved_comp,
            memory,
            realloc,
            funcs,
            inline_export_args,
        }
    }

    pub fn encode(mut self) -> Result<EncodedImports, GenerationError> {
        for interface in self.resolved_comp.imports.interfaces.iter() {
            self.encode_interface(interface)?;
        }

        self.encode_loose_funcs();

        let imports_instance = self.builder.inline_export(&self.inline_export_args);
        Ok(EncodedImports {
            imports_instance,
            funcs: self.funcs,
        })
    }

    fn encode_interface<'int>(
        &mut self,
        interface: &'int ResolvedInterface,
    ) -> Result<(), GenerationError> {
        ImportInterfaceEncoder::new(self, interface).encode()
    }

    fn encode_loose_funcs(&mut self) {
        // Import the loose functions
        for id in self.resolved_comp.imports.loose_funcs.iter().copied() {
            let import_func = &self.resolved_comp.imports.funcs[id];
            let import_name = import_func.name.as_str();

            let type_idx = self.encode_func_type(import_func);
            let func_idx = self.builder.import_func(import_name, type_idx);
            let core_func_idx = self.builder.lower_func(func_idx, self.memory, self.realloc);

            self.inline_export_args.push((
                import_name.to_owned(),
                InlineExportItem::Func(core_func_idx),
            ));
        }
    }

    fn encode_func_type(&mut self, import_func: &ImportFunction) -> ComponentTypeIndex {
        let param_vec: Vec<_> = import_func
            .params
            .iter()
            .map(|(param_name, param_type)| {
                let param_type = self.rtype_to_comp_valtype(*param_type);
                (param_name.to_owned(), param_type)
            })
            .collect();
        let params = param_vec
            .iter()
            .map(|(param_name, param_type)| (param_name.as_str(), *param_type));
        let results = import_func
            .results
            .map(|result_type| self.rtype_to_comp_valtype(result_type));

        self.builder.func_type(params, results)
    }

    fn rtype_to_comp_valtype(&self, rtype: ResolvedType) -> enc::ComponentValType {
        let comp = &self.resolved_comp;
        match rtype {
            ResolvedType::Primitive(ptype) => ptype.to_comp_valtype(comp),
            ResolvedType::Import(_) => todo!(),
            ResolvedType::Defined(type_id) => type_id.to_comp_valtype(comp),
        }
    }
}

struct ImportInterfaceEncoder<'a, 'b, 'c> {
    parent: &'a mut ImportEncoder<'b>,
    interface: &'c ResolvedInterface,

    instance_type: enc::InstanceType,
    exported_ids: HashMap<ImportTypeId, u32>,
    instance_type_items: u32,
}

impl<'a, 'b, 'c> ImportInterfaceEncoder<'a, 'b, 'c> {
    pub fn new(parent: &'a mut ImportEncoder<'b>, interface: &'c ResolvedInterface) -> Self {
        let instance_type = enc::InstanceType::new();
        let exported_ids = Default::default();
        let instance_type_items = 0;

        Self {
            parent,
            interface,
            instance_type,
            exported_ids,
            instance_type_items,
        }
    }

    fn encode(mut self) -> Result<(), GenerationError> {
        for item in &self.interface.items {
            match item {
                ImportItemId::Type(rtype) => {
                    self.encode_rtype(*rtype);
                }
                ImportItemId::Func(id) => {
                    self.encode_func(*id);
                }
            }
        }

        let instance_type_index = self.parent.builder.instance_type(&self.instance_type);
        let interface_name = self
            .parent
            .resolved_comp
            .wit
            .get_interface(self.interface.interface_id)
            .name
            .as_ref()
            .unwrap()
            .as_str();
        let interface_instance = self
            .parent
            .builder
            .import_instance(interface_name, instance_type_index);

        self.encode_aliases(interface_instance)?;
        Ok(())
    }

    fn encode_rtype(&mut self, rtype: ResolvedType) {
        match rtype {
            ResolvedType::Import(id) => {
                let import_type = &self.parent.resolved_comp.imports.types[id];
                match import_type {
                    ImportType::Enum(enum_type) => {
                        // Define type
                        self.instance_type
                            .ty()
                            .defined_type()
                            .enum_type(enum_type.cases.iter().map(|s| s.as_str()));
                        let enum_type_id = self.instance_type_items;
                        self.instance_type_items += 1;
                        // Export it from interface
                        let ty = enc::TypeBounds::Eq(enum_type_id);
                        let ty = enc::ComponentTypeRef::Type(ty);
                        self.instance_type.export(enum_type.name.as_str(), ty);
                        let enum_export_id = self.instance_type_items;
                        self.instance_type_items += 1;
                        // Remember the type index
                        self.exported_ids.insert(id, enum_export_id);
                    }
                }
            }
            _ => {
                // No op
            }
        }
    }

    fn encode_func(&mut self, id: ImportFuncId) {
        let import_func = &self.parent.resolved_comp.imports.funcs[id];
        let func_type_id = self.encode_func_type(import_func);
        let ty = enc::ComponentTypeRef::Func(func_type_id);
        self.instance_type.export(&import_func.name, ty);
    }

    fn encode_func_type(&mut self, import_func: &ImportFunction) -> u32 {
        let param_vec: Vec<(String, enc::ComponentValType)> = import_func
            .params
            .iter()
            .map(|(param_name, param_type)| {
                let param_type = self.rtype_to_comp_valtype(*param_type);
                (param_name.to_owned(), param_type)
            })
            .collect();
        let params = param_vec
            .iter()
            .map(|(param_name, param_type)| (param_name.as_str(), *param_type));
        let results = import_func
            .results
            .map(|result_type| self.rtype_to_comp_valtype(result_type));

        let mut builder = self.instance_type.ty().function();
        builder.params(params);
        match results {
            Some(ty) => {
                builder.result(ty);
            }
            None => {
                builder.results([] as [(&str, enc::ComponentValType); 0]);
            }
        }
        let func_type_id = self.instance_type_items;
        self.instance_type_items += 1;
        func_type_id
    }

    fn encode_aliases(
        &mut self,
        interface_instance: ComponentInstanceIndex,
    ) -> Result<(), GenerationError> {
        for item in &self.interface.items {
            match item {
                ImportItemId::Type(_) => {}
                ImportItemId::Func(id) => {
                    let import_func = &self.parent.resolved_comp.imports.funcs[*id];
                    let func_idx = self
                        .parent
                        .builder
                        .alias_func(interface_instance, import_func.name.as_str());
                    let core_func_idx = self.parent.builder.lower_func(func_idx, self.parent.memory, self.parent.realloc);
                    self.parent.inline_export_args.push((
                        import_func.name.to_owned(),
                        InlineExportItem::Func(core_func_idx),
                    ));
                }
            }
        }
        Ok(())
    }

    fn rtype_to_comp_valtype(&self, rtype: ResolvedType) -> enc::ComponentValType {
        let comp = &self.parent.resolved_comp;
        match rtype {
            ResolvedType::Primitive(ptype) => ptype.to_comp_valtype(comp),
            ResolvedType::Import(itype) => {
                let index = *self.exported_ids.get(&itype).unwrap();
                enc::ComponentValType::Type(index)
            }
            ResolvedType::Defined(type_id) => type_id.to_comp_valtype(comp),
        }
    }
}
