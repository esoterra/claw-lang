use std::collections::HashMap;

use crate::{GenerationError, types::EncodeType, builders::component::*};
use claw_resolver::{ImportItemId, ImportTypeId, ImportType, ResolvedInterface};

use claw_ast as ast;
use claw_resolver::{ResolvedComponent, ResolvedType};
use wasm_encoder as enc;

pub struct ImportEncoder<'gen> {
    builder: &'gen mut ComponentBuilder,
    resolved_comp: &'gen ResolvedComponent,

    inline_export_args: Vec<(String, InlineExportItem)>
}

impl<'gen> ImportEncoder<'gen> {
    pub fn new(
        builder: &'gen mut ComponentBuilder,
        resolved_comp: &'gen ResolvedComponent,
    ) -> Self {
        let inline_export_args = Vec::new();

        Self {
            builder,
            resolved_comp,
            inline_export_args,
        }
    }

    pub fn encode(mut self) -> Result<ComponentModuleInstanceIndex, GenerationError> {
        for interface in self.resolved_comp.imports.interfaces.iter() {
            self.encode_interface(interface)?;
        }

        self.encode_loose_funcs();

        let imports_instance = self.builder.inline_export(&self.inline_export_args);
        Ok(imports_instance)
    }

    fn encode_interface<'int>(&mut self, interface: &'int ResolvedInterface) -> Result<(), GenerationError> {
        ImportInterfaceEncoder::new(self, interface).encode()
    }

    fn encode_loose_funcs(&mut self) {
        // Import the loose functions
        for id in self.resolved_comp.imports.loose_funcs.iter().copied() {
            let import_func = &self.resolved_comp.imports.funcs[id];
            let import_name = import_func.name.as_str();

            let param_vec: Vec<_> = import_func.params.iter().map(|(param_name, param_type)| {
                let param_type = self.rtype_to_comp_valtype(*param_type);
                (param_name.to_owned(), param_type)
            }).collect();
            let params = param_vec.iter().map(|(param_name, param_type)| (param_name.as_str(), *param_type));
            let results = import_func.results.map(|result_type| self.rtype_to_comp_valtype(result_type));

            let type_idx = self.builder.func_type(params, results);
            let func_idx = self.builder.import_func(import_name, type_idx);
            let core_func_idx = self.builder.lower_func(func_idx);

            self.inline_export_args.push((
                import_name.to_owned(),
                InlineExportItem::Func(core_func_idx),
            ));
        }
    }

    fn rtype_to_comp_valtype(&self, rtype: ResolvedType) -> enc::ComponentValType {
        let comp = &self.resolved_comp.component;
        match rtype {
            ResolvedType::Primitive(ptype) => {
                ptype.to_comp_valtype(comp)
            }
            ResolvedType::Import(_) => todo!(),
            ResolvedType::Defined(type_id) => {
                type_id.to_comp_valtype(comp)
            },
        }
    }
}

struct ImportInterfaceEncoder<'a, 'b, 'c> {
    parent: &'a mut ImportEncoder<'b>,
    interface: &'c ResolvedInterface,

    instance_type: enc::InstanceType,
    exported_ids: HashMap<ImportTypeId, u32>,
    instance_type_items: u32
}

impl<'a, 'b, 'c> ImportInterfaceEncoder<'a, 'b, 'c> {
    pub fn new(parent: &'a mut ImportEncoder<'b>, interface: &'c ResolvedInterface) -> Self {
        let instance_type = parent.builder.start_instance_type();
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
                ImportItemId::Type(id) => {
                    let import_type = &self.parent.resolved_comp.imports.types[*id];
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
                            let ty = enc::ComponentTypeRef::Value(enc::ComponentValType::Type(
                                enum_type_id,
                            ));
                            self.instance_type.export(enum_type.name.as_str(), ty);
                            let enum_exported_type_id = self.instance_type_items;
                            self.instance_type_items += 1; // exports introduce an index
                            // Remember the type index
                            self.exported_ids.insert(*id, enum_exported_type_id);
                        }
                    }
                }
                ImportItemId::Func(id) => {
                    let import_func = &self.parent.resolved_comp.imports.funcs[*id];
                    let param_vec: Vec<(String, enc::ComponentValType)> = import_func.params.iter().map(|(param_name, param_type)| {
                        let param_type = self.rtype_to_comp_valtype(*param_type);
                        (param_name.to_owned(), param_type)
                    }).collect();
                    let params = param_vec.iter().map(|(param_name, param_type)| (param_name.as_str(), *param_type));
                    let results = import_func.results.map(|result_type| self.rtype_to_comp_valtype(result_type));
                    self.parent.builder.func_type(params, results);
                }
            }
        }

        let instance_type_index = self.parent.builder.end_instance_type(&self.instance_type);
        let interface_name = self.parent.resolved_comp.wit.get_interface(self.interface.interface_id).name.as_ref().unwrap().as_str();
        let interface_instance = self.parent.builder.import_instance(interface_name, instance_type_index);

        self.encode_aliases(interface_instance)?;
        Ok(())
    }

    fn encode_aliases(&mut self, interface_instance: ComponentInstanceIndex) -> Result<(), GenerationError> {
        for item in &self.interface.items {
            match item {
                ImportItemId::Type(_) => {},
                ImportItemId::Func(id) => {
                    let import_func = &self.parent.resolved_comp.imports.funcs[*id];
                    let func_idx = self.parent.builder.alias_func(interface_instance, import_func.name.as_str());
                    let core_func_idx = self.parent.builder.lower_func(func_idx);
                    self.parent.inline_export_args.push((import_func.name.to_owned(), InlineExportItem::Func(core_func_idx)));
                }
            }
        }
        Ok(())
    }

    fn rtype_to_comp_valtype(&self, rtype: ResolvedType) -> enc::ComponentValType {
        let comp = &self.parent.resolved_comp.component;
        match rtype {
            ResolvedType::Primitive(ptype) => {
                ptype.to_comp_valtype(comp)
            }
            ResolvedType::Import(itype) => {
                let index = *self.exported_ids.get(&itype).unwrap();
                enc::ComponentValType::Type(index)
            }
            ResolvedType::Defined(type_id) => {
                type_id.to_comp_valtype(comp)
            },
        }
    }
}