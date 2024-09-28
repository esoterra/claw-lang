use std::collections::HashMap;

use crate::builders::module::{ModuleBuilder, ModuleTypeIndex};
use crate::types::align_to;
use crate::{builders::component::*, types::EncodeType, GenerationError};
use crate::{MAX_FLAT_PARAMS, MAX_FLAT_RESULTS};
use claw_resolver::{
    ImportFuncId, ImportFunction, ImportItemId, ImportType, ImportTypeId, ResolvedInterface,
};

use claw_ast as ast;
use claw_resolver::{ResolvedComponent, ResolvedType};
use wasm_encoder as enc;

pub struct ImportEncoder<'gen> {
    builder: &'gen mut ComponentBuilder,
    comp: &'gen ast::Component,
    rcomp: &'gen ResolvedComponent,
    memory: ComponentCoreMemoryIndex,
    realloc: ComponentCoreFunctionIndex,

    funcs: HashMap<ImportFuncId, EncodedImportFunc>,

    inline_export_args: Vec<(String, InlineExportItem)>,
}

pub struct EncodedImports {
    pub imports_instance: ComponentModuleInstanceIndex,
    pub funcs: HashMap<ImportFuncId, EncodedImportFunc>,
}

pub struct EncodedImportFunc {
    pub spill_params: Option<SpilledParams>,
    pub spill_results: Option<SpilledResults>,
    pub core_params: Vec<enc::ValType>,
    pub core_results: Vec<enc::ValType>,
}

pub struct SpilledParams {
    pub size: u32,
    pub align: u32,
    pub params: Vec<ParamInfo>,
}

pub struct ParamInfo {
    pub mem_offset: u32,
}

pub struct SpilledResults {
    pub size: u32,
    pub align: u32,
}

impl EncodedImportFunc {
    pub fn new(
        import_func: &ImportFunction,
        comp: &ast::Component,
        rcomp: &ResolvedComponent,
    ) -> Self {
        // Encode Params
        let mut core_params = Vec::new();
        for (_, rtype) in import_func.params.iter() {
            rtype.append_flattened(comp, rcomp, &mut core_params);
        }
        let spill_params = core_params.len() > MAX_FLAT_PARAMS as usize;
        let spill_params = if spill_params {
            core_params.clear();
            core_params.push(enc::ValType::I32);

            let mut mem_offset = 0;
            let mut align = 0;
            let mut params = Vec::new();
            for (_, rtype) in import_func.params.iter() {
                let param_align = rtype.align(comp, rcomp);
                mem_offset = align_to(mem_offset, param_align);
                align = std::cmp::max(align, param_align);
                params.push(ParamInfo { mem_offset });
                mem_offset += rtype.mem_size(comp, rcomp);
            }
            let size = mem_offset;
            Some(SpilledParams {
                size,
                align,
                params,
            })
        } else {
            None
        };
        // Encode results
        let mut core_results = Vec::new();
        let spill_results = if let Some(rtype) = import_func.results {
            rtype.append_flattened(comp, rcomp, &mut core_results);
            let spill_results = core_results.len() > MAX_FLAT_RESULTS as usize;
            match spill_results {
                true => {
                    core_params.push(enc::ValType::I32);
                    core_results.clear();
                    let size = rtype.mem_size(comp, rcomp);
                    let align = rtype.align(comp, rcomp);
                    Some(SpilledResults { size, align })
                }
                false => None,
            }
        } else {
            None
        };

        Self {
            spill_params,
            spill_results,
            core_params,
            core_results,
        }
    }

    pub fn encode_mod_type(&self, builder: &mut ModuleBuilder) -> ModuleTypeIndex {
        let params = self.core_params.iter().copied();
        let results = self.core_results.iter().copied();
        builder.func_type(params, results)
    }
}

impl<'gen> ImportEncoder<'gen> {
    pub fn new(
        builder: &'gen mut ComponentBuilder,
        comp: &'gen ast::Component,
        rcomp: &'gen ResolvedComponent,
        memory: ComponentCoreMemoryIndex,
        realloc: ComponentCoreFunctionIndex,
    ) -> Self {
        let funcs = HashMap::new();
        let inline_export_args = Vec::new();

        Self {
            builder,
            comp,
            rcomp,
            memory,
            realloc,
            funcs,
            inline_export_args,
        }
    }

    pub fn encode(mut self) -> Result<EncodedImports, GenerationError> {
        for interface in self.rcomp.imports.interfaces.iter() {
            self.encode_interface(interface)?;
        }

        self.encode_loose_funcs();

        let imports_instance = self.builder.inline_export(&self.inline_export_args);
        Ok(EncodedImports {
            imports_instance,
            funcs: self.funcs,
        })
    }

    fn encode_interface(&mut self, interface: &ResolvedInterface) -> Result<(), GenerationError> {
        ImportInterfaceEncoder::new(self, interface).encode()
    }

    fn encode_loose_funcs(&mut self) {
        // Import the loose functions
        for id in self.rcomp.imports.loose_funcs.iter().copied() {
            let import_func = &self.rcomp.imports.funcs[id];
            let import_alias = import_func.alias.as_str();
            let import_name = import_func.name.as_str();

            let type_idx = self.encode_func_type(import_func);
            let func_idx = self.builder.import_func(import_name, type_idx);
            let core_func_idx = self.builder.lower_func(func_idx, self.memory, self.realloc);

            let enc_import_func = EncodedImportFunc::new(import_func, self.comp, self.rcomp);
            self.funcs.insert(id, enc_import_func);

            self.inline_export_args.push((
                import_alias.to_owned(),
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
        match rtype {
            ResolvedType::Primitive(ptype) => ptype.to_comp_valtype(self.comp, self.rcomp),
            ResolvedType::Import(_) => todo!(),
            ResolvedType::Defined(type_id) => type_id.to_comp_valtype(self.comp, self.rcomp),
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
        let interface_name = &self.interface.name;
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
                let import_type = &self.parent.rcomp.imports.types[id];
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
        let import_func = &self.parent.rcomp.imports.funcs[id];
        let func_type_id = self.encode_func_type(import_func);
        let ty = enc::ComponentTypeRef::Func(func_type_id);
        self.instance_type.export(&import_func.name, ty);

        let enc_import_func =
            EncodedImportFunc::new(import_func, self.parent.comp, self.parent.rcomp);
        self.parent.funcs.insert(id, enc_import_func);
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
                    let import_func = &self.parent.rcomp.imports.funcs[*id];
                    let func_idx = self
                        .parent
                        .builder
                        .alias_func(interface_instance, import_func.name.as_str());
                    let core_func_idx = self.parent.builder.lower_func(
                        func_idx,
                        self.parent.memory,
                        self.parent.realloc,
                    );
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
        match rtype {
            ResolvedType::Primitive(ptype) => {
                ptype.to_comp_valtype(self.parent.comp, self.parent.rcomp)
            }
            ResolvedType::Import(itype) => {
                let index = *self.exported_ids.get(&itype).unwrap();
                enc::ComponentValType::Type(index)
            }
            ResolvedType::Defined(type_id) => {
                type_id.to_comp_valtype(self.parent.comp, self.parent.rcomp)
            }
        }
    }
}
