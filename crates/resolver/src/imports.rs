use std::collections::HashMap;

use ast::NameId;
use claw_ast as ast;

use crate::types::ResolvedType;
use crate::wit::{self, InterfaceId};
use crate::ResolverError;
use cranelift_entity::{entity_impl, PrimaryMap};

#[derive(Default)]
pub struct ImportResolver {
    pub mapping: HashMap<String, ImportItemId>,
    pub types: PrimaryMap<ImportTypeId, ImportType>,
    pub funcs: PrimaryMap<ImportFuncId, ImportFunction>,

    pub interfaces: Vec<ResolvedInterface>,
    pub loose_funcs: Vec<ImportFuncId>,
}

#[derive(Copy, Clone, Debug)]
pub enum ImportItemId {
    Type(ResolvedType),
    Func(ImportFuncId),
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ImportFuncId(u32);
entity_impl!(ImportFuncId, "import-func");

#[derive(Clone, Debug)]
pub struct ImportFunction {
    pub alias: String,
    pub name: String,
    pub params: Vec<(String, ResolvedType)>,
    pub results: Option<ResolvedType>,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ImportTypeId(u32);
entity_impl!(ImportTypeId, "import-type");

pub enum ImportType {
    Enum(ImportEnum),
}

pub struct ImportEnum {
    pub name: String,
    pub cases: Vec<String>,
}

impl ImportResolver {
    pub fn resolve_imports(
        &mut self,
        comp: &ast::Component,
        wit: &wit::ResolvedWit,
    ) -> Result<(), ResolverError> {
        for (_, import) in comp.imports.iter() {
            match import {
                ast::Import::Plain(import) => {
                    self.resolve_plain_import(import, comp);
                }
                ast::Import::ImportFrom(import) => {
                    self.resolve_import_from(import, comp, wit)?;
                }
            }
        }
        Ok(())
    }

    pub fn resolve_plain_import(&mut self, import: &ast::PlainImport, comp: &ast::Component) {
        match &import.external_type {
            ast::ExternalType::Function(fn_type) => {
                self.resolve_plain_import_func(import.ident, import.alias, fn_type, comp);
            }
        };
    }

    fn resolve_plain_import_func(
        &mut self,
        name: NameId,
        alias: Option<NameId>,
        fn_type: &ast::FnType,
        comp: &ast::Component,
    ) {
        let name = comp.get_name(name);

        let params = fn_type
            .params
            .iter()
            .map(|(name, type_id)| {
                let name = comp.get_name(*name).to_owned();
                (name, ResolvedType::Defined(*type_id))
            })
            .collect();

        let results = fn_type.results.map(ResolvedType::Defined);

        let alias = match alias {
            Some(alias) => comp.get_name(alias),
            None => name,
        };
        let import_func = ImportFunction {
            alias: alias.to_owned(),
            name: name.to_owned(),
            params,
            results,
        };

        let import_func_id = self.funcs.push(import_func);
        let import_item_id = ImportItemId::Func(import_func_id);
        self.mapping.insert(alias.to_owned(), import_item_id);
        self.loose_funcs.push(import_func_id);
    }

    pub fn resolve_import_from(
        &mut self,
        import: &ast::ImportFrom,
        comp: &ast::Component,
        wit: &wit::ResolvedWit,
    ) -> Result<(), ResolverError> {
        let interface_id = wit.lookup_interface(&import.package, &import.interface)?;

        let mut resolver = InterfaceResolver::new(interface_id, self, wit);
        let mut bindings = Vec::new();
        for (name, alias) in import.items.iter() {
            let name = comp.get_name(*name);
            let item_id = resolver.resolve_name(name).unwrap();
            let name = match alias {
                Some(name) => comp.get_name(*name).to_owned(),
                None => name.to_owned(),
            };
            bindings.push((name, item_id));
        }

        let resolved = resolver.finalize();
        self.interfaces.push(resolved);

        for (name, item) in bindings {
            self.mapping.insert(name, item);
        }

        Ok(())
    }
}

pub struct InterfaceResolver<'ctx> {
    wit: &'ctx wit::ResolvedWit,
    imports: &'ctx mut ImportResolver,

    interface_id: InterfaceId,
    interface: &'ctx wit::Interface,

    resolved_types: HashMap<wit::TypeId, ResolvedType>,
    items: Vec<ImportItemId>,
}

pub struct ResolvedInterface {
    pub interface_id: InterfaceId,
    pub name: String,
    pub items: Vec<ImportItemId>,
}

impl<'ctx> InterfaceResolver<'ctx> {
    pub fn new(
        interface_id: InterfaceId,
        imports: &'ctx mut ImportResolver,
        wit: &'ctx wit::ResolvedWit,
    ) -> Self {
        let interface = wit.get_interface(interface_id);

        Self {
            wit,
            imports,
            interface_id,
            interface,
            resolved_types: Default::default(),
            items: Default::default(),
        }
    }

    pub fn finalize(self) -> ResolvedInterface {
        ResolvedInterface {
            interface_id: self.interface_id,
            name: self.wit.resolve.id_of(self.interface_id).unwrap(),
            items: self.items,
        }
    }

    pub fn resolve_name(&mut self, name: &str) -> Option<ImportItemId> {
        if let Some(func) = self.interface.functions.get(name) {
            return Some(self.resolve_import_func(name, func));
        }
        if let Some(type_id) = self.interface.types.get(name) {
            return Some(ImportItemId::Type(self.resolve_type_id(*type_id)));
        }
        None
    }

    fn resolve_import_func(&mut self, name: &str, func: &wit::Function) -> ImportItemId {
        let mut params = Vec::new();
        for (param_name, param_type) in func.params.iter() {
            let rtype = self.resolve_type(param_type);
            params.push((param_name.clone(), rtype));
        }

        let results = match &func.results {
            wit_parser::Results::Named(named_types) => {
                assert_eq!(named_types.len(), 0); // Can only handle "empty" named types
                None
            }
            wit_parser::Results::Anon(result_type) => Some(self.resolve_type(result_type)),
        };

        let import_func = ImportFunction {
            alias: name.to_owned(), // TODO fix
            name: name.to_owned(),
            params,
            results,
        };
        let import_func_id = self.imports.funcs.push(import_func);
        self.items.push(ImportItemId::Func(import_func_id));
        ImportItemId::Func(import_func_id)
    }

    fn resolve_type(&mut self, type_: &wit::Type) -> ResolvedType {
        type PType = ast::PrimitiveType;
        match type_ {
            // Primitives
            wit::Type::Bool => ResolvedType::Primitive(PType::Bool),
            wit::Type::U8 => ResolvedType::Primitive(PType::U8),
            wit::Type::U16 => ResolvedType::Primitive(PType::U16),
            wit::Type::U32 => ResolvedType::Primitive(PType::U32),
            wit::Type::U64 => ResolvedType::Primitive(PType::U64),
            wit::Type::S8 => ResolvedType::Primitive(PType::S8),
            wit::Type::S16 => ResolvedType::Primitive(PType::S16),
            wit::Type::S32 => ResolvedType::Primitive(PType::S32),
            wit::Type::S64 => ResolvedType::Primitive(PType::S64),
            wit::Type::Float32 => ResolvedType::Primitive(PType::F32),
            wit::Type::Float64 => ResolvedType::Primitive(PType::F64),
            wit::Type::Char => todo!(),
            wit::Type::String => ResolvedType::Primitive(PType::String),
            wit::Type::Id(id) => {
                if let Some(rtype) = self.resolved_types.get(id) {
                    *rtype
                } else {
                    self.resolve_type_id(*id)
                }
            }
        }
    }

    fn resolve_type_id(&mut self, type_id: wit::TypeId) -> ResolvedType {
        let type_def = self.wit.resolve.types.get(type_id).unwrap();
        let name = type_def.name.as_ref().unwrap().to_owned();
        assert_eq!(type_def.owner, wit::TypeOwner::Interface(self.interface_id));
        // Construct the ImportType
        let rtype = match &type_def.kind {
            wit::TypeDefKind::Enum(enum_type) => {
                let name = name.clone();
                let cases = enum_type
                    .cases
                    .iter()
                    .map(|case| &case.name)
                    .cloned()
                    .collect();
                let import_enum = ImportEnum { name, cases };
                let import_type = ImportType::Enum(import_enum);
                let import_type_id = self.imports.types.push(import_type);
                ResolvedType::Import(import_type_id)
            }
            wit::TypeDefKind::Type(t) => {
                return self.resolve_type(t);
            }
            a => panic!("Unsupported import type kind {:?}", a),
        };
        // Record item id and resolved type
        self.resolved_types.insert(type_id, rtype);
        // Record item in interface ordering
        let import_item_id = ImportItemId::Type(rtype);
        self.items.push(import_item_id);
        rtype
    }
}
