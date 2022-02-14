mod expressions;
mod functions;

use std::{
    collections::HashMap,
    sync::Arc
};
use crate::ast::{
    Span, M,
    module::{
        Module, Item, Global, Function
    },
    expressions::Expression,
};
use crate::ir;

use miette::{Diagnostic, NamedSource};
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
#[diagnostic()]
pub enum ResolverError {
    #[error("Failed to resolve")]
    Base {
        #[source_code]
        src: Arc<NamedSource>,
        #[label("This bit")]
        span: Span
    },
    #[error("Failed to resolve name \"{ident}\"")]
    NameError {
        #[source_code]
        src: Arc<NamedSource>,
        #[label("Name referenced here")]
        span: Span,
        ident: String
    },
    #[error("Not yet supported")]
    NotYetSupported
}

pub fn resolve(
    src: Arc<NamedSource>,
    ast: Module
) -> Result<ir::Module, ResolverError> {
    let mut root = ModuleContext::new();
    let mut module = ir::Module::new();

    let mut item_ids = Vec::new();

    // Scan each item
    // 1. add them to the ir::Module
    // 2. add them to the root context
    for item in ast.items.iter() {
        let (item_info, id) = match &item {
            Item::Global(global) => {
                let id = ModuleItem::Global(module.globals.len());
                let (ir_entry, item_info) = scan_global(global)?;
                module.globals.push(ir_entry);
                (item_info, id)
            },
            Item::Function(function) => {
                let id = ModuleItem::Function(module.functions.len());
                let (ir_entry, item_info) = scan_function(function)?;
                module.functions.push(ir_entry);
                (item_info, id)
            },
            _ => return Err(ResolverError::NotYetSupported)
        };
        item_ids.push(id);
        root.bind(item_info.identifier.value.clone(), id);
        if item_info.is_exported {
            module.exports.push(ir::Export {
                ident: item_info.identifier,
                id
            })
        }
    }

    // Resolve each item
    for (item, id) in ast.items.iter().zip(item_ids.iter()) {
        match &id {
            ModuleItem::Global(index) => {
                if let Item::Global(ast) = item {
                    resolve_global(&root, &mut module, *index, ast)?;
                } else { unreachable!() }
            },
            ModuleItem::Function(index) => {
                if let Item::Function(ast) = item {
                    functions::resolve_function(src.clone(), &root, &mut module, *index, ast)?;
                } else { unreachable!() }
            }
        }
    }

    Ok(module)
}

struct ItemInfo {
    is_exported: bool,
    identifier: M<String>,
}

fn scan_global(global: &Global) -> Result<(ir::Global, ItemInfo), ResolverError> {
    let Global {
        ident,
        valtype,
        mut_kwd,
        ..
    } = global;

    let item_info = ItemInfo {
        is_exported: false,
        identifier: ident.clone()
    };
    let ir_entry = ir::Global {
        ident: ident.clone(),
        type_: valtype.clone(),
        mutable: mut_kwd.is_some(),
        initial_value: ir::NeedsResolve::Unresolved
    };
    Ok((ir_entry, item_info))
}

fn scan_function(function: &Function) -> Result<(ir::Function, ItemInfo), ResolverError> {
    let Function {
        export_kwd,
        signature, ..
    } = function;

    let item_info = ItemInfo {
        is_exported: export_kwd.is_some(),
        identifier: signature.name.clone()
    };
    let ir_entry = ir::Function {
        signature: signature.clone(),
        type_graph: ir::NeedsResolve::Unresolved,
        locals: ir::NeedsResolve::Unresolved,
        body: ir::NeedsResolve::Unresolved
    };
    Ok((ir_entry, item_info))
}

fn resolve_global<'r, 'ast>(
    context: &ModuleContext,
    module: &'r mut ir::Module,
    global_index: usize,
    ast: &'ast Global
) -> Result<(), ResolverError> {
    let _ = context;

    let initial_value = match *ast.init_value.value.clone() {
        Expression::Literal { value } => {
            value.value.clone()
        },
        _ => panic!("Only literal expressions allowed in global initializer")
    };

    module.globals[global_index].initial_value = ir::NeedsResolve::Resolved(initial_value);

    Ok(())
}

#[derive(Clone, Copy, Debug)]
pub enum ModuleItem {
    Global(usize),
    Function(usize)
}

#[derive(Clone, Debug)]
pub struct ModuleContext {
    mappings: HashMap<String, ModuleItem>
}

impl ModuleContext {
    pub fn new() -> Self {
        ModuleContext {
            mappings: HashMap::new()
        }
    }

    pub fn bind(&mut self, key: String, id: ModuleItem) {
        self.mappings.insert(key, id);
    }

    pub fn lookup(&self, key: &String) -> Option<ModuleItem> {
        self.mappings.get(key).map(|id| *id)
    }
}