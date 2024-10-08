#![allow(clippy::single_match)]

mod expression;
mod function;
mod imports;
mod statement;
pub mod types;
pub mod wit;

use ast::{FunctionId, GlobalId};
use claw_ast as ast;
use claw_common::Source;

use std::collections::HashMap;
use wit::{ResolvedWit, WitError};

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

pub use function::*;
pub use imports::*;
pub use types::*;

pub struct ResolvedComponent {
    pub wit: ResolvedWit,
    pub global_vals: HashMap<GlobalId, ast::Literal>,
    pub imports: ImportResolver,
    pub funcs: HashMap<FunctionId, ResolvedFunction>,
}

#[derive(Clone, Copy, Debug)]
pub enum ItemId {
    ImportFunc(ImportFuncId),
    Type(ResolvedType),
    Global(GlobalId),
    Param(ParamId),
    Local(LocalId),
    Function(FunctionId),
}

#[derive(Error, Debug, Diagnostic)]
pub enum ResolverError {
    #[error("Failed to resolve")]
    Base {
        #[source_code]
        src: Source,
        #[label("This bit")]
        span: SourceSpan,
    },
    #[error("Conflicting types inferred for expression {type_a} != {type_b}")]
    TypeConflict {
        #[source_code]
        src: Source,
        #[label("This bit")]
        span: SourceSpan,

        type_a: ResolvedType,
        type_b: ResolvedType,
    },
    #[error("Failed to resolve name \"{ident}\"")]
    NameError {
        #[source_code]
        src: Source,
        #[label("Name referenced here")]
        span: SourceSpan,
        ident: String,
    },
    #[error("Assigned to immutable variable \"{ident}\"")]
    AssignedToImmutable {
        #[source_code]
        src: Source,
        #[label("Defined here")]
        defined_span: SourceSpan,
        #[label("Assigned here")]
        assigned_span: SourceSpan,
        ident: String,
    },
    #[error("Function call with wrong number of arguments \"{ident}\"")]
    CallArgumentsMismatch {
        #[source_code]
        src: Source,
        #[label("Here")]
        span: SourceSpan,
        ident: String,
    },
    #[error("{0} is not yet supported")]
    NotYetSupported(String),

    #[error(transparent)]
    #[diagnostic(transparent)]
    Wit(#[from] WitError),
}

pub fn resolve(
    comp: &ast::Component,
    wit: wit::ResolvedWit,
) -> Result<ResolvedComponent, ResolverError> {
    let mut mappings: HashMap<String, ItemId> = Default::default();

    let mut imports = ImportResolver::default();
    imports.resolve_imports(comp, &wit)?;
    for (name, import) in imports.mapping.iter() {
        match import {
            ImportItemId::Type(rtype) => {
                mappings.insert(name.to_owned(), ItemId::Type(*rtype));
            }
            ImportItemId::Func(func) => {
                mappings.insert(name.to_owned(), ItemId::ImportFunc(*func));
            }
        }
    }

    for (id, global) in comp.iter_globals() {
        let name = comp.get_name(global.ident);
        mappings.insert(name.to_owned(), ItemId::Global(id));
    }
    for (id, function) in comp.iter_functions() {
        let name = comp.get_name(function.ident);
        mappings.insert(name.to_owned(), ItemId::Function(id));
    }

    let mut global_vals: HashMap<GlobalId, ast::Literal> = HashMap::new();

    for (id, global) in comp.iter_globals() {
        let global_val = match comp.get_expression(global.init_value) {
            ast::Expression::Literal(literal) => literal.clone(),
            _ => panic!("Only literal expressions allowed in global initializer"),
        };
        global_vals.insert(id, global_val);
    }

    let mut funcs: HashMap<FunctionId, ResolvedFunction> = HashMap::new();

    for (id, function) in comp.iter_functions() {
        let resolver = FunctionResolver::new(comp, &imports, function, &mappings);
        funcs.insert(id, resolver.resolve()?);
    }

    Ok(ResolvedComponent {
        wit,
        global_vals,
        imports,
        funcs,
    })
}
