#![allow(clippy::single_match)]

mod expression;
mod statement;

use ast::{ExpressionId, FunctionId, GlobalId, ImportId, NameId, Span, TypeId};
use claw_ast as ast;
use claw_common::{Source, StackMap};

use cranelift_entity::{entity_impl, EntityList, ListPool, PrimaryMap};
use std::collections::{HashMap, VecDeque};

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[cfg(test)]
use miette::Report;

use expression::*;
use statement::*;

#[derive(Debug)]
struct ComponentContext<'ctx> {
    src: Source,
    component: &'ctx ast::Component,
    mappings: HashMap<String, ItemId>,
    global_vals: HashMap<GlobalId, ast::Literal>,
}

impl<'ctx> ComponentContext<'ctx> {
    fn func(&self, id: FunctionId) -> &ast::Function {
        &self.component.functions[id]
    }
}

pub struct ResolvedComponent {
    pub src: Source,
    pub component: ast::Component,
    pub global_vals: HashMap<GlobalId, ast::Literal>,
    pub resolved_funcs: HashMap<FunctionId, FunctionResolver>,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum ItemId {
    Import(ImportId),
    Global(GlobalId),
    Param(ParamId),
    Local(LocalId),
    Function(FunctionId),
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ParamId(u32);
entity_impl!(ParamId, "param");

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LocalId(u32);
entity_impl!(LocalId, "local");

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
}

#[cfg(test)]
#[derive(Error, Debug, Diagnostic)]
pub enum Notification {
    #[error("Skipping already resolved expression")]
    ExpressionSkipped {
        #[source_code]
        src: Source,
        #[label("here")]
        span: SourceSpan,
    },
    #[error("Resolved type of expression")]
    ExpressionResolved {
        #[source_code]
        src: Source,
        #[label("here")]
        span: SourceSpan,
    },
    #[error("No parent exists to be updated for")]
    ExpressionOrphan {
        #[source_code]
        src: Source,
        #[label("here")]
        span: SourceSpan,
    },
    #[error("Skipping already resolved local")]
    LocalSkipped {
        #[source_code]
        src: Source,
        #[label("here")]
        span: SourceSpan,
    },
    #[error("Resolved type of local")]
    LocalResolved {
        #[source_code]
        src: Source,
        #[label("here")]
        span: SourceSpan,
    },
}

pub fn resolve(src: Source, component: ast::Component) -> Result<ResolvedComponent, ResolverError> {
    let mut mappings: HashMap<String, ItemId> = Default::default();

    for (id, import) in component.imports.iter() {
        let name = component.get_name(import.ident);
        mappings.insert(name.to_owned(), ItemId::Import(id));
    }
    for (id, global) in component.globals.iter() {
        let name = component.get_name(global.ident);
        mappings.insert(name.to_owned(), ItemId::Global(id));
    }
    for (id, function) in component.functions.iter() {
        let name = component.get_name(function.ident);
        mappings.insert(name.to_owned(), ItemId::Function(id));
    }

    let mut global_vals: HashMap<GlobalId, ast::Literal> = HashMap::new();

    for (id, global) in component.globals.iter() {
        let global_val = match component.expr().get_exp(global.init_value) {
            ast::Expression::Literal(literal) => literal.clone(),
            _ => panic!("Only literal expressions allowed in global initializer"),
        };
        global_vals.insert(id, global_val);
    }

    let context = ComponentContext {
        src: src.clone(),
        component: &component,
        global_vals,
        mappings,
    };

    let mut resolved_funcs: HashMap<FunctionId, FunctionResolver> = HashMap::new();

    for (id, _function) in component.functions.iter() {
        let mut resolver = FunctionResolver::new(&context, id);
        resolver.resolve(&context)?;
        resolved_funcs.insert(id, resolver);
    }

    let ComponentContext {
        src, global_vals, ..
    } = context;
    Ok(ResolvedComponent {
        src,
        component,
        global_vals,
        resolved_funcs,
    })
}

pub struct FunctionResolver {
    id: FunctionId,

    pub params: PrimaryMap<ParamId, TypeId>,

    // Name Resolution
    /// Entries for each unique local
    pub locals: PrimaryMap<LocalId, LocalInfo>,
    /// The span for each unique local
    pub local_spans: HashMap<LocalId, Span>,
    /// The association between identifiers and their subjects during resolving
    mapping: StackMap<String, ItemId>,
    /// The resolved bindings of expressions to subjects
    pub bindings: HashMap<NameId, ItemId>,

    // Type Resolution
    resolver_queue: VecDeque<(ResolvedType, ResolverItem)>,

    // The parent expression (if there is one) for each expression
    expr_parent_map: HashMap<ExpressionId, ExpressionId>,
    /// The type of each expression
    pub expression_types: HashMap<ExpressionId, ResolvedType>,

    local_uses_list_pool: ListPool<ExpressionId>,
    // The expressions which use a given local
    local_uses: HashMap<LocalId, EntityList<ExpressionId>>,

    // Tye type of each local
    pub local_types: HashMap<LocalId, ResolvedType>,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum ResolverItem {
    Local(LocalId),
    Expression(ExpressionId),
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct LocalInfo {
    ident: NameId,
    mutable: bool,
    annotation: Option<TypeId>,
}

impl FunctionResolver {
    fn new(context: &ComponentContext<'_>, id: FunctionId) -> Self {
        let mut params = PrimaryMap::new();
        let mut mapping: StackMap<String, ItemId> = context.mappings.clone().into();

        let func = context.func(id);

        for (ident, valtype) in func.arguments.iter() {
            let param = params.push(*valtype);
            let name = context.component.get_name(*ident).to_owned();
            mapping.insert(name, ItemId::Param(param));
        }

        FunctionResolver {
            id,
            params,
            mapping,
            locals: Default::default(),
            local_spans: Default::default(),
            bindings: Default::default(),
            resolver_queue: Default::default(),
            expr_parent_map: Default::default(),
            expression_types: Default::default(),
            local_uses_list_pool: Default::default(),
            local_uses: Default::default(),
            local_types: Default::default(),
        }
    }

    fn resolve(&mut self, context: &ComponentContext<'_>) -> Result<(), ResolverError> {
        for statement in context.func(self.id).body.iter() {
            let statement = context.component.get_statement(*statement);
            statement.setup_resolve(self, context)?;
        }
        self.resolve_types(context)?;

        // TODO resolve locals layout

        Ok(())
    }

    fn name_error<T>(
        &self,
        ident: NameId,
        context: &ComponentContext<'_>,
    ) -> Result<T, ResolverError> {
        let span = context.component.name_span(ident);
        let ident = context.component.get_name(ident).to_owned();
        Err(ResolverError::NameError {
            src: context.src.clone(),
            span,
            ident,
        })
    }

    fn lookup_name(
        &self,
        context: &ComponentContext<'_>,
        ident: NameId,
    ) -> Result<ItemId, ResolverError> {
        let name = context.component.get_name(ident);
        match self.mapping.lookup(&name.to_owned()) {
            Some(item) => Ok(*item),
            None => self.name_error(ident, context),
        }
    }

    fn define_name(
        &mut self,
        context: &ComponentContext<'_>,
        ident: NameId,
        item: ItemId,
    ) -> Result<(), ResolverError> {
        self.bindings.insert(ident, item);
        let name = context.component.get_name(ident);
        self.mapping.insert(name.to_owned(), item);
        Ok(())
    }

    fn use_name(
        &mut self,
        context: &ComponentContext<'_>,
        ident: NameId,
    ) -> Result<ItemId, ResolverError> {
        let item = self.lookup_name(context, ident)?;
        self.bindings.insert(ident, item);
        Ok(item)
    }

    fn use_local(&mut self, local: LocalId, expression: ExpressionId) {
        let existing_uses = self.local_uses.get_mut(&local);
        if let Some(uses) = existing_uses {
            uses.push(expression, &mut self.local_uses_list_pool);
        } else {
            let mut uses = EntityList::new();
            uses.push(expression, &mut self.local_uses_list_pool);
            self.local_uses.insert(local, uses);
        }
    }

    fn set_expr_type(&mut self, id: ExpressionId, rtype: ResolvedType) {
        self.resolver_queue
            .push_back((rtype, ResolverItem::Expression(id)));
    }

    fn set_local_type(&mut self, id: LocalId, rtype: ResolvedType) {
        self.resolver_queue
            .push_back((rtype, ResolverItem::Local(id)));
    }

    fn resolve_types(&mut self, context: &ComponentContext<'_>) -> Result<(), ResolverError> {
        while let Some((next_type, next_item)) = self.resolver_queue.pop_front() {
            match next_item {
                ResolverItem::Expression(expression) => {
                    // Apply the inferred type and detect conflicts
                    if let Some(existing_type) = self.expression_types.get(&expression) {
                        if !next_type.with(context.component).type_eq(existing_type) {
                            let span = context.component.expr().get_span(expression);
                            return Err(ResolverError::TypeConflict {
                                src: context.src.clone(),
                                span,
                                type_a: *existing_type,
                                type_b: next_type,
                            });
                        } else {
                            #[cfg(test)]
                            self.notify_skipped_expression(expression, context);
                            continue;
                        }
                    } else {
                        self.expression_types.insert(expression, next_type);
                    }

                    #[cfg(test)]
                    self.notify_resolved_expression(expression, context);

                    let expression_val = context.component.expr().get_exp(expression);
                    expression_val.on_resolved(next_type, expression, self, context)?;

                    if let Some(parent_id) = self.expr_parent_map.get(&expression) {
                        let parent = context.component.expr().get_exp(*parent_id);
                        parent.on_child_resolved(next_type, *parent_id, self, context)?;
                    } else {
                        #[cfg(test)]
                        self.notify_ophaned_expression(expression, context);
                    }
                }
                ResolverItem::Local(local) => {
                    if let Some(existing_type) = self.local_types.get(&local) {
                        if !next_type.with(context.component).type_eq(existing_type) {
                            panic!("Local type error!!!");
                        } else {
                            #[cfg(test)]
                            self.notify_skipped_local(local, context);
                            continue;
                        }
                    } else {
                        self.local_types.insert(local, next_type);
                    }

                    #[cfg(test)]
                    self.notify_resolved_local(local, context);

                    if self.local_uses.contains_key(&local) {
                        let uses_len = {
                            let uses = self.local_uses.get(&local).unwrap();
                            uses.len(&self.local_uses_list_pool)
                        };
                        for i in 0..uses_len {
                            let local_use = {
                                let uses = self.local_uses.get(&local).unwrap();
                                uses.get(i, &self.local_uses_list_pool).unwrap()
                            };
                            self.set_expr_type(local_use, next_type);
                        }
                    }
                }
            }
        }

        Ok(())
    }

    #[cfg(test)]
    fn notify_skipped_expression(&self, expression: ExpressionId, context: &ComponentContext) {
        let src = context.component.src.clone();
        let span = context.component.expr().get_span(expression);
        let notification = Notification::ExpressionSkipped { src, span };
        println!("{:?}", Report::new(notification));
    }

    #[cfg(test)]
    fn notify_resolved_expression(&self, expression: ExpressionId, context: &ComponentContext) {
        let src = context.component.src.clone();
        let span = context.component.expr().get_span(expression);
        let notification = Notification::ExpressionResolved { src, span };
        println!("{:?}", Report::new(notification));
    }

    #[cfg(test)]
    fn notify_ophaned_expression(&self, expression: ExpressionId, context: &ComponentContext) {
        let src = context.component.src.clone();
        let span = context.component.expr().get_span(expression);
        let notification = Notification::ExpressionOrphan { src, span };
        println!("{:?}", Report::new(notification));
    }

    #[cfg(test)]
    fn notify_skipped_local(&self, local: LocalId, context: &ComponentContext) {
        let src = context.component.src.clone();
        let span = *self.local_spans.get(&local).unwrap();
        let notification = Notification::LocalSkipped { src, span };
        println!("{:?}", Report::new(notification));
    }

    #[cfg(test)]
    fn notify_resolved_local(&self, local: LocalId, context: &ComponentContext) {
        let src = context.component.src.clone();
        let span = *self.local_spans.get(&local).unwrap();
        let notification = Notification::LocalResolved { src, span };
        println!("{:?}", Report::new(notification));
    }

    pub fn get_resolved_local_type(
        &self,
        local: LocalId,
        context: &ast::Component,
    ) -> Result<ResolvedType, ResolverError> {
        let rtype = self.local_types.get(&local);
        match rtype {
            Some(rtype) => Ok(*rtype),
            None => {
                let span = self.local_spans.get(&local).unwrap().to_owned();
                Err(ResolverError::Base {
                    src: context.src.clone(),
                    span,
                })
            }
        }
    }

    pub fn get_resolved_type(
        &self,
        expression: ExpressionId,
        context: &ast::Component,
    ) -> Result<ResolvedType, ResolverError> {
        let rtype = self.expression_types.get(&expression);
        match rtype {
            Some(rtype) => Ok(*rtype),
            None => {
                let span = context.expr().get_span(expression);
                Err(ResolverError::Base {
                    src: context.src.clone(),
                    span,
                })
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ResolvedType {
    Primitive(ast::PrimitiveType),
    String,
    ValType(TypeId),
}

impl std::fmt::Display for ResolvedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolvedType::Primitive(p) => (p as &dyn std::fmt::Debug).fmt(f),
            ResolvedType::ValType(v) => (v as &dyn std::fmt::Debug).fmt(f),
            ResolvedType::String => f.write_str("string"),
        }
    }
}

struct ResolvedTypeContext<'ctx> {
    rtype: ResolvedType,
    context: &'ctx ast::Component,
}

impl ResolvedType {
    fn with<'ctx>(&'ctx self, context: &'ctx ast::Component) -> ResolvedTypeContext<'ctx> {
        ResolvedTypeContext {
            rtype: *self,
            context,
        }
    }
}

impl<'ctx> ResolvedTypeContext<'ctx> {
    pub fn type_eq(&self, other: &ResolvedType) -> bool {
        match (self.rtype, *other) {
            // Both primitive
            (ResolvedType::Primitive(left), ResolvedType::Primitive(right)) => left == right,
            // Both string
            (ResolvedType::String, ResolvedType::String) => true,
            // Both valtype
            (ResolvedType::ValType(left), ResolvedType::ValType(right)) => {
                let l_valtype = self.context.get_type(left);
                let r_valtype = self.context.get_type(right);
                l_valtype.eq(r_valtype, self.context)
            }
            // One primitive, other valtype
            (ResolvedType::Primitive(p), ResolvedType::ValType(v))
            | (ResolvedType::ValType(v), ResolvedType::Primitive(p)) => {
                let valtype = self.context.get_type(v);
                match valtype {
                    ast::ValType::Primitive(p2) => p == *p2,
                    _ => false,
                }
            }
            // One string, other valtype
            (ResolvedType::String, ResolvedType::ValType(v))
            | (ResolvedType::ValType(v), ResolvedType::String) => {
                let valtype = self.context.get_type(v);
                matches!(valtype, ast::ValType::String)
            }
            // Fallback
            _ => false,
        }
    }
}

impl From<TypeId> for ResolvedType {
    fn from(value: TypeId) -> Self {
        ResolvedType::ValType(value)
    }
}

// Helpers

pub(crate) fn setup_call(
    call: &ast::Call,
    fn_type: &dyn ast::FnTypeInfo,
    resolver: &mut FunctionResolver,
    context: &ComponentContext<'_>,
    expression: ExpressionId,
) -> Result<(), ResolverError> {
    if call.args.len() != fn_type.get_args().len() {
        let span = context.component.name_span(call.ident);
        let ident = context.component.get_name(call.ident).to_owned();
        return Err(ResolverError::CallArgumentsMismatch {
            src: context.src.clone(),
            span,
            ident,
        });
    }

    for (arg_expr, (_, arg_type)) in call.args.iter().zip(fn_type.get_args().iter()) {
        let rtype = ResolvedType::ValType(*arg_type);
        resolver.set_expr_type(*arg_expr, rtype);
    }

    let rtype = ResolvedType::ValType(fn_type.get_return_type().unwrap());
    resolver.set_expr_type(expression, rtype);

    Ok(())
}
