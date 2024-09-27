use ast::{ExpressionId, NameId, Span, StatementId, TypeId};
use claw_ast as ast;
use claw_common::{Source, StackMap};

use cranelift_entity::{entity_impl, EntityList, ListPool, PrimaryMap};
use std::collections::{HashMap, VecDeque};

#[cfg(test)]
use miette::{miette, LabeledSpan};

use crate::expression::*;
use crate::imports::ImportResolver;
use crate::statement::*;
use crate::types::ResolvedType;
use crate::{ItemId, ResolverError};

pub(crate) struct FunctionResolver<'ctx> {
    pub(crate) src: Source,
    pub(crate) component: &'ctx ast::Component,
    pub(crate) imports: &'ctx ImportResolver,
    pub(crate) function: &'ctx ast::Function,

    pub(crate) params: PrimaryMap<ParamId, TypeId>,

    // Name Resolution
    /// Entries for each unique local
    pub(crate) locals: PrimaryMap<LocalId, LocalInfo>,
    /// The span for each unique local
    pub(crate) local_spans: HashMap<LocalId, Span>,
    /// The association between identifiers and their subjects during resolving
    pub(crate) mapping: StackMap<String, ItemId>,
    /// The resolved bindings of expressions to subjects
    pub(crate) bindings: HashMap<NameId, ItemId>,

    // Type Resolution
    resolver_queue: VecDeque<(ResolvedType, ResolverItem)>,

    // The parent expression (if there is one) for each expression
    pub(crate) expr_parent_map: HashMap<ExpressionId, ExpressionId>,
    /// The type of each expression
    pub(crate) expression_types: HashMap<ExpressionId, ResolvedType>,

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
    pub ident: NameId,
    pub mutable: bool,
    pub annotation: Option<TypeId>,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ParamId(u32);
entity_impl!(ParamId, "param");

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LocalId(u32);
entity_impl!(LocalId, "local");

impl<'ctx> FunctionResolver<'ctx> {
    pub(crate) fn new(
        src: Source,
        component: &'ctx ast::Component,
        imports: &'ctx ImportResolver,
        function: &'ctx ast::Function,
        mappings: &'ctx HashMap<String, ItemId>,
    ) -> Self {
        let mut params = PrimaryMap::new();
        let mut mapping: StackMap<String, ItemId> = mappings.clone().into();

        for (ident, valtype) in function.params.iter() {
            let param = params.push(*valtype);
            let name = component.get_name(*ident).to_owned();
            mapping.insert(name, ItemId::Param(param));
        }

        FunctionResolver {
            src,
            component,
            imports,
            function,
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

    pub(crate) fn resolve(mut self) -> Result<ResolvedFunction, ResolverError> {
        self.setup_block(&self.function.body)?;
        self.resolve_types()?;

        Ok(ResolvedFunction {
            params: self.params,
            locals: self.locals,
            local_spans: self.local_spans,
            local_types: self.local_types,
            bindings: self.bindings,
            expression_types: self.expression_types,
        })
    }

    pub(crate) fn setup_block(&mut self, statements: &[StatementId]) -> Result<(), ResolverError> {
        // Take a checkpoint at the state of the mappings before this block
        let checkpoint = self.mapping.checkpoint();
        // Resolve all of the inner statements
        for statement in statements {
            self.setup_statement(*statement)?;
        }
        // Restore the state of the mappings from before the block
        self.mapping.restore(checkpoint);
        Ok(())
    }

    pub(crate) fn setup_statement(&mut self, statement: StatementId) -> Result<(), ResolverError> {
        self.component.get_statement(statement).setup_resolve(self)
    }

    pub(crate) fn setup_expression(
        &mut self,
        expression: ExpressionId,
    ) -> Result<(), ResolverError> {
        self.component
            .get_expression(expression)
            .setup_resolve(expression, self)
    }

    pub(crate) fn setup_child_expression(
        &mut self,
        parent: ExpressionId,
        child: ExpressionId,
    ) -> Result<(), ResolverError> {
        self.setup_expression(child)?;
        self.expr_parent_map.insert(child, parent);
        Ok(())
    }

    pub(crate) fn define_name(&mut self, ident: NameId, item: ItemId) -> Result<(), ResolverError> {
        self.bindings.insert(ident, item);
        let name = self.component.get_name(ident);
        self.mapping.insert(name.to_owned(), item);
        Ok(())
    }

    pub(crate) fn use_name(&mut self, ident: NameId) -> Result<ItemId, ResolverError> {
        let name = self.component.get_name(ident);
        let item = match self.mapping.lookup(&name.to_owned()) {
            Some(item) => *item,
            None => return self.name_error(ident),
        };
        self.bindings.insert(ident, item);
        Ok(item)
    }

    pub(crate) fn lookup_name(&self, ident: NameId) -> Result<ItemId, ResolverError> {
        match self.bindings.get(&ident) {
            Some(item) => Ok(*item),
            None => self.name_error(ident),
        }
    }

    fn name_error<T>(&self, ident: NameId) -> Result<T, ResolverError> {
        let span = self.component.name_span(ident);
        let ident = self.component.get_name(ident).to_owned();
        Err(ResolverError::NameError {
            src: self.src.clone(),
            span,
            ident,
        })
    }

    pub(crate) fn use_local(&mut self, local: LocalId, expression: ExpressionId) {
        let existing_uses = self.local_uses.get_mut(&local);
        if let Some(uses) = existing_uses {
            uses.push(expression, &mut self.local_uses_list_pool);
        } else {
            let mut uses = EntityList::new();
            uses.push(expression, &mut self.local_uses_list_pool);
            self.local_uses.insert(local, uses);
        }
    }

    pub(crate) fn set_expr_type(&mut self, id: ExpressionId, rtype: ResolvedType) {
        self.resolver_queue
            .push_back((rtype, ResolverItem::Expression(id)));
    }

    pub(crate) fn set_local_type(&mut self, id: LocalId, rtype: ResolvedType) {
        self.resolver_queue
            .push_back((rtype, ResolverItem::Local(id)));
    }

    fn resolve_types(&mut self) -> Result<(), ResolverError> {
        while let Some((next_type, next_item)) = self.resolver_queue.pop_front() {
            match next_item {
                ResolverItem::Expression(expression) => {
                    // Apply the inferred type and detect conflicts
                    if let Some(existing_type) = self.expression_types.get(&expression) {
                        if !next_type.type_eq(existing_type, self.component) {
                            let span = self.component.expression_span(expression);
                            return Err(ResolverError::TypeConflict {
                                src: self.src.clone(),
                                span,
                                type_a: *existing_type,
                                type_b: next_type,
                            });
                        } else {
                            #[cfg(test)]
                            self.notify_skipped_expression(expression);
                            continue;
                        }
                    } else {
                        self.expression_types.insert(expression, next_type);
                    }

                    #[cfg(test)]
                    self.notify_resolved_expression(expression);

                    let expression_val = self.component.get_expression(expression);
                    expression_val.on_resolved(next_type, expression, self)?;

                    if let Some(parent_id) = self.expr_parent_map.get(&expression) {
                        let parent = self.component.get_expression(*parent_id);
                        parent.on_child_resolved(next_type, *parent_id, self)?;
                    } else {
                        #[cfg(test)]
                        self.notify_orphaned_expression(expression);
                    }
                }
                ResolverItem::Local(local) => {
                    if let Some(existing_type) = self.local_types.get(&local) {
                        if !next_type.type_eq(existing_type, self.component) {
                            panic!("Local type error!!!");
                        } else {
                            #[cfg(test)]
                            self.notify_skipped_local(local);
                            continue;
                        }
                    } else {
                        self.local_types.insert(local, next_type);
                    }

                    #[cfg(test)]
                    self.notify_resolved_local(local);

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
    fn notify_skipped_expression(&self, expression: ExpressionId) {
        let span = self.component.expression_span(expression);
        let report = miette!(
            labels = vec![LabeledSpan::at(span, "here")],
            "Skipping already resolved expression"
        );
        println!("{:?}", report.with_source_code(self.src.clone()));
    }

    #[cfg(test)]
    fn notify_resolved_expression(&self, expression: ExpressionId) {
        let span = self.component.expression_span(expression);
        let report = miette!(
            labels = vec![LabeledSpan::at(span, "here")],
            "Resolved type of expression"
        );
        println!("{:?}", report.with_source_code(self.src.clone()));
    }

    #[cfg(test)]
    fn notify_orphaned_expression(&self, expression: ExpressionId) {
        let span = self.component.expression_span(expression);
        let report = miette!(
            labels = vec![LabeledSpan::at(span, "here")],
            "No parent exists to be updated for"
        );
        println!("{:?}", report.with_source_code(self.src.clone()));
    }

    #[cfg(test)]
    fn notify_skipped_local(&self, local: LocalId) {
        let span = self.local_spans.get(&local).unwrap();
        let report = miette!(
            labels = vec![LabeledSpan::at(*span, "here")],
            "Skipping already resolved local"
        );
        println!("{:?}", report.with_source_code(self.src.clone()));
    }

    #[cfg(test)]
    fn notify_resolved_local(&self, local: LocalId) {
        let span = self.local_spans.get(&local).unwrap();
        let report = miette!(
            labels = vec![LabeledSpan::at(*span, "here")],
            "Resolved type of local"
        );
        println!("{:?}", report.with_source_code(self.src.clone()));
    }
}

pub struct ResolvedFunction {
    pub params: PrimaryMap<ParamId, TypeId>,

    /// Entries for each unique local
    pub locals: PrimaryMap<LocalId, LocalInfo>,
    /// The span for each unique local
    pub local_spans: HashMap<LocalId, Span>,
    // Tye type of each local
    pub local_types: HashMap<LocalId, ResolvedType>,

    /// The resolved bindings of expressions to subjects
    pub bindings: HashMap<NameId, ItemId>,
    /// The type of each expression
    pub expression_types: HashMap<ExpressionId, ResolvedType>,
}

impl ResolvedFunction {
    pub fn local_type(
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
                    src: context.source(),
                    span,
                })
            }
        }
    }

    pub fn expression_type(
        &self,
        expression: ExpressionId,
        context: &ast::Component,
    ) -> Result<ResolvedType, ResolverError> {
        let rtype = self.expression_types.get(&expression);
        match rtype {
            Some(rtype) => Ok(*rtype),
            None => {
                let span = context.expression_span(expression);
                Err(ResolverError::Base {
                    src: context.source(),
                    span,
                })
            }
        }
    }
}
