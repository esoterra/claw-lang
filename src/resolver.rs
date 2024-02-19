use crate::{
    ast::{self, ExpressionId, FunctionId, GlobalId, ImportId, NameId, Span, TypeId},
    context::{WithContext, C},
    stack_map::StackMap,
};
use cranelift_entity::{entity_impl, EntityList, ListPool, PrimaryMap};
use std::collections::{HashMap, VecDeque};

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Debug)]
struct ComponentContext<'ctx> {
    src: crate::Source,
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
    pub src: crate::Source,
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
        src: crate::Source,
        #[label("This bit")]
        span: SourceSpan,
    },
    #[error("Failed to resolve name \"{ident}\"")]
    NameError {
        #[source_code]
        src: crate::Source,
        #[label("Name referenced here")]
        span: SourceSpan,
        ident: String,
    },
    #[error("Function call with wrong number of arguments \"{ident}\"")]
    CallArgumentsMismatch {
        #[source_code]
        src: crate::Source,
        #[label("Here")]
        span: SourceSpan,
        ident: String,
    },
    #[error("{0} is not yet supported")]
    NotYetSupported(String),
}

#[derive(Error, Debug, Diagnostic)]
pub enum Notification {
    #[error("Skipping already resolved expression")]
    ExpressionSkipped {
        #[source_code]
        src: crate::Source,
        #[label("here")]
        span: SourceSpan,
    },
    #[error("Resolved type of expression")]
    ExpressionResolved {
        #[source_code]
        src: crate::Source,
        #[label("here")]
        span: SourceSpan,
    },
    #[error("No parent exists to be updated for")]
    ExpressionOrphan {
        #[source_code]
        src: crate::Source,
        #[label("here")]
        span: SourceSpan,
    },
    #[error("Skipping already resolved local")]
    LocalSkipped {
        #[source_code]
        src: crate::Source,
        #[label("here")]
        span: SourceSpan,
    },
    #[error("Resolved type of local")]
    LocalResolved {
        #[source_code]
        src: crate::Source,
        #[label("here")]
        span: SourceSpan,
    },
}

pub fn resolve(
    src: crate::Source,
    component: ast::Component,
) -> Result<ResolvedComponent, ResolverError> {
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
        let name = component.get_name(function.signature.ident);
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

    params: PrimaryMap<ParamId, TypeId>,

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

        let sig = &context.func(id).signature;

        for (_i, (ident, valtype)) in sig.arguments.iter().enumerate() {
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

    fn resolve<'ctx>(&mut self, context: &ComponentContext<'ctx>) -> Result<(), ResolverError> {
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

    fn lookup_name<'ctx>(
        &self,
        context: &ComponentContext<'ctx>,
        ident: NameId,
    ) -> Result<ItemId, ResolverError> {
        let name = context.component.get_name(ident);
        match self.mapping.lookup(&name.to_owned()) {
            Some(item) => Ok(*item),
            None => self.name_error(ident, context),
        }
    }

    fn define_name<'ctx>(
        &mut self,
        context: &ComponentContext<'ctx>,
        ident: NameId,
        item: ItemId,
    ) -> Result<(), ResolverError> {
        self.bindings.insert(ident, item);
        let name = context.component.get_name(ident);
        self.mapping.insert(name.to_owned(), item);
        Ok(())
    }

    fn use_name<'ctx>(
        &mut self,
        context: &ComponentContext<'ctx>,
        ident: NameId,
    ) -> Result<ItemId, ResolverError> {
        let item = self.lookup_name(context, ident)?;
        self.bindings.insert(ident, item);
        Ok(item)
    }

    fn use_local<'ctx>(&mut self, local: LocalId, expression: ExpressionId) {
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
                            panic!("Expression type error!!!");
                        } else {
                            {
                                use miette::Report;
                                let src = context.component.src.clone();
                                let span = context.component.expr().get_span(expression);
                                let notification = Notification::ExpressionSkipped { src, span };
                                println!("{:?}", Report::new(notification));
                            }
                            continue;
                        }
                    } else {
                        self.expression_types.insert(expression, next_type);
                    }

                    {
                        use miette::Report;
                        let src = context.component.src.clone();
                        let span = context.component.expr().get_span(expression);
                        let notification = Notification::ExpressionResolved { src, span };
                        println!("{:?}", Report::new(notification));
                    }

                    let expression_val = context.component.expr().get_exp(expression);
                    expression_val.on_resolved(next_type, expression, self, context)?;

                    if let Some(parent_id) = self.expr_parent_map.get(&expression) {
                        let parent = context.component.expr().get_exp(*parent_id);
                        parent.on_child_resolved(next_type, *parent_id, self, context)?;
                    } else {
                        {
                            use miette::Report;
                            let src = context.component.src.clone();
                            let span = context.component.expr().get_span(expression);
                            let notification = Notification::ExpressionOrphan { src, span };
                            println!("{:?}", Report::new(notification));
                        }
                    }
                }
                ResolverItem::Local(local) => {
                    if let Some(existing_type) = self.local_types.get(&local) {
                        if !next_type.with(context.component).type_eq(existing_type) {
                            panic!("Local type error!!!");
                        } else {
                            {
                                use miette::Report;
                                let src = context.component.src.clone();
                                let span = self.local_spans.get(&local).unwrap().clone();
                                let notification = Notification::LocalSkipped { src, span };
                                println!("{:?}", Report::new(notification));
                            }
                            continue;
                        }
                    } else {
                        self.local_types.insert(local, next_type);
                    }

                    {
                        use miette::Report;
                        let src = context.component.src.clone();
                        let span = self.local_spans.get(&local).unwrap().clone();
                        let notification = Notification::LocalResolved { src, span };
                        println!("{:?}", Report::new(notification));
                    }

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
    Unit,
    Primitive(ast::PrimitiveType),
    ValType(TypeId),
}

impl<'ctx> C<'ctx, ResolvedType, ast::Component> {
    pub fn as_primitive(&self) -> Option<ast::PrimitiveType> {
        match self.value {
            ResolvedType::Unit => None,
            ResolvedType::Primitive(s) => Some(*s),
            ResolvedType::ValType(v) => match self.context.get_type(*v) {
                ast::ValType::Result { .. } => None,
                ast::ValType::String => None,
                ast::ValType::Primitive(s) => Some(*s),
            },
        }
    }

    pub fn type_eq(&self, other: &ResolvedType) -> bool {
        match (self.value, other) {
            (ResolvedType::Unit, ResolvedType::Unit) => true,
            (ResolvedType::Primitive(left), ResolvedType::Primitive(right)) => left == right,
            (ResolvedType::ValType(left), ResolvedType::ValType(right)) => {
                let l_valtype = self.context.get_type(*left);
                let r_valtype = self.context.get_type(*right);
                l_valtype.eq(r_valtype, self.context)
            }
            _ => false,
        }
    }
}

impl From<TypeId> for ResolvedType {
    fn from(value: TypeId) -> Self {
        ResolvedType::ValType(value)
    }
}

// Statements

trait ResolveStatement {
    /// Set up locals
    /// * Add them to resolver.locals
    /// * Identify the local_uses
    ///
    /// Perform name resolution
    /// * Updates resolver.mapping as it goes
    /// * Links identifiers to their targets in resolver.bindings
    ///
    /// Record expression parents
    fn setup_resolve<'ctx>(
        &self,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'ctx>,
    ) -> Result<(), ResolverError>;
}

macro_rules! gen_resolve_statement {
    ([$( $expr_type:ident ),*]) => {
        impl ResolveStatement for ast::Statement {
            fn setup_resolve<'ctx>(
                &self,
                resolver: &mut FunctionResolver,
                context: &ComponentContext<'ctx>,
            ) -> Result<(), ResolverError> {
                match self {
                    $(ast::Statement::$expr_type(inner) => {
                        let inner: &dyn ResolveStatement = inner;
                        inner.setup_resolve(resolver, context)
                    },)*
                }
            }
        }
    }
}

gen_resolve_statement!([Let, Assign, Call, If, Return]);

impl ResolveStatement for ast::Let {
    fn setup_resolve<'ctx>(
        &self,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'ctx>,
    ) -> Result<(), ResolverError> {
        let info = LocalInfo {
            ident: self.ident.to_owned(),
            mutable: self.mutable,
            annotation: self.annotation.to_owned(),
        };
        let local = resolver.locals.push(info);
        let span = context.component.name_span(self.ident);
        resolver.local_spans.insert(local, span);
        let item = ItemId::Local(local);
        resolver.define_name(context, self.ident, item)?;

        context
            .component
            .expr()
            .get_exp(self.expression)
            .setup_resolve(self.expression, resolver, context)
    }
}

impl ResolveStatement for ast::Assign {
    fn setup_resolve<'ctx>(
        &self,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'ctx>,
    ) -> Result<(), ResolverError> {
        resolver.use_name(context, self.ident)?;
        context
            .component
            .expr()
            .get_exp(self.expression)
            .setup_resolve(self.expression, resolver, context)
    }
}

impl ResolveStatement for ast::Call {
    fn setup_resolve<'ctx>(
        &self,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'ctx>,
    ) -> Result<(), ResolverError> {
        resolver.use_name(context, self.ident)?;
        for arg in self.args.iter() {
            context
                .component
                .expr()
                .get_exp(*arg)
                .setup_resolve(*arg, resolver, context)?;
        }
        Ok(())
    }
}

const RESOLVED_BOOL: ResolvedType = ResolvedType::Primitive(ast::PrimitiveType::Bool);

impl ResolveStatement for ast::If {
    fn setup_resolve<'ctx>(
        &self,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'ctx>,
    ) -> Result<(), ResolverError> {
        resolver.set_expr_type(self.condition, RESOLVED_BOOL);

        // Resolve condition in current context
        context
            .component
            .expr()
            .get_exp(self.condition)
            .setup_resolve(self.condition, resolver, context)?;
        // Take a checkpoint at the state of the mappings before this block
        let checkpoint = resolver.mapping.checkpoint();
        // Resolve all of the inner statements
        for statement in self.block.iter() {
            context
                .component
                .get_statement(*statement)
                .setup_resolve(resolver, context)?;
        }
        // Restore the state of the mappings from before the block
        resolver.mapping.restore(checkpoint);
        Ok(())
    }
}

impl ResolveStatement for ast::Return {
    fn setup_resolve<'ctx>(
        &self,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'ctx>,
    ) -> Result<(), ResolverError> {
        let return_type = context.func(resolver.id).signature.return_type;
        match (return_type, self.expression) {
            (Some(return_type), Some(expression)) => {
                let rtype = ResolvedType::ValType(return_type);
                resolver.set_expr_type(expression, rtype);

                context
                    .component
                    .expr()
                    .get_exp(expression)
                    .setup_resolve(expression, resolver, context)?;
            }
            (Some(_), None) => panic!(
                "Return statements must contain an expression when function has a return type"
            ),
            (None, Some(_)) => panic!(
                "Return statements can't contain an expression when function has no return type"
            ),
            (None, None) => {
                // No child expression or return type, so do nothing
            }
        }

        Ok(())
    }
}

// Expressions

trait ResolveExpression {
    /// Setup must
    /// * Call `define_name` when introducing new names
    /// * Call `use_name` when using a name
    /// * Call `setup_child` on each expression that is a child of this one.
    ///
    /// Setup may
    /// * Call `set_implied_type` if the type of an expression is known.
    fn setup_resolve<'ctx>(
        &self,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'ctx>,
    ) -> Result<(), ResolverError> {
        _ = (expression, resolver, context);
        Ok(())
    }

    /// Set up child and record parent-child relationship
    fn setup_child<'ctx>(
        &self,
        parent: ExpressionId,
        child: ExpressionId,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'ctx>,
    ) -> Result<(), ResolverError> {
        // Resolve the child
        context
            .component
            .expr()
            .get_exp(child)
            .setup_resolve(child, resolver, context)?;

        // Update the parent mapping
        resolver.expr_parent_map.insert(child, parent);

        Ok(())
    }

    fn on_resolved<'ctx>(
        &self,
        rtype: ResolvedType,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'ctx>,
    ) -> Result<(), ResolverError> {
        _ = (rtype, expression, resolver, context);
        Ok(())
    }

    fn on_child_resolved<'ctx>(
        &self,
        rtype: ResolvedType,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'ctx>,
    ) -> Result<(), ResolverError> {
        _ = (rtype, expression, resolver, context);
        Ok(())
    }
}

macro_rules! gen_resolve_expression {
    ([$( $expr_type:ident ),*]) => {
        impl ResolveExpression for ast::Expression {
            fn setup_resolve<'ctx>(
                &self,
                expression: ExpressionId,
                resolver: &mut FunctionResolver,
                context: &ComponentContext<'ctx>,
            ) -> Result<(), ResolverError> {
                match self {
                    $(ast::Expression::$expr_type(inner) => {
                        let inner: &dyn ResolveExpression = inner;
                        inner.setup_resolve(expression, resolver, context)
                    },)*
                }
            }

            fn on_resolved<'ctx>(&self,
                rtype: ResolvedType,
                expression: ExpressionId,
                resolver: &mut FunctionResolver,
                context: &ComponentContext<'ctx>,
            ) -> Result<(), ResolverError> {
                match self {
                    $(ast::Expression::$expr_type(inner) => inner.on_resolved(rtype, expression, resolver, context),)*
                }
            }

            fn on_child_resolved<'ctx>(&self,
                rtype: ResolvedType,
                expression: ExpressionId,
                resolver: &mut FunctionResolver,
                context: &ComponentContext<'ctx>,
            ) -> Result<(), ResolverError> {
                match self {
                    $(ast::Expression::$expr_type(inner) => inner.on_child_resolved(rtype, expression, resolver, context),)*
                }
            }
        }
    }
}

gen_resolve_expression!([Identifier, Literal, Call, Unary, Binary]);

impl ResolveExpression for ast::Identifier {
    fn setup_resolve<'ctx>(
        &self,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'ctx>,
    ) -> Result<(), ResolverError> {
        let item = resolver.use_name(context, self.ident)?;
        match item {
            ItemId::Global(global) => {
                let global = context.component.globals.get(global).unwrap();
                resolver.set_expr_type(expression, ResolvedType::ValType(global.type_id));
            }
            ItemId::Param(param) => {
                let param_type = *resolver.params.get(param).unwrap();
                resolver.set_expr_type(expression, ResolvedType::ValType(param_type));
            }
            ItemId::Local(local) => resolver.use_local(local, expression),
            _ => {}
        }
        Ok(())
    }

    fn on_resolved<'ctx>(
        &self,
        rtype: ResolvedType,
        _expression: ExpressionId,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'ctx>,
    ) -> Result<(), ResolverError> {
        let item = resolver.lookup_name(context, self.ident)?;
        match item {
            ItemId::Local(local) => resolver.set_local_type(local, rtype),
            _ => {}
        }
        Ok(())
    }
}

impl ResolveExpression for ast::Literal {}

impl<'ctx> C<'ctx, ItemId, ast::Component> {
    fn get_fn_type(&self) -> Option<&dyn ast::FnTypeInfo> {
        match self.value {
            ItemId::Import(import) => {
                let import = &self.context.imports[*import];
                match &import.external_type {
                    ast::ExternalType::Function(fn_type) => Some(fn_type),
                }
            }
            ItemId::Function(function) => {
                let function = &self.context.functions[*function];
                Some(&function.signature)
            }
            _ => None,
        }
    }
}

impl ResolveExpression for ast::Call {
    fn setup_resolve<'ctx>(
        &self,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'ctx>,
    ) -> Result<(), ResolverError> {
        let item = resolver.use_name(context, self.ident)?;
        let item: C<'_, ItemId, ast::Component> = item.with(context.component);
        let fn_type = item.get_fn_type().unwrap();

        setup_call(self, fn_type, resolver, context, expression)?;

        for arg in self.args.iter() {
            self.setup_child(expression, *arg, resolver, context)?;
        }

        Ok(())
    }
}

impl ResolveExpression for ast::UnaryExpression {
    fn setup_resolve<'ctx>(
        &self,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'ctx>,
    ) -> Result<(), ResolverError> {
        self.setup_child(expression, self.inner, resolver, context)
    }

    fn on_resolved<'ctx>(
        &self,
        rtype: ResolvedType,
        _expression: ExpressionId,
        resolver: &mut FunctionResolver,
        _context: &ComponentContext<'ctx>,
    ) -> Result<(), ResolverError> {
        resolver.set_expr_type(self.inner, rtype);
        Ok(())
    }

    fn on_child_resolved<'ctx>(
        &self,
        rtype: ResolvedType,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
        _context: &ComponentContext<'ctx>,
    ) -> Result<(), ResolverError> {
        resolver.set_expr_type(expression, rtype);
        Ok(())
    }
}

// Binary Operators

impl ast::BinaryExpression {
    fn is_relation(&self) -> bool {
        use ast::BinaryOp as BE;
        match self.op {
            BE::Multiply
            | BE::Divide
            | BE::Modulo
            | BE::Add
            | BE::Subtract
            | BE::BitShiftL
            | BE::BitShiftR
            | BE::ArithShiftR
            | BE::BitAnd
            | BE::BitXor
            | BE::BitOr
            | BE::LogicalAnd
            | BE::LogicalOr => false,
            _ => true,
        }
    }
}

impl ResolveExpression for ast::BinaryExpression {
    fn setup_resolve<'ctx>(
        &self,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'ctx>,
    ) -> Result<(), ResolverError> {
        if self.is_relation() {
            resolver.set_expr_type(expression, RESOLVED_BOOL);
        }
        self.setup_child(expression, self.left, resolver, context)?;
        self.setup_child(expression, self.right, resolver, context)?;
        Ok(())
    }

    fn on_resolved<'ctx>(
        &self,
        rtype: ResolvedType,
        _expression: ExpressionId,
        resolver: &mut FunctionResolver,
        _context: &ComponentContext<'ctx>,
    ) -> Result<(), ResolverError> {
        if !self.is_relation() {
            resolver.set_expr_type(self.left, rtype);
            resolver.set_expr_type(self.right, rtype);
        }
        Ok(())
    }

    fn on_child_resolved<'ctx>(
        &self,
        rtype: ResolvedType,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
        _context: &ComponentContext<'ctx>,
    ) -> Result<(), ResolverError> {
        if !self.is_relation() {
            resolver.set_expr_type(expression, rtype);
        }

        let left = resolver.expression_types.get(&self.left).copied();
        let right = resolver.expression_types.get(&self.right).copied();

        match (left, right) {
            (Some(_left), Some(_right)) => {
                // Both types known, do nothing
            }
            (Some(left), None) => {
                resolver.set_expr_type(self.right, left);
            }
            (None, Some(right)) => {
                resolver.set_expr_type(self.left, right);
            }
            (None, None) => {
                // Neither types known... how did we get here?
                unreachable!("If a child has been resolved, at least one child shouldn't be None")
            }
        }

        Ok(())
    }
}

// Helpers

fn setup_call<'ctx>(
    call: &ast::Call,
    fn_type: &dyn ast::FnTypeInfo,
    resolver: &mut FunctionResolver,
    context: &ComponentContext<'ctx>,
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
