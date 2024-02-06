use crate::{
    ast::{
        self, Arenas, Call, Expression, ExpressionId, FunctionId, GlobalId, ImportId, NameId,
        StatementId, TypeId,
    },
    stack_map::StackMap,
};
use cranelift_entity::{entity_impl, EntityList, EntityRef, EntitySet, ListPool, PrimaryMap};
use std::{
    collections::{HashMap, VecDeque},
    sync::Arc,
};

use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

#[derive(Debug)]
struct ComponentContext<'ctx> {
    src: Arc<NamedSource>,
    component: &'ctx ast::Component,
    mappings: HashMap<NameId, ItemId>,
    global_vals: HashMap<GlobalId, ast::Literal>,
}

pub struct FuncContext<'ctx> {
    parent: &'ctx ComponentContext<'ctx>,
    func: &'ctx ast::Function,
}

impl<'ctx> FuncContext<'ctx> {
    fn arenas(&self) -> &Arenas {
        &self.parent.component.arenas
    }

    fn name_error<T>(&self, ident: NameId) -> Result<T, ResolverError> {
        let span = self.arenas().name_span(ident);
        let ident = self.arenas().get_name(ident).to_owned();
        Err(ResolverError::NameError {
            src: self.parent.src.clone(),
            span,
            ident,
        })
    }

    fn get_expr(&self, expression: ExpressionId) -> &ast::Expression {
        self.arenas().expr().get_exp(expression)
    }

    fn get_stmt(&self, statement: StatementId) -> &ast::Statement {
        self.arenas().get_statement(statement)
    }
}

pub struct ResolvedComponent {
    pub src: Arc<NamedSource>,
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
    #[diagnostic()]
    #[error("Failed to resolve")]
    Base {
        #[source_code]
        src: Arc<NamedSource>,
        #[label("This bit")]
        span: SourceSpan,
    },
    #[diagnostic()]
    #[error("Failed to resolve name \"{ident}\"")]
    NameError {
        #[source_code]
        src: Arc<NamedSource>,
        #[label("Name referenced here")]
        span: SourceSpan,
        ident: String,
    },
    #[diagnostic()]
    #[error("Function call with wrong number of arguments \"{ident}\"")]
    CallArgumentsMismatch {
        #[source_code]
        src: Arc<NamedSource>,
        #[label("Here")]
        span: SourceSpan,
        ident: String,
    },
    #[diagnostic()]
    #[error("Errors when resolving types")]
    TypeErrors {
        #[related]
        errors: Vec<TypeError>,
    },
    #[diagnostic()]
    #[error("Not yet supported")]
    NotYetSupported,
}

#[derive(Error, Debug, Diagnostic)]
pub enum TypeError {
    #[diagnostic()]
    #[error("Could not determine type of expression")]
    TypeUnknown {
        #[source_code]
        src: Arc<NamedSource>,
        #[label("Here")]
        span: SourceSpan,
    },

    #[diagnostic()]
    #[error("Encountered conflicts when resolving types")]
    TypeConflict,
}

pub fn resolve(
    src: Arc<NamedSource>,
    component: ast::Component,
) -> Result<ResolvedComponent, ResolverError> {
    let mut mappings: HashMap<NameId, ItemId> = Default::default();

    for (id, import) in component.imports.iter() {
        mappings.insert(import.ident, ItemId::Import(id));
    }
    for (id, global) in component.globals.iter() {
        mappings.insert(global.ident, ItemId::Global(id));
    }
    for (id, function) in component.functions.iter() {
        mappings.insert(function.signature.ident, ItemId::Function(id));
    }

    let mut global_vals: HashMap<GlobalId, ast::Literal> = HashMap::new();

    for (id, global) in component.globals.iter() {
        let global_val = match component.arenas.expr().get_exp(global.init_value) {
            Expression::Literal(literal) => literal.clone(),
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

    for (id, function) in component.functions.iter() {
        let func_context = FuncContext {
            parent: &context,
            func: &function,
        };

        let mut resolver = FunctionResolver::new(&func_context);
        resolver.resolve(&func_context)?;
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

#[derive(Default)]
pub struct FunctionResolver {
    // Name Resolution
    /// Entries for each unique local
    pub locals: PrimaryMap<LocalId, LocalInfo>,
    /// The association between identifiers and their subjects during resolving
    mapping: StackMap<NameId, ItemId>,
    /// The resolved bindings of expressions to subjects
    pub bindings: HashMap<NameId, ItemId>,

    // Type Resolution
    resolver_queue: VecDeque<ResolverItem>,

    // The parent expression (if there is one) for each expression
    expr_parent_map: HashMap<ExpressionId, ExpressionId>,
    /// The type of each expression
    pub expression_types: HashMap<ExpressionId, ResolvedType>,

    expression_list_pool: ListPool<ExpressionId>,
    // The expressions which use a given global
    global_uses: HashMap<GlobalId, EntityList<ExpressionId>>,
    // The expressions which use a given param
    param_uses: HashMap<ParamId, EntityList<ExpressionId>>,
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

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum TypedItem {
    Global(GlobalId),
    Param(ParamId),
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
    fn new(context: &FuncContext<'_>) -> Self {
        let mut mapping: StackMap<NameId, ItemId> = context.parent.mappings.clone().into();

        let fn_type = &context.func.signature.fn_type;

        for (i, (ident, _valtype)) in fn_type.arguments.iter().enumerate() {
            mapping.insert(*ident, ItemId::Param(ParamId(i as u32)));
        }

        FunctionResolver {
            mapping,
            ..Default::default()
        }
    }

    fn resolve<'ctx>(&mut self, context: &FuncContext<'ctx>) -> Result<(), ResolverError> {
        for statement in context.func.body.iter() {
            let statement = context.get_stmt(*statement);
            statement.setup_resolve(self, context)?;
        }
        self.resolve_types(context)?;

        // TODO resolve locals layout

        Ok(())
    }

    fn lookup_name<'ctx>(
        &self,
        context: &FuncContext<'ctx>,
        ident: NameId,
    ) -> Result<ItemId, ResolverError> {
        match self.mapping.lookup(&ident) {
            Some(item) => Ok(*item),
            None => context.name_error(ident),
        }
    }

    fn bind_name<'ctx>(
        &mut self,
        context: &FuncContext<'ctx>,
        ident: NameId,
    ) -> Result<ItemId, ResolverError> {
        let item = self.lookup_name(context, ident)?;
        self.bindings.insert(ident, item);
        Ok(item)
    }

    fn bind_global_use<'ctx>(&mut self, global: GlobalId, expression: ExpressionId) {
        let existing_uses = self.global_uses.get_mut(&global);
        if let Some(uses) = existing_uses {
            uses.push(expression, &mut self.expression_list_pool);
        } else {
            let mut uses = EntityList::new();
            uses.push(expression, &mut self.expression_list_pool);
            self.global_uses.insert(global, uses);
        }
    }

    fn bind_param_use<'ctx>(&mut self, param: ParamId, expression: ExpressionId) {
        let existing_uses = self.param_uses.get_mut(&param);
        if let Some(uses) = existing_uses {
            uses.push(expression, &mut self.expression_list_pool);
        } else {
            let mut uses = EntityList::new();
            uses.push(expression, &mut self.expression_list_pool);
            self.param_uses.insert(param, uses);
        }
    }

    fn bind_local_use<'ctx>(&mut self, local: LocalId, expression: ExpressionId) {
        let existing_uses = self.local_uses.get_mut(&local);
        if let Some(uses) = existing_uses {
            uses.push(expression, &mut self.expression_list_pool);
        } else {
            let mut uses = EntityList::new();
            uses.push(expression, &mut self.expression_list_pool);
            self.local_uses.insert(local, uses);
        }
    }

    fn set_implied_type(&mut self, id: ExpressionId, inferred: ResolvedType) {
        self.expression_types.insert(id, inferred);
    }

    fn resolve_types(&mut self, context: &FuncContext<'_>) -> Result<(), ResolverError> {
        let mut errors = Vec::new();

        while let Some(next) = self.resolver_queue.front().copied() {
            match next {
                ResolverItem::Expression(expression) => {
                    if self.expression_types.contains_key(&expression) {
                        // Expression has already been resolved
                        continue;
                    }
                    let expr = context.get_expr(expression);
                    expr.infer_type(expression, self, context);
                }
                ResolverItem::Local(local) => {
                    todo!();
                }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(ResolverError::TypeErrors { errors })
        }
    }

    fn get_item_type(&self, context: &FuncContext<'_>, item: TypedItem) -> Option<ResolvedType> {
        match item {
            TypedItem::Global(global) => {
                let type_id = context
                    .parent
                    .component
                    .globals
                    .get(global)
                    .unwrap()
                    .type_id;
                Some(type_id.into())
            }
            TypedItem::Param(param) => {
                let type_id = context
                    .func
                    .signature
                    .fn_type
                    .arguments
                    .get(param.index())
                    .unwrap()
                    .1;
                Some(type_id.into())
            }
            TypedItem::Local(local) => self
                .local_types
                .get(&local)
                .cloned()
                .map(ResolvedType::from),
            TypedItem::Expression(expression) => self.expression_types.get(&expression).cloned(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ResolvedType {
    Bool,
    ValType(TypeId),
}

impl ResolvedType {
    fn type_eq(&self, other: &ResolvedType, arenas: &Arenas) -> bool {
        match (self, other) {
            (ResolvedType::Bool, ResolvedType::Bool) => true,
            (ResolvedType::ValType(left), ResolvedType::ValType(right)) => {
                let l_valtype = arenas.get_type(*left);
                let r_valtype = arenas.get_type(*right);
                l_valtype.eq(r_valtype, arenas)
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

pub type StatementSet = EntitySet<StatementId>;
pub type ExpressionSet = EntitySet<ExpressionId>;

// Statements

pub trait ResolveStatement {
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
        context: &FuncContext<'ctx>,
    ) -> Result<(), ResolverError>;
}

macro_rules! gen_resolve_statement {
    ([$( $expr_type:ident ),*]) => {
        impl ResolveStatement for ast::Statement {
            fn setup_resolve<'ctx>(
                &self,
                resolver: &mut FunctionResolver,
                context: &FuncContext<'ctx>,
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
        context: &FuncContext<'ctx>,
    ) -> Result<(), ResolverError> {
        let info = LocalInfo {
            ident: self.ident.to_owned(),
            mutable: self.mutable,
            annotation: self.annotation.to_owned(),
        };
        let local = resolver.locals.push(info);
        let item_id = ItemId::Local(local);
        resolver.bindings.insert(self.ident, item_id);

        context
            .get_expr(self.expression)
            .setup_resolve(self.expression, resolver, context)
    }
}

impl ResolveStatement for ast::Assign {
    fn setup_resolve<'ctx>(
        &self,
        resolver: &mut FunctionResolver,
        context: &FuncContext<'ctx>,
    ) -> Result<(), ResolverError> {
        resolver.bind_name(context, self.ident)?;
        context
            .get_expr(self.expression)
            .setup_resolve(self.expression, resolver, context)
    }
}

impl ResolveStatement for ast::Call {
    fn setup_resolve<'ctx>(
        &self,
        resolver: &mut FunctionResolver,
        context: &FuncContext<'ctx>,
    ) -> Result<(), ResolverError> {
        resolver.bind_name(context, self.ident)?;
        for arg in self.args.iter() {
            context
                .get_expr(*arg)
                .setup_resolve(*arg, resolver, context)?;
        }
        Ok(())
    }
}

impl ResolveStatement for ast::If {
    fn setup_resolve<'ctx>(
        &self,
        resolver: &mut FunctionResolver,
        context: &FuncContext<'ctx>,
    ) -> Result<(), ResolverError> {
        resolver.set_implied_type(self.condition, ResolvedType::Bool);

        // Resolve condition in current context
        context
            .get_expr(self.condition)
            .setup_resolve(self.condition, resolver, context)?;
        // Take a checkpoint at the state of the mappings before this block
        let checkpoint = resolver.mapping.checkpoint();
        // Resolve all of the inner statements
        for statement in self.block {
            context
                .get_stmt(statement)
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
        context: &FuncContext<'ctx>,
    ) -> Result<(), ResolverError> {
        context
            .get_expr(self.expression)
            .setup_resolve(self.expression, resolver, context)
    }
}

// Expressions

pub trait ResolveExpression {
    /// Perform name resolution
    /// * Updates resolver.mapping as it goes
    /// * Links identifiers to their targets in resolver.bindings
    ///
    /// Prepare for type resolution
    /// * Record expression parents
    /// * Either infer type of exception or add it to resolution queue
    fn setup_resolve<'ctx>(
        &self,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
        context: &FuncContext<'ctx>,
    ) -> Result<(), ResolverError>;

    /// Attempt to infer the type of this expression
    fn infer_type<'ctx>(
        &self,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
        context: &FuncContext<'ctx>,
    ) {
    }

    fn children(&self) -> Box<dyn Iterator<Item = ExpressionId>> {
        Box::new(std::iter::empty())
    }
}

macro_rules! gen_resolve_expression {
    ([$( $expr_type:ident ),*]) => {
        impl ResolveExpression for Expression {
            fn setup_resolve<'ctx>(
                &self,
                expression: ExpressionId,
                resolver: &mut FunctionResolver,
                context: &FuncContext<'ctx>,
            ) -> Result<(), ResolverError> {
                match self {
                    $(Expression::$expr_type(inner) => {
                        let inner: &dyn ResolveExpression = inner;
                        inner.setup_resolve(expression, resolver, context)
                    },)*
                }
            }

            fn infer_type<'ctx>(&self,
                expression: ExpressionId,
                resolver: &mut FunctionResolver,
                context: &FuncContext<'ctx>,
            ) {
                match self {
                    $(Expression::$expr_type(inner) => inner.infer_type(expression, resolver, context),)*
                }
            }

            fn children(&self) -> Box<dyn Iterator<Item = ExpressionId>> {
                match self {
                    $(Expression::$expr_type(inner) => inner.children(),)*
                }
            }
        }
    }
}

gen_resolve_expression!([
    Identifier,
    Literal,
    Call,
    Invert,
    Multiply,
    Divide,
    Modulo,
    Add,
    Subtract,
    BitShiftL,
    BitShiftR,
    ArithShiftR,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Equals,
    NotEquals,
    BitAnd,
    BitXor,
    BitOr,
    LogicalAnd,
    LogicalOr
]);

impl ResolveExpression for ast::Identifier {
    fn setup_resolve<'ctx>(
        &self,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
        context: &FuncContext<'ctx>,
    ) -> Result<(), ResolverError> {
        let item = resolver.bind_name(context, self.ident)?;
        match item {
            ItemId::Import(_) => {}
            ItemId::Global(global) => resolver.bind_global_use(global, expression),
            ItemId::Param(param) => resolver.bind_param_use(param, expression),
            ItemId::Local(local) => resolver.bind_local_use(local, expression),
            ItemId::Function(_) => {}
        }
        Ok(())
    }
}

impl ResolveExpression for ast::Literal {
    fn setup_resolve<'ctx>(
        &self,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
        context: &FuncContext<'ctx>,
    ) -> Result<(), ResolverError> {
        Ok(())
    }
}

impl ResolveExpression for ast::Call {
    fn setup_resolve<'ctx>(
        &self,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
        context: &FuncContext<'ctx>,
    ) -> Result<(), ResolverError> {
        resolver.bind_name(context, self.ident)?;
        for arg in self.args.iter() {
            context
                .get_expr(*arg)
                .setup_resolve(*arg, resolver, context)?;
        }
        Ok(())
    }

    fn children(&self) -> Box<dyn Iterator<Item = ExpressionId>> {
        Box::new(self.args.iter().map(|a| *a))
    }
}

impl ResolveExpression for ast::Invert {
    fn setup_resolve<'ctx>(
        &self,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
        context: &FuncContext<'ctx>,
    ) -> Result<(), ResolverError> {
        context
            .get_expr(self.inner)
            .setup_resolve(self.inner, resolver, context)
    }

    fn children(&self) -> Box<dyn Iterator<Item = ExpressionId>> {
        Box::new(std::iter::once(self.inner))
    }
}

// Binary Operators

macro_rules! resolve_binary_op {
    ($type_name:ident) => {
        impl ResolveExpression for ast::$type_name {
            fn setup_resolve<'ctx>(
                &self,
                expression: ExpressionId,
                resolver: &mut FunctionResolver,
                context: &FuncContext<'ctx>,
            ) -> Result<(), ResolverError> {
                context
                    .get_expr(self.left)
                    .setup_resolve(self.left, resolver, context)?;
                context
                    .get_expr(self.right)
                    .setup_resolve(self.right, resolver, context)?;
                Ok(())
            }

            fn children(&self) -> Box<dyn Iterator<Item = ExpressionId>> {
                let left = std::iter::once(self.left);
                let right = std::iter::once(self.right);
                Box::new(left.chain(right))
            }
        }
    };
}

resolve_binary_op!(Multiply);
resolve_binary_op!(Divide);
resolve_binary_op!(Modulo);
resolve_binary_op!(Add);
resolve_binary_op!(Subtract);
resolve_binary_op!(BitShiftL);
resolve_binary_op!(BitShiftR);
resolve_binary_op!(ArithShiftR);
resolve_binary_op!(BitAnd);
resolve_binary_op!(BitXor);
resolve_binary_op!(BitOr);
resolve_binary_op!(LogicalAnd);
resolve_binary_op!(LogicalOr);

// Binary Relations

macro_rules! resolve_binary_rel {
    ($type_name:ident) => {
        impl ResolveExpression for ast::$type_name {
            fn setup_resolve<'ctx>(
                &self,
                expression: ExpressionId,
                resolver: &mut FunctionResolver,
                context: &FuncContext<'ctx>,
            ) -> Result<(), ResolverError> {
                resolver.set_implied_type(expression, ResolvedType::Bool);
                context
                    .get_expr(self.left)
                    .setup_resolve(self.left, resolver, context)?;
                context
                    .get_expr(self.right)
                    .setup_resolve(self.right, resolver, context)?;
                Ok(())
            }

            fn children(&self) -> Box<dyn Iterator<Item = ExpressionId>> {
                let left = std::iter::once(self.left);
                let right = std::iter::once(self.right);
                Box::new(left.chain(right))
            }
        }
    };
}

resolve_binary_rel!(LessThan);
resolve_binary_rel!(LessThanEqual);
resolve_binary_rel!(GreaterThan);
resolve_binary_rel!(GreaterThanEqual);
resolve_binary_rel!(Equals);
resolve_binary_rel!(NotEquals);

// Helpers

fn resolve_call<'ctx>(
    call: &Call,
    resolver: &mut FunctionResolver,
    context: &FuncContext<'ctx>,
    expression: Option<ExpressionId>,
) -> Result<(), ResolverError> {
    let item = resolver.lookup_name(context, call.ident)?;
    resolver.bindings.insert(call.ident, item);

    match item {
        ItemId::Import(import) => {
            let import = context.parent.component.imports.get(import).unwrap();
            match &import.external_type {
                ast::ExternalType::Function(fn_type) => {
                    if call.args.len() != fn_type.arguments.len() {
                        let span = context.arenas().name_span(call.ident);
                        let ident = context.arenas().get_name(call.ident).to_owned();
                        return Err(ResolverError::CallArgumentsMismatch {
                            src: context.parent.src.clone(),
                            span,
                            ident,
                        });
                    }

                    for (arg_expr, (_, arg_type)) in call.args.iter().zip(fn_type.arguments.iter())
                    {
                        let inferred = ResolvedType::ValType(*arg_type);
                        resolver.set_implied_type(*arg_expr, inferred);
                    }

                    if let Some(expression) = expression {
                        let inferred = ResolvedType::ValType(fn_type.return_type);
                        resolver.set_implied_type(expression, inferred);
                    }
                }
            }
        }
        ItemId::Function(function) => {
            let function = context.parent.component.functions.get(function).unwrap();
            let fn_type = &function.signature.fn_type;

            for (arg_expr, (_, arg_type)) in call.args.iter().zip(fn_type.arguments.iter()) {
                let inferred = ResolvedType::ValType(*arg_type);
                resolver.set_implied_type(*arg_expr, inferred);
            }

            if let Some(expression) = expression {
                let inferred = ResolvedType::ValType(fn_type.return_type);
                resolver.set_implied_type(expression, inferred);
            }
        }
        ItemId::Global(_) => todo!(),
        ItemId::Param(_) => todo!(),
        ItemId::Local(_) => todo!(),
    }

    return Ok(());
}
