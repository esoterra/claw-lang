use ast::ExpressionId;
use claw_ast as ast;

use crate::{
    setup_call, ComponentContext, FunctionResolver, ItemId, ResolvedType, ResolverError,
    RESOLVED_BOOL,
};

pub trait ResolveExpression {
    /// Setup must
    /// * Call `define_name` when introducing new names
    /// * Call `use_name` when using a name
    /// * Call `setup_child` on each expression that is a child of this one.
    ///
    /// Setup may
    /// * Call `set_implied_type` if the type of an expression is known.
    fn setup_resolve(
        &self,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'_>,
    ) -> Result<(), ResolverError> {
        _ = (expression, resolver, context);
        Ok(())
    }

    /// Set up child and record parent-child relationship
    fn setup_child(
        &self,
        parent: ExpressionId,
        child: ExpressionId,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'_>,
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

    fn on_resolved(
        &self,
        rtype: ResolvedType,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'_>,
    ) -> Result<(), ResolverError> {
        _ = (rtype, expression, resolver, context);
        Ok(())
    }

    fn on_child_resolved(
        &self,
        rtype: ResolvedType,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'_>,
    ) -> Result<(), ResolverError> {
        _ = (rtype, expression, resolver, context);
        Ok(())
    }
}

macro_rules! gen_resolve_expression {
    ([$( $expr_type:ident ),*]) => {
        impl ResolveExpression for ast::Expression {
            fn setup_resolve(
                &self,
                expression: ExpressionId,
                resolver: &mut FunctionResolver,
                context: &ComponentContext<'_>,
            ) -> Result<(), ResolverError> {
                match self {
                    $(ast::Expression::$expr_type(inner) => {
                        let inner: &dyn ResolveExpression = inner;
                        inner.setup_resolve(expression, resolver, context)
                    },)*
                }
            }

            fn on_resolved(&self,
                rtype: ResolvedType,
                expression: ExpressionId,
                resolver: &mut FunctionResolver,
                context: &ComponentContext<'_>,
            ) -> Result<(), ResolverError> {
                match self {
                    $(ast::Expression::$expr_type(inner) => inner.on_resolved(rtype, expression, resolver, context),)*
                }
            }

            fn on_child_resolved(&self,
                rtype: ResolvedType,
                expression: ExpressionId,
                resolver: &mut FunctionResolver,
                context: &ComponentContext<'_>,
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
    fn setup_resolve(
        &self,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'_>,
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

    fn on_resolved(
        &self,
        rtype: ResolvedType,
        _expression: ExpressionId,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'_>,
    ) -> Result<(), ResolverError> {
        let item = resolver.lookup_name(context, self.ident)?;
        match item {
            ItemId::Local(local) => resolver.set_local_type(local, rtype),
            _ => {}
        }
        Ok(())
    }
}

impl ResolveExpression for ast::Literal {
    fn setup_resolve(
        &self,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
        _context: &ComponentContext<'_>,
    ) -> Result<(), ResolverError> {
        match self {
            ast::Literal::String(_) => {
                resolver.set_expr_type(
                    expression,
                    ResolvedType::Primitive(ast::PrimitiveType::String),
                );
            }
            _ => {}
        }
        Ok(())
    }
}

pub struct ItemContext<'ctx> {
    item: ItemId,
    component: &'ctx ast::Component,
}

impl ItemId {
    pub fn with<'ctx>(&self, component: &'ctx ast::Component) -> ItemContext<'ctx> {
        ItemContext {
            item: *self,
            component,
        }
    }
}

impl<'ctx> ItemContext<'ctx> {
    fn get_fn_type(&self) -> Option<&dyn ast::FnTypeInfo> {
        match self.item {
            ItemId::Import(import) => {
                let import = &self.component.imports[import];
                match &import.external_type {
                    ast::ExternalType::Function(fn_type) => Some(fn_type),
                }
            }
            ItemId::Function(function) => {
                let function = &self.component.functions[function];
                Some(function)
            }
            _ => None,
        }
    }
}

impl ResolveExpression for ast::Call {
    fn setup_resolve(
        &self,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'_>,
    ) -> Result<(), ResolverError> {
        let item = resolver.use_name(context, self.ident)?;
        let item = item.with(context.component);
        let fn_type = item.get_fn_type().unwrap();

        setup_call(self, fn_type, resolver, context, expression)?;

        for arg in self.args.iter() {
            self.setup_child(expression, *arg, resolver, context)?;
        }

        Ok(())
    }
}

impl ResolveExpression for ast::UnaryExpression {
    fn setup_resolve(
        &self,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'_>,
    ) -> Result<(), ResolverError> {
        self.setup_child(expression, self.inner, resolver, context)
    }

    fn on_resolved(
        &self,
        rtype: ResolvedType,
        _expression: ExpressionId,
        resolver: &mut FunctionResolver,
        _context: &ComponentContext<'_>,
    ) -> Result<(), ResolverError> {
        resolver.set_expr_type(self.inner, rtype);
        Ok(())
    }

    fn on_child_resolved(
        &self,
        rtype: ResolvedType,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
        _context: &ComponentContext<'_>,
    ) -> Result<(), ResolverError> {
        resolver.set_expr_type(expression, rtype);
        Ok(())
    }
}

// Binary Operators

impl ResolveExpression for ast::BinaryExpression {
    fn setup_resolve(
        &self,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'_>,
    ) -> Result<(), ResolverError> {
        if self.is_relation() {
            resolver.set_expr_type(expression, RESOLVED_BOOL);
        }
        self.setup_child(expression, self.left, resolver, context)?;
        self.setup_child(expression, self.right, resolver, context)?;
        Ok(())
    }

    fn on_resolved(
        &self,
        rtype: ResolvedType,
        _expression: ExpressionId,
        resolver: &mut FunctionResolver,
        _context: &ComponentContext<'_>,
    ) -> Result<(), ResolverError> {
        if !self.is_relation() {
            resolver.set_expr_type(self.left, rtype);
            resolver.set_expr_type(self.right, rtype);
        }
        Ok(())
    }

    fn on_child_resolved(
        &self,
        rtype: ResolvedType,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
        _context: &ComponentContext<'_>,
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
