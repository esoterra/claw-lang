use claw_ast as ast;

use crate::{
    ComponentContext, FunctionResolver, ItemId, LocalInfo, ResolveExpression, ResolvedType,
    ResolverError,
};

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
    fn setup_resolve(
        &self,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'_>,
    ) -> Result<(), ResolverError>;
}

macro_rules! gen_resolve_statement {
    ([$( $expr_type:ident ),*]) => {
        impl ResolveStatement for ast::Statement {
            fn setup_resolve(
                &self,
                resolver: &mut FunctionResolver,
                context: &ComponentContext<'_>,
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
    fn setup_resolve(
        &self,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'_>,
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
            .setup_resolve(self.expression, resolver, context)?;

        resolver.use_local(local, self.expression);

        if let Some(annotation) = self.annotation {
            resolver.set_local_type(local, ResolvedType::ValType(annotation))
        }

        Ok(())
    }
}

impl ResolveStatement for ast::Assign {
    fn setup_resolve(
        &self,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'_>,
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
    fn setup_resolve(
        &self,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'_>,
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

pub const RESOLVED_BOOL: ResolvedType = ResolvedType::Primitive(ast::PrimitiveType::Bool);

impl ResolveStatement for ast::If {
    fn setup_resolve(
        &self,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'_>,
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
    fn setup_resolve(
        &self,
        resolver: &mut FunctionResolver,
        context: &ComponentContext<'_>,
    ) -> Result<(), ResolverError> {
        let return_type = context.func(resolver.id).return_type;
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
