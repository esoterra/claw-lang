use ast::ExpressionId;
use claw_ast as ast;

use crate::types::{ResolvedType, RESOLVED_BOOL};
use crate::{FunctionResolver, ItemId, ResolverError};

pub(crate) trait ResolveExpression {
    /// Walk the AST from this node down setting up the resolver.
    ///
    /// Implementations must
    /// * Call [FunctionResolver::define_name] when introducing new names
    /// * Call [FunctionResolver::use_name] when using a name
    /// * Call [FunctionResolver::setup_child_expression] on each expression that is a child of this one.
    ///
    /// Implementations may
    /// * Call [FunctionResolver::set_expr_type] if the type of an expression is known.
    fn setup_resolve(
        &self,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
    ) -> Result<(), ResolverError> {
        _ = (expression, resolver);
        Ok(())
    }

    /// In a successful type resolution, this function will be called
    /// exactly once when the type of this expression is known.
    fn on_resolved(
        &self,
        rtype: ResolvedType,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
    ) -> Result<(), ResolverError> {
        _ = (rtype, expression, resolver);
        Ok(())
    }

    /// In a successful type resolution, this function will be called
    /// once for each child of this expression.
    fn on_child_resolved(
        &self,
        rtype: ResolvedType,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
    ) -> Result<(), ResolverError> {
        _ = (rtype, expression, resolver);
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
            ) -> Result<(), ResolverError> {
                match self {
                    $(ast::Expression::$expr_type(inner) => {
                        let inner: &dyn ResolveExpression = inner;
                        inner.setup_resolve(expression, resolver)
                    },)*
                }
            }

            fn on_resolved(&self,
                rtype: ResolvedType,
                expression: ExpressionId,
                resolver: &mut FunctionResolver,
            ) -> Result<(), ResolverError> {
                match self {
                    $(ast::Expression::$expr_type(inner) => inner.on_resolved(rtype, expression, resolver),)*
                }
            }

            fn on_child_resolved(&self,
                rtype: ResolvedType,
                expression: ExpressionId,
                resolver: &mut FunctionResolver,
            ) -> Result<(), ResolverError> {
                match self {
                    $(ast::Expression::$expr_type(inner) => inner.on_child_resolved(rtype, expression, resolver),)*
                }
            }
        }
    }
}

gen_resolve_expression!([Identifier, Literal, Enum, Call, Unary, Binary]);

impl ResolveExpression for ast::Identifier {
    fn setup_resolve(
        &self,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
    ) -> Result<(), ResolverError> {
        let item = resolver.use_name(self.ident)?;
        match item {
            ItemId::Global(global) => {
                let global = resolver.component.get_global(global);
                resolver.set_expr_type(expression, ResolvedType::Defined(global.type_id));
            }
            ItemId::Param(param) => {
                let param_type = *resolver.params.get(param).unwrap();
                resolver.set_expr_type(expression, ResolvedType::Defined(param_type));
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
    ) -> Result<(), ResolverError> {
        let item = resolver.lookup_name(self.ident)?;
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

impl ResolveExpression for ast::EnumLiteral {
    fn setup_resolve(
        &self,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
    ) -> Result<(), ResolverError> {
        let item = resolver.use_name(self.enum_name)?;
        match item {
            ItemId::Type(rtype) => {
                resolver.set_expr_type(expression, rtype);
            }
            _ => panic!("Can only use literals for enums"),
        };
        Ok(())
    }
}

impl ResolveExpression for ast::Call {
    fn setup_resolve(
        &self,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
    ) -> Result<(), ResolverError> {
        let item = resolver.use_name(self.ident)?;
        let (params, results): (Vec<_>, _) = match item {
            ItemId::ImportFunc(import_func) => {
                let import_func = &resolver.imports.funcs[import_func];
                let params = import_func.params.iter().map(|(_name, rtype)| *rtype);
                let results = import_func.results.unwrap();
                (params.collect(), results)
            }
            ItemId::Function(func) => {
                let func = &resolver.component.get_function(func);
                let params = func
                    .params
                    .iter()
                    .map(|(_name, type_id)| ResolvedType::Defined(*type_id));
                let results = ResolvedType::Defined(*func.results.as_ref().unwrap());
                (params.collect(), results)
            }
            _ => panic!("Can only call functions"),
        };
        assert_eq!(params.len(), self.args.len());
        for (arg, rtype) in self.args.iter().copied().zip(params.into_iter()) {
            resolver.setup_child_expression(expression, arg)?;
            resolver.set_expr_type(arg, rtype);
        }

        resolver.set_expr_type(expression, results);

        Ok(())
    }
}

impl ResolveExpression for ast::UnaryExpression {
    fn setup_resolve(
        &self,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
    ) -> Result<(), ResolverError> {
        resolver.setup_child_expression(expression, self.inner)
    }

    fn on_resolved(
        &self,
        rtype: ResolvedType,
        _expression: ExpressionId,
        resolver: &mut FunctionResolver,
    ) -> Result<(), ResolverError> {
        resolver.set_expr_type(self.inner, rtype);
        Ok(())
    }

    fn on_child_resolved(
        &self,
        rtype: ResolvedType,
        expression: ExpressionId,
        resolver: &mut FunctionResolver,
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
    ) -> Result<(), ResolverError> {
        if self.is_relation() {
            resolver.set_expr_type(expression, RESOLVED_BOOL);
        }
        resolver.setup_child_expression(expression, self.left)?;
        resolver.setup_child_expression(expression, self.right)?;
        Ok(())
    }

    fn on_resolved(
        &self,
        rtype: ResolvedType,
        _expression: ExpressionId,
        resolver: &mut FunctionResolver,
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
