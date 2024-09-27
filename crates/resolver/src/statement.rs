use claw_ast as ast;

use crate::types::{ResolvedType, RESOLVED_BOOL};
use crate::{FunctionResolver, ItemId, LocalInfo, ResolverError};

pub(crate) trait ResolveStatement {
    /// Set up locals
    /// * Add them to resolver.locals
    /// * Identify the local_uses
    ///
    /// Perform name resolution
    /// * Updates resolver.mapping as it goes
    /// * Links identifiers to their targets in resolver.bindings
    ///
    /// Record expression parents
    fn setup_resolve(&self, resolver: &mut FunctionResolver) -> Result<(), ResolverError>;
}

macro_rules! gen_resolve_statement {
    ([$( $expr_type:ident ),*]) => {
        impl ResolveStatement for ast::Statement {
            fn setup_resolve(
                &self,
                resolver: &mut FunctionResolver,
            ) -> Result<(), ResolverError> {
                match self {
                    $(ast::Statement::$expr_type(inner) => {
                        let inner: &dyn ResolveStatement = inner;
                        inner.setup_resolve(resolver)
                    },)*
                }
            }
        }
    }
}

gen_resolve_statement!([Let, Assign, Call, If, Return]);

impl ResolveStatement for ast::Let {
    fn setup_resolve(&self, resolver: &mut FunctionResolver) -> Result<(), ResolverError> {
        let info = LocalInfo {
            ident: self.ident.to_owned(),
            mutable: self.mutable,
            annotation: self.annotation.to_owned(),
        };
        let local = resolver.locals.push(info);
        let span = resolver.component.name_span(self.ident);
        resolver.local_spans.insert(local, span);
        let item = ItemId::Local(local);
        resolver.define_name(self.ident, item)?;

        resolver.setup_expression(self.expression)?;
        resolver.use_local(local, self.expression);

        if let Some(annotation) = self.annotation {
            resolver.set_local_type(local, ResolvedType::Defined(annotation))
        }

        Ok(())
    }
}

impl ResolveStatement for ast::Assign {
    fn setup_resolve(&self, resolver: &mut FunctionResolver) -> Result<(), ResolverError> {
        let item = resolver.use_name(self.ident)?;

        match item {
            ItemId::Global(global) => {
                let global = resolver.component.get_global(global);
                resolver.set_expr_type(self.expression, ResolvedType::Defined(global.type_id));

                if !global.mutable {
                    return Err(ResolverError::AssignedToImmutable {
                        src: resolver.component.source(),
                        defined_span: resolver.component.name_span(global.ident),
                        assigned_span: resolver.component.name_span(self.ident),
                        ident: resolver.component.get_name(self.ident).to_string(),
                    });
                }
            }
            ItemId::Param(param) => {
                let param_type = *resolver.params.get(param).unwrap();
                resolver.set_expr_type(self.expression, ResolvedType::Defined(param_type));
            }
            ItemId::Local(local) => {
                resolver.use_local(local, self.expression);

                let local = resolver.locals.get(local).unwrap();

                if !local.mutable {
                    return Err(ResolverError::AssignedToImmutable {
                        src: resolver.component.source(),
                        defined_span: resolver.component.name_span(local.ident),
                        assigned_span: resolver.component.name_span(self.ident),
                        ident: resolver.component.get_name(self.ident).to_string(),
                    });
                }
            }
            _ => {}
        }

        resolver.setup_expression(self.expression)
    }
}

impl ResolveStatement for ast::Call {
    fn setup_resolve(&self, resolver: &mut FunctionResolver) -> Result<(), ResolverError> {
        resolver.use_name(self.ident)?;
        for arg in self.args.iter() {
            resolver.setup_expression(*arg)?;
        }
        Ok(())
    }
}

impl ResolveStatement for ast::If {
    fn setup_resolve(&self, resolver: &mut FunctionResolver) -> Result<(), ResolverError> {
        resolver.set_expr_type(self.condition, RESOLVED_BOOL);
        resolver.setup_expression(self.condition)?;
        resolver.setup_block(&self.block)
    }
}

impl ResolveStatement for ast::Return {
    fn setup_resolve(&self, resolver: &mut FunctionResolver) -> Result<(), ResolverError> {
        let return_type = resolver.function.results;
        match (return_type, self.expression) {
            (Some(return_type), Some(expression)) => {
                let rtype = ResolvedType::Defined(return_type);
                resolver.set_expr_type(expression, rtype);
                resolver.setup_expression(expression)?;
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
