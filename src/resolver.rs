use crate::{
    ast::{self, Expression, ExpressionId, ValType, M, NameId},
    id_map::IdMap,
    stack_map::StackMap,
};
use std::{collections::HashMap, sync::Arc};

use id_arena::{Arena, Id};
use miette::{Diagnostic, NamedSource, SourceSpan};
use petgraph::{prelude::GraphMap, Undirected};
use thiserror::Error;

#[derive(Debug)]

struct ComponentContext<'ctx> {
    src: Arc<NamedSource>,
    component: &'ctx ast::Component,
    mappings: HashMap<String, ItemId>,
    global_vals: IdMap<ast::Global, ast::Literal>,
}

struct FuncContext<'ctx> {
    parent: &'ctx ComponentContext<'ctx>,
    func: &'ctx ast::Function,
}

pub struct ResolvedComponent {
    pub src: Arc<NamedSource>,
    pub component: ast::Component,
    pub global_vals: IdMap<ast::Global, ast::Literal>,
    pub resolved_funcs: IdMap<ast::Function, FunctionResolver>,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum ItemId {
    Import(ImportId),
    Global(GlobalId),
    Param(ParamId),
    Local(LocalId),
    // Function(FunctionId),
}

type ImportId = Id<ast::Import>;
type GlobalId = Id<ast::Global>;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ParamId(pub usize);

type LocalId = Id<LocalInfo>;

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
    let mut mappings: HashMap<String, ItemId> = Default::default();

    for _import in component.imports.iter() {
        todo!("Resolver: Scanning imports.");
    }
    for (id, global) in component.globals.iter() {
        mappings.insert(global.ident.value.clone(), ItemId::Global(id));
    }
    for (_id, _function) in component.functions.iter() {
        // mappings.insert(function.signature.name.value, ItemId::Function(id));
    }

    let mut global_vals: IdMap<ast::Global, ast::Literal> = Default::default();

    for (id, global) in component.globals.iter() {
        let global_val = match global.expressions.get_exp(global.init_value) {
            Expression::Literal { value } => value.value.clone(),
            _ => panic!("Only literal expressions allowed in global initializer"),
        };
        global_vals.insert(id, global_val);
    }

    let context = ComponentContext {
        src,
        component: &component,
        global_vals,
        mappings,
    };

    let mut resolved_funcs: IdMap<ast::Function, FunctionResolver> = Default::default();

    for (id, function) in component.functions.iter() {
        let func_context = FuncContext {
            parent: &context,
            func: &function,
        };

        let mut resolver = FunctionResolver::new(&func_context);
        resolver.resolve(&func_context)?;
        resolved_funcs.insert(id, resolver);
    }

    let ComponentContext { src, global_vals, .. } = context;
    Ok(ResolvedComponent {
        src,
        component,
        global_vals,
        resolved_funcs,
    })
}

pub struct FunctionResolver {
    // Name Resolution

    /// Entries for each unique local
    pub locals: Arena<LocalInfo>,
    /// The association between identifiers and their subjects during resolving
    mapping: StackMap<String, ItemId>,
    /// The resolved bindings of expressions to subjects
    pub bindings: HashMap<NameId, ItemId>,

    // Type Resolution

    /// The type of each local variable
    pub local_types: IdMap<LocalInfo, ValType>,
    /// The type of each expression
    pub expression_types: IdMap<Expression, ValType>,
    /// Types of items which are constrained to be equal
    type_constraints: GraphMap<TypedItem, (), Undirected>,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum TypedItem {
    Global(GlobalId),
    Param(ParamId),
    Local(LocalId),
    Expression(ExpressionId),
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct LocalInfo {
    ident: M<String>,
    mutable: bool,
    annotation: Option<M<ValType>>,
}

impl FunctionResolver {
    fn new(context: &FuncContext) -> Self {
        let mut mapping: StackMap<String, ItemId> = context.parent.mappings.clone().into();

        for (i, (ident, _valtype)) in context.func.signature.arguments.iter().enumerate() {
            mapping.insert(ident.value.clone(), ItemId::Param(ParamId(i)));
        }

        FunctionResolver {
            // Name Resolution
            locals: Default::default(),
            mapping,
            bindings: Default::default(),
            // Type Resolution
            local_types: Default::default(),
            expression_types: Default::default(),
            type_constraints: Default::default(),
        }
    }

    fn resolve(&mut self, context: &FuncContext) -> Result<(), ResolverError> {
        self.resolve_block(context, context.func.body.as_ref())?;
        self.resolve_types(context)?;

        // TODO resolve locals layout

        Ok(())
    }

    fn resolve_block(
        &mut self,
        context: &FuncContext,
        block: &ast::Block,
    ) -> Result<(), ResolverError> {
        // Take a checkpoint at the state of the mappings before this block
        let checkpoint = self.mapping.checkpoint();
        // Resolve all of the inner statements
        for statement in block.statements.iter() {
            self.resolve_statement(context, statement.as_ref())?;
        }
        // Restore the state of the mappings from before the block
        self.mapping.restore(checkpoint);
        Ok(())
    }

    fn resolve_statement(
        &mut self,
        context: &FuncContext,
        statement: &ast::Statement,
    ) -> Result<(), ResolverError> {
        match &statement {
            ast::Statement::Let {
                let_kwd: _,
                mut_kwd,
                ident,
                name_id,
                annotation,
                assign_op: _,
                expression,
            } => {
                let info = LocalInfo {
                    ident: ident.to_owned(),
                    mutable: mut_kwd.is_some(),
                    annotation: annotation.to_owned(),
                };
                let local = self.locals.alloc(info);
                let item_id = ItemId::Local(local);
                self.bindings.insert(*name_id, item_id);
                self.mapping.insert(ident.value.clone(), item_id);
                self.resolve_expression(context, *expression)?;
                self.bind_local(*expression, local);
            }
            ast::Statement::Assign {
                ident,
                name_id,
                assign_op: _,
                expression,
            } => {
                self.resolve_assign(context, ident, *name_id, *expression)?;
            }
            ast::Statement::If {
                if_kwd: _,
                condition,
                block,
            } => {
                self.resolve_expression(context, *condition)?;
                self.set_implied_type(*condition, ValType::Bool);
                self.resolve_block(context, block.as_ref())?;
            }
            ast::Statement::Return {
                return_kwd: _,
                expression,
            } => {
                self.resolve_expression(context, *expression)?;
                let ret_valtype = context.func.signature.return_type.value.to_owned();
                self.set_implied_type(*expression, ret_valtype);
            }
        };
        Ok(())
    }

    fn resolve_assign(
        &mut self,
        context: &FuncContext,
        ident: &M<String>,
        name_id: NameId,
        expression: ExpressionId,
    ) -> Result<(), ResolverError> {
        if let Some(item) = self.mapping.lookup(ident.as_ref()) {
            let item = *item;
            self.resolve_expression(context, expression)?;
            self.bindings.insert(name_id, item);
            match item {
                ItemId::Import(_) => {
                    unimplemented!()
                },
                ItemId::Global(global) => {
                    self.bind_global(expression, global);
                    Ok(())
                },
                ItemId::Param(_) => {
                    unimplemented!()
                },
                ItemId::Local(local) => {
                    self.bind_local(expression, local);
                    Ok(())
                }
                // ItemId::Function(_) => {
                //     unimplemented!("Cannot bind variable identifier {} to function", ident.value);
                // },
            }
        } else {
            Err(ResolverError::NameError {
                src: context.parent.src.clone(),
                span: ident.span.clone(),
                ident: ident.value.clone(),
            })
        }
    }

    fn resolve_expression<'fb, 'ast>(
        &mut self,
        context: &FuncContext,
        expression: ExpressionId,
    ) -> Result<(), ResolverError> {
        let expression_ast = context.func.expressions.get_exp(expression);
        match expression_ast {
            Expression::Literal { .. } => Ok(()),
            Expression::Identifier { ident, name_id } => {
                if let Some(item) = self.mapping.lookup(ident.as_ref()) {
                    self.bindings.insert(*name_id, *item);
                    match item {
                        ItemId::Import(_) => {
                            unimplemented!("Cannot reference imports");
                        }
                        ItemId::Global(global) => {
                            self.bind_global(expression, *global);
                        }
                        ItemId::Param(param) => {
                            self.bind_param(expression, *param);
                        }
                        ItemId::Local(local) => {
                            self.bind_local(expression, *local);
                        }
                    };
                    Ok(())
                } else {
                    Err(ResolverError::NameError {
                        src: context.parent.src.clone(),
                        span: ident.span.clone(),
                        ident: ident.value.clone(),
                    })
                }
            }
            Expression::Binary {
                left,
                operator,
                right,
            } => {
                use ast::BinaryOp;
                self.resolve_expression(context, *left)?;
                self.resolve_expression(context, *right)?;
                match operator.as_ref() {
                    BinaryOp::Mult
                    | BinaryOp::Div
                    | BinaryOp::Mod
                    | BinaryOp::Add
                    | BinaryOp::Sub
                    | BinaryOp::BitShiftL
                    | BinaryOp::BitShiftR
                    | BinaryOp::ArithShiftR
                    | BinaryOp::BitAnd
                    | BinaryOp::BitXor
                    | BinaryOp::BitOr => {
                        self.link_expressions(expression, *left);
                        self.link_expressions(expression, *right);
                    }
                    BinaryOp::LT
                    | BinaryOp::LTE
                    | BinaryOp::GT
                    | BinaryOp::GTE
                    | BinaryOp::EQ
                    | BinaryOp::NEQ => {
                        self.set_implied_type(expression, ValType::Bool);
                        self.link_expressions(*left, *right);
                    }
                    BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
                        self.set_implied_type(expression, ValType::Bool);
                        self.set_implied_type(*left, ValType::Bool);
                        self.set_implied_type(*right, ValType::Bool);
                    }
                }
                Ok(())
            }
            _ => panic!("Unsupported expression type"),
        }
    }

    fn bind_global(&mut self, expression: ExpressionId, global: GlobalId) {
        let a = TypedItem::Expression(expression);
        let b = TypedItem::Global(global);
        self.type_constraints.add_edge(a, b, ());
    }

    fn bind_param(&mut self, expression: ExpressionId, param: ParamId) {
        let a = TypedItem::Expression(expression);
        let b = TypedItem::Param(param);
        self.type_constraints.add_edge(a, b, ());
    }

    fn bind_local(&mut self, expression: ExpressionId, local: LocalId) {
        let a = TypedItem::Expression(expression);
        let b = TypedItem::Local(local);
        self.type_constraints.add_edge(a, b, ());
    }

    fn set_implied_type(&mut self, expression: ExpressionId, valtype: ValType) {
        self.expression_types.insert(
            expression,
            valtype,
        )
    }

    fn link_expressions(&mut self, a: ExpressionId, b: ExpressionId) {
        let a = TypedItem::Expression(a);
        let b = TypedItem::Expression(b);
        self.type_constraints.add_edge(a, b, ());
    }

    fn resolve_types(&mut self, context: &FuncContext) -> Result<(), ResolverError> {
        let mut errors = Vec::new();
        loop {
            let mut updated = false;

            for (id, _expression) in context.func.expressions.expressions().iter() {
                let current = TypedItem::Expression(id);
                let mut current_type = self.expression_types.get(id).cloned();
                for neighbor in self.type_constraints.neighbors(current) {
                    let other_type = self.get_item_type(context, neighbor);
                    if let Some(other_type) = other_type {
                        if let Some(valtype) = current_type.clone() {
                            if valtype != other_type {
                                errors.push(TypeError::TypeConflict);
                            }
                        } else {
                            updated = true;
                            self.expression_types.insert(id, other_type.clone());
                            current_type = Some(other_type);
                        }
                    }
                }
            }

            for (id, _local) in self.locals.iter() {
                let current = TypedItem::Local(id);
                let mut current_type = self.local_types.get(id).cloned();
                for neighbor in self.type_constraints.neighbors(current) {
                    let other_type = self.get_item_type(context, neighbor);
                    if let Some(other_type) = other_type {
                        if let Some(valtype) = current_type.clone() {
                            if valtype != other_type {
                                errors.push(TypeError::TypeConflict);
                            }
                        } else {
                            updated = true;
                            self.local_types.insert(id, other_type.clone());
                            current_type = Some(other_type);
                        }
                    }
                }
            }

            if !updated {
                break;
            }
        }
        
        if errors.is_empty() {
            Ok(())
        } else {
            Err(ResolverError::TypeErrors { errors })
        }
    }

    fn get_item_type(&self, context: &FuncContext, item: TypedItem) -> Option<ValType> {
        match item {
            TypedItem::Global(global) => {
                Some(context.parent.component.globals.get(global).unwrap().valtype.as_ref().clone())
            },
            TypedItem::Param(param) => {
                Some(context.func.signature.arguments.get(param.0).unwrap().1.as_ref().clone())
            },
            TypedItem::Local(local) => {
                self.local_types.get(local).cloned()
            },
            TypedItem::Expression(expression) => {
                self.expression_types.get(expression).cloned()
            },
        }
    }
}
