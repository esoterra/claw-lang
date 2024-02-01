use crate::{
    ast::{
        self, Call, Expression, ExpressionId, FunctionId, GlobalId, ImportId, NameId, ValType, M,
    },
    stack_map::StackMap,
};
use cranelift_entity::{entity_impl, EntityRef, PrimaryMap};
use std::{collections::HashMap, sync::Arc};

use miette::{Diagnostic, NamedSource, SourceSpan};
use petgraph::{prelude::GraphMap, Undirected};
use thiserror::Error;

#[derive(Debug)]
struct ComponentContext<'ctx> {
    src: Arc<NamedSource>,
    component: &'ctx ast::Component,
    mappings: HashMap<String, ItemId>,
    global_vals: HashMap<GlobalId, ast::Literal>,
}

struct FuncContext<'ctx> {
    parent: &'ctx ComponentContext<'ctx>,
    func: &'ctx ast::Function,
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
    let mut mappings: HashMap<String, ItemId> = Default::default();

    for (id, import) in component.imports.iter() {
        mappings.insert(import.name.as_ref().clone(), ItemId::Import(id));
    }
    for (id, global) in component.globals.iter() {
        mappings.insert(global.ident.as_ref().clone(), ItemId::Global(id));
    }
    for (id, function) in component.functions.iter() {
        mappings.insert(function.signature.name.as_ref().clone(), ItemId::Function(id));
    }

    let mut global_vals: HashMap<GlobalId, ast::Literal> = HashMap::new();

    for (id, global) in component.globals.iter() {
        let global_val = match global.expressions.get_exp(global.init_value) {
            Expression::Literal { literal } => literal.value.clone(),
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

pub struct FunctionResolver {
    // Name Resolution
    /// Entries for each unique local
    pub locals: PrimaryMap<LocalId, LocalInfo>,
    /// The association between identifiers and their subjects during resolving
    mapping: StackMap<String, ItemId>,
    /// The resolved bindings of expressions to subjects
    pub bindings: HashMap<NameId, ItemId>,

    // Type Resolution
    /// The type of each local variable
    pub local_types: HashMap<LocalId, ValType>,
    /// The type of each expression
    pub expression_types: HashMap<ExpressionId, ValType>,
    /// Types of items which are constrained to be equal
    type_constraints: GraphMap<TypedItem, (), Undirected>,
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
    ident: M<String>,
    mutable: bool,
    annotation: Option<M<ValType>>,
}

impl FunctionResolver {
    fn new(context: &FuncContext<'_>) -> Self {
        let mut mapping: StackMap<String, ItemId> = context.parent.mappings.clone().into();

        let fn_type = &context.func.signature.fn_type;

        for (i, (ident, _valtype)) in fn_type.arguments.iter().enumerate() {
            mapping.insert(ident.as_ref().to_owned(), ItemId::Param(ParamId(i as u32)));
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

    fn resolve<'ctx>(
        &mut self,
        context: &FuncContext<'ctx>,
    ) -> Result<(), ResolverError> {
        self.resolve_block(context, context.func.body.as_ref())?;
        self.resolve_types(context)?;

        // TODO resolve locals layout

        Ok(())
    }

    fn resolve_block<'ctx>(
        &mut self,
        context: &FuncContext<'ctx>,
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

    fn resolve_statement<'ctx>(
        &mut self,
        context: &FuncContext<'ctx>,
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
                let local = self.locals.push(info);
                let item_id = ItemId::Local(local);
                self.bindings.insert(*name_id, item_id);
                self.mapping.insert(ident.as_ref().to_owned(), item_id);
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
            ast::Statement::Call { call } => {
                self.resolve_call(context, call, None)?;
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
                let fn_type = &context.func.signature.fn_type;

                self.resolve_expression(context, *expression)?;
                let ret_valtype = fn_type.return_type.value.to_owned();
                self.set_implied_type(*expression, ret_valtype);
            }
        };
        Ok(())
    }

    fn resolve_assign<'ctx>(
        &mut self,
        context: &FuncContext<'ctx>,
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
                }
                ItemId::Global(global) => {
                    self.bind_global(expression, global);
                    Ok(())
                }
                ItemId::Param(_) => {
                    unimplemented!()
                }
                ItemId::Local(local) => {
                    self.bind_local(expression, local);
                    Ok(())
                }
                ItemId::Function(_) => unimplemented!(),
            }
        } else {
            Err(ResolverError::NameError {
                src: context.parent.src.clone(),
                span: ident.span.clone(),
                ident: ident.as_ref().to_owned(),
            })
        }
    }

    fn resolve_expression<'ctx:>(
        &mut self,
        context: &FuncContext<'ctx>,
        expression: ExpressionId,
    ) -> Result<(), ResolverError> {
        let expression_ast: &Expression = context.func.expressions.get_exp(expression);
        match expression_ast {
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
                        ItemId::Function(_) => unimplemented!(),
                    };
                    Ok(())
                } else {
                    Err(ResolverError::NameError {
                        src: context.parent.src.clone(),
                        span: ident.span.clone(),
                        ident: ident.as_ref().to_owned(),
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
            Expression::Call { call } => {
                let context: &FuncContext<'ctx> = context;
                let call: &Call = call;
                self.resolve_call(context, call, Some(expression))
            },
            Expression::Literal { .. } => Ok(()),
            _ => panic!("Unsupported expression type"),
        }
    }

    fn resolve_call<'ctx>(
        &mut self,
        context: &FuncContext<'ctx>,
        call: &Call,
        expression: Option<ExpressionId>,
    ) -> Result<(), ResolverError> {
        // Resolve the argument expressions
        for arg in call.args.iter() {
            self.resolve_expression(context, *arg)?;
        }

        let item = self.lookup_name(context, &call.ident)?;
        self.bindings.insert(call.name_id, item);

        match item {
            ItemId::Import(import) => {
                let import = context.parent.component.imports.get(import).unwrap();
                match &import.external_type {
                    ast::ExternalType::Function(fn_type) => {
                        if call.args.len() != fn_type.arguments.len() {
                            return Err(ResolverError::CallArgumentsMismatch {
                                src: context.parent.src.clone(),
                                span: call.ident.span.clone(),
                                ident: call.ident.as_ref().to_owned(),
                            });
                        }

                        for (arg_expr, (arg_name, arg_type)) in
                            call.args.iter().zip(fn_type.arguments.iter())
                        {
                            let _ = arg_name;
                            self.set_implied_type(*arg_expr, arg_type.value.clone());
                        }

                        if let Some(expression) = expression {
                            self.set_implied_type(expression, fn_type.return_type.as_ref().clone());
                        }
                    }
                }
            }
            ItemId::Function(function) => {
                let function = context.parent.component.functions.get(function).unwrap();
                let fn_type = &function.signature.fn_type;

                for (arg_expr, (arg_name, arg_type)) in
                    call.args.iter().zip(fn_type.arguments.iter())
                {
                    let _ = arg_name;
                    self.set_implied_type(*arg_expr, arg_type.value.clone());
                }

                if let Some(expression) = expression {
                    self.set_implied_type(expression, fn_type.return_type.as_ref().clone());
                }
            }
            ItemId::Global(_) => todo!(),
            ItemId::Param(_) => todo!(),
            ItemId::Local(_) => todo!(),
        }

        return Ok(());
    }

    fn lookup_name<'ctx>(
        &self,
        context: &FuncContext<'ctx>,
        ident: &M<String>,
    ) -> Result<ItemId, ResolverError> {
        match self.mapping.lookup(ident.as_ref()) {
            Some(item) => Ok(*item),
            None => Err(ResolverError::NameError {
                src: context.parent.src.clone(),
                span: ident.span.clone(),
                ident: ident.as_ref().to_owned(),
            }),
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

    fn set_implied_type(&mut self, id: ExpressionId, valtype: ValType) {
        self.expression_types.insert(id, valtype);
    }

    fn link_expressions(&mut self, a: ExpressionId, b: ExpressionId) {
        let a = TypedItem::Expression(a);
        let b = TypedItem::Expression(b);
        self.type_constraints.add_edge(a, b, ());
    }

    fn resolve_types(
        &mut self,
        context: &FuncContext<'_>,
    ) -> Result<(), ResolverError> {
        let mut errors = Vec::new();
        loop {
            let mut updated = false;

            for (id, _expression) in context.func.expressions.expressions().iter() {
                let current = TypedItem::Expression(id);
                let mut current_type = self.expression_types.get(&id).cloned();
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
                let mut current_type = self.local_types.get(&id).cloned();
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

    fn get_item_type(&self, context: &FuncContext<'_>, item: TypedItem) -> Option<ValType> {
        match item {
            TypedItem::Global(global) => Some(
                context
                    .parent
                    .component
                    .globals
                    .get(global)
                    .unwrap()
                    .valtype
                    .as_ref()
                    .clone(),
            ),
            TypedItem::Param(param) => Some(
                context
                    .func
                    .signature
                    .fn_type
                    .arguments
                    .get(param.index())
                    .unwrap()
                    .1
                    .as_ref()
                    .clone(),
            ),
            TypedItem::Local(local) => self.local_types.get(&local).cloned(),
            TypedItem::Expression(expression) => self.expression_types.get(&expression).cloned(),
        }
    }
}
