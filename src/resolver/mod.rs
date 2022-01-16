mod expressions;

use std::{
    collections::HashMap,
    rc::Rc,
    sync::Arc
};
use crate::ast::{
    Span, M, MBox, Place,
    module::{
        Module, Item, Global, Function
    },
    statements::Statement,
    expressions::Expression, types::ValType, 
};
use crate::ir::{self, NeedsResolve};
use self::expressions::{resolve_expression, TypeContext};

use miette::{Diagnostic, NamedSource};
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
#[error("Failed to resolve")]
#[diagnostic()]
pub enum ResolverError {
    Base {
        #[source_code]
        src: Arc<NamedSource>,
        #[label("This bit")]
        span: Span
    },
    NotYetSupported
}

pub fn resolve(ast: Module) -> Result<ir::Module, ResolverError> {
    let mut root = Context::root();
    let mut module = ir::Module::new();

    let mut item_ids = Vec::new();

    // Scan each item
    // 1. add them to the ir::Module
    // 2. add them to the root context
    for item in ast.items.iter() {
        match &item {
            Item::Global(global) => {
                let id = ItemID::Global(module.globals.len());
                let ir_entry = scan_global(global);
                root.bind(ir_entry.ident.value, id);
                item_ids.push(id);
            },
            Item::Function(function) => {
                let id = ItemID::Function(module.functions.len());
                let ir_entry = scan_function(function);
                root.bind(ir_entry.signature.name.value, id);
                item_ids.push(id);
            },
            _ => return Err(ResolverError::NotYetSupported)
        }
    }

    // Resolve each item
    let parent = Rc::new(root);
    for (item, id) in ast.items.iter().zip(item_ids.iter()) {
        match &id {
            ItemID::Global(index) => {
                let ir_entry = module.get_global_mut(*index).unwrap();
                if let Item::Global(ast) = item {
                    resolve_global(parent.clone(), &mut module, ast);
                } else { unreachable!() }
            },
            ItemID::Function(index) => {
                let ir_entry = module.get_function_mut(*index).unwrap();
                if let Item::Function(ast) = item {
                    resolve_function(parent.clone(), &mut module, *index, ast);
                } else { unreachable!() }
            }
        }
    }

    Ok(module)
}



fn scan_global(_global: &Global) -> ir::Global {
    unreachable!() // TODO
}

fn scan_function(_function: &Function) -> ir::Function {
    unreachable!() // TODO
}

fn resolve_global<'r, 'ast>(
    context: Rc<Context>,
    module: &'r mut ir::Module,
    ast: &'ast Global
) {
    unreachable!() // TODO
}

fn resolve_function<'r, 'ast>(
    context: Rc<Context>,
    module: &'r mut ir::Module,
    fn_index: usize,
    ast: &'ast Function
) -> Result<(), ResolverError> {
    let return_type = module.functions[fn_index].signature.return_type.clone();
    if let Some(root) = &ast.body.root_statement {
        let mut ops = Vec::new();
        resolve_statement(context, return_type, module, &root.value, &mut ops)?;
        let function = &mut module.functions[fn_index];
        function.body = NeedsResolve::Resolved(ops);
    }

    Ok(())
}

fn resolve_statement<'r, 'ast, 'ops>(
    context: Rc<Context>,
    return_type: M<ValType>,
    module: &'r ir::Module,
    statement: &'ast Statement,
    ops: &'ops mut Vec<ir::Operation>
) -> Result<Vec<ir::Operation>, ResolverError> {
    match &statement {
        Statement::Assign {
            place,
            assign_op,
            expression,
            next
        } => {
            resolve_assign(context, return_type, module, place, expression, ops);
        },
        Statement::Return {
            return_kwd,
            expression
        } => {
            resolve_return(context, return_type, module, expression, ops);
        }
    }

    unreachable!()
}

fn resolve_assign<'r, 'ast, 'ops>(
    context: Rc<Context>,
    return_type: M<ValType>,
    module: &'r ir::Module,
    place: &M<Place>,
    expression: &'ast MBox<Expression>,
    ops: &'ops mut Vec<ir::Operation>
) -> Result<(), ResolverError> {
    let name = if let Place::Identifier { ident } = &place.value {
        ident.value.clone()
    } else { panic!("Only identifiers supported as assignment targets") };

    let global_index = match context.lookup(&name) {
        Some(ItemID::Global(index)) => index,
        _ => panic!("No global found with matching name")
    };
    let result_type = module.get_global(global_index).unwrap().type_.clone();

    let type_context = TypeContext::new(return_type, result_type);
    resolve_expression(context, type_context, module, &expression.value, ops)?;
    ops.push(ir::Operation::GlobalSet { index: global_index });

    Ok(())
}

fn resolve_return<'r, 'ast, 'ops>(
    context: Rc<Context>,
    return_type: M<ValType>,
    module: &'r ir::Module,
    expression: &'ast MBox<Expression>,
    ops: &'ops mut Vec<ir::Operation>
) -> Result<(), ResolverError> {
    let type_context = TypeContext::new(return_type.clone(), return_type);
    resolve_expression(context, type_context, module, &expression.value, ops)?;
    ops.push(ir::Operation::Return);
    Ok(())
}

#[derive(Clone, Copy, Debug)]
pub enum ItemID {
    Global(usize),
    Function(usize)
}

#[derive(Clone, Debug)]
pub struct Context {
    parent: Option<Rc<Context>>,
    mappings: HashMap<String, ItemID>
}

impl Context {
    pub fn root() -> Self {
        Context {
            parent: None,
            mappings: HashMap::new()
        }
    }

    pub fn new_child(parent: Rc<Context>) -> Self {
        Context {
            parent: Some(parent),
            mappings: HashMap::new()
        }
    }

    pub fn bind(&mut self, key: String, id: ItemID) {
        self.mappings.insert(key, id);
    }

    pub fn lookup(&self, key: &String) -> Option<ItemID> {
        if let Some(id) = self.mappings.get(key) {
            return Some(*id);
        }
        if let Some(parent) = &self.parent {
            return parent.lookup(key);
        }
        return None;
    }
}