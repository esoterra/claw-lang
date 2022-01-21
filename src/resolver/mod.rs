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
    expressions::Expression,
    types::ValType,
};
use crate::ir;
use crate::resolver::expressions::{resolve_expression, TypeContext};

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
        let (item_info, id) = match &item {
            Item::Global(global) => {
                let id = ItemID::Global(module.globals.len());
                let (ir_entry, item_info) = scan_global(global)?;
                module.globals.push(ir_entry);
                (item_info, id)
            },
            Item::Function(function) => {
                let id = ItemID::Function(module.functions.len());
                let (ir_entry, item_info) = scan_function(function)?;
                module.functions.push(ir_entry);
                (item_info, id)
            },
            _ => return Err(ResolverError::NotYetSupported)
        };
        item_ids.push(id);
        root.bind(item_info.identifier.value.clone(), id);
        if item_info.is_exported {
            module.exports.push(ir::Export {
                ident: item_info.identifier,
                id
            })
        }
    }

    // Resolve each item
    let parent = Rc::new(root);
    for (item, id) in ast.items.iter().zip(item_ids.iter()) {
        match &id {
            ItemID::Global(index) => {
                if let Item::Global(ast) = item {
                    resolve_global(parent.clone(), &mut module, *index, ast)?;
                } else { unreachable!() }
            },
            ItemID::Function(index) => {
                if let Item::Function(ast) = item {
                    resolve_function(parent.clone(), &mut module, *index, ast)?;
                } else { unreachable!() }
            }
        }
    }

    Ok(module)
}

struct ItemInfo {
    is_exported: bool,
    identifier: M<String>,
}

fn scan_global(global: &Global) -> Result<(ir::Global, ItemInfo), ResolverError> {
    let Global {
        ident,
        valtype,
        mut_kwd,
        ..
    } = global;

    let item_info = ItemInfo {
        is_exported: false,
        identifier: ident.clone()
    };
    let ir_entry = ir::Global {
        ident: ident.clone(),
        type_: valtype.clone(),
        mutable: mut_kwd.is_some(),
        initial_value: ir::NeedsResolve::Unresolved
    };
    Ok((ir_entry, item_info))
}

fn scan_function(function: &Function) -> Result<(ir::Function, ItemInfo), ResolverError> {
    let Function {
        export_kwd,
        signature, ..
    } = function;

    let item_info = ItemInfo {
        is_exported: export_kwd.is_some(),
        identifier: signature.name.clone()
    };
    let ir_entry = ir::Function {
        signature: signature.clone(),
        body: ir::NeedsResolve::Unresolved
    };
    Ok((ir_entry, item_info))
}

fn resolve_global<'r, 'ast>(
    context: Rc<Context>,
    module: &'r mut ir::Module,
    global_index: usize,
    ast: &'ast Global
) -> Result<(), ResolverError> {
    let _ = context;
    let global_type = module.globals[global_index].type_.clone();
    let type_context = TypeContext::new(global_type.clone(), global_type.clone());
    let initial_value = resolve_expression(
        context.clone(),type_context , module, &ast.init_value.value
    )?;

    let constant = match initial_value {
        ir::Instruction::Constant { value } => value,
        _ => panic!("Unsupported initial value expression type")
    };

    module.globals[global_index].initial_value = ir::NeedsResolve::Resolved(constant);

    Ok(())
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
        function.body = ir::NeedsResolve::Resolved(ops);
    }

    Ok(())
}

fn resolve_statement<'r, 'ast, 'ops>(
    context: Rc<Context>,
    return_type: M<ValType>,
    module: &'r ir::Module,
    statement: &'ast Statement,
    ops: &'ops mut Vec<ir::Instruction>
) -> Result<(), ResolverError> {
    match &statement {
        Statement::Assign {
            place,
            assign_op,
            expression,
            next
        } => {
            let _ = assign_op;
            resolve_assign(context.clone(), return_type.clone(), module, place, expression, ops)?;
            if let Some(next_statement) = next {
                resolve_statement(context, return_type, module, &next_statement.value, ops)
            } else { Ok(()) }
        },
        Statement::Return {
            return_kwd,
            expression
        } => {
            let _ = return_kwd;
            resolve_return(context, return_type, module, expression, ops)
        }
    }
}

fn resolve_assign<'r, 'ast, 'ops>(
    context: Rc<Context>,
    return_type: M<ValType>,
    module: &'r ir::Module,
    place: &M<Place>,
    expression: &'ast MBox<Expression>,
    ops: &'ops mut Vec<ir::Instruction>
) -> Result<(), ResolverError> {
    let name = match &place.value {
        Place::Identifier { ident } => ident.value.clone(),
        // _ => panic!("Only identifiers supported as assignment targets")
    };

    let global_index = match context.lookup(&name) {
        Some(ItemID::Global(index)) => index,
        _ => panic!("No global found with matching name")
    };
    let result_type = module.get_global(global_index).unwrap().type_.clone();

    let type_context = TypeContext::new(return_type, result_type);
    let value = resolve_expression(context, type_context, module, &expression.value)?;
    ops.push(ir::Instruction::GlobalSet { index: global_index, value: Box::new(value) });

    Ok(())
}

fn resolve_return<'r, 'ast, 'ops>(
    context: Rc<Context>,
    return_type: M<ValType>,
    module: &'r ir::Module,
    expression: &'ast MBox<Expression>,
    ops: &'ops mut Vec<ir::Instruction>
) -> Result<(), ResolverError> {
    let type_context = TypeContext::new(return_type.clone(), return_type);
    let value = resolve_expression(context, type_context, module, &expression.value)?;
    ops.push(ir::Instruction::Return { value: Box::new(value) });
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