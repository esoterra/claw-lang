use std::{collections::HashMap, sync::Arc};

use miette::NamedSource;

use crate::ast::{
    M, MBox, Place,
    module::{Function, FunctionSignature},
    statements::Statement,
    expressions::Expression, types::ValType
};
use crate::resolver::{
    ResolverError, ModuleContext, ModuleItem,
    expressions::resolve_expression
};
use crate::ir;
use crate::ir::type_graph::{TypeGraph, TypeNode};

pub struct FunctionBuilder {
    pub src: Arc<NamedSource>,
    pub context: FunctionContext,
    pub return_type: TypeNode,
    pub locals: Vec<TypeNode>,
    pub type_graph: TypeGraph
}

impl FunctionBuilder {
    fn new(
        src: Arc<NamedSource>,
        module_context: &ModuleContext,
        module: &ir::Module,
        signature: &FunctionSignature
    ) -> Self {
        let mut type_graph = TypeGraph::new();

        let mut context = FunctionContext::from_module(&mut type_graph, &module, module_context);

        let mut locals = Vec::new();

        for (ident, valtype) in signature.arguments.iter() {
            let node = type_graph.add_declared_type(valtype.clone());
            let item = FunctionItem::Param {
                node,
                index: locals.len()
            };
            context.bind(ident.value.clone(), item);
            locals.push(node);
        }

        let return_node = type_graph.add_declared_type(signature.return_type.clone());

        FunctionBuilder {
            src,
            context,
            return_type: return_node,
            locals,
            type_graph
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionContext {
    mapping: HashMap<String, FunctionItem>,
    history: Vec<(String, Option<FunctionItem>)>
}

#[derive(Debug, Clone, Copy)]
pub enum FunctionItem {
    Function {
        index: usize
    },
    Global {
        node: TypeNode,
        mutable: bool,
        index: usize,
    },
    Param {
        node: TypeNode,
        index: usize
    },
    Local {
        node: TypeNode,
        mutable: bool,
        index: usize
    }
}

impl FunctionContext {
    pub fn from_module(type_graph: &mut TypeGraph, module: &ir::Module, module_context: &ModuleContext) -> Self {
        let mut mapping = HashMap::new();

        // Translate module context item into function context
        for (ident, item) in module_context.mappings.iter() {
            let item = match item {
                ModuleItem::Global(index) => {
                    let global = module.get_global(*index).unwrap();
                    let global_type = global.type_.clone();
                    let mutable = global.mutable;
                    FunctionItem::Global {
                        node: type_graph.add_declared_type(global_type),
                        mutable,
                        index: *index
                    }
                },
                ModuleItem::Function(index) => {
                    FunctionItem::Function {
                        index: *index
                    }
                },
                _ => todo!()
            };
            mapping.insert(ident.clone(), item);
        }

        FunctionContext {
            mapping,
            history: Vec::new()
        }
    }

    fn bind(&mut self, name: String, item: FunctionItem) {
        self.mapping.insert(name, item);
    }

    pub fn push(&mut self, name: String, item: FunctionItem) {
        let previous = self.mapping.get(&name).map(|f| *f);
        self.history.push((name.clone(), previous));
        self.mapping.insert(name, item);
    }

    pub fn pop(&mut self) {
        if let Some((id, value)) = self.history.pop() {
            if let Some(value) = value {
                self.mapping.insert(id, value);
            } else {
                self.mapping.remove(&id);
            }
        } else {
            panic!("pop called more than push");
        }
    }

    pub fn lookup(&self, name: &String) -> Option<FunctionItem> {
        self.mapping.get(name).map(|f| *f)
    }
}

pub fn resolve_function<'r, 'ast>(
    src: Arc<NamedSource>,
    module_context: &'r ModuleContext,
    module: &'r mut ir::Module,
    fn_index: usize,
    ast: &'ast Function
) -> Result<(), ResolverError> {
    if let Some(root) = &ast.body.root_statement {
        // Initialize FunctionBuilder
        let signature = &module.functions[fn_index].signature;
        let mut f_builder = FunctionBuilder::new(src, module_context, module, signature);
        let mut instructions = Vec::new();
        // Resolve root statement
        resolve_statement(&mut f_builder, &mut instructions, &root.value)?;
        // Update IR
        let function = &mut module.functions[fn_index];
        f_builder.type_graph.resolve_all();
        function.type_graph = ir::NeedsResolve::Resolved(f_builder.type_graph);
        function.locals = ir::NeedsResolve::Resolved(f_builder.locals);
        function.body = ir::NeedsResolve::Resolved(instructions);
    }

    Ok(())
}

fn resolve_statement<'fb, 'inst, 'ast>(
    f_builder: &'fb mut FunctionBuilder,
    instructions: &'inst mut Vec<ir::Instruction>,
    statement: &'ast Statement
) -> Result<(), ResolverError> {
    match &statement {
        Statement::Let {
            let_kwd: _,
            mut_kwd,
            ident,
            annotation,
            assign_op: _,
            expression,
            next
        } => {
            let mutable = mut_kwd.is_some();
            let ident = ident.value.clone();
            let annotation = annotation.clone();
            let expression = &expression.value;
            resolve_let(f_builder, instructions, mutable, ident,
                annotation, expression, next
            )
        },
        Statement::Assign {
            place,
            assign_op,
            expression,
            next
        } => {
            let _ = assign_op;
            resolve_assign(f_builder, instructions, place, expression)?;
            if let Some(next_statement) = next {
                resolve_statement(f_builder, instructions, &next_statement.value)
            } else { Ok(()) }
        },
        Statement::Return {
            return_kwd: _,
            expression
        } => {
            resolve_return(f_builder, instructions, expression)
        }
    }
}

fn resolve_let<'fb, 'inst, 'ast>(
    f_builder: &'fb mut FunctionBuilder,
    instructions: &'inst mut Vec<ir::Instruction>,
    mutable: bool,
    ident: String,
    annotation: Option<M<ValType>>,
    expression: &'ast Expression,
    next: &'ast Option<MBox<Statement>>
) -> Result<(), ResolverError> {
    // Create a TypeNode for this local variable
    let node = match annotation {
        Some(valtype) =>
            f_builder.type_graph.add_declared_type(valtype.clone()),
        None =>
            f_builder.type_graph.add_inferred_type()
    };
    // Add the variable to the locals table
    let index = f_builder.locals.len();
    f_builder.locals.push(node);
    // Resolve expression
    let (value_node, value) = resolve_expression(f_builder, expression)?;
    f_builder.type_graph.constrain_equal(node, value_node);
    // Generate set
    instructions.push(ir::Instruction::LocalSet {
        index,
        value
    });
    // Associate this local with its identifier
    let item = FunctionItem::Local { node, mutable, index };
    println!("push {} = {:?}", ident, item);
    f_builder.context.push(ident.clone(), item);
    // Resolve any child statements
    if let Some(next_statement) = next {
        resolve_statement(f_builder, instructions, &next_statement.value)?;
    }
    // Unbind identifier
    println!("pop {}", ident);
    f_builder.context.pop();
    Ok(())
}

fn resolve_assign<'fb, 'instrs, 'ast>(
    f_builder: &'fb mut FunctionBuilder,
    instructions: &'instrs mut Vec<ir::Instruction>,
    place: &'ast M<Place>,
    expression: &'ast MBox<Expression>
) -> Result<(), ResolverError> {
    let name = match &place.value {
        Place::Identifier { ident } => ident.value.clone(),
        _ => panic!("Only identifiers supported as assignment targets")
    };

    let (global_node, mutable, global_index) = match f_builder.context.lookup(&name) {
        Some(FunctionItem::Global { node, mutable, index }) => {
            (node, mutable, index)
        }
        _ => panic!("No global found with matching name")
    };
    assert_eq!(mutable, true);

    let (expression_node, value) = resolve_expression(f_builder, &expression.value)?;
    f_builder.type_graph.constrain_equal(global_node, expression_node);
    instructions.push(ir::Instruction::GlobalSet { index: global_index, value });

    Ok(())
}

fn resolve_return<'fb, 'instrs, 'ast>(
    f_builder: &'fb mut FunctionBuilder,
    instructions: &'instrs mut Vec<ir::Instruction>,
    expression: &'ast MBox<Expression>,
) -> Result<(), ResolverError> {
    let (value_node, value) = resolve_expression(f_builder, &expression.value)?;
    f_builder.type_graph.constrain_equal(f_builder.return_type, value_node);
    instructions.push(ir::Instruction::Return { value });
    Ok(())
}