use std::collections::HashMap;

use crate::ast::{
    M, MBox, Place,
    module::{Function, FunctionSignature},
    statements::Statement,
    expressions::Expression
};
use crate::resolver::{
    ResolverError, ModuleContext, ModuleItem,
    expressions::resolve_expression
};
use crate::ir;
use crate::ir::type_graph::{TypeGraph, TypeNode};

pub struct FunctionBuilder {
    pub context: FunctionContext,
    pub return_type: TypeNode,
    params: Vec<TypeNode>,
    locals: Vec<TypeNode>,
    pub type_graph: TypeGraph
}

impl FunctionBuilder {
    fn new(
        module_context: &ModuleContext,
        module: &ir::Module, signature:
        &FunctionSignature
    ) -> Self {
        let mut type_graph = TypeGraph::new();

        let mut context = FunctionContext::from_module(&mut type_graph, &module, module_context);

        let mut params = Vec::new();

        for (ident, valtype) in signature.arguments.iter() {
            let node = type_graph.add_declared_type(valtype.clone());
            let item = FunctionItem::Param {
                node,
                index: params.len()
            };
            context.bind(ident.value.clone(), item);
            params.push(node);
        }

        let return_node = type_graph.add_declared_type(signature.return_type.clone());

        FunctionBuilder {
            context,
            return_type: return_node,
            params: Vec::new(),
            locals: Vec::new(),
            type_graph
        }
    }
}

pub struct FunctionContext {
    mapping: HashMap<String, Vec<FunctionItem>>,
    pop_order: Vec<String>
}

#[derive(Clone, Copy)]
pub enum FunctionItem {
    Function {
        index: usize
    },
    Global {
        node: TypeNode,
        index: usize,
    },
    Param {
        node: TypeNode,
        index: usize
    },
    Local {
        node: TypeNode,
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
                    let global_type = module.get_global(*index).unwrap().type_.clone();
                    FunctionItem::Global {
                        node: type_graph.add_declared_type(global_type),
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
            mapping.insert(ident.clone(), vec![item]);
        }

        FunctionContext {
            mapping,
            pop_order: Vec::new()
        }
    }

    fn bind(&mut self, name: String, item: FunctionItem) {
        if let Some(entries) = self.mapping.get_mut(&name) {
            entries.push(item)
        } else {
            self.mapping.insert(name, vec![item]);
        }
    }

    pub fn push(&mut self, name: String, item: FunctionItem) {
        self.pop_order.push(name.clone());
        if let Some(entries) = self.mapping.get_mut(&name) {
            entries.push(item)
        } else {
            self.mapping.insert(name, vec![item]);
        }
    }

    pub fn pop(&mut self) {
        let to_pop = self.pop_order.pop().expect("pop called more than push");
        self.mapping.get_mut(&to_pop).unwrap().pop();
    }

    pub fn lookup(&self, name: &String) -> Option<FunctionItem> {
        self.mapping.get(name).map(|stack| *stack.last().unwrap())
    }
}

pub fn resolve_function<'r, 'ast>(
    module_context: &'r ModuleContext,
    module: &'r mut ir::Module,
    fn_index: usize,
    ast: &'ast Function
) -> Result<(), ResolverError> {
    if let Some(root) = &ast.body.root_statement {
        // Initialize FunctionBuilder
        let signature = &module.functions[fn_index].signature;
        let mut f_builder = FunctionBuilder::new(module_context, module, signature);
        let mut instructions = Vec::new();
        // Resolve root statement
        resolve_statement(&mut f_builder, &mut instructions, &root.value)?;
        // Update IR
        let function = &mut module.functions[fn_index];
        f_builder.type_graph.resolve_all();
        function.type_graph = ir::NeedsResolve::Resolved(f_builder.type_graph);
        function.body = ir::NeedsResolve::Resolved(instructions);
    }

    Ok(())
}

fn resolve_statement<'fb, 'instrs, 'ast>(
    f_builder: &'fb mut FunctionBuilder,
    instructions: &'instrs mut Vec<ir::Instruction>,
    statement: &'ast Statement
) -> Result<(), ResolverError> {
    match &statement {
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
            return_kwd,
            expression
        } => {
            let _ = return_kwd;
            resolve_return(f_builder, instructions, expression)
        }
    }
}

fn resolve_assign<'fb, 'instrs, 'ast>(
    f_builder: &'fb mut FunctionBuilder,
    instructions: &'instrs mut Vec<ir::Instruction>,
    place: &'ast M<Place>,
    expression: &'ast MBox<Expression>
) -> Result<(), ResolverError> {
    let name = match &place.value {
        Place::Identifier { ident } => ident.value.clone(),
        // _ => panic!("Only identifiers supported as assignment targets")
    };

    let (global_node, global_index) = match f_builder.context.lookup(&name) {
        Some(FunctionItem::Global { node, index }) => {
            (node, index)
        }
        _ => panic!("No global found with matching name")
    };
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