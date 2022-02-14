use crate::ast::{
    Place,
    expressions::{BinaryOp, Expression},
    types::{ValType, BasicVal}
};
use crate::resolver::{
    ResolverError,
    functions::{FunctionItem, FunctionBuilder}
};
use crate::ir;
use crate::ir::type_graph::TypeNode;

pub fn resolve_expression<'fb, 'ast>(
    f_builder: &'fb mut FunctionBuilder,
    ast: &'ast Expression,
) -> Result<(TypeNode, Box<ir::Instruction>), ResolverError> {
    let node = f_builder.type_graph.add_inferred_type();

    let instr = match ast {
        Expression::Literal { value } => {
            ir::Instruction::Constant {
                node,
                value: value.value.clone()
            }
        },
        Expression::Place { place } => {
            match &place.value {
                Place::Identifier { ident } => {
                    let id = f_builder.context.lookup(&ident.value);
                    match id {
                        Some(FunctionItem::Global { index, .. }) => ir::Instruction::GlobalGet { index },
                        Some(FunctionItem::Param { index, .. }) => ir::Instruction::LocalGet { index },
                        Some(FunctionItem::Local { index, .. }) => ir::Instruction::LocalGet { index },
                        None => {
                            return Err(ResolverError::NameError {
                                src: f_builder.src.clone(),
                                span: place.span.clone(),
                                ident: ident.value.clone()
                            })
                        },
                        other => panic!("Unsupported item dereferenced: {:?}", other)
                    }
                },
                // _ => panic!("Dereferencing place expressions other than identifiers not supported")
            }
        },
        Expression::Binary {
            left,
            operator,
            right
        } => {
            let (left_node, left) = resolve_expression(f_builder, &left.value)?;
            let (right_node, right) = resolve_expression(f_builder, &right.value)?;
            match operator.value {
                BinaryOp::Add => {
                    f_builder.type_graph.constrain_equal(node, left_node);
                    f_builder.type_graph.constrain_equal(node, right_node);
                    ir::Instruction::Add { node, left, right }
                },
                BinaryOp::Sub => {
                    f_builder.type_graph.constrain_equal(node, left_node);
                    f_builder.type_graph.constrain_equal(node, right_node);
                    ir::Instruction::Subtract { node, left, right }
                },
                BinaryOp::Mult => {
                    f_builder.type_graph.constrain_equal(node, left_node);
                    f_builder.type_graph.constrain_equal(node, right_node);
                    ir::Instruction::Multiply { node, left, right }
                },
                BinaryOp::EQ => {
                    f_builder.type_graph.constrain_type(node, ValType::Basic(BasicVal::I32));
                    f_builder.type_graph.constrain_equal(left_node, right_node);
                    ir::Instruction::Subtract { node, left, right }
                },
                _ => panic!("Unsupported binary operator {:?}", operator.value)
            }
        },
        _ => panic!("Unsupported expression type")
    };
    Ok((node, Box::new(instr)))
}