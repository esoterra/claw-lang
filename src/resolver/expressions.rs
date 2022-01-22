use crate::ast::{
    Place,
    expressions::{BinaryOp, Expression}
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
                    if let Some(FunctionItem::Global { index, .. }) = id {
                        ir::Instruction::GlobalGet { index }
                    } else { todo!("Implement dereferencing names other than globals") }
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
            f_builder.type_graph.constrain_equal(node, left_node);
            f_builder.type_graph.constrain_equal(node, right_node);
            assert_eq!(operator.value, BinaryOp::Add);
            ir::Instruction::Add {
                node,
                left, right
            }
        },
        _ => panic!("Unsupported expression type")
    };
    Ok((node, Box::new(instr)))
}