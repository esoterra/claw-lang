use crate::ast::{
    expressions::{BinaryOp, Expression, ExpressionData, ExpressionId},
    types::ValType
};
use crate::resolver::{
    ResolverError,
    functions::{FunctionItem, FunctionBuilder}
};
use crate::ir;
use crate::ir::type_graph::TypeNode;

pub fn resolve_expression<'fb, 'ast>(
    f_builder: &'fb mut FunctionBuilder,
    data: &ExpressionData,
    id: ExpressionId,
) -> Result<(TypeNode, Box<ir::Instruction>), ResolverError> {
    let node = f_builder.type_graph.add_inferred_type(data.get_span(id));

    let instr = match data.get_exp(id) {
        Expression::Literal { value } => {
            ir::Instruction::Constant {
                node,
                value: value.value.clone()
            }
        },
        Expression::Identifier { ident } => {
            let id = f_builder.context.lookup(ident.as_ref());
            match id {
                Some(FunctionItem::Global { index, .. }) => ir::Instruction::GlobalGet { index },
                Some(FunctionItem::Param { index, .. }) => {
                    f_builder.type_graph.constrain_equal(node, f_builder.locals[index]);
                    ir::Instruction::LocalGet { node, index }
                },
                Some(FunctionItem::Local { index, .. }) => {
                    f_builder.type_graph.constrain_equal(node, f_builder.locals[index]);
                    ir::Instruction::LocalGet { node, index }
                },
                None => {
                    return Err(ResolverError::NameError {
                        src: f_builder.src.clone(),
                        span: ident.span.clone(),
                        ident: ident.value.clone()
                    })
                },
                other => panic!("Unsupported item dereferenced: {:?}", other)
            }
        },
        Expression::Binary {
            left,
            operator,
            right
        } => {
            let (left_node, left) = resolve_expression(f_builder, data, left.to_owned())?;
            let (right_node, right) = resolve_expression(f_builder, data, right.to_owned())?;
            match operator.value {
                BinaryOp::Add => {
                    let op = ir::BinArithOp::Add;
                    f_builder.type_graph.constrain_equal(node, left_node);
                    f_builder.type_graph.constrain_equal(node, right_node);
                    ir::Instruction::BinaryArith { node, op, left, right }
                },
                BinaryOp::Sub => {
                    let op = ir::BinArithOp::Sub;
                    f_builder.type_graph.constrain_equal(node, left_node);
                    f_builder.type_graph.constrain_equal(node, right_node);
                    ir::Instruction::BinaryArith { node, op, left, right }
                },
                BinaryOp::Mult => {
                    let op = ir::BinArithOp::Mul;
                    f_builder.type_graph.constrain_equal(node, left_node);
                    f_builder.type_graph.constrain_equal(node, right_node);
                    ir::Instruction::BinaryArith { node, op, left, right }
                },
                BinaryOp::EQ => {
                    let op = ir::BinRelOp::EQ;
                    f_builder.type_graph.constrain_type(node, ValType::Bool);
                    f_builder.type_graph.constrain_equal(left_node, right_node);
                    ir::Instruction::BinaryRel { node, left, op, right }
                },
                BinaryOp::LT => {
                    let op = ir::BinRelOp::LT;
                    f_builder.type_graph.constrain_type(node, ValType::Bool);
                    f_builder.type_graph.constrain_equal(left_node, right_node);
                    ir::Instruction::BinaryRel { node, op, left, right }
                },
                _ => panic!("Unsupported binary operator {:?}", operator.as_ref())
            }
        },
        _ => panic!("Unsupported expression type")
    };
    Ok((node, Box::new(instr)))
}