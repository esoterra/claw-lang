use std::rc::Rc;

use crate::ast::Place;
use crate::ast::expressions::{BinaryOp, Literal};
use crate::ast::{
    M,
    types::{ValType, BasicVal},
    expressions::Expression
};
use crate::ir::{self, Instruction};
use crate::resolver::Context;

use super::{ResolverError, ItemID};

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct TypeContext {
    return_type: M<ValType>,
    result_type: M<ValType>
}

impl TypeContext {
    pub fn new(
        return_type: M<ValType>,
        result_type: M<ValType>
    ) -> Self {
        TypeContext { return_type, result_type }
    }
}


pub fn resolve_expression<'r, 'ast, 'ops>(
    context: Rc<Context>,
    type_context: TypeContext,
    module: &'r ir::Module,
    ast: &'ast Expression,
) -> Result<ir::Instruction, ResolverError> {

    match ast {
        Expression::Literal { value } => {
            match (&value.value, type_context.result_type.value) {
                (Literal::Integer(num), ValType::Basic(BasicVal::U32)) => {
                    let constant = ir::Constant::I32 { value: *num as i32 };
                    Ok(Instruction::Constant {
                        value: constant,
                        result_type: BasicVal::I32
                    })
                },
                _ => panic!("Unsupported literal value")
            }
        },
        Expression::Place { place } => {
            match &place.value {
                Place::Identifier { ident } => {
                    let id = context.lookup(&ident.value);
                    if let Some(ItemID::Global(index)) = id {
                        Ok(Instruction::GlobalGet { index })
                    } else { panic!("Dereferencing names other than globals not supported") }
                },
                // _ => panic!("Dereferencing place expressions other than identifiers not supported")
            }
        },
        Expression::Binary {
            left,
            operator,
            right
        } => {
            let left = resolve_expression(context.clone(), type_context.clone(), module, &left.value)?;
            let right = resolve_expression(context.clone(), type_context.clone(), module, &right.value)?;
            assert_eq!(operator.value, BinaryOp::Add);
            if let ValType::Basic(basic_type) = type_context.result_type.value {
                Ok(Instruction::Add {
                    result_type: basic_type,
                    left: Box::new(left),
                    right: Box::new(right)
                })
            } else { panic!("Addition only supported for basic types") }
        },
        _ => panic!("Unsupported expression type")
    }
}