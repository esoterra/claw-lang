use std::rc::Rc;

use crate::ast::Place;
use crate::ast::expressions::{BinaryOp, Literal};
use crate::ast::{
    M,
    types::{ValType, BasicVal},
    expressions::Expression
};
use crate::ir::{self, Operation};
use crate::resolver::Context;

use super::{ResolverError, ItemID};


#[derive(Clone)]
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
    ops: &'ops mut Vec<ir::Operation>
) -> Result<(), ResolverError> {

    match ast {
        Expression::Literal { value } => {
            match (&value.value, type_context.result_type.value) {
                (Literal::Integer(num), ValType::Basic(BasicVal::I32)) => {
                    ops.push(Operation::Constant { value: ir::Constant::I32 { value: *num as i32 } });
                }
                _ => panic!("Unsupported literal value")
            }
        },
        Expression::Place { place } => {
            if let Place::Identifier { ident } = &place.value {
                let id = context.lookup(&ident.value);
                if let Some(ItemID::Global(index)) = id {
                    ops.push(Operation::GlobalGet { index });
                } else { panic!("Dereferencing names other than globals not supported") }
            } else { panic!("Dereferencing place expressions other than identifiers not supported") }
        },
        Expression::Binary {
            left,
            operator,
            right
        } => {
            resolve_expression(context.clone(), type_context.clone(), module, &left.value, ops)?;
            resolve_expression(context.clone(), type_context.clone(), module, &right.value, ops)?;
            assert_eq!(operator.value, BinaryOp::Add);
            if let ValType::Basic(basic_type) = type_context.result_type.value {
                ops.push(Operation::Add { result_type: basic_type });
            } else { panic!("Addition only supported for basic types") }
        },
        _ => panic!("Unsupported expression type")
    }

    Ok(())
}