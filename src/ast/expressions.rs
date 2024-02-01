use std::collections::HashMap;

use super::{merge, NameId, Span, M};
use cranelift_entity::{entity_impl, PrimaryMap};

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ExpressionId(u32);
entity_impl!(ExpressionId, "expression");

#[derive(Clone, Debug, Default)]
pub struct ExpressionData {
    expressions: PrimaryMap<ExpressionId, Expression>,
    expression_spans: HashMap<ExpressionId, Span>,
}

impl ExpressionData {
    pub fn alloc(&mut self, expression: Expression, span: Span) -> ExpressionId {
        let id = self.expressions.push(expression);
        self.expression_spans.insert(id, span);
        id
    }

    pub fn alloc_merge(
        &mut self,
        expression: Expression,
        left: ExpressionId,
        right: ExpressionId,
    ) -> ExpressionId {
        let id = self.expressions.push(expression);
        let lhs = self.get_span(left);
        let rhs = self.get_span(right);
        self.expression_spans.insert(id, merge(&lhs, &rhs));
        id
    }

    pub fn get_exp(&self, id: ExpressionId) -> &Expression {
        self.expressions.get(id).unwrap()
    }

    pub fn get_span(&self, id: ExpressionId) -> Span {
        self.expression_spans.get(&id).unwrap().clone()
    }

    pub fn expressions(&self) -> &PrimaryMap<ExpressionId, Expression> {
        &self.expressions
    }

    pub fn eq(&self, left: ExpressionId, right: ExpressionId) -> bool {
        if self.get_span(left) != self.get_span(right) {
            return false;
        }
        let lhs = self.get_exp(left);
        let rhs = self.get_exp(right);
        match (lhs, rhs) {
            (
                Expression::Unary {
                    operator: l_op,
                    inner: l_inner,
                },
                Expression::Unary {
                    operator: r_op,
                    inner: r_inner,
                },
            ) => {
                return l_op == r_op && self.eq(*l_inner, *r_inner);
            }
            (
                Expression::Binary {
                    left: l_left,
                    operator: l_op,
                    right: l_right,
                },
                Expression::Binary {
                    left: r_left,
                    operator: r_op,
                    right: r_right,
                },
            ) => {
                return self.eq(*l_left, *r_left) && l_op == r_op && self.eq(*l_right, *r_right);
            }
            (Expression::Call { call: l_call }, Expression::Call { call: r_call }) => {
                return l_call.ident.as_ref() == r_call.ident.as_ref()
                    && l_call.ident.span == r_call.ident.span
                    && l_call
                        .args
                        .iter()
                        .zip(r_call.args.iter())
                        .map(|(l, r)| l == r)
                        .all(|v| v);
            }
            (
                Expression::Identifier {
                    ident: l_ident,
                    name_id: _,
                },
                Expression::Identifier {
                    ident: r_ident,
                    name_id: _,
                },
            ) => {
                return l_ident == r_ident;
            }
            (
                Expression::Literal { literal: l_value },
                Expression::Literal { literal: r_value },
            ) => {
                return l_value == r_value;
            }
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Unary {
        operator: M<UnaryOp>,
        inner: ExpressionId,
    },
    Binary {
        left: ExpressionId,
        operator: M<BinaryOp>,
        right: ExpressionId,
    },
    /// Function calls and variant case constructors.
    Call {
        call: Call,
    },
    Identifier {
        ident: M<String>,
        name_id: NameId,
    },
    Literal {
        literal: M<Literal>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum UnaryOp {
    LogicalInvert,
    ArithmeticNegate,
}

/// The supported binary operators
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BinaryOp {
    // Arithmetic Operations
    Mult,
    Div,
    Mod,
    Add,
    Sub,
    // Shifting Operations
    BitShiftL,
    BitShiftR,
    ArithShiftR,
    // Comparisons
    LT,
    LTE,
    GT,
    GTE,
    EQ,
    NEQ,
    // Bitwise Operations
    BitAnd,
    BitXor,
    BitOr,
    // Logical Operations
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Integer(u64),
    Float(f64),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Call {
    pub ident: M<String>,
    pub name_id: NameId,
    pub args: Vec<ExpressionId>,
}
