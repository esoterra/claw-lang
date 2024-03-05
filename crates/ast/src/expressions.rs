use super::{merge, NameId, Span};
use cranelift_entity::{entity_impl, PrimaryMap};
use std::collections::HashMap;

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

    pub fn get_exp(&self, id: ExpressionId) -> &Expression {
        self.expressions.get(id).unwrap()
    }

    pub fn get_span(&self, id: ExpressionId) -> Span {
        *self.expression_spans.get(&id).unwrap()
    }

    pub fn expressions(&self) -> &PrimaryMap<ExpressionId, Expression> {
        &self.expressions
    }

    pub fn alloc_ident(&mut self, ident: NameId, span: Span) -> ExpressionId {
        let expr = Expression::Identifier(Identifier { ident });
        self.alloc(expr, span)
    }

    pub fn alloc_literal(&mut self, literal: Literal, span: Span) -> ExpressionId {
        self.alloc(Expression::Literal(literal), span)
    }

    pub fn alloc_call(
        &mut self,
        ident: NameId,
        args: Vec<ExpressionId>,
        span: Span,
    ) -> ExpressionId {
        let expr = Expression::Call(Call { ident, args });
        self.alloc(expr, span)
    }

    pub fn alloc_unary_op(&mut self, op: UnaryOp, inner: ExpressionId, span: Span) -> ExpressionId {
        let expr = match op {
            UnaryOp::Negate => Expression::Unary(UnaryExpression { op, inner }),
        };
        self.alloc(expr, span)
    }

    pub fn alloc_bin_op(
        &mut self,
        op: BinaryOp,
        left: ExpressionId,
        right: ExpressionId,
    ) -> ExpressionId {
        let span = merge(&self.get_span(left), &self.get_span(right));
        self.alloc(
            Expression::Binary(BinaryExpression { op, left, right }),
            span,
        )
    }
}

pub trait ContextEq<Context> {
    fn context_eq(&self, other: &Self, context: &Context) -> bool;
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Identifier(Identifier),
    Literal(Literal),
    Call(Call),
    Unary(UnaryExpression),
    Binary(BinaryExpression),
}

impl ContextEq<super::Component> for ExpressionId {
    fn context_eq(&self, other: &Self, context: &super::Component) -> bool {
        let self_span = context.expr().get_span(*self);
        let other_span = context.expr().get_span(*other);
        if self_span != other_span {
            return false;
        }

        let self_expr = context.expr().get_exp(*self);
        let other_expr = context.expr().get_exp(*other);
        if !self_expr.context_eq(other_expr, context) {
            return false;
        }
        true
    }
}

impl ContextEq<super::Component> for Expression {
    fn context_eq(&self, other: &Self, context: &super::Component) -> bool {
        match (self, other) {
            (Expression::Identifier(left), Expression::Identifier(right)) => {
                left.context_eq(right, context)
            }
            (Expression::Literal(left), Expression::Literal(right)) => {
                left.context_eq(right, context)
            }
            (Expression::Call(left), Expression::Call(right)) => left.context_eq(right, context),
            (Expression::Unary(left), Expression::Unary(right)) => left.context_eq(right, context),
            (Expression::Binary(left), Expression::Binary(right)) => {
                left.context_eq(right, context)
            }
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    pub ident: NameId,
}

impl ContextEq<super::Component> for Identifier {
    fn context_eq(&self, other: &Self, context: &super::Component) -> bool {
        context.get_name(self.ident) == context.get_name(other.ident)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Integer(u64),
    Float(f64),
    String(String),
}

impl ContextEq<super::Component> for Literal {
    fn context_eq(&self, other: &Self, _context: &super::Component) -> bool {
        self == other
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Call {
    pub ident: NameId,
    pub args: Vec<ExpressionId>,
}

impl ContextEq<super::Component> for Call {
    fn context_eq(&self, other: &Self, context: &super::Component) -> bool {
        let ident_eq = self.ident.context_eq(&other.ident, context);
        let args_eq = self
            .args
            .iter()
            .zip(other.args.iter())
            .map(|(l, r)| l.context_eq(r, context))
            .all(|v| v);

        ident_eq && args_eq
    }
}

// Unary Operators

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOp {
    Negate,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnaryExpression {
    pub op: UnaryOp,
    pub inner: ExpressionId,
}

impl ContextEq<super::Component> for UnaryExpression {
    fn context_eq(&self, other: &Self, context: &super::Component) -> bool {
        let self_inner = context.expr().get_exp(self.inner);
        let other_inner = context.expr().get_exp(other.inner);
        self_inner.context_eq(other_inner, context)
    }
}

// Binary Operators

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryOp {
    // Arithmetic Operations
    Multiply,
    Divide,
    Modulo,
    Add,
    Subtract,

    // Shifting Operations
    BitShiftL,
    BitShiftR,
    ArithShiftR,

    // Comparisons
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Equals,
    NotEquals,

    // Bitwise Operations
    BitOr,
    BitXor,
    BitAnd,

    // Logical Operations
    LogicalOr,
    LogicalAnd,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryExpression {
    pub op: BinaryOp,
    pub left: ExpressionId,
    pub right: ExpressionId,
}

impl ContextEq<super::Component> for BinaryExpression {
    fn context_eq(&self, other: &Self, context: &super::Component) -> bool {
        let self_left = context.expr().get_exp(self.left);
        let other_left = context.expr().get_exp(other.left);
        let left_eq = self_left.context_eq(other_left, context);

        let self_right = context.expr().get_exp(self.right);
        let other_right = context.expr().get_exp(other.right);
        let right_eq = self_right.context_eq(other_right, context);

        left_eq && right_eq
    }
}

impl BinaryExpression {
    pub fn is_relation(&self) -> bool {
        use BinaryOp as BE;
        matches!(
            self.op,
            BE::LessThan
                | BE::LessThanEqual
                | BE::GreaterThan
                | BE::GreaterThanEqual
                | BE::Equals
                | BE::NotEquals
        )
    }
}
