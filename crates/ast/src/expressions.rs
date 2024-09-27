use super::NameId;
use cranelift_entity::entity_impl;

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ExpressionId(u32);
entity_impl!(ExpressionId, "expression");

pub trait ContextEq<Context> {
    fn context_eq(&self, other: &Self, context: &Context) -> bool;
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Identifier(Identifier),
    Enum(EnumLiteral),
    Literal(Literal),
    Call(Call),
    Unary(UnaryExpression),
    Binary(BinaryExpression),
}

impl ContextEq<super::Component> for ExpressionId {
    fn context_eq(&self, other: &Self, context: &super::Component) -> bool {
        let self_span = context.expression_span(*self);
        let other_span = context.expression_span(*other);
        if self_span != other_span {
            dbg!(self_span, other_span);
            return false;
        }

        let self_expr = context.get_expression(*self);
        let other_expr = context.get_expression(*other);
        if !self_expr.context_eq(other_expr, context) {
            dbg!(self_expr, other_expr);
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

impl From<Identifier> for Expression {
    fn from(val: Identifier) -> Self {
        Expression::Identifier(val)
    }
}

impl ContextEq<super::Component> for Identifier {
    fn context_eq(&self, other: &Self, context: &super::Component) -> bool {
        context.get_name(self.ident) == context.get_name(other.ident)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnumLiteral {
    pub enum_name: NameId,
    pub case_name: NameId,
}

impl From<EnumLiteral> for Expression {
    fn from(val: EnumLiteral) -> Self {
        Expression::Enum(val)
    }
}

impl ContextEq<super::Component> for EnumLiteral {
    fn context_eq(&self, other: &Self, context: &super::Component) -> bool {
        context.get_name(self.enum_name) == context.get_name(other.enum_name)
            && context.get_name(self.case_name) == context.get_name(other.case_name)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Integer(u64),
    Float(f64),
    String(String),
}

impl From<Literal> for Expression {
    fn from(val: Literal) -> Self {
        Expression::Literal(val)
    }
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

impl From<Call> for Expression {
    fn from(val: Call) -> Self {
        Expression::Call(val)
    }
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

impl From<UnaryExpression> for Expression {
    fn from(val: UnaryExpression) -> Self {
        Expression::Unary(val)
    }
}

impl ContextEq<super::Component> for UnaryExpression {
    fn context_eq(&self, other: &Self, context: &super::Component) -> bool {
        let self_inner = context.get_expression(self.inner);
        let other_inner = context.get_expression(other.inner);
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

impl From<BinaryExpression> for Expression {
    fn from(val: BinaryExpression) -> Self {
        Expression::Binary(val)
    }
}

impl ContextEq<super::Component> for BinaryExpression {
    fn context_eq(&self, other: &Self, context: &super::Component) -> bool {
        let self_left = context.get_expression(self.left);
        let other_left = context.get_expression(other.left);
        let left_eq = self_left.context_eq(other_left, context);

        let self_right = context.get_expression(self.right);
        let other_right = context.get_expression(other.right);
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
