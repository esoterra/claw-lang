use super::{M, MBox, Place};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    Unary {
        operator: M<UnaryOp>,
        inner: MBox<Expression>
    },
    Binary {
        left: MBox<Expression>,
        operator: M<BinaryOp>,
        right: MBox<Expression>
    },
    Place {
        place: M<Place>,
    },
    Literal {
        value: M<Literal>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum UnaryOp {
    LogicalInvert,
    ArithmeticNegate,
    Dereference
}

/// The supported binary operators
#[derive(Debug, PartialEq, Eq, Clone)]
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
    LogicalOr
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Literal {
    Integer(u64)
}