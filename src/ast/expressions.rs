use super::{M, MBox};

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
    Variable {
        ident: M<String>,
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
    // 
    Mult,
    Div,
    Mod,
    //
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