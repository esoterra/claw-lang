use super::{M, MBox};

#[derive(Debug, PartialEq, Clone)]
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
    /// Function calls and variant case constructors.
    Invocation {
        ident: M<String>,
        args: Vec<(M<String>, MBox<Expression>)>
    },
    Identifier {
        ident: M<String>,
    },
    Literal {
        value: M<Literal>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum UnaryOp {
    LogicalInvert,
    ArithmeticNegate
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
    LogicalOr
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Integer(u64),
    Float(f64)
}