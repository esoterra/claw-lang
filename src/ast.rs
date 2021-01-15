use codespan::Span;

/// The metadata wrapper type
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct M<T> {
    pub span: Span,
    pub value: T
}

/// The metadata wrapper type
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MBox<T> {
    pub span: Span,
    pub value: Box<T>
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ResultType {
    pub results: Vec<M<ValType>>
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ValType {
    Basic(BasicVal),
    Ptr(PointerVal),
    Ident(String)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BasicVal {
    I32, I64, U32, U64, S32, S64, F32, F64
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PointerVal {
    pub kind: M<PointerKind>,
    pub memtype: M<Pointable>,
    pub offset: Option<M<u64>>,
    pub align: Option<M<u64>>,
    pub memory: Option<M<String>>
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PointerKind {
    Ptr, Slice
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Pointable {
    Basic(BasicVal),
    U8, U16, S8, S16, I8, I16
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FnType {
    pub arg_types: Vec<M<ValType>>,
    pub restype: M<ResultType>
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Module {
    pub items: Vec<Item>
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Item {
    Import(Import),
    Function(Function),
    Table(Table),
    Memory(Memory),
    Global(Global),
}

/// 
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Import {
    pub import_kwd: Span,
    pub import_name: M<String>,
    pub from_kwd: Span,
    pub module_name: M<String>,
    pub external_type: M<ExternalType>
}

/// 
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExternalType {
    Function(FnType),
    Memory(MemType),
    MutGlobal {
        mutability: Span,
        value_type: M<ValType>
    },
    ConstGlobal {
        mutability: Span,
        value_type: M<ValType>
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MemType {
    min_pages: M<u64>,
    max_pages: Option<M<u64>>
}

/// 
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Memory {
    pub export: Option<Span>,
    pub name: M<String>,
    pub min_size: M<u64>,
    pub max_size: Option<M<u64>>
}

/// 
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Table {
    pub export: Option<Span>,
    pub name: M<String>,
    pub min_size: M<u64>,
    pub max_size: M<u64>
}

/// 
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Global {
    pub export: Option<Span>,
    pub mutable: Option<Span>,
    pub name: M<String>,
    pub init_value: M<Expression>
}

/// 
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Function {
    pub export: Option<Span>,
    pub signature: FunctionSignature,
    pub body: Block
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionSignature {
    pub name: M<String>,
    pub arguments: Vec<(M<String>, M<ValType>)>,
    pub result_type: M<ResultType>
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Block {
    pub start_brace: Span,
    pub statements: Vec<Statement>,
    pub end: Option<MBox<StatementEnd>>,
    pub end_brace: Span
}


#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    ExpressionStmt {
        expression: M<Expression>,
        semicolon: Span
    },
    If {
        kwd: Span,
        condition: M<Expression>,
        then_block: M<Block>,
        else_block: Option<M<ElseBlock>>
    },
    ForSlice {
        for_kwd: Span,
        loop_ident: M<String>,
        in_kwd: Span,
        span_expression: M<Expression>,
        body: Block
    },
    ForRange {
        for_kwd: Span,
        loop_ident: M<String>,
        in_kwd: Span,
        lower_expr: M<Expression>,
        upper_expr: M<Expression>,
        body: Block
    },
    Loop {
        kwd: Span,
        body: Block
    },
    LetMut {
        let_kwd: Span,
        mut_kwd: Span,
        ident: M<String>,
        expression: M<Expression>
    },
    Let {
        let_kwd: Span,
        ident: M<String>,
        expression: M<Expression>
    },
    Assign {
        lvalue: M<LValue>,
        eq_op: Span,
        expression: M<Expression>
    },
    OpAssign {
        lvalue: M<LValue>,
        op_assign: M<OpAssign>,
        expression: M<Expression>
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LValue {
    Ident(String),
    Property {
        base: Box<LValue>,
        prop_name: M<String>
    },
    Index {
        base: Box<LValue>,
        lbracket: Span,
        index: M<Expression>,
        rbracket: Span
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum OpAssign {
    BitOr,
    BitAnd,
    BitXor,
    Add,
    Sub,
    Mult,
    Div
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ElseBlock {
    pub kwd: Span,
    pub body: Block
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum StatementEnd {
    Break {
        kwd: Span,
        result: Option<M<Expression>>
    },
    Return {
        kwd: Span,
        result: Option<M<Expression>>
    },
    Continue {
        kwd: Span
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    Unary {
        operator: M<UnaryOp>,
        operand: MBox<Expression>
    },
    Binary {
        left: MBox<Expression>,
        operator: M<BinaryOp>,
        right: MBox<Expression>
    },
    Block {
        block: Block
    },
    Parenthetical {
        lparen: Span,
        expression: MBox<Expression>,
        rparen: Span
    },
    If {
        kwd: Span,
        condition: MBox<Expression>,
        then_block: MBox<Block>,
        else_block: Option<MBox<ElseBlock>>
    },
    Property {
        base: MBox<Expression>,
        property_name: M<String>
    },
    Index {
        base: MBox<Expression>,
        lbracket: Span,
        index: MBox<Expression>,
        rbracket: Span
    },
    Slice {
        base: MBox<Expression>,
        lbracket: Span,
        left_index: Option<MBox<Expression>>,
        colon: Span,
        right_index: Option<MBox<Expression>>,
        rbracket: Span
    },
    StaticProperty {
        base: M<String>,
        path: Vec<M<String>>
    },
    FunctionCall {
        name: M<String>,
        args: Vec<M<Expression>>
    },
    MethodCall {
        base: M<String>,
        name: M<String>,
        args: Vec<M<Expression>>
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum UnaryOp {
    LogicalInvert,
    ArithmeticNegate,
    Dereference
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BinaryOp {
    Mult, Div, Mod,
    Add, Sub,
    BitShiftL, BitShiftR, ArithShiftR,
    LT, LTE, GT, GTE,
    EQ, NEQ,
    BitAnd, BitXor, BitOr,
    LogicalAnd, LogicalOr
}
