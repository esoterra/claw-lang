use codespan::Span;

/// The metadata wrapper type
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct M<T> {
    pub span: Span,
    pub value: T
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
    pub restype: ResultType
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Module {
    pub items: Vec<M<Item>>
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
    pub module_name: M<String>,
    pub import_name: M<String>,
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
pub struct Function {
    pub export: Option<Span>,
    pub signature: M<FunctionSignature>,
    pub body: M<Statements>
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionSignature {
    pub name: M<String>,
    pub arguments: Vec<(M<String>, M<ValType>)>,
    pub result_type: ResultType
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Statements {
    pub statements: Vec<M<Statement>>,
    pub end: Option<M<StatementEnd>>
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    ExpressionStmt {
        // TODO
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
        body: M<Block>
    },
    ForRange {
        for_kwd: Span,
        loop_ident: M<String>,
        in_kwd: Span,
        lower_expr: M<Expression>,
        upper_expr: M<Expression>,
        body: M<Block>
    },
    Loop {
        kwd: Span,
        body: M<Block>
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
        // TODO
    },
    OpAssign {
        // TODO
        // ADD TO GRAMMAR
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ElseBlock {
    pub kwd: Span,
    pub body: Block
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Block {
    pub start_brace: Span,
    pub statements: M<Statements>,
    pub end_brace: Span
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
    Continue
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {

}


/// 
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Memory {
    pub export: Option<Span>,
    pub name: M<String>,
    pub min_size: M<u32>,
    pub max_size: Option<M<u32>>
}

/// 
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Table {
    pub export: Option<Span>,
    pub name: M<String>,
    pub min_size: M<u32>,
    pub max_size: M<u32>
}

/// 
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Global {
    pub export: Option<Span>,
    pub mutable: Option<Span>,
    pub name: M<String>,
    pub init_value: M<Expression>
}