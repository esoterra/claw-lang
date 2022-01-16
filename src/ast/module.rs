use super::{
    Span, M,
    types::{
        FnType, ValType
    },
    statements::Block,
    expressions::Expression, MBox
};

/// Each Wrought source file represents a module
/// and this struct represents the root of the AST.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Module {
    /// The root of a module is composed of a sequence of items
    pub items: Vec<Item>
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Item {
    Import(Import),
    Function(Function),
    Table(Table),
    Memory(Memory),
    Global(Global),
    Struct(StructDeclaration)
}

/// 
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Import {
    pub from_kwd: Span,
    pub module_name: M<String>,
    pub import_kwd: Span,
    pub import_name: M<String>,
    pub external_type: M<ExternalType>
}

/// 
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExternalType {
    Function(FnType),
    Memory(MemType),
    MutGlobal {
        mut_kwd: Span,
        value_type: M<ValType>
    },
    ConstGlobal {
        const_kwd: Span,
        value_type: M<ValType>
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MemType {
    mem_kwd: Span,
    min_pages: M<u64>,
    max_pages: Option<M<u64>>
}

/// 
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Function {
    pub export_kwd: Option<Span>,
    pub signature: FunctionSignature,
    pub body: Block
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionSignature {
    pub fn_kwd: Span,
    pub name: M<String>,
    pub arguments: Vec<(M<String>, M<ValType>)>,
    pub arrow: Span,
    pub result_type: M<ValType>
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
pub struct Memory {
    pub export: Option<Span>,
    pub name: M<String>,
    pub min_size: M<u64>,
    pub max_size: Option<M<u64>>
}

/// 
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Global {
    pub export_kwd: Option<Span>,
    pub let_kwd: Span,
    pub mut_kwd: Option<Span>,
    pub ident: M<String>,
    pub valtype: M<ValType>,
    pub assign: Span,
    pub init_value: MBox<Expression>,
    pub semicolon: Span
}

/// 
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StructDeclaration {
    pub struct_kwd: Span,
    pub name: M<String>,
    pub braces: (Span, Span),
    pub members: Vec<StructMember>
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StructMember {
    pub name: M<String>,
    pub colon: Span,
    pub value_type: M<ValType>
}