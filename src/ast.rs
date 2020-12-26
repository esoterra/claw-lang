use codespan::Span;

/// The metadata wrapper type
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct M<T> {
    pub span: Span,
    pub value: T
}

/// The type for values that WASM operates on
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ValueType {
    I32, I64, F32, F64
}

/// The mutability of a value
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Mutability {
    Mutable,
    Immutable
}

/// A module
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Module {
    imports: Vec<Import>,
    memory: Vec<Memory>,
    table: Vec<Table>,
    globals: Vec<Global>,
    functions: Vec<Function>
}

/// 
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Import {
    pub span: Span,
    pub module_name: M<String>,
    pub import_name: M<String>,
    pub type_expr: M<ImportType>
}

/// 
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ImportType {
    FunctionImport {
        arguments: Vec<M<ValueType>>,
        result: Vec<M<ValueType>>
    },
    GlobalImport {
        mutability: Option<M<Mutability>>,
        value_type: M<ValueType>
    },
    MemoryImport {
        size: M<u32>
    },
    TableImport {
        size: M<u32>
    }
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
pub struct Function {
    pub export: Option<Span>,
    pub name: M<String>,
    pub arguments: Vec<(M<String>, M<ValueType>)>,
    pub result_type: Vec<M<ValueType>>,
    pub body: M<Expression>
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
    pub mutability: Option<M<Mutability>>,
    pub name: M<String>,
    pub init_value: M<Expression>
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Expression {

}