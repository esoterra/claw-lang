use id_arena::Arena;

use crate::ast::expressions::ExpressionData;

use super::{
    Span, M,
    types::{
        FnType, ValType
    },
    expressions::ExpressionId, NameId, Call
};

/// Each Claw source file represents a Component
/// and this struct represents the root of the AST.
#[derive(Debug, Default)]
pub struct Component {
    // Top level items
    pub imports: Arena<Import>,
    pub globals: Arena<Global>,
    pub functions: Arena<Function>,
}

/// 
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Import {
    pub import_kwd: Span,
    pub name: M<String>,
    pub colon: Span,
    pub external_type: ExternalType
}

/// 
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExternalType {
    Function(FnType)
}

/// 
#[derive(Debug)]
pub struct Function {
    pub export_kwd: Option<Span>,
    pub signature: FunctionSignature,
    pub body: M<Block>,
    pub expressions: ExpressionData
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionSignature {
    pub name: M<String>,
    pub colon: Span,
    pub fn_type: FnType
}

/// 
#[derive(Debug, Clone)]
pub struct Global {
    pub export_kwd: Option<Span>,
    pub let_kwd: Span,
    pub mut_kwd: Option<Span>,
    pub ident: M<String>,
    pub colon: Span,
    pub valtype: M<ValType>,
    pub assign: Span,
    pub init_value: ExpressionId,
    pub semicolon: Span,
    pub expressions: ExpressionData
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub start_brace: Span,
    pub statements: Vec<M<Statement>>,
    pub end_brace: Span
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let {
        let_kwd: Span,
        mut_kwd: Option<Span>,
        ident: M<String>,
        name_id: NameId,
        annotation: Option<M<ValType>>,
        assign_op: Span,
        expression: ExpressionId
    },
    Assign {
        ident: M<String>,
        name_id: NameId,
        assign_op: Span,
        expression: ExpressionId
    },
    Call {
        call: Call
    },
    If {
        if_kwd: Span,
        condition: ExpressionId,
        block: M<Block>
    },
    Return {
        return_kwd: Span,
        expression: ExpressionId
    }
}