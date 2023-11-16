use super::{
    Span, M, MBox,
    expressions::ExpressionId,
    types::ValType
};

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub start_brace: Span,
    pub root_statement: Option<MBox<Statement>>,
    pub end_brace: Span
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let {
        let_kwd: Span,
        mut_kwd: Option<Span>,
        ident: M<String>,
        annotation: Option<M<ValType>>,
        assign_op: Span,
        expression: ExpressionId,
        next: Option<MBox<Statement>>
    },
    Assign {
        ident: M<String>,
        assign_op: Span,
        expression: ExpressionId,
        next: Option<MBox<Statement>>
    },
    If {
        if_kwd: Span,
        condition: ExpressionId,
        block: M<Block>,
        next: Option<MBox<Statement>>
    },
    Return {
        return_kwd: Span,
        expression: ExpressionId
    }
}