use super::{
    Span, M, MBox, Place,
    expressions::Expression, types::ValType
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
        expression: MBox<Expression>,
        next: Option<MBox<Statement>>
    },
    Assign {
        place: M<Place>,
        assign_op: Span,
        expression: MBox<Expression>,
        next: Option<MBox<Statement>>
    },
    If {
        if_kwd: Span,
        condition: MBox<Expression>,
        block: M<Block>,
        next: Option<MBox<Statement>>
    },
    Return {
        return_kwd: Span,
        expression: MBox<Expression>
    }
}