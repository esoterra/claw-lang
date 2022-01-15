use super::{
    Span, MBox, Place,
    expressions::Expression, M
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Block {
    pub start_brace: Span,
    pub root_statement: MBox<Statement>,
    pub end_brace: Span
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Statement {
    pub inner: StatementType,
    pub next: Option<MBox<Statement>>
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum StatementType {
    Assign {
        place: M<Place>,
        assign_op: Span,
        expression: MBox<Expression>
    },
    Return {
        return_kwd: Span,
        expression: MBox<Expression>
    }
}