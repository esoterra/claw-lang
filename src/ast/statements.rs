use super::{
    Span, M, MBox, Place,
    expressions::Expression
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Block {
    pub start_brace: Span,
    pub root_statement: M<Statement>,
    pub end_brace: Span
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    Assign {
        place: Place,
        equals_op: Span,
        expression: MBox<Expression>
    },
    Return {
        return_kwd: Span,
        expression: MBox<Expression>
    }
}