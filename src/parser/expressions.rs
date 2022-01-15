use super::{ParserError, ParseInput};

use crate::ast::{expressions::Expression, M};

pub fn parse_expression(input: &mut ParseInput) -> Result<M<Expression>, ParserError> {
    unreachable!()
}

fn parse_place(input: &mut ParseInput) -> Result<Expression, ParserError> {
    unreachable!()
}

fn parse_literal(input: &mut ParseInput) -> Result<Expression, ParserError> {
    unreachable!()
}

fn parse_add(input: &mut ParseInput) -> Result<Expression, ParserError> {
    unreachable!()
}