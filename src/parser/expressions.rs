use super::ParserError;

use crate::lexer::TokenData;
use crate::ast::expressions::Expression;

pub fn parse_expression(input: &[TokenData]) -> Result<Expression, ParserError> {
    unreachable!()
}

fn parse_place(input: &[TokenData]) -> Result<Expression, ParserError> {
    unreachable!()
}

fn parse_literal(input: &[TokenData]) -> Result<Expression, ParserError> {
    unreachable!()
}

fn parse_add(input: &[TokenData]) -> Result<Expression, ParserError> {
    unreachable!()
}