use super::ParserError;

use crate::lexer::{ Token, TokenData };
use crate::ast::{ M,
    statements::{
        Block, Statement
    }
};

pub fn parse_block(input: &[TokenData]) -> Result<Block, ParserError> {
    unreachable!()
}

pub fn parse_statement(input: &[TokenData]) -> Result<Statement, ParserError> {
    unreachable!()
}

fn parse_assign(input: &[TokenData]) -> Result<Statement, ParserError> {
    unreachable!()
}

fn parse_return(input: &[TokenData]) -> Result<Statement, ParserError> {
    unreachable!()
}