use super::{ParserError, ParseInput};

use crate::ast::MBox;
use crate::lexer::{ Token, TokenData };
use crate::ast::{ M,
    statements::{
        Block, Statement
    }
};

pub fn parse_block(input: &mut ParseInput) -> Result<Block, ParserError> {
    unreachable!()
}

pub fn parse_statement(input: &mut ParseInput) -> Result<MBox<Statement>, ParserError> {
    unreachable!()
}

fn parse_assign(input: &mut ParseInput) -> Result<MBox<Statement>, ParserError> {
    unreachable!()
}

fn parse_return(input: &mut ParseInput) -> Result<MBox<Statement>, ParserError> {
    unreachable!()
}