use super::ParserError;

use crate::lexer::TokenData;
use crate::ast::module::{
    Module, Item, Global, Function
};

pub fn parse_module(input: &[TokenData]) -> Result<Module, ParserError> {
    unreachable!()
}

fn parse_item(input: &[TokenData]) -> Result<Item, ParserError> {
    unreachable!()
}

fn parse_global(input: &[TokenData]) -> Result<Global, ParserError> {
    unreachable!()
}

fn parse_fn(input: &[TokenData]) -> Result<Function, ParserError> {
    unreachable!()
}