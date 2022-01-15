mod expressions;
mod module;
mod statements;

use std::sync::Arc;

use crate::ast::Span;
use crate::lexer::{TokenData, Token};
use crate::ast::module::Module;

use miette::{Diagnostic, SourceSpan, NamedSource};
use thiserror::Error;

use self::module::parse_module;

#[derive(Error, Debug, Diagnostic)]
#[error("Failed to parse")]
#[diagnostic()]
pub enum ParserError{
    Base {
        // #[source_code]
        // src: Arc<NamedSource>,
        // #[label("Unable to parse this code")]
        // span: SourceSpan,
    },
    UnexpectedToken,
    EndOfInput,
    NotYetSupported
}


pub struct ParseInput {
    src: Arc<NamedSource>, 
    tokens: Vec<TokenData>,
    index: usize
}

impl ParseInput {
    pub fn new(src: Arc<NamedSource>, tokens: Vec<TokenData>) -> Self {
        ParseInput {
            src, tokens,
            index: 0
        }
    }

    pub fn get_source(&self) -> Arc<NamedSource> {
        self.src.clone()
    }

    pub fn done(&self) -> bool {
        self.index >= self.tokens.len()
    }

    pub fn peek(&mut self) -> Result<&TokenData, ParserError> {
        self.tokens.get(self.index).ok_or(ParserError::EndOfInput)
    }

    pub fn next(&mut self) -> Result<&TokenData, ParserError> {
        let result = self.tokens.get(self.index);
        self.index += 1;
        result.ok_or(ParserError::EndOfInput)
    }

    pub fn assert_next(&mut self, token: Token) -> Result<Span, ParserError> {
        let next = self.next()?;
        if next.token == token {
            Ok(next.span.clone())
        } else {
            Err(ParserError::EndOfInput)
        }
    }

    pub fn next_if(&mut self, token: Token) -> Option<Span> {
        let next = self.next().ok()?;
        if next.token == token {
            Some(next.span.clone())
        } else {
            None
        }
    }

    pub fn has(&self, num: usize) -> bool {
        self.index + num <= self.tokens.len()
    }

    pub fn slice_next(&mut self, num: usize) -> Result<&[TokenData], ParserError> {
        if self.has(num) {
            let result = &self.tokens[self.index..self.index+num];
            self.index += num;
            Ok(result)
        } else {
            Err(ParserError::EndOfInput)
        }
    }
}


pub fn parse(src: Arc<NamedSource>, tokens: Vec<TokenData>) -> Result<Module, ParserError> {
    let mut parse_input = ParseInput::new(src, tokens);
    parse_module(&mut parse_input)
}







