mod component;
mod expressions;
mod statements;
mod types;

use std::sync::Arc;

use crate::ast::component::Component;
use crate::ast::Span;
use crate::lexer::{Token, TokenData};

use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

use self::component::parse_component;

#[derive(Error, Debug, Diagnostic)]
pub enum ParserError {
    #[error("Failed to parse")]
    Base {
        #[source_code]
        src: crate::Source,
        #[label("Unable to parse this code")]
        span: SourceSpan,
    },
    #[error("Unexpected token {token:?} with description '{description}'")]
    UnexpectedToken {
        #[source_code]
        src: crate::Source,
        #[label("Here")]
        span: SourceSpan,
        description: String,
        token: Token,
    },
    #[error("End of input reached")]
    EndOfInput,
    #[error("Feature {feature} not supported yet at {token:?}")]
    NotYetSupported { feature: String, token: Token },
}

pub fn parse(src: crate::Source, tokens: Vec<TokenData>) -> Result<Component, ParserError> {
    let mut input = ParseInput::new(src.clone(), tokens);
    parse_component(src, &mut input)
}

#[derive(Debug, Clone)]
pub struct ParseInput {
    src: crate::Source,
    tokens: Vec<TokenData>,
    index: usize,
}

impl ParseInput {
    pub fn new(src: crate::Source, tokens: Vec<TokenData>) -> Self {
        ParseInput {
            src,
            tokens,
            index: 0,
        }
    }

    pub fn unsupported_error(&self, feature: &str) -> ParserError {
        ParserError::NotYetSupported {
            feature: feature.to_string(),
            token: self.tokens[self.index].token.clone(),
        }
    }

    pub fn unexpected_token(&self, description: &str) -> ParserError {
        let data = &self.tokens[self.index - 1];
        ParserError::UnexpectedToken {
            src: self.src.clone(),
            span: data.span.clone(),
            description: description.to_string(),
            token: data.token.clone(),
        }
    }

    pub fn get_source(&self) -> crate::Source {
        self.src.clone()
    }

    pub fn has(&self, num: usize) -> bool {
        self.index + num <= self.tokens.len()
    }

    pub fn done(&self) -> bool {
        self.index >= self.tokens.len()
    }

    pub fn peek(&self) -> Result<&TokenData, ParserError> {
        self.tokens.get(self.index).ok_or(ParserError::EndOfInput)
    }

    pub fn peekn(&self, n: usize) -> Option<&Token> {
        self.tokens.get(self.index + n).map(|t| &t.token)
    }

    pub fn next(&mut self) -> Result<&TokenData, ParserError> {
        let result = self.tokens.get(self.index);
        self.index += 1;
        result.ok_or(ParserError::EndOfInput)
    }

    pub fn assert_next(&mut self, token: Token, description: &str) -> Result<Span, ParserError> {
        let next = self.next()?;
        if next.token == token {
            Ok(next.span.clone())
        } else {
            Err(self.unexpected_token(description))
        }
    }

    pub fn next_if(&mut self, token: Token) -> Option<Span> {
        {
            let next = self.peek().ok()?;
            if next.token != token {
                return None;
            }
        }
        Some(self.next().ok()?.span.clone())
    }

    pub fn slice_next(&mut self, num: usize) -> Result<&[TokenData], ParserError> {
        if self.has(num) {
            let result = &self.tokens[self.index..self.index + num];
            self.index += num;
            Ok(result)
        } else {
            Err(ParserError::EndOfInput)
        }
    }
}

pub fn make_input<'src>(source: &'src str) -> (crate::Source, ParseInput) {
    let src = Arc::new(NamedSource::new("test", source.to_string()));
    let tokens = crate::lexer::tokenize(src.clone(), source).unwrap();
    (src.clone(), ParseInput::new(src, tokens))
}

pub fn make_span(start: usize, len: usize) -> Span {
    Span::new(start.into(), len.into())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_peek() {
        let (_src, mut input) = make_input("export func");
        assert_eq!(input.peek().unwrap().token, Token::Export);
        assert_eq!(input.peek().unwrap().token, Token::Export);
        assert_eq!(input.peek().unwrap().token, Token::Export);
        input.next().unwrap();
        assert_eq!(input.peek().unwrap().token, Token::Func);
        assert_eq!(input.peek().unwrap().token, Token::Func);
        assert_eq!(input.peek().unwrap().token, Token::Func);
    }

    #[test]
    fn test_peekn() {
        let (_src, mut input) = make_input("export func () -> {}");
        assert_eq!(input.peekn(0).unwrap(), &Token::Export);
        assert_eq!(input.peekn(1).unwrap(), &Token::Func);
        assert_eq!(input.peekn(2).unwrap(), &Token::LParen);
        input.next().unwrap();
        assert_eq!(input.peekn(0).unwrap(), &Token::Func);
        assert_eq!(input.peekn(1).unwrap(), &Token::LParen);
        assert_eq!(input.peekn(2).unwrap(), &Token::RParen);
    }
}
