mod expressions;
mod component;
mod statements;
mod types;

use std::sync::Arc;

use crate::ast::Span;
use crate::lexer::{TokenData, Token};
use crate::ast::component::Component;

use miette::{Diagnostic, SourceSpan, NamedSource};
use thiserror::Error;

use self::component::parse_component;

#[derive(Error, Debug, Diagnostic)]
pub enum ParserError{
    #[diagnostic()]
    #[error("Failed to parse")]
    Base {
        #[source_code]
        src: Arc<NamedSource>,
        #[label("Unable to parse this code")]
        span: SourceSpan,
    },
    #[diagnostic()]
    #[error("Unexpected token {token:?} with description '{description}'")]
    UnexpectedToken {
        #[source_code]
        src: Arc<NamedSource>,
        #[label("Here")]
        span: SourceSpan,
        description: String,
        token: Token
    },
    #[diagnostic()]
    #[error("End of input reached")]
    EndOfInput,
    #[diagnostic()]
    #[error("Feature {feature} not supported yet at {token:?}")]
    NotYetSupported {
        feature: String,
        token: Token
    }
}


pub fn parse(src: Arc<NamedSource>, tokens: Vec<TokenData>) -> Result<Component, ParserError> {
    let mut parse_input = ParseInput::new(src, tokens);
    parse_component(&mut parse_input)
}


#[derive(Debug, Clone)]
pub struct ParseInput {
    src: Arc<NamedSource>, 
    tokens: Vec<TokenData>,
    index: usize
}

#[derive(Debug, Clone, Copy)]
pub struct Checkpoint {
    index: usize
}

impl ParseInput {
    pub fn new(src: Arc<NamedSource>, tokens: Vec<TokenData>) -> Self {
        ParseInput {
            src,
            tokens,
            index: 0
        }
    }

    pub fn unsupported_error(&self, feature: &str) -> ParserError {
        ParserError::NotYetSupported {
            feature: feature.to_string(),
            token: self.tokens[self.index].token.clone()
        }
    }

    pub fn unexpected_token(&self, description: &str) -> ParserError {
        let data = &self.tokens[self.index-1];
        ParserError::UnexpectedToken {
            src: self.src.clone(),
            span: data.span.clone(),
            description: description.to_string(),
            token: data.token.clone()
        }
    }

    pub fn checkpoint(&self) -> Checkpoint {
        Checkpoint { index: self.index }
    }

    pub fn restore(&mut self, checkpoint: Checkpoint) {
        self.index = checkpoint.index
    }

    pub fn get_source(&self) -> Arc<NamedSource> {
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
            let result = &self.tokens[self.index..self.index+num];
            self.index += num;
            Ok(result)
        } else {
            Err(ParserError::EndOfInput)
        }
    }
}


#[cfg(test)]
mod tests {
    
    use std::sync::Arc;
    use miette::NamedSource;

    use crate::{
        lexer::{tokenize, Token},
        ast::Span,
        parser::ParseInput
    };

    pub fn make_input(source: &str) -> ParseInput {
        let src = Arc::new(NamedSource::new("test", source.to_string()));
        let tokens = tokenize(src.clone(), source.to_string()).unwrap();
        ParseInput::new(src, tokens)
    }

    pub fn make_span(start: usize, len: usize) -> Span {
        Span::new(start.into(), len.into())
    }

    #[test]
    fn test_peek() {
        let mut input = make_input("export func");
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
        let mut input = make_input("export func () -> {}");
        assert_eq!(input.peekn(0).unwrap(), &Token::Export);
        assert_eq!(input.peekn(1).unwrap(), &Token::Func);
        assert_eq!(input.peekn(2).unwrap(), &Token::LParen);
        input.next().unwrap();
        assert_eq!(input.peekn(0).unwrap(), &Token::Func);
        assert_eq!(input.peekn(1).unwrap(), &Token::LParen);
        assert_eq!(input.peekn(2).unwrap(), &Token::RParen);
    }
}

