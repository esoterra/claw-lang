mod expressions;
mod module;
mod statements;

use std::sync::Arc;

use crate::lexer::TokenData;
use crate::ast::module::Module;

use miette::{Diagnostic, SourceSpan, NamedSource};
use thiserror::Error;

use self::module::parse_module;

#[derive(Error, Debug, Diagnostic)]
#[error("Failed to parse")]
#[diagnostic()]
pub struct ParserError {
    #[source_code]
    src: Arc<NamedSource>,
    #[label("Unable to parse this code")]
    span: SourceSpan,
}

pub fn parse(tokens: Vec<TokenData>) -> Result<Module, ParserError> {
    parse_module(&tokens)
}







