use claw_codegen::{generate, GenerationError};
use claw_common::make_source;
use claw_parser::{parse, tokenize, LexerError, ParserError};
use claw_resolver::{resolve, wit::ResolvedWit, ResolverError};
use wit_parser::Resolve;

use miette::Diagnostic;
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
pub enum Error {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Lexer(#[from] LexerError),

    #[error(transparent)]
    #[diagnostic(transparent)]
    Parser(#[from] ParserError),

    #[error(transparent)]
    #[diagnostic(transparent)]
    Resolver(#[from] ResolverError),

    #[error(transparent)]
    #[diagnostic(transparent)]
    Generator(#[from] GenerationError),
}

pub fn compile(source_name: String, source_code: &str, wit: Resolve) -> Result<Vec<u8>, Error> {
    let src = make_source(source_name.as_str(), source_code);

    let tokens = tokenize(src.clone(), source_code)?;

    let comp = parse(src.clone(), tokens)?;

    let wit = ResolvedWit::new(wit);

    let rcomp = resolve(&comp, wit)?;

    let output = generate(&comp, &rcomp)?;

    Ok(output)
}
