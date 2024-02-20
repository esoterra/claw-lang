#![allow(clippy::while_let_loop)]
#![allow(clippy::while_let_on_iterator)]
#![allow(clippy::single_match)]
#![allow(clippy::should_implement_trait)]

use std::sync::Arc;

use miette::NamedSource;
use resolver::ResolvedComponent;

pub mod ast;
pub mod codegen;
pub mod context;
pub mod diagnostic;
pub mod lexer;
pub mod parser;
pub mod resolver;

pub mod stack_map;

pub type Source = Arc<NamedSource<String>>;

use diagnostic::OkPretty;

pub fn make_source(name: &str, source: &str) -> Source {
    Arc::new(NamedSource::new(name, source.to_owned()))
}

pub fn compile(source_name: String, source_code: &str) -> Option<ResolvedComponent> {
    let src = make_source(source_name.as_str(), source_code);

    let tokens = lexer::tokenize(src.clone(), source_code).ok_pretty()?;

    let ast = parser::parse(src.clone(), tokens).ok_pretty()?;

    resolver::resolve(src, ast).ok_pretty()
}
