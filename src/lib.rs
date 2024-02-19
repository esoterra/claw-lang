#![allow(clippy::while_let_loop)]
#![allow(clippy::while_let_on_iterator)]
#![allow(clippy::single_match)]
#![allow(clippy::should_implement_trait)]

use std::sync::Arc;

use miette::{NamedSource, Report};
use resolver::ResolvedComponent;

pub mod ast;
pub mod codegen;
pub mod context;
pub mod lexer;
pub mod parser;
pub mod resolver;

pub mod stack_map;

pub type Source = Arc<NamedSource<String>>;

pub fn make_source(name: &str, source: &str) -> Source {
    Arc::new(NamedSource::new(name, source.to_owned()))
}

pub fn compile(source_name: String, source_code: &str) -> Option<ResolvedComponent> {
    let src = make_source(source_name.as_str(), source_code);

    let tokens = match lexer::tokenize(src.clone(), source_code) {
        Ok(token_data) => token_data,
        Err(error) => {
            println!("{:?}", Report::new(error));
            return None;
        }
    };

    let ast = match parser::parse(src.clone(), tokens) {
        Ok(ast) => ast,
        Err(error) => {
            println!("{:?}", Report::new(error));
            return None;
        }
    };

    match resolver::resolve(src, ast) {
        Ok(resolved) => Some(resolved),
        Err(error) => {
            println!("{:?}", Report::new(error));
            None
        }
    }
}
