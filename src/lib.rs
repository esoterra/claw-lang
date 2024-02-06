use std::sync::Arc;

use miette::{NamedSource, Report};
use resolver::ResolvedComponent;

pub mod ast;
pub mod codegen;
pub mod lexer;
pub mod parser;
pub mod resolver;

pub mod stack_map;

pub fn compile<'src>(source_name: String, source_code: &'src str) -> Option<ResolvedComponent> {
    let src = Arc::new(NamedSource::new(
        source_name,
        Box::leak(source_code.to_owned().into_boxed_str()) as &'static str,
    ));

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
            return None;
        }
    }
}
