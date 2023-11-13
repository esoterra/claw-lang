use std::sync::Arc;

use miette::{NamedSource, Report};

pub mod ast;
pub mod gen;
pub mod ir;
pub mod lexer;
pub mod parser;
pub mod resolver;

pub fn compile<SN: ToString, SC: ToString>(
        source_name: SN,
        source_code: SC
    ) -> Option<ir::Module> {
    let source_name = source_name.to_string();
    let source_code = source_code.to_string();
    let src = Arc::new(NamedSource::new(source_name, source_code.clone()));

    let tokens = match lexer::tokenize(src.clone(), source_code) {
        Ok(token_data) => token_data,
        Err(errors) => {
            for error in errors {
                println!("{:?}", Report::new(error));
            }
            return None;
        }
    };

    let ast = match parser::parse(src.clone(), tokens) {
        Ok(module) => module,
        Err(error) => {
            println!("{:?}", Report::new(error));
            return None;
        }
    };

    match resolver::resolve(src, ast) {
        Ok(ir) => Some(ir),
        Err(error) => {
            println!("{:?}", Report::new(error));
            return None;
        }
    }
}