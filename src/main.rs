use std::{path::PathBuf, sync::Arc};

use clap::{Parser, ArgEnum};

pub mod ast;
pub mod ir;
pub mod lexer;
pub mod parser;
pub mod resolver;

use lexer::tokenize;
use miette::{Report, NamedSource};
use parser::parse;
use resolver::resolve;

#[derive(Parser, Debug)]
struct Arguments {
    #[clap(subcommand)]
    command: Command
}

#[derive(Parser, Debug)]
enum Command {
    Compile(Compile),
    Check(Check)
}

#[derive(Parser, Debug)]
struct Compile {
    #[clap(long)]
    input_path: PathBuf

}

#[derive(Parser, Debug)]
struct Check {
    #[clap(long)]
    input_path: PathBuf,
    #[clap(long, arg_enum)]
    phase: Phase
}

#[derive(ArgEnum, Clone, Debug)]
enum Phase {
    Lex,
    Parse
}

fn main() {
    let args = Arguments::parse();

    match args.command {
        Command::Check(Check { input_path, phase: Phase::Lex }) => {
            if let Some(_) = check_lex(input_path) {
                println!("Finished");
            } else {
                println!("Ended before finishing");
            }
        },
        Command::Compile(Compile { input_path }) => {
            let ir = compile(input_path);
            println!("Result: {:#?}", ir);
        }
        _ => println!("Not yet implemented!")
    }
}

fn check_lex(input_path: PathBuf) -> Option<()> {
    let file_name = input_path.file_name()?.to_string_lossy().to_string();
    let file_string = std::fs::read_to_string(input_path).ok()?;
    let src = Arc::new(NamedSource::new(file_name, file_string.clone()));

    match tokenize(src, file_string) {
        Ok(tokens) => {
            for token_data in tokens {
                println!("At {} matched {:?}", token_data.span.offset(), token_data.token);
            }
        },
        Err(errors) => {
            for error in errors {
                println!("{:?}", Report::new(error));
            }
        }
    }

    Some(())
}

fn compile(input_path: PathBuf) -> Option<ir::Module> {
    let file_name = input_path.file_name()?.to_string_lossy().to_string();
    let file_string = std::fs::read_to_string(input_path).ok()?;
    let src = Arc::new(NamedSource::new(file_name, file_string.clone()));

    let tokens = match tokenize(src.clone(), file_string) {
        Ok(token_data) => token_data,
        Err(errors) => {
            for error in errors {
                println!("{:?}", Report::new(error));
            }
            return None;
        }
    };

    let ast = match parse(src, tokens) {
        Ok(module) => module,
        Err(error) => {
            println!("{:?}", Report::new(error));
            return None;
        }
    };

    let resolved = match resolve(ast) {
        Ok(ir) => ir,
        Err(error) => {
            println!("{:?}", Report::new(error));
            return None;
        }
    };

    Some(resolved)
}

