use std::{path::PathBuf, sync::Arc, fs};

use clap::{Parser, ArgEnum};

pub mod ast;
pub mod gen;
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
    #[clap(short, long)]
    input: PathBuf,
    #[clap(short, long)]
    output: PathBuf,
    #[clap(short, long, arg_enum)]
    format: Format
}

#[derive(Debug, ArgEnum, Clone, Copy)]
enum Format {
    Wasm,
    WAT
}

impl Compile {
    fn run(&self) -> Option<()> {
        let file_name = self.input.file_name()?.to_string_lossy().to_string();
        let file_string = std::fs::read_to_string(&self.input).ok()?;
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
    
        let ast = match parse(src.clone(), tokens) {
            Ok(ast) => ast,
            Err(error) => {
                println!("{:?}", Report::new(error));
                return None;
            }
        };
    
        let resolved = match resolve(src, ast) {
            Ok(ir) => ir,
            Err(error) => {
                println!("{:?}", Report::new(error));
                return None;
            }
        };
    
        match self.format {
            Format::Wasm => {
                let wasm = gen::generate_wasm(resolved);
                match fs::write(&self.output, wasm) {
                    Ok(_) => println!("Done"),
                    Err(err) => println!("Error: {:?}", err),
                }
            },
            Format::WAT => {
                let wat = gen::generate_wat(resolved);
                match fs::write(&self.output, wat) {
                    Ok(_) => println!("Done"),
                    Err(err) => println!("Error: {:?}", err),
                }
            },
        }
    
        Some(())
    }
}

#[derive(Parser, Debug)]
struct Check {
    #[clap(long)]
    input: PathBuf,
    #[clap(long, arg_enum)]
    phase: Phase
}

#[derive(ArgEnum, Clone, Debug)]
enum Phase {
    Lex,
    Parse
}

impl Check {
    fn run(&self) -> Option<()> {
        match self.phase {
            Phase::Lex => self.check_lex(),
            Phase::Parse => unimplemented!(),
        }
    }

    fn check_lex(&self) -> Option<()> {
        let file_name = self.input.file_name()?.to_string_lossy().to_string();
        let file_string = std::fs::read_to_string(&self.input).ok()?;
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
    
}

fn main() {
    let args = Arguments::parse();

    match args.command {
        Command::Check(check) => check.run(),
        Command::Compile(compile) => compile.run()
    };
}

