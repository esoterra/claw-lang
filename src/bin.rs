use std::{fs, path::PathBuf, sync::Arc};

use clap::{ArgEnum, Parser};

use claw::lexer::tokenize;
use claw::parser::parse;
use claw::resolver::resolve;
use miette::{NamedSource, Report};

#[derive(Parser, Debug)]
struct Arguments {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Parser, Debug)]
enum Command {
    Compile(Compile),
    Check(Check),
}

#[derive(Parser, Debug)]
struct Compile {
    #[clap(short, long)]
    input: PathBuf,
    #[clap(short, long)]
    output: PathBuf,
}

#[derive(Debug, ArgEnum, Clone, Copy)]
enum Format {
    Wasm,
    WAT,
}

impl Compile {
    fn run(&self) -> Option<()> {
        let file_name = self.input.file_name()?.to_string_lossy().to_string();
        let file_string = std::fs::read_to_string(&self.input).ok()?;
        let src = Arc::new(NamedSource::new(file_name, file_string.clone()));

        let tokens = match tokenize(src.clone(), &file_string) {
            Ok(token_data) => token_data,
            Err(error) => {
                println!("{:?}", Report::new(error));
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

        let generator = claw::codegen::CodeGenerator::default();
        let wasm = generator.generate(&resolved);
        match fs::write(&self.output, wasm) {
            Ok(_) => println!("Done"),
            Err(err) => println!("Error: {:?}", err),
        }

        Some(())
    }
}

#[derive(Parser, Debug)]
struct Check {
    #[clap(long)]
    input: PathBuf,
    #[clap(long, arg_enum)]
    phase: Phase,
}

#[derive(ArgEnum, Clone, Debug)]
enum Phase {
    Lex,
    Parse,
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

        match tokenize(src, &file_string) {
            Ok(tokens) => {
                for token_data in tokens {
                    println!(
                        "At {} matched {:?}",
                        token_data.span.offset(),
                        token_data.token
                    );
                }
            }
            Err(error) => {
                println!("{:?}", Report::new(error));
            }
        }

        Some(())
    }
}

fn main() {
    let args = Arguments::parse();

    match args.command {
        Command::Check(check) => check.run(),
        Command::Compile(compile) => compile.run(),
    };
}
