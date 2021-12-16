
use std::{path::PathBuf, rc::Rc};

use clap::Clap;

pub mod ast;
pub mod lexer;
pub mod parser;

use lexer::tokenize;
use miette::NamedSource;

#[derive(Clap, Debug)]
struct Arguments {
    #[clap(subcommand)]
    command: Command
}

#[derive(Clap, Debug)]
enum Command {
    Compile(Compile),
    Check(Check)
}

#[derive(Clap, Debug)]
struct Compile {
    #[clap(long)]
    input_path: PathBuf

}

#[derive(Clap, Debug)]
struct Check {
    #[clap(long)]
    input_path: PathBuf,
    #[clap(long, arg_enum)]
    phase: Phase
}

#[derive(Clap, Debug)]
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
        }
        _ => println!("Not yet implemented!")
    }
}

fn check_lex(input_path: PathBuf) -> Option<()> {
    let file_name = input_path.file_name()?.to_string_lossy().to_string();
    let file_string = std::fs::read_to_string(input_path).ok()?;
    let src: NamedSource = NamedSource::new(file_name, file_string.clone());

    match tokenize(Rc::new(src), file_string) {
        Ok(tokens) => {
            for token_data in tokens {
                println!("At {} matched {:?}", token_data.span.offset(), token_data.token);
            }
        },
        Err(errors) => {
            for error in errors {
                println!("{}", error);
            }
        }
    }

    Some(())
}

