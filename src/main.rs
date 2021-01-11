
use std::path::PathBuf;

extern crate clap;
use clap::Clap;

pub mod ast;
pub mod lexer;
pub mod parser;

#[derive(Clap, Debug)]
struct Arguments {
    #[clap(subcommand)]
    command: Command
}

#[derive(Clap, Debug)]
enum Command {
    Compile {
        #[clap(long)]
        input_path: PathBuf
    }
}

fn main() {
    let args = Arguments::parse();

    match args.command {
        Command::Compile { input_path } => {
            println!("Compiling {:?}", input_path);
        }
    }
}
