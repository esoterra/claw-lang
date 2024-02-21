use std::{fs, path::PathBuf, sync::Arc};

use clap::Parser;

use claw_codegen::CodeGenerator;
use claw_common::OkPretty;
use claw_parser::{parse, tokenize};
use claw_resolver::resolve;
use miette::NamedSource;

#[derive(Parser, Debug)]
struct Arguments {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Parser, Debug)]
enum Command {
    Compile(Compile),
}

#[derive(Parser, Debug)]
struct Compile {
    #[clap(short, long)]
    input: PathBuf,
    #[clap(short, long)]
    output: PathBuf,
}

impl Compile {
    fn run(&self) -> Option<()> {
        let file_name = self.input.file_name()?.to_string_lossy().to_string();
        let file_string = std::fs::read_to_string(&self.input).ok()?;
        let src = Arc::new(NamedSource::new(file_name, file_string.clone()));

        let tokens = tokenize(src.clone(), &file_string).ok_pretty()?;

        let ast = parse(src.clone(), tokens).ok_pretty()?;

        let resolved = resolve(src, ast).ok_pretty()?;

        let generator = CodeGenerator::default();
        let wasm = generator.generate(&resolved).ok_pretty()?;

        match fs::write(&self.output, wasm) {
            Ok(_) => println!("Done"),
            Err(err) => println!("Error: {:?}", err),
        }

        Some(())
    }
}

fn main() {
    let args = Arguments::parse();

    match args.command {
        Command::Compile(compile) => compile.run(),
    };
}
