use std::{fs, path::PathBuf, sync::Arc};

use clap::Parser;

use claw_codegen::generate;
use claw_common::OkPretty;
use claw_parser::{parse, tokenize};
use claw_resolver::{resolve, wit::ResolvedWit};
use miette::NamedSource;
use wit_parser::Resolve;

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
    #[clap(long)]
    wit: Option<PathBuf>,
    #[clap(short, long)]
    output: PathBuf,
}

impl Compile {
    fn run(self) -> Option<()> {
        let file_name = self.input.file_name()?.to_string_lossy().to_string();
        let file_string = std::fs::read_to_string(&self.input).ok()?;
        let src = Arc::new(NamedSource::new(file_name, file_string.clone()));

        let tokens = tokenize(src.clone(), &file_string).ok_pretty()?;

        let comp = parse(src.clone(), tokens).ok_pretty()?;

        let mut wit = Resolve::new();
        if let Some(wit_path) = self.wit {
            wit.push_path(wit_path).unwrap();
        }
        let wit = ResolvedWit::new(wit);
        let rcomp = resolve(&comp, wit).ok_pretty()?;

        let wasm = generate(&comp, &rcomp).ok_pretty()?;

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
