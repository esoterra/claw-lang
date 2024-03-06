use claw_codegen::generate;
use claw_common::{make_source, OkPretty};
use claw_parser::{parse, tokenize};
use claw_resolver::{resolve, wit::ResolvedWit};
use wit_parser::Resolve;

pub fn compile(source_name: String, source_code: &str, wit: Resolve) -> Option<Vec<u8>> {
    let src = make_source(source_name.as_str(), source_code);

    let tokens = tokenize(src.clone(), source_code).ok_pretty()?;

    let ast = parse(src.clone(), tokens).ok_pretty()?;

    let wit = ResolvedWit::new(wit);

    let resolved = resolve(src, ast, wit).ok_pretty()?;

    generate(&resolved).ok_pretty()
}
