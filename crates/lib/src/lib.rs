use claw_codegen::CodeGenerator;
use claw_common::{make_source, OkPretty};
use claw_parser::{parse, tokenize};
use claw_resolver::resolve;

pub fn compile(source_name: String, source_code: &str) -> Option<Vec<u8>> {
    let src = make_source(source_name.as_str(), source_code);

    let tokens = tokenize(src.clone(), source_code).ok_pretty()?;

    let ast = parse(src.clone(), tokens).ok_pretty()?;

    let resolved = resolve(src, ast).ok_pretty()?;

    let gen = CodeGenerator::default();
    gen.generate(&resolved).ok_pretty()
}