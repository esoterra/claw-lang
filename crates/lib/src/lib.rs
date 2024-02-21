use claw_common::{OkPretty, make_source};
use claw_parser::{tokenize, parse};
use claw_resolver::resolve;
use claw_codegen::CodeGenerator;

pub fn compile(source_name: String, source_code: &str) -> Option<Vec<u8>> {
    let src = make_source(source_name.as_str(), source_code);

    let tokens = tokenize(src.clone(), source_code).ok_pretty()?;

    let ast = parse(src.clone(), tokens).ok_pretty()?;

    let resolved = resolve(src, ast).ok_pretty()?;

    let gen = CodeGenerator::default();
    gen.generate(&resolved).ok_pretty()
}
