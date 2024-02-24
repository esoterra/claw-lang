use wasm_opt::{Feature, OptimizationOptions, ShrinkLevel};
use std::{env, fs};
use uuid::Uuid;

pub fn optimize(input_bytes: Vec<u8>) -> Vec<u8> {
    let id = Uuid::new_v4();
    let dir = env::temp_dir();
    let tmp_input = dir.join(format!("claw.opt.input.{}.wasm", id));
    let tmp_output = dir.join(format!("claw.opt.output.{}.wasm", id));
    fs::write(&tmp_input, &input_bytes).unwrap();
    OptimizationOptions::new_opt_level_2()
        .shrink_level(ShrinkLevel::Level1)
        .enable_feature(Feature::All)
        .run(&tmp_input, &tmp_output)
        .unwrap();
    let bytes = fs::read(&tmp_output).unwrap();
    fs::remove_file(&tmp_input).unwrap();
    fs::remove_file(&tmp_output).unwrap();
    bytes
}
