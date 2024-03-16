use std::{env, fs, path::Path};
use wat;

fn main() {
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("allocator.wasm");

    let wat = include_str!("./allocator.wat");
    let wasm = wat::parse_str(wat).unwrap();

    fs::write(&dest_path, wasm).unwrap();
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=allocator.wat");
}
