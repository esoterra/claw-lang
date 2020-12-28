# WASM Docs

 * [WASM Roadmap](https://webassembly.org/roadmap/)
 * [WASM Spec](https://webassembly.github.io/spec/core/)
   * [List of All Instructions](https://webassembly.github.io/spec/core/valid/instructions.html)
   * [Definitions of Numeric Operations](https://webassembly.github.io/spec/core/exec/numerics.html)
   * [Functions](https://webassembly.github.io/spec/core/valid/modules.html#functions)
 * [WASM Spec Repo](https://github.com/WebAssembly/spec)
 * [Reference Types Overview](https://github.com/WebAssembly/reference-types/blob/master/proposals/reference-types/Overview.md)

# Articles
 * [Multi-Value All the WASM!](https://hacks.mozilla.org/2019/11/multi-value-all-the-wasm/)

# Ecosystem
 * [List of Rust + WASM crates](https://rustwasm.github.io/book/reference/crates.html)
 * [The WebAssembly Binary Toolkit](https://github.com/WebAssembly/wabt)

# Libraries Used
 * [logos](https://crates.io/crates/logos) - Fast lexer / tokenizer library
 * [codespan](https://crates.io/crates/codespan) - Library for representing spans of code
 * [codespan-reporting](https://crates.io/crates/codespan-reporting) - Library for creating pretty diagnostics using code snippets

 * [parity-wasm](https://crates.io/crates/parity-wasm) - Library for creating or reading WASM modules
   * [parity_wasm::elements::Module](https://docs.rs/parity-wasm/0.42.1/parity_wasm/elements/struct.Module.html) - The WASM module struct
