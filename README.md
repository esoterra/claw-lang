# Wrought

The Wrought programming language is an imperative expression language designed to directly match the semantics of WebAssembly.
Its syntax is based on the Rust programming language with deviations to simplify lexical analysis and model WASM more closely.
Wrought offers a small subset of what is available in major multi-paradigm languages and is not designed for large scale production usage.

For more information visit https://wrought.cc

# Wrought Tooling

This crate contains all of the core Wrought language command line tooling.
For information about the commands and how to write them, run the help utility using `-h`.

The compiler is under development and it is not yet possible to compile Wrought files to WebAssembly.

Until then, additions will be incrementally be added to the check command which runs the compiler phases up to and including the one specified outputting any error information and dumping the output of the last phase if successful.

# Core Dependencies

Wrought's tooling is implemented using the following libraries. It would not have been made without them.

* `clap` - command line parsing
* `miette` - for formatting diagnostics
* `logos` - the lexer / tokenizer generator