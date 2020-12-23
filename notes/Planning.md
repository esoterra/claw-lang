# Introduction

This project is to create a WebAssembly Language Like Rust (wallrs) that can be used to learn, experiment with, or develop WebAssembly modules.

# Goals

## 1. Match the semantics of WebAssembly modules
Using most languages to create WebAssembly modules involves some combination of defining imports and webassembly features as Foreign Function Interfaces or re-mapping the system calls and standard facilities of the language to WASM features.
The compilers and build tools for these languages targetting WASM necessarily incoporate the complexity of
 * the language itself,
 * the interface with "all" systems or compilation outputs it can produce,
 * the WASM specification/format.
By corresponding and compiling directly to the semantics of WASM, complexity in wallrs is reduced to the bare minimum in all of the above categories.

1. Each wallrs source code file can be deterministically converted into a single WASM module.
2. Basic wallrs code corresponds directly to WASM, specifying and using imports, exports, functions, tables, and indices directly.
3. The language will be an expression language in the style of rust and will offer infix numeric operations with the typical operator precedence that are directly converted into their equivalent WASM representation.
4. Either WASM control expressions/instructions or wrappers/abstractions over them will be available for control flow

## 2. Offer syntactic sugars to make coding more ergonomic
While there exists two exising text encodings of WASM in WAT and WAST, neither of these formats is easy-to-read or quick / productive to write in.
Without creating semantic mismatch, wallrs will introduce sugars and features that make it easier to read and write.

1. Wallrs source code may support 3-statement for loops, for-in loops, switch/match, or other control expressions/statements as needed.
2. WASM instrutions, blocks, and sections that take indices will accept expressions of the corresponding type (e.g. typeidx) will accept expressions or identifiers of the type being indexed.
   1. When an expression is provided instead of an index, the expression will be added to the appropriate table (and interned to remove duplicates) and replaced with its index.
   2. When an identifier is provided instead of an index, the identifier will be resolved to an index and replaced with it

## 3. Allow for inlining/folding other wallrs or WASM files into the generated WASM binary
Even for very trivial projects, it becomes infeasible to include the logic for the entire binary in the source code of your file.
C programmers are not known to write and include allocators or string code in simple programs source.
For this reason, wallrs will support linking to and folding external modules both wallrs or WASM into a parent wallrs file.
This will not match the semantics of "import" from the C programming language and will not copy-and-paste the contents into your binary,
but will instead require the user to specify how the parent or its imports satisfy the childs imports and how the childs exports are used within the parent or exported from it.

1. Users may identify a wallrs or WASM file in the local file-system and indicate that its contents should be added to this compilation unit, providing for the way its imports should be satisfied and its exports used and bound.

# Use Cases

## 1. Writing and compiling trivial examples

When users are learning about WebAssembly, a format like wallrs that is very literal and direct in how it represents logic,
while being easier to understand than WAT/WAST, will provide a good playground for learning and exploring.
This would make wallrs a great choice for the "hello, world!" of the WASM world.

## 2. Writing small WASM modules for performance sensitive domains/problems

In dynamically typed environments with WASM runtimes (e.g. JavaScript and Python) wallrs will enable the creation of small optimized WASM modules for performance sensitive hot-paths.
Using wallrs, the user can avoid the complexity and indirection associated with current building and bundling options for compile-to-WASM languages.

## 3. Reverse engineering WASM modules

De-compiling WASM into wallrs will make it easier to understand what that WASM module did or even make alterations to it.

# Constraints / Decisions

## 1. Be styled after the Rust programming language
 * Rust's imperative expression language syntax matches the semantics/constructs of WASM well
 * The syntax of the C, Rust, Java, etc. imperative structured programming languages is well known

## 2. Use tooling written in the Rust programming language
 * Rust programs can be compiled to native code or WASM
 * Rust code encourages correct and performant practices and idioms
 * The Rust compiler makes it easy to avoid memory errors and undefined behavior

## 3. Use a hand-written recursive-descent parser
 * Recursive descent parsers are easy to write and are well understood
 * Recursive descent parsers do not require adding in additional libraries or frameworks that would add complexity, compile time, and binary size

## 4. Use existing libraries for reading and writing WebAssembly formats (e.g. WASM, WAT, WAST)
 * Avoid needing to re-implement the WASM specification, but instead utilize existing APIs for generating and reading valid WASM files

## 5. Provide access to an existing WASM runtime in the language tooling
 * This prevents users from needing to download another dependency to develop and test Wallrs files
 * Avoids needing to re-implement the execution of the WASM specification
