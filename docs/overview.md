# Overview

Claw's semantics, syntax, and language features are designed to extend and be compatible with the emerging Component Model ecosystem.

* Claw's types match the Component Model's types,
* Claw's interface and type syntax match WIT, and
* Claw's expression syntax matches the WAVE value encoding.

What these projects don't provide is a way to implement simple programs and define functions and behavior. Claw provides this missing functionality by extending the design from those projects as intuitively as possible and filling in missing gaps by following established conventions in commonly used programming languages.

## Basic Syntax

* All identifiers formatted as lower snake case
* Package references are formatted as registry ids 
    * Pattern `<namespace>:<package>/<item>@<version>`
    * e.g. `wasi:cli@0.2.0`, `wasi:cli/environment`
* Generics use angle brackets
    * e.g. `list<string>`, `option<u32>`
* Statements end with semicolon (`;`)
* Blocks are delimited with braces (`{ ... }`)
* Documentation comments preceding functions, types, fields, etc. use `///`

Proposed:
* Loose comments in Claw files use `//`

## References
* [Nim Manual](https://nim-lang.org/docs/manual.html)
* [Nim by Example](https://nim-by-example.github.io/for_iterators/)
* [SSA for Functional Programmers](https://wingolog.org/archives/2011/07/12/static-single-assignment-for-functional-programmers)
* [Efects Analysis in Guile](https://wingolog.org/archives/2014/05/18/effects-analysis-in-guile)
* [Efficiently Computing SSA Form and CFG](http://www.cs.cmu.edu/afs/cs/academic/class/15745-s07/www/papers/cytron-efficientSSA.pdf)