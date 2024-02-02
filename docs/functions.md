# Functions

* Rust-like function type syntax
    * e.g. `func(i: s32) -> s32`
    * e.g. `func(a: string) -> option<string>`

Proposed:

* Implementation - `func foo(i: s32) -> s32 { ... }`
* Variables
    * Declared using - `let`
        * Initialization required
        * e.g. `let bar = 1 + 2;`
    * Assigned to using pattern `<name> = <expression>;`
        * e.g. `bar = 3;`
* Conditional control flow using pattern `if <cond> { ... }`