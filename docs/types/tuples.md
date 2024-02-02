# Tuples

* Immutable `tuple` type
* Tuple field types defined using generic parameters
    * e.g. `tuple<s32, string>`, `tuple<u8, u8, foo>`
* Tuple field types are heterogeneous


* Wave-style literal expression
    * e.g. `(-1, "foo")`, `(0, 2, { x: 1, y: 3 })`
* Dot-based field access expression
    * Similar to records
    * Uses numbers for field names
    * e.g. `my-bar.0`, `my-bar.1`, etc.