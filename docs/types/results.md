# Results

* A `result` value type
* Takes two type parameters for the ok and erro types
    * e.g. `result<s32, string>` has ok type `s32` and error type `string`

* Wave-style literal expressions
    * e.g. `let r: result<s32, u8> = ok(1);`
    * e.g. `let r: result<f32, string> = err("foobar");`