(module
   (global $G0 (mut i64) (i64.const 0))
   (func $F0 (result i64)
      (global.set $G0 (i64.add (global.get $G0) (i64.const 1)))
      (return (global.get $G0))
   )
   (export "increment" (func $F0))
)