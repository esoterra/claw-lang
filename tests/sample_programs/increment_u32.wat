(module
   (global $G0 (mut i32) (i32.const 0))
   (func $F0 (result i32)
      (global.set $G0 (i32.add (global.get $G0) (i32.const 1)))
      (return (global.get $G0))
   )
   (export "increment" (func $F0))
)