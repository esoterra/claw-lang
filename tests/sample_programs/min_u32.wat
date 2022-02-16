(module
   (func $F0 (param $L0 i32) (param $L1 i32) (result i32)
      (i32.lt (local.get $L0) (local.get $L1))
      (if
         (return (local.get $L0))
      end)
      (return (local.get $L1))
   )
   (export "min" (func $F0))
)