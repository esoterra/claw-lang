(module
   (func $F0 (param i64) (param i64) (param i64) (param i64) (result i64)
      (return (i64.add (i64.add (i64.mul (i64.mul (local.get $L0) (local.get $L3)) (local.get $L3)) (i64.mul (local.get $L1) (local.get $L3))) (local.get $L2)))
   )
   (export "quad" (func $F0))
)