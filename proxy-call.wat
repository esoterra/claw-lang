(component
  (core module (;0;)
    (type (;0;) (func (param i32) (result i32)))
    (type (;1;) (func (param i32) (result i32)))
    (import "claw" "imported" (func (;0;) (type 0)))
    (func (;1;) (type 1) (param i32) (result i32)
      local.get 0
      call 0
      return
    )
    (export "exported" (func 1))
  )
  (type (;0;) (func (param "a" u32) (result u32)))
  (type (;1;) (func (param "a" u32) (result u32)))
  (import "imported" (func (;0;) (type 0)))
  (core func (;0;) (canon lower (func 0)))
  (core instance (;0;)
    (export "imported" (func 0))
  )
  (core instance (;1;) (instantiate 0
      (with "claw" (instance 0))
    )
  )
  (alias core export 1 "exported" (core func (;1;)))
  (func (;1;) (type 1) (canon lift (core func 1)))
  (export (;2;) "exported" (func 1) (func (type 1)))
)