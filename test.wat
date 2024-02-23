(component
  (core module (;0;)
    (type (;0;) (func (param i32 i32 i32 i32) (result i32)))
    (type (;1;) (func))
    (func $realloc (;0;) (type 0) (param $old_ptr i32) (param $old_size i32) (param $align i32) (param $new_size i32) (result i32)
      (local $ret i32)
      local.get $old_ptr
      if ;; label = @1
        local.get $old_size
        local.get $new_size
        i32.gt_u
        if ;; label = @2
          local.get $old_ptr
          return
        end
      end
      global.get $last
      local.get $align
      i32.const -1
      i32.add
      i32.add
      local.get $align
      i32.const -1
      i32.add
      i32.const -1
      i32.xor
      i32.and
      global.set $last
      global.get $last
      local.set $ret
      global.get $last
      local.get $new_size
      i32.add
      global.set $last
      loop $loop ;; label = @1
        memory.size
        i32.const 65536
        i32.mul
        global.get $last
        i32.lt_u
        if ;; label = @2
          i32.const 1
          memory.grow
          i32.const -1
          i32.eq
          if ;; label = @3
            unreachable
          end
          br 1 (;@1;)
        end
      end
      local.get $ret
      i32.const 222
      local.get $new_size
      memory.fill
      local.get $old_ptr
      if ;; label = @1
        local.get $ret
        local.get $old_ptr
        local.get $old_size
        memory.copy
      end
      local.get $ret
    )
    (func $clear (;1;) (type 1)
      i32.const 8
      global.set $last
    )
    (memory $memory (;0;) 1)
    (global $last (;0;) (mut i32) i32.const 8)
    (export "memory" (memory $memory))
    (export "realloc" (func $realloc))
    (export "clear" (func $clear))
  )
  (core module (;1;)
    (type (;0;) (func (param i32 i32 i32 i32) (result i32)))
    (type (;1;) (func (param i32 i32) (result i32)))
    (import "alloc" "memory" (memory (;0;) 1))
    (import "alloc" "realloc" (func (;0;) (type 0)))
    (func (;1;) (type 1) (param i32 i32) (result i32)
      (local i32 i32 i32)
      i32.const 0
      i32.const 0
      i32.const 4
      i32.const 8
      call 0
      local.set 2
      local.get 0
      local.set 3
      local.get 1
      local.set 4
      local.get 2
      i32.const 0
      i32.add
      local.get 3
      i32.store align=16
      local.get 2
      i32.const 4
      i32.add
      local.get 4
      i32.store align=16
      return
    )
    (export "identity" (func 1))
  )
  (core instance (;0;))
  (core instance (;1;) (instantiate 0))
  (core instance (;2;) (instantiate 1
      (with "claw" (instance 0))
      (with "alloc" (instance 1))
    )
  )
  (alias core export 1 "memory" (core memory (;0;)))
  (alias core export 1 "realloc" (core func (;0;)))
  (alias core export 2 "identity" (core func (;1;)))
  (type (;0;) (func (param "s" string) (result string)))
  (func (;0;) (type 0) (canon lift (core func 1) (memory 0) (realloc 0)))
  (export (;1;) "identity" (func 0) (func (type 0)))
)