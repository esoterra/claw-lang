# Records

Records are collections of named fields.
Those fields may have different types.

## Type Definition

```rust
record foo {
    x: s32,
    y: s32
}
```

## Constructing

```rust
let f: foo = { x: 1, y: 3 };
```

## Destructuring

```rust
let { x, y } = my-foo;
let { x as x2, y } = my-foo;
```

## Operators

### Field Access

```rust
assert!(foo.x == 1);
```
