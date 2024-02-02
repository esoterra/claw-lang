# Booleans

The boolean type is named `bool`.

## Constructing

The `bool` type has two literal expressions `true` and `false`.

```rust
if true {
    foo(); // always happens
}
```

## Operators

### `not`

Unary logical inverse operator.

```rust
assert!(not true == false);
assert!(not false == true)
```

### `and`

The logical and operator.

```rust
assert!(true and true == true);
assert!(true and false == false);
assert!(false and true == false);
assert!(false and false == false);
```

### `or`

The logical or operator.

```rust
assert!(true or true == true);
assert!(true or false == true);
assert!(false or true == true);
assert!(false and false == false);
```
