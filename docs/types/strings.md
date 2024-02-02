# Strings

The `string` type is a valid sequence of unicode scalar values encoded in utf-8.

## Constructing

Strings can be created using double-quoted string literals.

```rust
let a: string = "asdf";
let b: string = "foobar";
let c: string = "Lorem ipsum...";
```

## Operators

### Indexing

Strings can be indexed using square brackets.

Traps if index is out of bounds or not at a code point boundary.

```rust
let foo: string = "asdf";
assert!(foo[2] == "d");
```

### Slicing

Strings can be "sliced" to create a new `string` value that is a substring of the input `string`.

Traps if slice is out of bounds or not at a code point boundary.

```rust
let foo: string = "asdfgh";
assert!(foo[2: 3] == "dfg");
```

### Concatenation

Strings can be concatenated together to form a new `string` using `+`.

```rust
let foo: list<u8> = "abc" + "def";
assert!(foo == "abcdef");
```

## Methods

### `is-empty`

Type: `func() -> bool`

Returns `true` if this `string` has length zero.

```rust
assert!("".is-empty() == true);
assert!("abc".is-empty() == false);
```

### `length`

Type: `func() -> u32`

```rust
assert!("abcd".len() == 4);
```

### `get`

Type: `func(i: u32) -> option<T>`

Examples
```rust
let foobar: list<string> = "foobar";
assert!(foobar.get(3) == some("b"));
assert!(foobar.get(100) == none);
```

### `get-slice`

Type: `func(i: u32, length: u32) -> option<string>`

```rust
let foo: string = "lorem ipsum dolor sit amet";
assert!(foo.get-slice(6, 5) == some("ipsum"));
assert!(foo.get-slice(40, 3) == none);
```

### `contains`

Type: `func(value: string) -> bool`

```rust
assert!("asdf".contains("df"));
```

### `index-of`

Type: `func(value: string) -> option<u32>`

```rust
let foo: string = "BAC";
assert!(foo.index-of("A") == some(1));
assert!(foo.index-of("D") == none);
```

### `begins-with`

Type: `func(prefix: string) -> bool`

```rust
let foo: list<u16> = "john smith";
assert!(foo.begins-with("john") == true);
assert!(foo.begins-with("james") == false);
```

### `as-bytes`

Type: `func() -> list<u8>`

```rust
assert!("abc".as-bytes() == [97, 98, 99]);
```
