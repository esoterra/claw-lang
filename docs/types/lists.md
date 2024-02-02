# Lists

The `list<T>` type is a generic sequential container value type.

## Constructing

Lists can be created using `list` literals.

```rust
let list-1: list<string> = ["a", "b", "c"];
let list-2: list<f32> = [1.2, 4, 0, 5.6];
let list-3: list<u8> = [97, 98, 99];
```

Lists of `u8` (i.e. bytes) can be constructed using the raw string literal.

```rust
let l: list<u8> = r"abc";
assert!(l == [97, 98, 99]);
```

## Destructuring

List destructuring binds two names:
* The first element in the list as `option<T>`
* The rest of the list as `list<T>`

Examples
```rust
// Empty list
let [first, ..rest] = [];
assert!(first == none);
assert!(rest == []);

// List with one element
let [first, ..rest] = ["foo"];
assert!(first == some("foo"));
assert!(rest == []);

// List with multiple elements
let [first, ..rest] = ["foo", "bar", "biz", "baz"];
assert!(first == some("foo"));
assert!(rest == ["bar", "biz", "baz"]);
```

## Operators

### Indexing

Lists can be indexed using square brackets.

Traps if index is out of bounds.

```rust
let foo: list<u8> = [5, 1, 4, 3, 2, 6];
assert!(foo[2] == 4);
```

### Slicing

Lists can be "sliced" to create a new `list` value that is a subsequence of the input `list`.

Traps if slice is out of bounds.

```rust
let foo: list<u8> = [5, 1, 4, 3, 2, 6];
assert!(foo[2: 3] == [4, 3, 2]);
```

### Concatenation

Lists can be concatenated together to form a new `list` using `+`.

```rust
let foo: list<u8> = [0, 1] + [2, 3];
assert!(foo == [0, 1, 2, 3]);
```

## Methods

### `is-empty`

Type: `func() -> bool`

Returns `true` if this `list` has length zero.

```rust
assert!([].is-empty() == true);
assert!([5, 2].is-empty() == false);
```

### `length`

Type: `func() -> u32`

```rust
assert!([8, 7, 4, 5].len() == 4);
```

### `get`

Type: `func(i: u32) -> option<T>`

Examples
```rust
let foobar: list<string> = ["foo", "bar"];
assert!(foobar.get(1) == some("bar"));
assert!(foobar.get(100) == none);
```

### `get-slice`

Type: `func(i: u32, length: u32) -> option<list<T>>`

```rust
let foo: list<f32> = [2.4, 1, 63, 0.8];
assert!(foo.get-slice(1, 2) == some([1, 63]));
assert!(foo.get-slice(3, 2) == none);
```

### `contains`

Type: `func(value: T) -> bool`

```rust
assert!([5, 2, 6].contains(6));
```

### `index-of`

Type: `func(value: T) -> option<u32>`

```rust
let foo: list<string> = ["B", "A", "C"];
assert!(foo.index-of("A") == some(1));
assert!(foo.index-of("D") == none);
```

### `begins-with`

Type: `func(prefix: list<T>) -> bool`

```rust
let foo: list<u16> = [8, 7, 4, 5];
assert!(foo.begins-with([8, 7]) == true);
assert!(foo.begins-with([7, 8]) == false);
```