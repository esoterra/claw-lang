# Resources

Owned handles - `file`

Borrowed handles - `borrow<file>`

```rust
resource file {
    self: record {
        cursor: u32,
        contents: list<u8>
    }

    func read(n: u32) -> list<u8> {
        let next = self.contents[self.cursor..self.cursor+n];
        self.cursor += n;
        return next;
    }
}

func foo() {
    let f = new file { cursor: 0, contents: r"foobar" };
}
```

## Type

```rust
resource file {
    read: func(n: u32) -> list<u8>;
}
```

## Definition

Resource definitions are like resource types except that they provide
* a representation for the resource and
* function definitions (methods) instead of function types.

```rust
resource foo {
    self: list<string>;

    func bar() -> u32 { ... }
}
```

### Methods

Methods are like any other function, except that `self` is bound to the current resource's representation.

This acts both as an expressoin (e.g. `self.x + 2`) and can be used as the place for an assignment (e.g. `self.x = 3;`).
