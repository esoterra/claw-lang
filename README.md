# Component Language for Wasm (CLAW)

Claw is a programming language that compiles to Wasm Components.
Values in Claw have the exact same types as Component model values and the imports/exports of a Claw source file represent a Component "World".

This means that there's no bindings generators or indirection required.
You can receive component values as arguments, operate on them, create them, and return them.

```js
let mut counter: s64 = 0;

export func increment() -> s64 {
    counter = counter + 1;
    return counter;
}

export func decrement() -> s64 {
    counter = counter - 1;
    return counter;
}
```

(support for the full range of component model values is still a WIP)

## Use Cases & Goals

### Component Testing

Claw's ability to define component imports and simple logic easily will be well suited for writing Component tests.

```js
import add: func(lhs: s32, rhs: s32) -> s32;

export func test() -> result<(), string> {
    if add(1, 1) == 2 {
        return ok(());
    } else {
        return err("test failed");
    }
}
```

By adding a `check!(...)` builtin that returns `ok(())` when the condition is true and `err("<nice message>")` when its false
and a Rust-style `?` early return operator, we can make writing these tests a lot easier and make the output much better.

```js
import add: func(lhs: s32, rhs: s32) -> s32;

export tests: interface {
    func test() -> result<(), string> {
        check!(add(1, 1) == 2)?;
        ...
        return ok(());
    }

    ...
}
```

### Adapters & Polyfills

Sometimes users will have components written for one world but want to run them in another.

Claw could make it easy to write simple adapters or polyfills so that users can run their existing code more places.

### Virtualizations & Mocks

With components, we can achieve an incredible local dev experience where resources like file systems and key value stores
can be implemented as simple components and used to run applications for testing and development.

Claw can be well suited to writing simple in-memory virtualizations that make testing and development easy.

### Extensions

Some applications (e.g. [database](https://docs.singlestore.com/cloud/reference/code-engine-powered-by-wasm/create-wasm-udfs/))
can already be extended using Wasm and as this becomes more common users may want to write small pieces of logic that act as filters or policy,
define how to process events, or implement missing math or domain functions.

Claw can make writing these extensions easy while still generating really small Components that can stored, transmitted, and instantiated quickly.

### Simple Services

TODO
