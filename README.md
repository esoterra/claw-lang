<div align="center">
  <h1><code>claw-cli</code></h1>

  <p>
    <strong>The compiler for the Claw programming language</strong>
  </p>

  <p>
    <a href="https://github.com/esoterra/claw-lang/actions?query=workflow%3ACI"><img src="https://github.com/esoterra/claw-lang/workflows/CI/badge.svg" alt="build status" /></a>
    <a href="https://crates.io/crates/claw-cli"><img src="https://img.shields.io/crates/v/claw-cli.svg?style=flat-square" alt="Crates.io version" /></a>
    <a href="https://crates.io/crates/claw-cli"><img src="https://img.shields.io/crates/d/claw-cli.svg?style=flat-square" alt="Download" /></a>
    <a href="https://docs.rs/claw-cli"><img src="https://img.shields.io/badge/docs-latest-blue.svg?style=flat-square" alt="docs.rs docs" /></a>
  </p>
</div>

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

With components, we can achieve an incredible local dev experience where resources like message buses and key value stores
can be implemented as simple components and used to run applications for testing and development.

Claw can be well suited to writing simple in-memory virtualizations that make testing and development easy.

### Extensions

Some applications (e.g. [database](https://docs.singlestore.com/cloud/reference/code-engine-powered-by-wasm/create-wasm-udfs/))
can already be extended using Wasm and as this becomes more common users may want to write small pieces of logic that act as filters or policy,
define how to process events, or implement missing math or domain functions.

Claw can make writing these extensions easy while still generating really small Components that can stored, transmitted, and instantiated quickly.

### Simple Services

TODO

## Relationship with Other Projects

There are several projects for representing different aspects of the Component Model

* [WIT](https://github.com/WebAssembly/component-model/blob/main/design/mvp/WIT.md) - The official IDL for the Component Model
* [WAC](https://github.com/peterhuene/wac/) - An extension of WIT that adds the ability to define how to wire components together
* [WAVE](https://github.com/lann/wave) - A format for encoding Component-Model values in an idiomatic json-like way

Claw will use WIT syntax for defining types, WAC syntax for defining composition, and WAVE syntax for literal expressions
combining them all together so that it's intuitive to use these different tools.

![image](https://github.com/esoterra/claw-lang/assets/3458116/de0673f1-7b92-48c6-b1c3-e52479797778)

