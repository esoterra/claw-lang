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
