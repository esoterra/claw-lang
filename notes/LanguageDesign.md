
# Integer Types

The integer types in WebAssembly are the following
 * Signed Integers (s*nn*): `s32`, `s64`
 * Unsigned Integers (u*nn*): `u32`, `u64`
 * Integers (i*nn*): `i32`, `i64` (no sign information)


Standard arithmetic operations on integers tend to operate on `i32` or `i63` as signedness does not impact the way that they operate.

Conventional programming language models all track signedness information for integers statically, because it is common for programmers to expect a specific signedness. Wallrs would follow this convention and store, propagate, and validate signed integer types in expressions.

A programming model for this system should
 * allow casting from s*nn* or u*nn* integers to i*nn* integers implicitly,
 * allow casting between s*nn* and u*nn* integers explicitly,
 * validate that the type inputs of arithmetic expressions match,

# Floating-Point Types

WebAssembly provides a 32 and 64 bit floating point type.
These types support all of the standard operations.

# Variables

## Globals

WebAssembly allows modules to define mutable and constant global variables of the different numeric types.
There are instructions for storing to and reading from these variables and they can be represented by standard assignment and referencing.

## Locals

# Memory Operations and Pointers

WASM supports loads from and stores to linear memories that are defined as sequences of bytes.

## Transliteration
Here is a direct conversion of the supported memory loads and stores into Rust-like pseudocode.

The offset and alignment are separate from the arguments using a bracketed "generics" syntax because this information must be determined statically. The offset is a base value that the address is added to, and the align is a hint to the runtime about how the memory is aligned.
```rust
// Store some or all of an i32 value, the sign is irrelevant
fn i32.store_32<offset: u32, align: u32>(address: u32, value: i32)
fn i32.store_16<offset: u32, align: u32>(address: u32, value: i32)
fn i32.store_8<offset: u32, align: u32>(address: u32, value: i32)

// Store some or all of an i64 value, the sign is irrelevant
fn i64.store_64<offset: u32, align: u32>(address: u32, value: i64)
fn i64.store_32<offset: u32, align: u32>(address: u32, value: i64)
fn i64.store_16<offset: u32, align: u32>(address: u32, value: i64)
fn i64.store_8<offset: u32, align: u32>(address: u32, value: i64)

// Load an integer as its exact size
fn i32.load_32<offset: u32, align: u32>(address: u32) -> i32
fn i64.load_64<offset: u32, align: u32>(address: u32) -> i64

// Loading a smaller vallue into a 32-bit integer (requires signedness)
fn u32.load_16<offset: u32, align: u32>(address: u32) -> u32
fn u32.load_8<offset: u32, align: u32>(address: u32) -> u32
fn i32.load_16<offset: u32, align: u32>(address: u32) -> i32
fn i32.load_8<offset: u32, align: u32>(address: u32) -> i32

// Loading a smaller vallue into a 64-bit integer (requires signedness)
fn u64.load_32<offset: u32, align: u32>(address: u32) -> u64
fn u64.load_16<offset: u32, align: u32>(address: u32) -> u64
fn u64.load_8<offset: u32, align: u32>(address: u32) -> u64
fn i64.load_32<offset: u32, align: u32>(address: u32) -> i64
fn i64.load_16<offset: u32, align: u32>(address: u32) -> i64
fn i64.load_8<offset: u32, align: u32>(address: u32) -> i64
```

While correct, these transliterated memory functions are not particularly ergonomic nor do they look the way people expect to be able to interact with memory.

Particularly the statically determined offset and alignment are not something users will want to provide manually.

**Note:** In the current version of WASM, a module is only allowed to define **or** import one linear memory, and all memory ops are implicitly associated with that memory. For this reason, the specific memory is not provided as an argument to memory operations.

## Pointer Types
Instead of implementing each of the above functions wallrs will introduce its own "pointer" type whose type information contains the offset and the alignment used for load and store operations. When/if WASM is expanded to allow multiple memories in the same module, this too would be identified by the type.

Pointer types will use square brackets to enclose their parameters.
This simplifies lexical analysis, which no longer needs to handle the conflicting usages of `<` and `>` caused by operators `<=` and parameter groups `<K,V>`.

```rust
// A pointer type which uses base offset of 0 and has alignment value of 2
Ptr[u32,0,2]
// It may be acceptable to infer the alignment to match the type being used
// In this case, u32 is 4 bytes so the alignment of 2 (2^2 == 4) can be omitted
Ptr[u32,0]
// A pointer without an offset could be inferred to use the offset of zero
// allowing this short-form, which is then very familiar to c or rust programmers.
Ptr[u32]
```

 * A load from or store to a pointer would implicitly use the statically known offset and alignment from the pointers type.
 * Pointers would know what type they point to (e.g. u8, u16, u32, u64) and so would know which load instruction to use to load from or store to it.
 * Pointer loads and stores still have to return 32 or 64 bit values and infer the correct type from the context.
 * Pointers would still be stored as simple `i32` values representing a byte index and casting them to other pointer values or to `i32` would not change the underlying value.
 * Pointers would know what size they are and thus could support c-like pointer arithmetic where a `u32`-pointer plus one has a byte index four larger.
 * Pointers could support c-like indexing which simply performs pointer arithmetic and dereferences/references based on context.

```rust
// We can then support standard malloc, calloc, and free functions using these types.
fn malloc_u32() -> Ptr[u32] { ... }
fn calloc_u32(n: u32) -> Ptr[u32] { ... }
fn free_u32(ptr: Ptr[u32]) { ... }

// A function which returns a pointer to a region of memory n u32s long that contain increasing integers from 0 to n-1.
fn increasing_vals(n: u32) -> Ptr[u32] {
    let ptr: Ptr[u32] = calloc_u32(n);

    for i in 0..n {
        ptr[i] = i;
    }

    ptr
}
```

## Slice Types

Since WASM Multi-Value has been standardized, we are able to return multiple values from a function (plus some other goodies). This means that it is possible to return an address, length pair from functions and makes it possible for us to pass around tuples of form (`i32`, `i32`) to represent slices of memory.

```rust
fn alloc_u32() -> Ptr[u32] { ... }
fn alloc_u32_slice(n: u32) -> Slice[u32] { ... }
fn free_u32(ptr: Ptr[u32]) { ... }
fn free_u32_slice(ptr: Slice[u32]) { ... }
```

Slices would offer indexing and use as an iterable type as in the below.

```rust
fn zero_slice(slice: Slice[u32]) {
    for ptr in slice {
        *ptr = 0;
    }
}
```

The existence of slices may preclude the need for indexing and arithmetic on pointer types, which no longer need to do the double job of pointing to singe instances and contiguous groups of values.

## Struct Types

Structs in wallrs have two distinct representations:
 * a stack representation, which is a series of scalar values;
 * and a memory representation, which is a series of scalar values, gaps, and structs.
 
This difference is due to the fact that we can store inner structs together in memory but not on the stack and that it doesn't make sense to store "gaps" on the stack, since we cannot reference it anyway in WebAssembly.

```rust
struct MyType {
    alice: u8
    bob: u16,
    charlie: u32
}

// Stack Representation (u8, u16, u32) -> (alice, bob, charlie)

// Memory layout (8 bytes)
// (alice, ---, bob[0], bob[1], charlie[0], charlie[1], charlie[2], charlie[3])
```

Struct pointers (e.g. `Ptr[MyType]`) are also stored as `i32` byte offsets into memory and preserve alignment, offset, and memory type information. Attribute access on a struct pointer (`my_ptr.foo`) has a result type of pointer to field type (`Ptr[u32]`) with alignment based on struct layout and parent, and an offset and memory matching the parent.

### Structs of Structs
The stack representation of a struct of structs is generated by interpolating the inner structs representation into the parent.
The memory representation of a struct of structs treats the entire inner struct as a unit and determines where to place it by alignment.

### Structs of Slices
A slice is simply a struct with the following structure definition which happens to support indexing and iteration. It is treated like any other struct for stack and memory representation.

```rust
struct Slice[T,...] {
    start: i32,
    end: i32
}
```

# Control Flow

# Tables

In WebAssembly 


