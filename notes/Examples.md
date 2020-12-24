# Examples

## Counter

A simple example with one exported function that increments and returns a global counter that starts at zero.

### Raw
```wallrs
global counter: mut i32 = 0u32;

table main_table[1] = [increment];

fn increment() -> i32 {
    counter = counter + 1;
    counter
}

export increment: fn() -> i32 = increment;
```

### With Sugar
```wallrs
global counter: mut i32 = 0u32;

table main_table[1];

#[at(main_table[0])]
export fn increment() -> i32 {
    counter = counter + 1;
    counter
}
```

## Hodgepodge

```wallrs
from other import other_func: fn() -> i32;
from other import other_table: table[12];
from other import other_mem: mem[1024];
from other import other_global: mut i64;

export my_func: fn() -> i32 = my_func;
export my_table: table[12] = my_table;
export my_mem: mem[1024] = my_mem;
export my_global: const i64 = my_global;

type Num = i32;

#[start]
fn my_func() {
    counter = counter + 1;
    counter
}

table my_table[10] = [my_func];

mem my_mem[1024];

global my_global: const i64 = 1000i32;
```