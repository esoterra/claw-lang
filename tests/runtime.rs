use claw::{codegen, compile};

use std::fs;

use wasmtime::component::{bindgen, Component, Linker};
use wasmtime::{Config, Engine, Store};

#[allow(dead_code)]
struct Runtime {
    engine: Engine,
    component: Component,
    linker: Linker<()>,
    store: Store<()>,
}

impl Runtime {
    pub fn new(name: &str) -> Self {
        let path = format!("./tests/programs/{}.claw", name);
        let input = fs::read_to_string(path).unwrap();
        let output = compile(name, input);
        let output = match output {
            Some(output) => output,
            None => panic!("Failed to compile '{}'", name),
        };
        let output = codegen::generate(output);

        let mut config = Config::new();
        config.wasm_component_model(true);
        let engine = Engine::new(&config).unwrap();

        let component = Component::new(&engine, output).unwrap();
        let linker = Linker::new(&engine);
        let store = Store::new(&engine, ());

        Runtime {
            engine,
            component,
            linker,
            store,
        }
    }
}

#[test]
fn test_counter_s64() {
    bindgen!("counter-s64" in "tests/programs");

    let mut runtime = Runtime::new("counter_s64");

    let (counter_s64, _) =
        CounterS64::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    for i in 1..200 {
        // Increase by one
        assert_eq!(counter_s64.call_increment(&mut runtime.store).unwrap(), i);
        // Increase then decrease by one
        assert_eq!(
            counter_s64.call_increment(&mut runtime.store).unwrap(),
            i + 1
        );
        assert_eq!(counter_s64.call_decrement(&mut runtime.store).unwrap(), i);
    }

    for i in (1..200).rev() {
        assert_eq!(
            counter_s64.call_decrement(&mut runtime.store).unwrap(),
            i - 1
        );
    }
}

#[test]
fn test_identity_u64() {
    bindgen!("identity-u64" in "tests/programs");

    let mut runtime = Runtime::new("identity_u64");

    let (identity_u64, _) =
        IdentityU64::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    for i in [0, 1, 2, 12, 5634, 34] {
        assert_eq!(
            identity_u64.call_identity(&mut runtime.store, i).unwrap(),
            i
        );
    }
}

#[test]
fn test_increment_u32() {
    bindgen!("increment-u32" in "tests/programs");

    let mut runtime = Runtime::new("increment_u32");

    let (increment_u32, _) =
        IncrementU32::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    for i in 1..200 {
        assert_eq!(increment_u32.call_increment(&mut runtime.store).unwrap(), i);
    }
}

#[test]
fn test_increment_u64() {
    bindgen!("increment-u64" in "tests/programs");

    let mut runtime = Runtime::new("increment_u64");

    let (increment_u64, _) =
        IncrementU64::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    for i in 1..200 {
        assert_eq!(increment_u64.call_increment(&mut runtime.store).unwrap(), i);
    }
}

#[test]
fn test_min_u32() {
    bindgen!("min-u32" in "tests/programs");

    let mut runtime = Runtime::new("min_u32");

    let (min_u32, _) =
        MinU32::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    for i in 1..200 {
        for j in 1..200 {
            let expected = std::cmp::min(i, j);
            let actual = min_u32.call_min(&mut runtime.store, i, j).unwrap();
            assert_eq!(expected, actual);
        }
    }
}

#[test]
fn test_max_u32() {
    bindgen!("max-u32" in "tests/programs");

    let mut runtime = Runtime::new("max_u32");

    let (max_u32, _) =
        MaxU32::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    for i in 1..200 {
        for j in 1..200 {
            let expected = std::cmp::max(i, j);
            let actual = max_u32.call_max(&mut runtime.store, i, j).unwrap();
            assert_eq!(expected, actual);
        }
    }
}

#[test]
fn test_quadratic_f64() {
    bindgen!("quadratic-f64" in "tests/programs");

    let mut runtime = Runtime::new("quadratic_f64");

    let (quadratic_f64, _) =
        QuadraticF64::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    for x in 0..10 {
        let x = x as f64;
        let expected = 2.0 * x * x + 3.0 * x + 4.0;
        let actual = quadratic_f64
            .call_quad(&mut runtime.store, 2.0, 3.0, 4.0, x)
            .unwrap();
        assert_eq!(expected, actual);
    }
}

#[test]
fn test_quadratic_let_f64() {
    bindgen!("quadratic-f64" in "tests/programs");

    let mut runtime = Runtime::new("quadratic_let_f64");

    let (quadratic_f64, _) =
        QuadraticF64::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    for x in 0..10 {
        let x = x as f64;
        let expected = 2.0 * x * x + 3.0 * x + 4.0;
        let actual = quadratic_f64
            .call_quad(&mut runtime.store, 2.0, 3.0, 4.0, x)
            .unwrap();
        assert_eq!(expected, actual);
    }
}

#[test]
fn test_less_equal_u32() {
    bindgen!("less-equal-u32" in "tests/programs");

    let mut runtime = Runtime::new("compare_u32");

    let (less_equal_u32, _) =
        LessEqualU32::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    let expected = vec![0, 1, 2, 3, 4, 5, 7, 7, 7, 7];
    let mut actual = Vec::new();
    for x in 0..10 {
        let x = x as u32;
        actual.push(less_equal_u32.call_le(&mut runtime.store, x, 5).unwrap());
    }
    assert_eq!(expected, actual);
}

#[test]
fn test_greater_equal_u32() {
    bindgen!("greater-equal-u32" in "tests/programs");

    let mut runtime = Runtime::new("compare_u32");

    let (less_equal_u32, _) =
        GreaterEqualU32::instantiate(&mut runtime.store, &runtime.component, &runtime.linker)
            .unwrap();

    let expected = vec![7, 7, 7, 7, 7, 5, 6, 7, 8, 9];
    let mut actual = Vec::new();
    for x in 0..10 {
        let x = x as u32;
        actual.push(less_equal_u32.call_ge(&mut runtime.store, x, 5).unwrap());
    }
    assert_eq!(expected, actual);
}
