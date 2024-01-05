use claw::{compile, codegen};

use std::fs;

use wasmtime::{Engine, Store, Config};
use wasmtime::component::{bindgen, Component, Linker};

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

        let generator = codegen::CodeGenerator::default();
        let component_bytes = generator.generate(&output);

        println!("{}", wasmprinter::print_bytes(&component_bytes).unwrap());

        let mut config = Config::new();
        config.wasm_component_model(true);
        let engine = Engine::new(&config).unwrap();
    
        let component = Component::new(&engine, &component_bytes).unwrap();
        let linker = Linker::new(&engine);
        let store = Store::new(&engine, ());

        Runtime {
            engine,
            component,
            linker,
            store
        }
    }
}

#[test]
fn test_counter_s64() {
    bindgen!("counter-s64" in "tests/programs");

    let mut runtime = Runtime::new("counter_s64");

    let (counter_s64, _) = CounterS64::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    for i in 1..200 {
        // Increase by one
        assert_eq!(counter_s64.call_increment(&mut runtime.store).unwrap(), i);
        // Increase then decrease by one
        assert_eq!(counter_s64.call_increment(&mut runtime.store).unwrap(), i+1);
        assert_eq!(counter_s64.call_decrement(&mut runtime.store).unwrap(), i);
    }

    for i in (1..200).rev() {
        assert_eq!(counter_s64.call_decrement(&mut runtime.store).unwrap(), i-1);
    }
}

#[test]
fn test_factorial_u64() {
    bindgen!("factorial-u64" in "tests/programs");

    let mut runtime = Runtime::new("factorial_u64");

    let (identity_u64, _) = FactorialU64::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    for (i, val) in [1, 1, 2, 6, 24, 120].iter().enumerate() {
        assert_eq!(identity_u64.call_factorial(&mut runtime.store, i as u64).unwrap(), *val);
    }
}

#[test]
fn test_identity() {
    bindgen!("identity" in "tests/programs");

    let mut runtime = Runtime::new("identity");

    let (identity, _) = Identity::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    for i in [0, 1, 2, 12, 5634, 34] {
        assert_eq!(identity.call_identity(&mut runtime.store, i).unwrap(), i);
    }
}

#[test]
fn test_increment_u32() {
    bindgen!("increment-u32" in "tests/programs");

    let mut runtime = Runtime::new("increment_u32");

    let (increment_u32, _) = IncrementU32::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    for i in 1..200 {
        assert_eq!(increment_u32.call_increment(&mut runtime.store).unwrap(), i);
    }
}

#[test]
fn test_increment_u64() {
    bindgen!("increment-u64" in "tests/programs");

    let mut runtime = Runtime::new("increment_u64");

    let (increment_u64, _) = IncrementU64::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    for i in 1..200 {
        assert_eq!(increment_u64.call_increment(&mut runtime.store).unwrap(), i);
    }
}

#[test]
fn test_min_u32() {
    bindgen!("min-u32" in "tests/programs");

    let mut runtime = Runtime::new("min_u32");

    let (min_u32, _) = MinU32::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    for i in 1..200 {
        for j in 1..200 {
            let expected = std::cmp::min(i, j);
            let actual = min_u32.call_min(&mut runtime.store, i, j).unwrap();
            assert_eq!(expected, actual);
        }
    }
}

#[test]
fn test_proxy_call() {
    bindgen!("proxy-call" in "tests/programs");

    let mut runtime = Runtime::new("proxy_call");

    impl ProxyCallImports for () {
        fn imported(&mut self, a: u32) -> Result<u32, wasmtime::Error> {
            Ok(a)
        }
    }

    ProxyCall::add_to_linker(&mut runtime.linker, |s| s).unwrap();

    let (proxy_call, _) = ProxyCall::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    for x in 0..10 {
        let actual = proxy_call.call_exported(&mut runtime.store, x).unwrap();
        assert_eq!(x, actual);
    }
}

#[test]
fn test_quadratic_f64() {
    bindgen!("quadratic-f64" in "tests/programs");

    let mut runtime = Runtime::new("quadratic_f64");

    let (quadratic_f64, _) = QuadraticF64::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    for x in 0..10 {
        let x = x as f64;
        let expected = 2.0 * x * x + 3.0 * x + 4.0;
        let actual = quadratic_f64.call_quad(&mut runtime.store, 2.0, 3.0, 4.0, x).unwrap();
        assert_eq!(expected, actual);
    }
}

#[test]
fn test_quadratic_let_f64() {
    bindgen!("quadratic-f64" in "tests/programs");

    let mut runtime = Runtime::new("quadratic_let_f64");

    let (quadratic_f64, _) = QuadraticF64::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    for x in 0..10 {
        let x = x as f64;
        let expected = 2.0 * x * x + 3.0 * x + 4.0;
        let actual = quadratic_f64.call_quad(&mut runtime.store, 2.0, 3.0, 4.0, x).unwrap();
        assert_eq!(expected, actual);
    }
}