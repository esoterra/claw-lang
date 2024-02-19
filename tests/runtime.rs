use claw::{codegen, compile};

use std::fs;

use miette::Report;
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
        let output = compile(name.to_owned(), &input);
        let output = match output {
            Some(output) => output,
            None => panic!("Failed to compile '{}'", name),
        };

        let generator = codegen::CodeGenerator::default();
        let component_bytes = match generator.generate(&output) {
            Ok(wasm) => wasm,
            Err(error) => {
                panic!("{:?}", Report::new(error))
            }
        };

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
            store,
        }
    }
}

#[test]
fn test_counter() {
    bindgen!("counter" in "tests/programs");

    let mut runtime = Runtime::new("counter");

    let (counter_s64, _) =
        Counter::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    for i in 1..200 {
        // Increase by one
        assert_eq!(
            counter_s64.call_increment_s32(&mut runtime.store).unwrap(),
            i
        );
        assert_eq!(
            counter_s64.call_increment_s64(&mut runtime.store).unwrap(),
            i as i64
        );
        // Increase then decrease by one
        assert_eq!(
            counter_s64.call_increment_s32(&mut runtime.store).unwrap(),
            i + 1
        );
        assert_eq!(
            counter_s64.call_increment_s64(&mut runtime.store).unwrap(),
            i as i64 + 1
        );
        assert_eq!(
            counter_s64.call_decrement_s32(&mut runtime.store).unwrap(),
            i
        );
        assert_eq!(
            counter_s64.call_decrement_s64(&mut runtime.store).unwrap(),
            i as i64
        );
    }

    for i in (1..200).rev() {
        assert_eq!(
            counter_s64.call_decrement_s32(&mut runtime.store).unwrap(),
            i - 1
        );
        assert_eq!(
            counter_s64.call_decrement_s64(&mut runtime.store).unwrap(),
            i as i64 - 1
        );
    }
}

#[test]
fn test_factorial() {
    bindgen!("factorial" in "tests/programs");

    let mut runtime = Runtime::new("factorial");

    let (factorial, _) =
        Factorial::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    for (i, val) in [1, 1, 2, 6, 24, 120].iter().enumerate() {
        assert_eq!(
            factorial
                .call_factorial(&mut runtime.store, i as u64)
                .unwrap(),
            *val
        );
    }
}

#[test]
fn test_identity() {
    bindgen!("identity" in "tests/programs");

    let mut runtime = Runtime::new("identity");

    let (identity, _) =
        Identity::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    for i in [0, 1, 2, 12, 5634, 34] {
        assert_eq!(identity.call_identity(&mut runtime.store, i).unwrap(), i);
    }
}

#[test]
fn test_compare() {
    bindgen!("compare" in "tests/programs");

    let mut runtime = Runtime::new("compare");

    let (compare, _) =
        Compare::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    for i in 1..200 {
        for j in 1..200 {
            let expected_min = std::cmp::min(i, j);
            let expected_max = std::cmp::max(i, j);
            let actual_min = compare.call_min_u32(&mut runtime.store, i, j).unwrap();
            let actual_max = compare.call_max_u32(&mut runtime.store, i, j).unwrap();
            assert_eq!(
                expected_min, actual_min,
                "expected min({}, {}) to be {} not {}",
                i, j, expected_min, actual_min
            );
            assert_eq!(
                expected_max, actual_max,
                "expected max({}, {}) to be {} not {}",
                i, j, expected_max, actual_max
            );

            let i = i as u64;
            let j = j as u64;
            let expected_min = expected_min as u64;
            let expected_max = expected_max as u64;
            let actual_min = compare.call_min_u64(&mut runtime.store, i, j).unwrap();
            let actual_max = compare.call_max_u64(&mut runtime.store, i, j).unwrap();
            assert_eq!(
                expected_min, actual_min,
                "expected min({}, {}) to be {} not {}",
                i, j, expected_min, actual_min
            );
            assert_eq!(
                expected_max, actual_max,
                "expected max({}, {}) to be {} not {}",
                i, j, expected_max, actual_max
            );
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

    let (proxy_call, _) =
        ProxyCall::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    for x in 0..10 {
        let actual = proxy_call.call_exported(&mut runtime.store, x).unwrap();
        assert_eq!(x, actual);
    }
}

#[test]
fn test_quadratic() {
    bindgen!("quadratic" in "tests/programs");

    let mut runtime = Runtime::new("quadratic");

    let (quadratic, _) =
        Quadratic::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    for x in 0..10 {
        let expected = 2 * x * x + 3 * x + 4;
        let actual_f32 = quadratic
            .call_quad_f32(&mut runtime.store, 2.0, 3.0, 4.0, x as f32)
            .unwrap();
        let actual_f32_let = quadratic
            .call_quad_f32_let(&mut runtime.store, 2.0, 3.0, 4.0, x as f32)
            .unwrap();
        let actual_f64 = quadratic
            .call_quad_f64(&mut runtime.store, 2.0, 3.0, 4.0, x as f64)
            .unwrap();
        let actual_f64_let = quadratic
            .call_quad_f64_let(&mut runtime.store, 2.0, 3.0, 4.0, x as f64)
            .unwrap();

        assert_eq!(expected as f32, actual_f32);
        assert_eq!(expected as f32, actual_f32_let);
        assert_eq!(expected as f64, actual_f64);
        assert_eq!(expected as f64, actual_f64_let);
    }
}

#[test]
fn test_unary() {
    bindgen!("unary" in "tests/programs");

    let mut runtime = Runtime::new("unary");

    let (unary, _) =
        Unary::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    for x in 0..(10 as i32) {
        unary.call_set(&mut runtime.store, x).unwrap();
        let inverse = unary.call_get_inverse(&mut runtime.store).unwrap();
        assert_eq!(-x, inverse);
    }
}
