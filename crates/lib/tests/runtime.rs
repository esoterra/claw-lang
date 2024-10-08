use claw_common::UnwrapPretty;
use compile_claw::compile;

use std::fs;

use wasmtime::component::{bindgen, Component, Linker};
use wasmtime::{Config, Engine, Store};
use wit_parser::Resolve;

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
        let mut wit = Resolve::new();
        wit.push_path("./tests/programs/wit").unwrap();
        let component_bytes = compile(name.to_owned(), &input, wit).unwrap_pretty();

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
fn test_arithmetic() {
    bindgen!("arithmetic" in "tests/programs/wit");

    let mut runtime = Runtime::new("arithmetic");

    let (arithmetic, _) =
        Arithmetic::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    assert!(arithmetic.call_test_u8_masking(&mut runtime.store).unwrap());
}

#[test]
fn test_counter() {
    bindgen!("counter" in "tests/programs/wit");

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
    bindgen!("factorial" in "tests/programs/wit");

    let mut runtime = Runtime::new("factorial");

    let (factorial, _) =
        Factorial::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    for (i, val) in [1, 1, 2, 6, 24, 120].iter().enumerate() {
        let fact = factorial
            .call_factorial(&mut runtime.store, i as u64)
            .unwrap();
        assert_eq!(
            fact, *val,
            "factorial({}) was {} instead of {}",
            i, fact, *val
        );
    }
}

#[test]
fn test_identity() {
    bindgen!("identity" in "tests/programs/wit");

    let mut runtime = Runtime::new("identity");

    let (identity, _) =
        Identity::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    for i in [0, 1, 2, 12, 5634, 34] {
        assert_eq!(identity.call_identity(&mut runtime.store, i).unwrap(), i);
    }
}

#[test]
fn test_compare() {
    bindgen!("compare" in "tests/programs/wit");

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
    bindgen!("proxy-call" in "tests/programs/wit");

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
    bindgen!("quadratic" in "tests/programs/wit");

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
fn test_strings() {
    bindgen!("strings" in "tests/programs/wit");

    let mut runtime = Runtime::new("strings");

    let (strings, _) =
        Strings::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    let long_string = "Z".repeat(1000);
    let cases = [
        "",
        "asdf",
        "673hlksdfkjh5r;4hj6s",
        "a",
        long_string.as_str(),
    ];

    for case in cases {
        assert_eq!(
            case,
            strings.call_identity(&mut runtime.store, case).unwrap()
        );
    }

    assert_eq!(
        strings.call_hello_world(&mut runtime.store).unwrap(),
        "hello, world!"
    );

    for case in cases {
        assert_eq!(
            format!("{}Lorem Ipsum", case).as_str(),
            strings
                .call_concat(&mut runtime.store, case, "Lorem Ipsum")
                .unwrap()
        );
    }
}

#[test]
fn test_timer_proxy() {
    bindgen!("timer-proxy" in "tests/programs/wit");

    let mut runtime = Runtime::new("timer-proxy");

    impl TimerProxyImports for () {
        fn foo(&mut self, a: String) -> wasmtime::Result<String> {
            wasmtime::Result::Ok(a)
        }
    }

    use wasi::logging::logging;
    impl logging::Host for () {
        fn log(
            &mut self,
            _level: logging::Level,
            context: String,
            message: String,
        ) -> wasmtime::Result<()> {
            println!("{}: {}", context, message);
            wasmtime::Result::Ok(())
        }
    }

    use wasi::clocks::monotonic_clock;
    impl monotonic_clock::Host for () {
        fn now(&mut self) -> wasmtime::Result<monotonic_clock::Instant> {
            wasmtime::Result::Ok(monotonic_clock::Instant::from(1u64))
        }
    }

    TimerProxy::add_to_linker(&mut runtime.linker, |s| s).unwrap();

    let (timer, _) =
        TimerProxy::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    let found = timer.call_foo(&mut runtime.store, "asdf").unwrap();
    assert_eq!(found, "asdf");
}

#[test]
fn test_unary() {
    bindgen!("unary" in "tests/programs/wit");

    let mut runtime = Runtime::new("unary");

    let (unary, _) =
        Unary::instantiate(&mut runtime.store, &runtime.component, &runtime.linker).unwrap();

    for x in 0..10_i32 {
        unary.call_set(&mut runtime.store, x).unwrap();
        let inverse = unary.call_get_inverse(&mut runtime.store).unwrap();
        assert_eq!(-x, inverse);
    }
}
