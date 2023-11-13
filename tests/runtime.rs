use wrought::{compile, gen};

use std::fs;

use wasmtime::{Engine, Instance, Module, Store};

struct Runtime {
    engine: Engine,
    module: Module,
    store: Store<()>,
    instance: Instance,
}

impl Runtime {
    pub fn new(name: &str) -> Self {
        let path = format!("./tests/sample_programs/{}.wrt", name);
        let input = fs::read_to_string(path).unwrap();
        let output = compile(name, input);
        let output = match output {
            Some(output) => output,
            None => panic!("Failed to compile '{}'", name),
        };
        let output = gen::generate_wasm(output);

        let engine = Engine::default();
        let module = Module::new(&engine, output).unwrap();
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).unwrap();

        Runtime {
            engine,
            module,
            store,
            instance,
        }
    }
}

#[test]
fn test_identity_u64() {
    let mut runtime = Runtime::new("identity_u64");

    let identity_func = runtime
        .instance
        .get_typed_func::<u64, u64>(&mut runtime.store, "identity")
        .unwrap();

    for i in [0, 1, 2, 12, 5634, 34] {
        assert_eq!(identity_func.call(&mut runtime.store, i).unwrap(), i);
    }
}

#[test]
fn test_increment_u32() {
    let mut runtime = Runtime::new("increment_u32");

    let increment = runtime
        .instance
        .get_typed_func::<(), u32>(&mut runtime.store, "increment")
        .unwrap();

    for i in 1..200 {
        assert_eq!(increment.call(&mut runtime.store, ()).unwrap(), i);
    }
}

#[test]
fn test_increment_u64() {
    let mut runtime = Runtime::new("increment_u64");

    let increment = runtime
        .instance
        .get_typed_func::<(), u64>(&mut runtime.store, "increment")
        .unwrap();

    for i in 1..200 {
        assert_eq!(increment.call(&mut runtime.store, ()).unwrap(), i);
    }
}


#[test]
fn test_min_u32() {
    let mut runtime = Runtime::new("min_u32");

    let increment = runtime
        .instance
        .get_typed_func::<(u32, u32), u32>(&mut runtime.store, "min")
        .unwrap();

    for i in 1..200 {
        for j in 1..200 {
            let expected = std::cmp::min(i, j);
            let actual = increment.call(&mut runtime.store, (i, j)).unwrap();
            assert_eq!(expected, actual);
        }
    }
}