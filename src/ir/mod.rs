use crate::ast::{
    M,
    module::FunctionSignature,
    types::{ValType, BasicVal}
};

pub enum NeedsResolve<T> {
    Resolved(T),
    Unresolved
}

pub struct Module {
    pub globals: Vec<Global>,
    pub functions: Vec<Function>
}

impl Module {
    pub fn new() -> Self {
        Module {
            globals: Vec::new(),
            functions: Vec::new()
        }
    }

    pub fn get_global(&self, index: usize) -> Option<&Global> {
        self.globals.get(index)
    }

    pub fn get_global_mut(&mut self, index: usize) -> Option<&mut Global> {
        self.globals.get_mut(index)
    }

    pub fn get_function(&self, index: usize) -> Option<&Function> {
        self.functions.get(index)
    }

    pub fn get_function_mut(&mut self, index: usize) -> Option<&mut Function> {
        self.functions.get_mut(index)
    }
}

pub enum Constant {
    I32 {
        value: i32
    }
}

pub struct Global {
    pub ident: M<String>,
    pub type_: M<ValType>,
    pub initial_value: NeedsResolve<Constant>
}

pub struct Function {
    pub signature: FunctionSignature,
    pub body: NeedsResolve<Vec<Operation>>
}

pub enum Operation {
    Constant {
        value: Constant
    },
    GlobalGet {
        index: usize
    },
    GlobalSet {
        index: usize
    },
    Add {
        result_type: BasicVal
    },
    Return
}