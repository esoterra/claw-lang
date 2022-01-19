use crate::{ast::{
    M,
    module::FunctionSignature,
    types::{ValType, BasicVal}
}, resolver::ItemID};

#[derive(Debug)]
pub enum NeedsResolve<T> {
    Resolved(T),
    Unresolved
}

#[derive(Debug)]
pub struct Module {
    pub globals: Vec<Global>,
    pub functions: Vec<Function>,
    pub exports: Vec<Export>
}

impl Module {
    pub fn new() -> Self {
        Module {
            globals: Vec::new(),
            functions: Vec::new(),
            exports: Vec::new()
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

#[derive(Debug)]
pub enum Constant {
    I32 {
        value: i32
    },
    I64 {
        value: i64
    },
    U32 {
        value: u32
    },
    U64 {
        value: u64
    },
    F32 {
        value: f32
    },
    F64 {
        value: f64
    }
}

#[derive(Debug)]
pub struct Export {
    pub ident: M<String>,
    pub id: ItemID
}

#[derive(Debug)]
pub struct Global {
    pub ident: M<String>,
    pub type_: M<ValType>,
    pub mutable: bool,
    pub initial_value: NeedsResolve<Constant>
}

#[derive(Debug)]
pub struct Function {
    pub signature: FunctionSignature,
    pub body: NeedsResolve<Vec<Instruction>>
}

#[derive(Debug)]
pub enum Instruction {
    Constant {
        value: Constant
    },
    GlobalGet {
        index: usize
    },
    GlobalSet {
        index: usize,
        value: Box<Instruction>
    },
    Add {
        result_type: BasicVal,
        left: Box<Instruction>,
        right: Box<Instruction>
    },
    Return {
        value: Box<Instruction>
    }
}