pub mod type_graph;

use crate::ast::{
    M,
    module::FunctionSignature,
    types::ValType,
    expressions::Literal
};
use crate::resolver::ModuleItem;

use self::type_graph::TypeNode;

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
pub struct Export {
    pub ident: M<String>,
    pub id: ModuleItem
}

#[derive(Debug)]
pub struct Global {
    pub ident: M<String>,
    pub type_: M<ValType>,
    pub mutable: bool,
    pub initial_value: NeedsResolve<Literal>
}

#[derive(Debug)]
pub struct Function {
    pub signature: FunctionSignature,
    pub type_graph: NeedsResolve<type_graph::TypeGraph>,
    pub locals: NeedsResolve<Vec<TypeNode>>,
    pub body: NeedsResolve<Vec<Instruction>>
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Constant {
        node: TypeNode,
        value: Literal
    },
    GlobalGet {
        index: usize
    },
    GlobalSet {
        index: usize,
        value: Box<Instruction>
    },
    LocalGet {
        index: usize
    },
    LocalSet {
        index: usize,
        value: Box<Instruction>
    },
    BinaryArith {
        node: TypeNode,
        op: BinArithOp,
        left: Box<Instruction>,
        right: Box<Instruction>
    },
    BinaryRel {
        node: TypeNode,
        op: BinRelOp,
        left: Box<Instruction>,
        right: Box<Instruction>
    },
    BinaryLog {
        op: BinRelOp,
        left: Box<Instruction>,
        right: Box<Instruction>
    },
    If {
        body: Vec<Instruction>
    },
    Return {
        value: Box<Instruction>
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinArithOp {
    Add,
    Sub,
    Mul,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinRelOp {
    EQ,
    LT
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinLogOp {
    AND,
    OR,
}