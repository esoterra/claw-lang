use std::collections::VecDeque;

use crate::ast::{
    M,
    types::ValType
};

#[derive(Clone, Copy, Debug)]
pub struct TypeNode {
    index: usize,
}

#[derive(Clone, Debug, PartialEq)]
struct TypeInfo {
    declared_type: Option<M<ValType>>,
    inferences: Vec<ValType>,
    neighbors: Vec<usize>
}

#[derive(Clone, Debug)]
pub struct TypeGraph {
    data: Vec<TypeInfo>
}

impl TypeGraph {
    pub fn new() -> Self {
        TypeGraph {
            data: Vec::new()
        }
    }

    pub fn add_declared_type(&mut self, declaration: M<ValType>) -> TypeNode {
        let index = self.data.len();
        let info = TypeInfo {
            declared_type: Some(declaration),
            inferences: Vec::new(),
            neighbors: Vec::new()
        };
        self.data.push(info);
        TypeNode { index }
    }

    pub fn add_inferred_type(&mut self) -> TypeNode {
        let index = self.data.len();
        let info = TypeInfo {
            declared_type: None,
            inferences: Vec::new(),
            neighbors: Vec::new()
        };
        self.data.push(info);
        TypeNode { index }
    }

    pub fn type_of(&self, node: TypeNode) -> Option<ValType> {
        let info = &self.data[node.index];
        if let Some(declared_type) = &info.declared_type {
            return Some(declared_type.value.clone());
        }
        if info.inferences.len() == 1 {
            return info.inferences.first().map(|v| v.clone());
        }
        None
    }

    pub fn constrain_type(&mut self, node: TypeNode, valtype: ValType) {
        self.data[node.index].inferences.push(valtype);
    }

    /// Makes no guarantee that duplicate edges will not be added
    pub fn constrain_equal(&mut self, node1: TypeNode, node2: TypeNode) {
        self.data[node1.index].neighbors.push(node2.index);
        self.data[node2.index].neighbors.push(node1.index);
    }

    pub fn resolve_all(&mut self) {
        let mut queue = VecDeque::new();

        for (index, info) in self.data.iter().enumerate() {
            if info.declared_type.is_some() {
                queue.push_back(index);
            }
        }

        while let Some(index) = queue.pop_front() {
            for neighbor in self.data[index].neighbors.clone().iter() {
                if self.propagate(index, *neighbor) {
                    queue.push_back(*neighbor)
                }
            }
        }
    }

    fn propagate(&mut self, source_index: usize, dest_index: usize) -> bool {
        let source_declared = self.data[source_index].declared_type.clone();
        if let Some(declared_type) = source_declared {
            let dest_inferences = &mut self.data[dest_index].inferences;
            if dest_inferences.contains(&declared_type.value) {
                return false;
            } else {
                dest_inferences.push(declared_type.value);
                return true;
            }
        }

        if self.data[source_index].inferences.len() == 1 {
            let source_inference = self.data[source_index].inferences.first().unwrap().clone();
            let dest_inferences = &mut self.data[dest_index].inferences;
            if dest_inferences.contains(&source_inference) {
                return false;
            } else {
                dest_inferences.push(source_inference);
                return true;
            }
        }

        false
    }
}