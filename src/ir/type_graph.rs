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
    inferences: Vec<ValType>
}

#[derive(Clone, Debug)]
pub struct TypeGraph {
    data: Vec<(TypeInfo, Vec<usize>)>
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
            inferences: Vec::new()
        };
        let entry = (info, Vec::new());
        self.data.push(entry);
        TypeNode { index }
    }

    pub fn add_inferred_type(&mut self) -> TypeNode {
        let index = self.data.len();
        let info = TypeInfo {
            declared_type: None,
            inferences: Vec::new()
        };
        let entry = (info, Vec::new());
        self.data.push(entry);
        TypeNode { index }
    }

    pub fn type_of(&self, node: TypeNode) -> Option<ValType> {
        let info = &self.data[node.index].0;
        if let Some(declared_type) = &info.declared_type {
            return Some(declared_type.value.clone());
        }
        if info.inferences.len() == 1 {
            return info.inferences.first().map(|v| v.clone());
        }
        None
    }

    /// Makes no guarantee that duplicate edges will not be added
    pub fn constrain_equal(&mut self, node1: TypeNode, node2: TypeNode) {
        self.data[node1.index].1.push(node2.index);
        self.data[node2.index].1.push(node1.index);
    }

    pub fn resolve_all(&mut self) {
        let mut queue = VecDeque::new();

        for (index, (info, ..)) in self.data.iter().enumerate() {
            if info.declared_type.is_some() {
                println!("Queued Declared {}", index);
                queue.push_back(index);
            }
        }

        while let Some(index) = queue.pop_front() {
            for neighbor in self.data[index].1.clone().iter() {
                if self.propagate(index, *neighbor) {
                    println!("Queued Updated {}", index);
                    queue.push_back(*neighbor)
                }
            }
        }
    }

    fn propagate(&mut self, source_index: usize, dest_index: usize) -> bool {
        let source_declared = self.data[source_index].0.declared_type.clone();
        if let Some(declared_type) = source_declared {
            let dest_inferences = &mut self.data[dest_index].0.inferences;
            if dest_inferences.contains(&declared_type.value) {
                return false;
            } else {
                dest_inferences.push(declared_type.value);
                return true;
            }
        }

        if self.data[source_index].0.inferences.len() == 1 {
            let source_inference = self.data[source_index].0.inferences.first().unwrap().clone();
            let dest_inferences = &mut self.data[dest_index].0.inferences;
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