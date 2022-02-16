use std::collections::{VecDeque, HashSet};
use std::sync::Arc;

use miette::{Diagnostic, SourceSpan, NamedSource};
use thiserror::Error;

use crate::ast::{
    M, Span,
    types::ValType
};

#[derive(Error, Debug, Diagnostic)]
pub enum TypeError {
    #[diagnostic()]
    #[error("Could not determine type of expression")]
    TypeUnknown {
        #[source_code]
        src: Arc<NamedSource>,
        #[label("Here")]
        span: SourceSpan,
    },
    #[diagnostic()]
    #[error("Expression has conflicting constraints {inferences:?}")]
    TypeConflict {
        #[source_code]
        src: Arc<NamedSource>,
        #[label("Here")]
        span: SourceSpan,
        inferences: Vec<ValType>
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TypeNode {
    index: usize,
}

#[derive(Clone, Debug, PartialEq)]
struct TypeInfo {
    declared_type: Option<ValType>,
    span: Span,
    inferences: Vec<ValType>,
    neighbors: Vec<TypeNode>
}

#[derive(Clone, Debug)]
pub struct TypeGraph {
    src: Arc<NamedSource>,
    data: Vec<TypeInfo>
}

impl TypeGraph {
    pub fn new(src: Arc<NamedSource>) -> Self {
        TypeGraph {
            src,
            data: Vec::new()
        }
    }

    pub fn add_declared_type(&mut self, declaration: M<ValType>) -> TypeNode {
        let index = self.data.len();
        let info = TypeInfo {
            declared_type: Some(declaration.value.clone()),
            span: declaration.span.clone(),
            inferences: Vec::new(),
            neighbors: Vec::new()
        };
        self.data.push(info);
        TypeNode { index }
    }

    pub fn add_inferred_type(&mut self, span: Span) -> TypeNode {
        let index = self.data.len();
        let info = TypeInfo {
            declared_type: None,
            span,
            inferences: Vec::new(),
            neighbors: Vec::new()
        };
        self.data.push(info);
        TypeNode { index }
    }

    pub fn type_of(&self, node: TypeNode) -> Option<ValType> {
        let info = &self.data[node.index];
        if let Some(declared_type) = &info.declared_type {
            return Some(declared_type.clone());
        }
        let unique_inferences = info.inferences.iter().collect::<HashSet<&ValType>>();
        if unique_inferences.len() == 1 {
            return info.inferences.first().map(|v| v.clone());
        }
        None
    }

    pub fn constrain_type(&mut self, node: TypeNode, valtype: ValType) {
        self.data[node.index].inferences.push(valtype);
    }

    /// Makes no guarantee that duplicate edges will not be added
    pub fn constrain_equal(&mut self, node1: TypeNode, node2: TypeNode) {
        self.data[node1.index].neighbors.push(node2);
        self.data[node2.index].neighbors.push(node1);
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
                if self.propagate(index, neighbor.index) {
                    queue.push_back(neighbor.index)
                }
            }
        }
    }

    fn propagate(&mut self, source_index: usize, dest_index: usize) -> bool {
        let source_declared = self.data[source_index].declared_type.clone();
        if let Some(declared_type) = source_declared {
            let dest_inferences = &mut self.data[dest_index].inferences;
            if dest_inferences.contains(&declared_type) {
                return false;
            } else {
                dest_inferences.push(declared_type);
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

    pub fn get_errors(&self) -> Vec<TypeError> {
        let mut errors = Vec::new();
        for node in self.data.iter() {
            if node.declared_type.is_some() {
                continue;
            }
            if node.inferences.len() == 0 {
                errors.push(TypeError::TypeUnknown {
                    src: self.src.clone(),
                    span: node.span.clone()
                });
            }
            let unique_inferences = node.inferences.iter().collect::<HashSet<&ValType>>();
            if unique_inferences.len() > 1 {
                errors.push(TypeError::TypeConflict {
                    src: self.src.clone(),
                    span: node.span.clone(),
                    inferences: node.inferences.clone()
                });
            }
        }
        errors
    }
}