use cranelift_entity::entity_impl;

use super::{Component, NameId};

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeId(u32);
entity_impl!(TypeId, "type");

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeDefId(u32);
entity_impl!(TypeDefId, "typedef");

/// The type for all values
#[derive(Debug, Hash, Clone)]
pub enum ValType {
    Result(ResultType),
    Primitive(PrimitiveType),
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveType {
    // The boolean type
    Bool,
    // 8-bit Integers
    U8,
    S8,
    // 16-bit Integers
    U16,
    S16,
    // 32-bit Integers
    U32,
    S32,
    // 64-bit Integers
    U64,
    S64,
    // Floating Point Numbers
    F32,
    F64,
    // String type
    String,
}

#[derive(Debug, Hash, Clone)]
pub struct ResultType {
    pub ok: TypeId,
    pub err: TypeId,
}

impl ValType {
    pub fn eq(&self, other: &Self, comp: &Component) -> bool {
        match (self, other) {
            (ValType::Result(left), ValType::Result(right)) => {
                let l_ok = comp.get_type(left.ok);
                let r_ok = comp.get_type(right.ok);
                let ok_eq = l_ok.eq(r_ok, comp);

                let l_err = comp.get_type(left.err);
                let r_err = comp.get_type(right.err);
                let err_eq = l_err.eq(r_err, comp);

                ok_eq && err_eq
            }
            (ValType::Primitive(left), ValType::Primitive(right)) => left == right,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum TypeDefinition {
    Record(RecordTypeDef),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct RecordTypeDef {
    fields: Vec<(NameId, TypeId)>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct FnType {
    pub params: Vec<(NameId, TypeId)>,
    pub results: Option<TypeId>,
}
