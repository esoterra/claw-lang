use cranelift_entity::entity_impl;

use super::{Arenas, NameId};

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeId(u32);
entity_impl!(TypeId, "type");

/// The type for all values
#[derive(Debug, Hash, Clone)]
pub enum ValType {
    // TypeName(NameId),

    // Result Type
    Result { ok: TypeId, err: TypeId },

    // String Type
    String,
    // Unsigned Integers
    U64,
    U32,
    U16,
    U8,
    // Signed Integers
    S64,
    S32,
    S16,
    S8,
    // Floating Point Numbers
    F64,
    F32,
    // The boolean type
    Bool,
}

impl ValType {
    pub fn eq(&self, other: &Self, arenas: &Arenas) -> bool {
        match (self, other) {
            (
                ValType::Result {
                    ok: l_ok,
                    err: l_err,
                },
                ValType::Result {
                    ok: r_ok,
                    err: r_err,
                },
            ) => {
                let l_ok = arenas.get_type(*l_ok);
                let r_ok = arenas.get_type(*r_ok);
                let ok_eq = l_ok.eq(r_ok, arenas);

                let l_err = arenas.get_type(*l_err);
                let r_err = arenas.get_type(*r_err);
                let err_eq = l_err.eq(r_err, arenas);

                ok_eq && err_eq
            }
            (ValType::String, ValType::String) => true,
            (ValType::U64, ValType::U64) => true,
            (ValType::U32, ValType::U32) => true,
            (ValType::U16, ValType::U16) => true,
            (ValType::U8, ValType::U8) => true,
            (ValType::S64, ValType::S64) => true,
            (ValType::S32, ValType::S32) => true,
            (ValType::S16, ValType::S16) => true,
            (ValType::S8, ValType::S8) => true,
            (ValType::F64, ValType::F64) => true,
            (ValType::F32, ValType::F32) => true,
            (ValType::Bool, ValType::Bool) => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum TypeDefinition {
    // TODO
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct FnType {
    pub arguments: Vec<(NameId, TypeId)>,
    pub return_type: TypeId,
}
