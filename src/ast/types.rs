use cranelift_entity::entity_impl;

use super::{Component, NameId};

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
    Primitive(PrimitiveType),
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveType {
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

impl PrimitiveType {
    pub fn core_type_mask(&self) -> Option<i32> {
        use PrimitiveType as P;
        match self {
            P::U8 | P::S8 => Some(0xFF),
            P::U16 | P::S16 => Some(0xFFFF),
            _ => None,
        }
    }
}

impl ValType {
    pub fn eq(&self, other: &Self, comp: &Component) -> bool {
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
                let l_ok = comp.get_type(*l_ok);
                let r_ok = comp.get_type(*r_ok);
                let ok_eq = l_ok.eq(r_ok, comp);

                let l_err = comp.get_type(*l_err);
                let r_err = comp.get_type(*r_err);
                let err_eq = l_err.eq(r_err, comp);

                ok_eq && err_eq
            }
            (ValType::String, ValType::String) => true,
            (ValType::Primitive(left), ValType::Primitive(right)) => left == right,
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
    pub return_type: Option<TypeId>,
}

impl super::FnTypeInfo for FnType {
    fn get_args(&self) -> &[(NameId, TypeId)] {
        self.arguments.as_slice()
    }

    fn get_return_type(&self) -> Option<TypeId> {
        self.return_type
    }
}
