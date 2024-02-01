use super::{MBox, Span, M};

/// The type for all values
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum ValType {
    // Result Type
    Result {
        ok: MBox<ValType>,
        err: MBox<ValType>,
    },
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
    F32,
    F64,
    // The boolean type
    Bool,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct FnType {
    pub arguments: Vec<(M<String>, M<ValType>)>,
    pub arrow: Span,
    pub return_type: M<ValType>,
}
