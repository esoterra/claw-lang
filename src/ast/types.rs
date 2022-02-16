use super::M;

/// The type for all values
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum ValType {
    /// Basic values that WASM can operate on
    Basic(BasicVal),
    /// Pointer and slice types
    Ptr(PointerVal),
    /// Named user-defined types
    Ident(String)
}

/// The set of primitive values WASM can operate on directly
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum BasicVal {
    // Integers (any signed-ness)
    I32, I64,
    // Unsigned Integers
    U32, U64,
    // Signed Integers
    S32, S64,
    // Floating Point Numbers
    F32, F64,
    // The boolean type
    Bool
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct PointerVal {
    pub kind: M<PointerKind>,
    pub mem_type: M<Pointable>,
    pub offset: Option<M<u64>>,
    pub align: Option<M<u64>>,
    pub memory: Option<M<String>>
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum PointerKind {
    /// A pointer that represents a single location in memory
    Ptr,
    /// A pointer and length that represents a range of memory
    Slice
}

/// The valid targets for pointer types
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Pointable {
    /// All of the basic values can be pointed to
    Basic(BasicVal),
    // Additional smaller types can be pointed to
    // even though WASM does not let you operate on them directly.
    // They must always be loaded into basic values to be used, extending them.
    U8, U16, S8, S16, I8, I16
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct FnType {
    pub param_types: Vec<M<ValType>>,
    pub result_type: M<ValType>
}