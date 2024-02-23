use ast::TypeId;
use claw_ast as ast;

use claw_resolver::ResolvedType;
use wasm_encoder as enc;

const PTYPE_FLAT_SIZE: u32 = 1;

const STRING_FLAT_SIZE: u32 = 2;
const STRING_COMP_VALTYPE: enc::ComponentValType =
    enc::ComponentValType::Primitive(enc::PrimitiveValType::String);
const STRING_ALIGNMENT: u32 = 2;
const STRING_MEM_SIZE: u32 = 8;

pub trait EncodeType {
    fn flat_size(&self, comp: &ast::Component) -> u32;

    fn append_flattened(&self, comp: &ast::Component, out: &mut Vec<enc::ValType>);

    fn flatten(&self, comp: &ast::Component) -> Vec<enc::ValType> {
        let mut out = Vec::new();
        self.append_flattened(comp, &mut out);
        out
    }

    fn append_fields(&self, comp: &ast::Component, out: &mut Vec<FieldInfo>);

    fn fields(&self, comp: &ast::Component) -> Vec<FieldInfo> {
        let mut out = Vec::new();
        self.append_fields(comp, &mut out);
        out
    }

    fn to_comp_valtype(&self, comp: &ast::Component) -> enc::ComponentValType;

    fn mem_arg(&self, comp: &ast::Component) -> enc::MemArg {
        enc::MemArg {
            align: self.align(comp),
            offset: 0,
            memory_index: 0,
        }
    }

    fn align(&self, comp: &ast::Component) -> u32;

    fn mem_size(&self, comp: &ast::Component) -> u32;
}

impl EncodeType for ResolvedType {
    fn flat_size(&self, comp: &ast::Component) -> u32 {
        match *self {
            ResolvedType::Primitive(ptype) => ptype.flat_size(comp),
            ResolvedType::ValType(type_id) => type_id.flat_size(comp),
        }
    }

    fn append_flattened(&self, comp: &ast::Component, out: &mut Vec<enc::ValType>) {
        match *self {
            ResolvedType::Primitive(ptype) => ptype.append_flattened(comp, out),
            ResolvedType::ValType(type_id) => type_id.append_flattened(comp, out),
        }
    }

    fn append_fields(&self, comp: &ast::Component, out: &mut Vec<FieldInfo>) {
        match *self {
            ResolvedType::Primitive(ptype) => ptype.append_fields(comp, out),
            ResolvedType::ValType(type_id) => type_id.append_fields(comp, out),
        }
    }

    fn to_comp_valtype(&self, comp: &ast::Component) -> enc::ComponentValType {
        match *self {
            ResolvedType::Primitive(ptype) => ptype.to_comp_valtype(comp),
            ResolvedType::ValType(type_id) => type_id.to_comp_valtype(comp),
        }
    }

    fn align(&self, comp: &ast::Component) -> u32 {
        match *self {
            ResolvedType::Primitive(ptype) => ptype.align(comp),
            ResolvedType::ValType(type_id) => type_id.align(comp),
        }
    }

    fn mem_size(&self, comp: &ast::Component) -> u32 {
        match *self {
            ResolvedType::Primitive(ptype) => ptype.mem_size(comp),
            ResolvedType::ValType(type_id) => type_id.mem_size(comp),
        }
    }
}

impl EncodeType for TypeId {
    fn flat_size(&self, comp: &ast::Component) -> u32 {
        let valtype = comp.get_type(*self);
        valtype.flat_size(comp)
    }

    fn append_flattened(&self, comp: &ast::Component, out: &mut Vec<enc::ValType>) {
        let valtype = comp.get_type(*self);
        valtype.append_flattened(comp, out);
    }

    fn append_fields(&self, comp: &ast::Component, out: &mut Vec<FieldInfo>) {
        let valtype = comp.get_type(*self);
        valtype.append_fields(comp, out);
    }

    fn to_comp_valtype(&self, comp: &ast::Component) -> enc::ComponentValType {
        let valtype = comp.get_type(*self);
        valtype.to_comp_valtype(comp)
    }

    fn align(&self, comp: &ast::Component) -> u32 {
        let valtype = comp.get_type(*self);
        valtype.align(comp)
    }

    fn mem_size(&self, comp: &ast::Component) -> u32 {
        let valtype = comp.get_type(*self);
        valtype.mem_size(comp)
    }
}

impl EncodeType for ast::ValType {
    fn flat_size(&self, comp: &ast::Component) -> u32 {
        match *self {
            ast::ValType::Result { .. } => todo!(),
            ast::ValType::String => STRING_FLAT_SIZE,
            ast::ValType::Primitive(ptype) => ptype.flat_size(comp),
        }
    }

    fn append_flattened(&self, comp: &ast::Component, out: &mut Vec<enc::ValType>) {
        match *self {
            ast::ValType::Result { .. } => todo!(),
            ast::ValType::String => {
                out.push(enc::ValType::I32);
                out.push(enc::ValType::I32);
            }
            ast::ValType::Primitive(ptype) => ptype.append_flattened(comp, out),
        }
    }

    fn append_fields(&self, comp: &ast::Component, out: &mut Vec<FieldInfo>) {
        match *self {
            ast::ValType::Result { .. } => todo!(),
            ast::ValType::String => {
                out.push(STRING_OFFSET_FIELD);
                out.push(STRING_LENGTH_FIELD);
            }
            ast::ValType::Primitive(ptype) => ptype.append_fields(comp, out),
        }
    }

    fn to_comp_valtype(&self, comp: &ast::Component) -> enc::ComponentValType {
        match *self {
            ast::ValType::Result { .. } => todo!(),
            ast::ValType::String => STRING_COMP_VALTYPE,
            ast::ValType::Primitive(ptype) => ptype.to_comp_valtype(comp),
        }
    }

    fn align(&self, comp: &ast::Component) -> u32 {
        match *self {
            ast::ValType::Result { .. } => todo!(),
            ast::ValType::String => STRING_ALIGNMENT,
            ast::ValType::Primitive(ptype) => ptype.align(comp),
        }
    }

    fn mem_size(&self, comp: &ast::Component) -> u32 {
        match *self {
            ast::ValType::Result { .. } => todo!(),
            ast::ValType::String => STRING_MEM_SIZE,
            ast::ValType::Primitive(ptype) => ptype.mem_size(comp),
        }
    }
}

impl EncodeType for ast::PrimitiveType {
    fn flat_size(&self, _comp: &ast::Component) -> u32 {
        PTYPE_FLAT_SIZE
    }

    fn append_flattened(&self, _comp: &ast::Component, out: &mut Vec<enc::ValType>) {
        let valtype = match *self {
            ast::PrimitiveType::Bool
            | ast::PrimitiveType::U8
            | ast::PrimitiveType::S8
            | ast::PrimitiveType::U16
            | ast::PrimitiveType::S16
            | ast::PrimitiveType::U32
            | ast::PrimitiveType::S32 => enc::ValType::I32,
            ast::PrimitiveType::U64 | ast::PrimitiveType::S64 => enc::ValType::I64,
            ast::PrimitiveType::F32 => enc::ValType::F32,
            ast::PrimitiveType::F64 => enc::ValType::F64,
        };
        out.push(valtype);
    }

    fn append_fields(&self, _comp: &ast::Component, out: &mut Vec<FieldInfo>) {
        let field = match self {
            ast::PrimitiveType::Bool => BOOL_FIELD,
            ast::PrimitiveType::U8 => U8_FIELD,
            ast::PrimitiveType::S8 => S8_FIELD,
            ast::PrimitiveType::U16 => U16_FIELD,
            ast::PrimitiveType::S16 => S16_FIELD,
            ast::PrimitiveType::U32 => U32_FIELD,
            ast::PrimitiveType::S32 => S32_FIELD,
            ast::PrimitiveType::U64 => U64_FIELD,
            ast::PrimitiveType::S64 => S64_FIELD,
            ast::PrimitiveType::F32 => F32_FIELD,
            ast::PrimitiveType::F64 => F64_FIELD,
        };
        out.push(field);
    }

    fn to_comp_valtype(&self, _comp: &ast::Component) -> enc::ComponentValType {
        enc::ComponentValType::Primitive(ptype_to_pvaltype(*self))
    }

    fn align(&self, _comp: &ast::Component) -> u32 {
        ptype_align(*self)
    }

    fn mem_size(&self, _comp: &ast::Component) -> u32 {
        ptype_mem_size(*self)
    }
}

pub fn ptype_mem_arg(ptype: ast::PrimitiveType) -> enc::MemArg {
    enc::MemArg {
        offset: 0,
        align: ptype_align(ptype),
        memory_index: 0,
    }
}

fn ptype_align(ptype: ast::PrimitiveType) -> u32 {
    match ptype {
        ast::PrimitiveType::Bool | ast::PrimitiveType::U8 | ast::PrimitiveType::S8 => 0,
        ast::PrimitiveType::U16 | ast::PrimitiveType::S16 => 1,
        ast::PrimitiveType::U32 | ast::PrimitiveType::S32 | ast::PrimitiveType::F32 => 2,
        ast::PrimitiveType::U64 | ast::PrimitiveType::S64 | ast::PrimitiveType::F64 => 3,
    }
}

fn ptype_mem_size(ptype: ast::PrimitiveType) -> u32 {
    match ptype {
        ast::PrimitiveType::Bool | ast::PrimitiveType::U8 | ast::PrimitiveType::S8 => 1,
        ast::PrimitiveType::U16 | ast::PrimitiveType::S16 => 2,
        ast::PrimitiveType::U32 | ast::PrimitiveType::S32 | ast::PrimitiveType::F32 => 4,
        ast::PrimitiveType::U64 | ast::PrimitiveType::S64 | ast::PrimitiveType::F64 => 8,
    }
}

pub fn ptype_to_pvaltype(ptype: ast::PrimitiveType) -> enc::PrimitiveValType {
    use ast::PrimitiveType as PType;
    match ptype {
        PType::U64 => enc::PrimitiveValType::U64,
        PType::U32 => enc::PrimitiveValType::U32,
        PType::U16 => enc::PrimitiveValType::U16,
        PType::U8 => enc::PrimitiveValType::U8,
        PType::S64 => enc::PrimitiveValType::S64,
        PType::S32 => enc::PrimitiveValType::S32,
        PType::S16 => enc::PrimitiveValType::S16,
        PType::S8 => enc::PrimitiveValType::S8,
        PType::F32 => enc::PrimitiveValType::Float32,
        PType::F64 => enc::PrimitiveValType::Float64,
        PType::Bool => enc::PrimitiveValType::Bool,
    }
}

pub fn align_to(offset: u32, alignment: u32) -> u32 {
    offset.div_ceil(alignment) * alignment
}

/// Info about a field required for reading/writing it
#[derive(Debug)]
pub struct FieldInfo {
    pub ptype: ast::PrimitiveType,
    pub index_offset: u32,
    pub mem_offset: u32,
}

// Statically known field info

pub const BOOL_FIELD: FieldInfo = FieldInfo {
    ptype: ast::PrimitiveType::Bool,
    index_offset: 0,
    mem_offset: 0,
};

pub const U8_FIELD: FieldInfo = FieldInfo {
    ptype: ast::PrimitiveType::U8,
    index_offset: 0,
    mem_offset: 0,
};

pub const S8_FIELD: FieldInfo = FieldInfo {
    ptype: ast::PrimitiveType::S8,
    index_offset: 0,
    mem_offset: 0,
};

pub const U16_FIELD: FieldInfo = FieldInfo {
    ptype: ast::PrimitiveType::U16,
    index_offset: 0,
    mem_offset: 0,
};

pub const S16_FIELD: FieldInfo = FieldInfo {
    ptype: ast::PrimitiveType::S16,
    index_offset: 0,
    mem_offset: 0,
};

pub const U32_FIELD: FieldInfo = FieldInfo {
    ptype: ast::PrimitiveType::U32,
    index_offset: 0,
    mem_offset: 0,
};

pub const S32_FIELD: FieldInfo = FieldInfo {
    ptype: ast::PrimitiveType::S32,
    index_offset: 0,
    mem_offset: 0,
};

pub const U64_FIELD: FieldInfo = FieldInfo {
    ptype: ast::PrimitiveType::U64,
    index_offset: 0,
    mem_offset: 0,
};

pub const S64_FIELD: FieldInfo = FieldInfo {
    ptype: ast::PrimitiveType::S64,
    index_offset: 0,
    mem_offset: 0,
};

pub const F32_FIELD: FieldInfo = FieldInfo {
    ptype: ast::PrimitiveType::F32,
    index_offset: 0,
    mem_offset: 0,
};

pub const F64_FIELD: FieldInfo = FieldInfo {
    ptype: ast::PrimitiveType::F64,
    index_offset: 0,
    mem_offset: 0,
};

pub const STRING_OFFSET_FIELD: FieldInfo = FieldInfo {
    ptype: ast::PrimitiveType::U32,
    index_offset: 0,
    mem_offset: 0,
};

pub const STRING_LENGTH_FIELD: FieldInfo = FieldInfo {
    ptype: ast::PrimitiveType::U32,
    index_offset: 1,
    mem_offset: 4,
};
