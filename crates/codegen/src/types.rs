use ast::TypeId;
use claw_ast as ast;

use claw_resolver::{types::ResolvedType, ImportType, ResolvedComponent};
use wasm_encoder as enc;

const STRING_ALIGNMENT: u32 = 2;
const STRING_MEM_SIZE: u32 = 8;

fn string_append_flatten(out: &mut Vec<enc::ValType>) {
    out.push(enc::ValType::I32);
    out.push(enc::ValType::I32);
}

fn string_append_fields(out: &mut Vec<FieldInfo>) {
    out.push(STRING_OFFSET_FIELD);
    out.push(STRING_LENGTH_FIELD);
}

pub const STRING_CONTENTS_ALIGNMENT: u32 = 0;

pub trait EncodeType {
    fn flat_size(&self, comp: &ast::Component, rcomp: &ResolvedComponent) -> u32;

    fn append_flattened(
        &self,
        comp: &ast::Component,
        rcomp: &ResolvedComponent,
        out: &mut Vec<enc::ValType>,
    );

    fn flatten(&self, comp: &ast::Component, rcomp: &ResolvedComponent) -> Vec<enc::ValType> {
        let mut out = Vec::new();
        self.append_flattened(comp, rcomp, &mut out);
        out
    }

    fn append_fields(
        &self,
        comp: &ast::Component,
        rcomp: &ResolvedComponent,
        out: &mut Vec<FieldInfo>,
    );

    fn fields(&self, comp: &ast::Component, rcomp: &ResolvedComponent) -> Vec<FieldInfo> {
        let mut out = Vec::new();
        self.append_fields(comp, rcomp, &mut out);
        out
    }

    fn to_comp_valtype(
        &self,
        comp: &ast::Component,
        rcomp: &ResolvedComponent,
    ) -> enc::ComponentValType;

    #[allow(dead_code)]
    fn mem_arg(&self, comp: &ast::Component, rcomp: &ResolvedComponent) -> enc::MemArg {
        enc::MemArg {
            align: self.align(comp, rcomp),
            offset: 0,
            memory_index: 0,
        }
    }

    fn align(&self, comp: &ast::Component, rcomp: &ResolvedComponent) -> u32;

    fn mem_size(&self, comp: &ast::Component, rcomp: &ResolvedComponent) -> u32;
}

impl EncodeType for ResolvedType {
    fn flat_size(&self, comp: &ast::Component, rcomp: &ResolvedComponent) -> u32 {
        match *self {
            ResolvedType::Primitive(ptype) => ptype.flat_size(comp, rcomp),
            ResolvedType::Import(id) => {
                let import_type = &rcomp.imports.types[id];
                import_type.flat_size(comp, rcomp)
            }
            ResolvedType::Defined(type_id) => type_id.flat_size(comp, rcomp),
        }
    }

    fn append_flattened(
        &self,
        comp: &ast::Component,
        rcomp: &ResolvedComponent,
        out: &mut Vec<enc::ValType>,
    ) {
        match *self {
            ResolvedType::Primitive(ptype) => ptype.append_flattened(comp, rcomp, out),
            ResolvedType::Import(id) => {
                let import_type = &rcomp.imports.types[id];
                import_type.append_flattened(comp, rcomp, out)
            }
            ResolvedType::Defined(type_id) => type_id.append_flattened(comp, rcomp, out),
        }
    }

    fn append_fields(
        &self,
        comp: &ast::Component,
        rcomp: &ResolvedComponent,
        out: &mut Vec<FieldInfo>,
    ) {
        match *self {
            ResolvedType::Primitive(ptype) => ptype.append_fields(comp, rcomp, out),
            ResolvedType::Import(id) => {
                let import_type = &rcomp.imports.types[id];
                import_type.append_fields(comp, rcomp, out)
            }
            ResolvedType::Defined(type_id) => type_id.append_fields(comp, rcomp, out),
        }
    }

    fn to_comp_valtype(
        &self,
        comp: &ast::Component,
        rcomp: &ResolvedComponent,
    ) -> enc::ComponentValType {
        match *self {
            ResolvedType::Primitive(ptype) => ptype.to_comp_valtype(comp, rcomp),
            ResolvedType::Import(id) => {
                let import_type = &rcomp.imports.types[id];
                import_type.to_comp_valtype(comp, rcomp)
            }
            ResolvedType::Defined(type_id) => type_id.to_comp_valtype(comp, rcomp),
        }
    }

    fn align(&self, comp: &ast::Component, rcomp: &ResolvedComponent) -> u32 {
        match *self {
            ResolvedType::Primitive(ptype) => ptype.align(comp, rcomp),
            ResolvedType::Import(id) => {
                let import_type = &rcomp.imports.types[id];
                import_type.align(comp, rcomp)
            }
            ResolvedType::Defined(type_id) => type_id.align(comp, rcomp),
        }
    }

    fn mem_size(&self, comp: &ast::Component, rcomp: &ResolvedComponent) -> u32 {
        match *self {
            ResolvedType::Primitive(ptype) => ptype.mem_size(comp, rcomp),
            ResolvedType::Import(id) => {
                let import_type = &rcomp.imports.types[id];
                import_type.mem_size(comp, rcomp)
            }
            ResolvedType::Defined(type_id) => type_id.mem_size(comp, rcomp),
        }
    }
}

impl EncodeType for ImportType {
    fn flat_size(&self, _: &ast::Component, _: &ResolvedComponent) -> u32 {
        1
    }

    fn append_flattened(
        &self,
        _: &ast::Component,
        _: &ResolvedComponent,
        out: &mut Vec<enc::ValType>,
    ) {
        out.push(enc::ValType::I32);
    }

    fn append_fields(&self, _: &ast::Component, _: &ResolvedComponent, out: &mut Vec<FieldInfo>) {
        out.push(FieldInfo {
            stack_type: enc::ValType::I32,
            signedness: Signedness::Unsigned,
            arith_mask: None,
            index_offset: 0,
            mem_offset: 0,
            align: 1,
            mems_size: 0,
        })
    }

    fn to_comp_valtype(&self, _: &ast::Component, _: &ResolvedComponent) -> enc::ComponentValType {
        todo!()
    }

    fn align(&self, _: &ast::Component, _: &ResolvedComponent) -> u32 {
        1
    }

    fn mem_size(&self, _: &ast::Component, _: &ResolvedComponent) -> u32 {
        1
    }
}

impl EncodeType for TypeId {
    fn flat_size(&self, comp: &ast::Component, rcomp: &ResolvedComponent) -> u32 {
        let valtype = comp.get_type(*self);
        valtype.flat_size(comp, rcomp)
    }

    fn append_flattened(
        &self,
        comp: &ast::Component,
        rcomp: &ResolvedComponent,
        out: &mut Vec<enc::ValType>,
    ) {
        let valtype = comp.get_type(*self);
        valtype.append_flattened(comp, rcomp, out);
    }

    fn append_fields(
        &self,
        comp: &ast::Component,
        rcomp: &ResolvedComponent,
        out: &mut Vec<FieldInfo>,
    ) {
        let valtype = comp.get_type(*self);
        valtype.append_fields(comp, rcomp, out);
    }

    fn to_comp_valtype(
        &self,
        comp: &ast::Component,
        rcomp: &ResolvedComponent,
    ) -> enc::ComponentValType {
        let valtype = comp.get_type(*self);
        valtype.to_comp_valtype(comp, rcomp)
    }

    fn align(&self, comp: &ast::Component, rcomp: &ResolvedComponent) -> u32 {
        let valtype = comp.get_type(*self);
        valtype.align(comp, rcomp)
    }

    fn mem_size(&self, comp: &ast::Component, rcomp: &ResolvedComponent) -> u32 {
        let valtype = comp.get_type(*self);
        valtype.mem_size(comp, rcomp)
    }
}

impl EncodeType for ast::ValType {
    fn flat_size(&self, comp: &ast::Component, rcomp: &ResolvedComponent) -> u32 {
        match *self {
            ast::ValType::Result(_) => todo!(),
            ast::ValType::Primitive(ptype) => ptype.flat_size(comp, rcomp),
        }
    }

    fn append_flattened(
        &self,
        comp: &ast::Component,
        rcomp: &ResolvedComponent,
        out: &mut Vec<enc::ValType>,
    ) {
        match *self {
            ast::ValType::Result(_) => todo!(),
            ast::ValType::Primitive(ptype) => ptype.append_flattened(comp, rcomp, out),
        }
    }

    fn append_fields(
        &self,
        comp: &ast::Component,
        rcomp: &ResolvedComponent,
        out: &mut Vec<FieldInfo>,
    ) {
        match *self {
            ast::ValType::Result(_) => todo!(),
            ast::ValType::Primitive(ptype) => ptype.append_fields(comp, rcomp, out),
        }
    }

    fn to_comp_valtype(
        &self,
        comp: &ast::Component,
        rcomp: &ResolvedComponent,
    ) -> enc::ComponentValType {
        match *self {
            ast::ValType::Result(_) => todo!(),
            ast::ValType::Primitive(ptype) => ptype.to_comp_valtype(comp, rcomp),
        }
    }

    fn align(&self, comp: &ast::Component, rcomp: &ResolvedComponent) -> u32 {
        match *self {
            ast::ValType::Result(_) => todo!(),
            ast::ValType::Primitive(ptype) => ptype.align(comp, rcomp),
        }
    }

    fn mem_size(&self, comp: &ast::Component, rcomp: &ResolvedComponent) -> u32 {
        match *self {
            ast::ValType::Result(_) => todo!(),
            ast::ValType::Primitive(ptype) => ptype.mem_size(comp, rcomp),
        }
    }
}

impl EncodeType for ast::PrimitiveType {
    fn flat_size(&self, _: &ast::Component, _: &ResolvedComponent) -> u32 {
        match *self {
            ast::PrimitiveType::String => 2,
            _ => 1,
        }
    }

    fn append_flattened(
        &self,
        _: &ast::Component,
        _: &ResolvedComponent,
        out: &mut Vec<enc::ValType>,
    ) {
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
            ast::PrimitiveType::String => {
                string_append_flatten(out);
                return;
            }
        };
        out.push(valtype);
    }

    fn append_fields(&self, _: &ast::Component, _: &ResolvedComponent, out: &mut Vec<FieldInfo>) {
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
            ast::PrimitiveType::String => {
                string_append_fields(out);
                return;
            }
        };
        out.push(field);
    }

    fn to_comp_valtype(&self, _: &ast::Component, _: &ResolvedComponent) -> enc::ComponentValType {
        enc::ComponentValType::Primitive(ptype_to_pvaltype(*self))
    }

    fn align(&self, _: &ast::Component, _: &ResolvedComponent) -> u32 {
        ptype_align(*self)
    }

    fn mem_size(&self, _: &ast::Component, _: &ResolvedComponent) -> u32 {
        ptype_mem_size(*self)
    }
}

fn ptype_align(ptype: ast::PrimitiveType) -> u32 {
    match ptype {
        ast::PrimitiveType::Bool | ast::PrimitiveType::U8 | ast::PrimitiveType::S8 => 0,
        ast::PrimitiveType::U16 | ast::PrimitiveType::S16 => 1,
        ast::PrimitiveType::U32 | ast::PrimitiveType::S32 | ast::PrimitiveType::F32 => 2,
        ast::PrimitiveType::U64 | ast::PrimitiveType::S64 | ast::PrimitiveType::F64 => 3,
        ast::PrimitiveType::String => STRING_ALIGNMENT,
    }
}

fn ptype_mem_size(ptype: ast::PrimitiveType) -> u32 {
    match ptype {
        ast::PrimitiveType::Bool | ast::PrimitiveType::U8 | ast::PrimitiveType::S8 => 1,
        ast::PrimitiveType::U16 | ast::PrimitiveType::S16 => 2,
        ast::PrimitiveType::U32 | ast::PrimitiveType::S32 | ast::PrimitiveType::F32 => 4,
        ast::PrimitiveType::U64 | ast::PrimitiveType::S64 | ast::PrimitiveType::F64 => 8,
        ast::PrimitiveType::String => STRING_MEM_SIZE,
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
        PType::F32 => enc::PrimitiveValType::F32,
        PType::F64 => enc::PrimitiveValType::F64,
        PType::Bool => enc::PrimitiveValType::Bool,
        PType::String => enc::PrimitiveValType::String,
    }
}

pub fn align_to(offset: u32, alignment: u32) -> u32 {
    offset.div_ceil(alignment) * alignment
}

/// Info about a field required for reading/writing it
#[derive(Debug)]
pub struct FieldInfo {
    // Type info
    pub stack_type: enc::ValType,
    pub signedness: Signedness,
    // Arithmetic
    pub arith_mask: Option<i32>,
    // Index
    pub index_offset: u32,
    // Memory information
    pub mem_offset: u32,
    pub align: u32,
    pub mems_size: u32,
}

impl FieldInfo {
    pub fn mem_arg(&self) -> enc::MemArg {
        enc::MemArg {
            offset: 0,
            align: self.align,
            memory_index: 0,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Signedness {
    Unsigned,
    Signed,
}

// Statically known field info

pub const BOOL_FIELD: FieldInfo = FieldInfo {
    stack_type: enc::ValType::I32,
    signedness: Signedness::Unsigned,
    arith_mask: None,
    index_offset: 0,
    mem_offset: 0,
    align: 0,
    mems_size: 1,
};

pub const U8_FIELD: FieldInfo = FieldInfo {
    stack_type: enc::ValType::I32,
    signedness: Signedness::Unsigned,
    arith_mask: Some(0xFF),
    index_offset: 0,
    mem_offset: 0,
    align: 0,
    mems_size: 1,
};

pub const S8_FIELD: FieldInfo = FieldInfo {
    stack_type: enc::ValType::I32,
    signedness: Signedness::Signed,
    arith_mask: Some(0xFF),
    index_offset: 0,
    mem_offset: 0,
    align: 0,
    mems_size: 1,
};

pub const U16_FIELD: FieldInfo = FieldInfo {
    stack_type: enc::ValType::I32,
    signedness: Signedness::Unsigned,
    arith_mask: Some(0xFFFF),
    index_offset: 0,
    mem_offset: 0,
    align: 1,
    mems_size: 2,
};

pub const S16_FIELD: FieldInfo = FieldInfo {
    stack_type: enc::ValType::I32,
    signedness: Signedness::Signed,
    arith_mask: Some(0xFFFF),
    index_offset: 0,
    mem_offset: 0,
    align: 1,
    mems_size: 2,
};

pub const U32_FIELD: FieldInfo = FieldInfo {
    stack_type: enc::ValType::I32,
    signedness: Signedness::Unsigned,
    arith_mask: None,
    index_offset: 0,
    mem_offset: 0,
    align: 2,
    mems_size: 4,
};

pub const S32_FIELD: FieldInfo = FieldInfo {
    stack_type: enc::ValType::I32,
    signedness: Signedness::Signed,
    arith_mask: None,
    index_offset: 0,
    mem_offset: 0,
    align: 2,
    mems_size: 4,
};

pub const U64_FIELD: FieldInfo = FieldInfo {
    stack_type: enc::ValType::I64,
    signedness: Signedness::Unsigned,
    arith_mask: None,
    index_offset: 0,
    mem_offset: 0,
    align: 3,
    mems_size: 8,
};

pub const S64_FIELD: FieldInfo = FieldInfo {
    stack_type: enc::ValType::I64,
    signedness: Signedness::Signed,
    arith_mask: None,
    index_offset: 0,
    mem_offset: 0,
    align: 3,
    mems_size: 8,
};

pub const F32_FIELD: FieldInfo = FieldInfo {
    stack_type: enc::ValType::F32,
    signedness: Signedness::Unsigned,
    arith_mask: None,
    index_offset: 0,
    mem_offset: 0,
    align: 2,
    mems_size: 4,
};

pub const F64_FIELD: FieldInfo = FieldInfo {
    stack_type: enc::ValType::F64,
    signedness: Signedness::Unsigned,
    arith_mask: None,
    index_offset: 0,
    mem_offset: 0,
    align: 3,
    mems_size: 8,
};

pub const STRING_OFFSET_FIELD: FieldInfo = FieldInfo {
    stack_type: enc::ValType::I32,
    signedness: Signedness::Unsigned,
    arith_mask: None,
    index_offset: 0,
    mem_offset: 0,
    align: 2,
    mems_size: 4,
};

pub const STRING_LENGTH_FIELD: FieldInfo = FieldInfo {
    stack_type: enc::ValType::I32,
    signedness: Signedness::Unsigned,
    arith_mask: None,
    index_offset: 1,
    mem_offset: 4,
    align: 2,
    mems_size: 4,
};
