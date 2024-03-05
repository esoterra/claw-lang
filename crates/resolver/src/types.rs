use ast::TypeId;
use claw_ast as ast;

use crate::imports::ImportTypeId;

#[derive(Clone, Copy, Debug)]
pub enum ResolvedType {
    Primitive(ast::PrimitiveType),
    Import(ImportTypeId),
    Defined(TypeId),
}

impl From<TypeId> for ResolvedType {
    fn from(value: TypeId) -> Self {
        ResolvedType::Defined(value)
    }
}

pub const RESOLVED_BOOL: ResolvedType = ResolvedType::Primitive(ast::PrimitiveType::Bool);

impl std::fmt::Display for ResolvedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolvedType::Primitive(p) => (p as &dyn std::fmt::Debug).fmt(f),
            ResolvedType::Import(_) => write!(f, "imported type"),
            ResolvedType::Defined(v) => (v as &dyn std::fmt::Debug).fmt(f),
        }
    }
}

impl ResolvedType {
    pub fn type_eq(&self, other: &ResolvedType, comp: &ast::Component) -> bool {
        match (*self, *other) {
            // Both primitive
            (ResolvedType::Primitive(left), ResolvedType::Primitive(right)) => left == right,
            // Both valtype
            (ResolvedType::Defined(left), ResolvedType::Defined(right)) => {
                let l_valtype = comp.get_type(left);
                let r_valtype = comp.get_type(right);
                l_valtype.eq(r_valtype, comp)
            }
            // One primitive, other valtype
            (ResolvedType::Primitive(p), ResolvedType::Defined(v))
            | (ResolvedType::Defined(v), ResolvedType::Primitive(p)) => {
                let valtype = comp.get_type(v);
                match valtype {
                    ast::ValType::Primitive(p2) => p == *p2,
                    _ => false,
                }
            }
            _ => todo!(),
        }
    }
}
