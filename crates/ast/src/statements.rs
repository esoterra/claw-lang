use cranelift_entity::entity_impl;

use super::{expressions::ExpressionId, types::TypeId, Call, NameId};

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct StatementId(u32);
entity_impl!(StatementId, "name");

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(Let),
    Assign(Assign),
    Call(Call),
    If(If),
    For(For),
    Return(Return),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Let {
    pub mutable: bool,
    pub ident: NameId,
    pub annotation: Option<TypeId>,
    pub expression: ExpressionId,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Assign {
    pub ident: NameId,
    pub expression: ExpressionId,
}

#[derive(Debug, PartialEq, Clone)]
pub struct If {
    pub condition: ExpressionId,
    pub block: Vec<StatementId>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct For {
    pub ident: NameId,
    pub annotation: Option<TypeId>,
    pub range_lower: ExpressionId,
    pub range_upper: ExpressionId,
    pub block: Vec<StatementId>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Return {
    pub expression: Option<ExpressionId>,
}
