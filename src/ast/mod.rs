pub mod component;
pub mod expressions;
pub mod statements;
pub mod types;

use cranelift_entity::entity_impl;
use miette::SourceSpan;

pub type Span = SourceSpan;

pub use component::*;
pub use expressions::*;
pub use statements::*;
pub use types::*;

pub fn merge(left: &Span, right: &Span) -> Span {
    let left_most = left.offset();
    let right_most = right.offset() + right.len();
    let len = right_most - left_most;
    Span::from((left_most, len))
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NameId(u32);
entity_impl!(NameId, "name");

#[cfg(test)]
impl ContextEq<Component> for NameId {
    fn context_eq(&self, other: &Self, context: &Component) -> bool {
        let self_str = context.get_name(*self);
        let other_str = context.get_name(*other);
        let str_eq = self_str == other_str;

        let self_span = context.name_span(*self);
        let other_span = context.name_span(*other);
        let span_eq = self_span == other_span;

        str_eq && span_eq
    }
}
