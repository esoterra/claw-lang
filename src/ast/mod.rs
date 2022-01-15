pub mod expressions;
pub mod module;
pub mod statements;
pub mod types;

use miette::SourceSpan;

pub type Span = SourceSpan;

/// The metadata wrapper type
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct M<T> {
    pub span: Span,
    pub value: T
}

impl<T> M<T> {
    pub fn new(span: Span, value: T) -> Self {
        M { span, value }
    }
}

/// The metadata wrapper type
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MBox<T> {
    pub span: Span,
    pub value: Box<T>
}

/// A place value represents a memory location
/// e.g. foobar, foobar[0], foobar[0..199]
/// Places can be dereferenced, assigned to, and operated on
/// They are used in expressions and statements
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Place {
    Identifier {
        ident: M<String>
    }
}