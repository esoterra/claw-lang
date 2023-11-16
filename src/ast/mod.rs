pub mod expressions;
pub mod module;
pub mod statements;
pub mod types;

use miette::SourceSpan;

pub type Span = SourceSpan;

/// The metadata wrapper type
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct M<T> {
    pub span: Span,
    pub value: T
}

impl<T> AsRef<T> for M<T> {
    fn as_ref(&self) -> &T {
        &self.value
    }
}

impl<T> M<T> {
    pub fn new(value: T, span: Span) -> Self {
        M { span, value }
    }

    pub fn new_range(value: T, left: Span, right: Span) -> Self {
        let span = merge(&left, &right);
        M { span, value }
    }
}

/// The metadata wrapper type
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct MBox<T> {
    pub span: Span,
    pub value: Box<T>
}

impl<T> MBox<T> {
    pub fn new(value: T, span: Span) -> Self {
        MBox { span, value: Box::new(value) }
    }

    pub fn new_range(value: T, left: Span, right: Span) -> Self {
        let span = merge(&left, &right);
        MBox { span, value: Box::new(value) }
    }
}

impl<T> AsRef<T> for MBox<T> {
    fn as_ref(&self) -> &T {
        &self.value
    }
}

pub fn merge(left: &Span, right: &Span) -> Span {
    let left_most = left.offset();
    let right_most = right.offset() + right.len();
    let len = right_most - left_most;
    Span::from((left_most, len))
}