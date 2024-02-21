mod diagnostic;
mod stack_map;

use std::sync::Arc;
use miette::NamedSource;

pub use diagnostic::*;
pub use stack_map::*;

pub type Source = Arc<NamedSource<String>>;

pub fn make_source(name: &str, source: &str) -> Source {
    Arc::new(NamedSource::new(name, source.to_owned()))
}