use std::sync::Arc;

use miette::{SourceCode, SourceSpan};

/// A source file and its contents.
#[derive(Clone)]
pub struct ArcSource {
    name: Arc<String>,
    text: Arc<String>,
}

impl std::fmt::Debug for ArcSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ArcSource")
            .field("name", &self.name)
            .field("source", &"<redacted>");
        Ok(())
    }
}

impl ArcSource {
    pub(crate) fn new(name: Arc<String>, text: Arc<String>) -> Self {
        Self { name, text }
    }

    pub fn from_owned(name: String, text: String) -> Self {
        Self::new(Arc::new(name), Arc::new(text))
    }

    /// Get this source's file name.
    pub fn name(&self) -> &Arc<String> {
        &self.name
    }

    /// Get this source's text contents.
    pub fn text(&self) -> &Arc<String> {
        &self.name
    }
}

impl SourceCode for ArcSource {
    fn read_span<'a>(
        &'a self,
        span: &SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> std::result::Result<Box<dyn miette::SpanContents<'a> + 'a>, miette::MietteError> {
        let contents = self
            .text
            .read_span(span, context_lines_before, context_lines_after)?;
        Ok(Box::new(miette::MietteSpanContents::new_named(
            self.name.to_string(),
            contents.data(),
            contents.span().clone(),
            contents.line(),
            contents.column(),
            contents.line_count(),
        )))
    }
}
