use miette::{Diagnostic, NamedSource, Report, SourceSpan};
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
#[error("oops!")]
struct StructError {
    // The Source that we're gonna be printing snippets out of.
    // This can be a String if you don't have or care about file names.
    #[source_code]
    src: NamedSource<String>,
    // Snippets and highlights can be included in the diagnostic!
    #[label("This bit here")]
    bad_bit: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
enum EnumError {
    #[error("oops!")]
    Base {
        // The Source that we're gonna be printing snippets out of.
        // This can be a String if you don't have or care about file names.
        #[source_code]
        src: NamedSource<String>,
        // Snippets and highlights can be included in the diagnostic!
        #[label("This bit here")]
        bad_bit: SourceSpan,
    },
}

#[derive(Error, Debug, Diagnostic)]
enum NestedEnumError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Error(#[from] EnumError),
}

#[test]
fn test_miette_struct_error() {
    let src = NamedSource::new("foobar.txt", "[1.3, \"foobar\", false]".to_owned());
    let error = StructError {
        src,
        bad_bit: SourceSpan::from((6, 8)),
    };
    println!("{:?}", Report::new(error));

    let src = NamedSource::new("foobar.txt", "[1.3, \"foobar\", false]".to_owned());
    let error = EnumError::Base {
        src,
        bad_bit: SourceSpan::from((6, 8)),
    };
    println!("{:?}", Report::new(error));

    let src = NamedSource::new("foobar.txt", "[1.3, \"foobar\", false]".to_owned());
    let error = EnumError::Base {
        src,
        bad_bit: SourceSpan::from((6, 8)),
    };
    let error = NestedEnumError::Error(error);
    println!("{:?}", Report::new(error));

    // panic!();
}
