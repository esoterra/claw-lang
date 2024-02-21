use miette::{Diagnostic, Report};

pub trait UnwrapPretty {
    type Output;

    fn unwrap_pretty(self) -> Self::Output;
}

impl<T, E> UnwrapPretty for Result<T, E>
where
    E: Diagnostic + Sync + Send + 'static,
{
    type Output = T;

    fn unwrap_pretty(self) -> Self::Output {
        match self {
            Ok(output) => output,
            Err(diagnostic) => {
                panic!("{:?}", Report::new(diagnostic));
            }
        }
    }
}

pub trait OkPretty {
    type Output;

    fn ok_pretty(self) -> Option<Self::Output>;
}

impl<T, E> OkPretty for Result<T, E>
where
    E: Diagnostic + Sync + Send + 'static,
{
    type Output = T;

    fn ok_pretty(self) -> Option<Self::Output> {
        match self {
            Ok(output) => Some(output),
            Err(diagnostic) => {
                println!("{:?}", Report::new(diagnostic));
                None
            }
        }
    }
}
