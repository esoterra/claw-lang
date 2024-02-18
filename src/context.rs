pub struct C<'ctx, T, Context> {
    pub value: &'ctx T,
    pub context: &'ctx Context
}

pub trait WithContext<Context>
where
    Self: Sized
{
    fn with<'ctx>(&'ctx self, context: &'ctx Context) -> C<'ctx, Self, Context> {
        C {
            value: self,
            context
        }
    }
}

impl<T, Context> WithContext<Context> for T {}


