use crate::lexer::Token;
use crate::ast::{
    M,
    types::{ValType, BasicVal}
};
use crate::parser::{ParseInput, ParserError};

pub fn parse_valtype(input: &mut ParseInput) -> Result<M<ValType>, ParserError> {
    let next = input.next()?;
    match next.token {
        Token::U32 => {
            let span = next.span.clone();
            Ok(M::new(ValType::Basic(BasicVal::U32), span))
        },
        _ => Err(input.unsupported_error("Non-U32 Types"))
    }
}