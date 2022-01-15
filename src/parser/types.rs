use crate::lexer::Token;
use crate::ast::{
    M,
    types::{ValType, BasicVal}
};
use crate::parser::{ParseInput, ParserError};

pub fn parse_valtype(input: &mut ParseInput) -> Result<M<ValType>, ParserError> {
    let next = input.next()?;
    match next.token {
        Token::I32 => {
            let span = next.span.clone();
            Ok(M::new(ValType::Basic(BasicVal::I32), span))
        },
        _ => Err(ParserError::NotYetSupported)
    }
}