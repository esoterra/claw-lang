use crate::lexer::Token;
use crate::ast::{
    M,
    types::{ValType, BasicVal}
};
use crate::parser::{ParseInput, ParserError};

pub fn parse_valtype(input: &mut ParseInput) -> Result<M<ValType>, ParserError> {
    let next = input.next()?;
    let span = next.span.clone();
    let valtype = match next.token {
        Token::U32 => ValType::Basic(BasicVal::U32),
        Token::U64 => ValType::Basic(BasicVal::U64),
        _ => return Err(input.unsupported_error("Unsupported value type"))
    };
    Ok(M::new(valtype, span))
}