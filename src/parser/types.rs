use crate::ast::{Arenas, TypeId, ValType};
use crate::lexer::Token;
use crate::parser::{ParseInput, ParserError};

pub fn parse_valtype(input: &mut ParseInput, arenas: &mut Arenas) -> Result<TypeId, ParserError> {
    let next = input.next()?;
    let span = next.span.clone();
    let valtype = match next.token {
        Token::U32 => ValType::U32,
        Token::U64 => ValType::U64,
        Token::S32 => ValType::S32,
        Token::S64 => ValType::S64,
        Token::F32 => ValType::F32,
        Token::F64 => ValType::F64,
        _ => return Err(input.unsupported_error("Unsupported value type")),
    };
    let name_id = arenas.new_type(valtype, span);
    Ok(name_id)
}
