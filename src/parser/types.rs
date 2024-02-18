use crate::ast::{Component, PrimitiveType, TypeId, ValType};
use crate::lexer::Token;
use crate::parser::{ParseInput, ParserError};

pub fn parse_valtype(input: &mut ParseInput, comp: &mut Component) -> Result<TypeId, ParserError> {
    let next = input.next()?;
    let span = next.span.clone();
    let valtype = match next.token {
        Token::U32 => ValType::Primitive(PrimitiveType::U32),
        Token::U64 => ValType::Primitive(PrimitiveType::U64),
        Token::S32 => ValType::Primitive(PrimitiveType::S32),
        Token::S64 => ValType::Primitive(PrimitiveType::S64),
        Token::F32 => ValType::Primitive(PrimitiveType::F32),
        Token::F64 => ValType::Primitive(PrimitiveType::F64),
        _ => return Err(input.unsupported_error("Unsupported value type")),
    };
    let name_id = comp.new_type(valtype, span);
    Ok(name_id)
}
