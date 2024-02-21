use crate::lexer::Token;
use crate::{ParseInput, ParserError};
use ast::{Component, PrimitiveType, TypeId, ValType};
use claw_ast as ast;

pub fn parse_valtype(input: &mut ParseInput, comp: &mut Component) -> Result<TypeId, ParserError> {
    let next = input.next()?;
    let span = next.span;
    let valtype = match next.token {
        Token::Bool => ValType::Primitive(PrimitiveType::Bool),
        Token::U8 => ValType::Primitive(PrimitiveType::U8),
        Token::U16 => ValType::Primitive(PrimitiveType::U16),
        Token::U32 => ValType::Primitive(PrimitiveType::U32),
        Token::U64 => ValType::Primitive(PrimitiveType::U64),
        Token::S8 => ValType::Primitive(PrimitiveType::S8),
        Token::S16 => ValType::Primitive(PrimitiveType::S16),
        Token::S32 => ValType::Primitive(PrimitiveType::S32),
        Token::S64 => ValType::Primitive(PrimitiveType::S64),
        Token::F32 => ValType::Primitive(PrimitiveType::F32),
        Token::F64 => ValType::Primitive(PrimitiveType::F64),
        _ => return Err(input.unexpected_token("Not a legal type")),
    };
    let name_id = comp.new_type(valtype, span);
    Ok(name_id)
}
