use crate::ast::{Component, NameId, PackageName};
use crate::lexer::Token;
use crate::{ParseInput, ParserError};

/// Parse an identifier
pub fn parse_ident(input: &mut ParseInput, comp: &mut Component) -> Result<NameId, ParserError> {
    match &input.peek()?.token {
        Token::Identifier(ident) => {
            let ident = ident.clone();
            let span = input.next().unwrap().span;
            Ok(comp.new_name(ident, span))
        }
        _ => Err(input.unexpected_token("Expected identifier")),
    }
}

/// Parse an interface name into package and interface portions
pub fn parse_interface_name(input: &mut ParseInput) -> Result<(PackageName, String), ParserError> {
    let namespace = parse_identifier(input)?;
    input.assert_next(
        Token::Colon,
        "Package namespace and name must be separated by a colon",
    )?;
    let name = parse_identifier(input)?;
    input.assert_next(Token::Div, "Interface name comes after a '/'")?;
    let interface = parse_identifier(input)?;

    let package = PackageName {
        namespace,
        name,
        version: None,
    };
    Ok((package, interface))
}

fn parse_identifier(input: &mut ParseInput) -> Result<String, ParserError> {
    match &input.next()?.token {
        Token::Identifier(ident) => Ok(ident.clone()),
        _ => Err(input.unexpected_token("Expected identifier")),
    }
}
