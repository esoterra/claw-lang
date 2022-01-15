use super::{ParserError, ParseInput};

use crate::{ast::{expressions::{Expression, Literal, BinaryOp}, M, MBox, Place}, lexer::Token};

pub fn parse_expression(input: &mut ParseInput) -> Result<MBox<Expression>, ParserError> {
    let mut root = parse_leaf(input)?;
    while !input.done() && input.peek().unwrap().token == Token::Add {
        let add = input.assert_next(Token::Add)?;
        let next_leaf = parse_leaf(input)?;
        let left = root.span.clone();
        let right = next_leaf.span.clone();
        let new_root = Expression::Binary {
            left: root,
            operator: M::new(BinaryOp::Add, add),
            right: next_leaf
        };
        root = MBox::new_range(new_root, left, right)
    }
    Ok(root)
}

fn parse_leaf(input: &mut ParseInput) -> Result<MBox<Expression>, ParserError> {
    let checkpoint = input.checkpoint();
    if let Ok(value) = parse_parenthetical(input) {
        return Ok(value);
    }
    input.restore(checkpoint);
    if let Ok(value) = parse_literal(input) {
        let span = value.span.clone();
        return Ok(MBox::new(Expression::Literal { value }, span))
    }
    input.restore(checkpoint);
    if let Ok(place) = parse_place(input) {
        let span = place.span.clone();
        return Ok(MBox::new(Expression::Place { place }, span))
    }

    Err(ParserError::UnexpectedToken)
}

fn parse_parenthetical(input: &mut ParseInput) -> Result<MBox<Expression>, ParserError> {
    let _left = input.assert_next(Token::LParen)?;
    let inner = parse_expression(input)?;
    let _right = input.assert_next(Token::LParen)?;
    Ok(inner)
}

/// A place expression describes a memory location
/// e.g. foobar, foobar[0], foobar[10][0]
/// For now, ony plain identifiers are supported
pub fn parse_place(input: &mut ParseInput) -> Result<M<Place>, ParserError> {
    let next = input.next()?;
    if let Token::Identifier(ident) = &next.token {
        let ident_m = M::new(ident.clone(), next.span.clone());
        Ok(M::new(
            Place::Identifier {
                ident: ident_m
            },
            next.span.clone()
        ))
    } else {
        Err(ParserError::UnexpectedToken)
    }
}

fn parse_literal(input: &mut ParseInput) -> Result<M<Literal>, ParserError> {
    let next = input.next()?;
    match &next.token {
        Token::StringLiteral(_value) => Err(ParserError::NotYetSupported),
        Token::DecIntLiteral(value) => Ok(
            M::new(Literal::Integer(*value), next.span.clone())
        ),
        Token::DecFloatLiteral(_value) => Err(ParserError::NotYetSupported),
        Token::BinLiteral(_value) => Err(ParserError::NotYetSupported),
        Token::HexLiteral(_value) => Err(ParserError::NotYetSupported),
        _ => Err(ParserError::UnexpectedToken)
    }
}
