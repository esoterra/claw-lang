use crate::lexer::Token;
use crate::ast::{
    M, MBox, Place,
    expressions::{Expression, Literal, BinaryOp}
};
use crate::parser::{ParserError, ParseInput};

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

#[cfg(test)]
mod tests {
    use crate::parser::tests::{make_input, make_span};
    use super::*;

    use crate::ast::expressions::{
        Expression, Literal
    };

    #[test]
    fn parsing_supports_dec_integer() {
        let cases = &[
            ("0", 0, make_span(0, 1)),
            ("1", 1, make_span(0, 1)),
            ("32", 32, make_span(0, 2)),
            ("129", 129, make_span(0, 3))
        ];
        for (source, value, span) in cases.into_iter() {
            let parsed_literal = M::new(Literal::Integer(*value), span.clone());
            let parsed_expression = MBox::new(
                Expression::Literal {
                    value: M::new(Literal::Integer(*value), span.clone())
                },
                span.clone()
            );
            // parse_literal
            let mut input = make_input(source);
            assert_eq!(
                parse_literal(&mut input).unwrap(),
                parsed_literal
            );
            // parse_leaf
            let mut input = make_input(source);
            assert_eq!(
                parse_leaf(&mut input).unwrap(),
                parsed_expression
            );
            // parse_expression
            let mut input = make_input(source);
            assert_eq!(
                parse_expression(&mut input).unwrap(),
                parsed_expression
            );
        }
    }

    #[test]
    fn parsing_supports_idents() {
        let cases = &[
            ("foo", make_span(0, 3)),
            ("foobar", make_span(0, 6)),
            ("asdf", make_span(0, 4)),
            ("asdf2", make_span(0, 5))
        ];
        for (source, span) in cases.into_iter() {
            let parsed_place = M::new(
                Place::Identifier {
                    ident: M::new(source.to_string(), span.clone())
                },
                span.clone()
            );
            let parsed_expression = MBox::new(
                Expression::Place {
                    place: M::new(
                        Place::Identifier {
                            ident: M::new(source.to_string(), span.clone())
                        },
                        span.clone()
                    )
                },
                span.clone()
            );
            // parse_place
            let mut input = make_input(source);
            assert_eq!(
                parse_place(&mut input).unwrap(),
                parsed_place
            );
            // parse_leaf
            let mut input = make_input(source);
            assert_eq!(
                parse_leaf(&mut input).unwrap(),
                parsed_expression
            );
            // parse_expression
            let mut input = make_input(source);
            assert_eq!(
                parse_expression(&mut input).unwrap(),
                parsed_expression
            );
        }
    }
}