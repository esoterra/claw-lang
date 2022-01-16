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
    let _right = input.assert_next(Token::RParen)?;
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
        let cases = [
            ("0", 0, make_span(0, 1)),
            ("1", 1, make_span(0, 1)),
            ("32", 32, make_span(0, 2)),
            ("129", 129, make_span(0, 3))
        ];
        for (source, value, span) in cases {
            let parsed_literal = M::new(Literal::Integer(value), span.clone());
            let parsed_expression = MBox::new(
                Expression::Literal {
                    value: M::new(Literal::Integer(value), span.clone())
                },
                span.clone()
            );
            assert_eq!(
                parse_literal(&mut make_input(source)).unwrap(),
                parsed_literal
            );
            assert_eq!(
                parse_leaf(&mut make_input(source)).unwrap(),
                parsed_expression
            );
            assert_eq!(
                parse_expression(&mut make_input(source)).unwrap(),
                parsed_expression
            );
        }
    }

    #[test]
    fn parsing_supports_idents() {
        let cases = [
            ("foo", make_span(0, 3)),
            ("foobar", make_span(0, 6)),
            ("asdf", make_span(0, 4)),
            ("asdf2", make_span(0, 5))
        ];
        for (source, span) in cases {
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
            assert_eq!(
                parse_place(&mut make_input(source)).unwrap(),
                parsed_place
            );
            assert_eq!(
                parse_leaf(&mut make_input(source)).unwrap(),
                parsed_expression
            );
            assert_eq!(
                parse_expression(&mut make_input(source)).unwrap(),
                parsed_expression
            );
        }
    }

    #[test]
    fn parsing_supports_parenthesized_idents() {
        let cases = [
            ("(foo)", "foo", make_span(1, 3)),
            ("(foobar)", "foobar", make_span(1, 6)),
            ("(asdf)", "asdf", make_span(1, 4)),
            ("(asdf2)", "asdf2", make_span(1, 5))
        ];
        for (source, ident, span) in cases {
            let parsed_expression = MBox::new(
                Expression::Place {
                    place: M::new(
                        Place::Identifier {
                            ident: M::new(ident.to_string(), span.clone())
                        },
                        span.clone()
                    )
                },
                span.clone()
            );
            assert_eq!(
                parse_parenthetical(&mut make_input(source)).unwrap(),
                parsed_expression
            );
            assert_eq!(
                parse_leaf(&mut make_input(source)).unwrap(),
                parsed_expression
            );
            assert_eq!(
                parse_expression(&mut make_input(source)).unwrap(),
                parsed_expression
            );
        }
    }
}