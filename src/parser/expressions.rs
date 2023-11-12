use crate::lexer::Token;
use crate::ast::{
    M, MBox,
    expressions::{Expression, Literal, BinaryOp}
};
use crate::parser::{ParserError, ParseInput};

pub fn parse_expression(input: &mut ParseInput) -> Result<MBox<Expression>, ParserError> {
    pratt_parse(input, 0)
}

/// Pratt parsing of expressions based on
/// https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
fn pratt_parse(input: &mut ParseInput, min_bp: u8) -> Result<MBox<Expression>, ParserError> {
    let mut lhs = parse_leaf(input)?;
    loop {
        let checkpoint = input.checkpoint();
        if let Some(bin_op) = try_parse_bin_op(input) {
            let (l_bp, r_bp) = infix_binding_power(bin_op.value);

            if l_bp < min_bp {
                input.restore(checkpoint);
                break;
            }

            let rhs = pratt_parse(input, r_bp)?;
            let left = lhs.span.clone();
            let right = rhs.span.clone();
            let new_root = Expression::Binary {
                left: lhs,
                operator: bin_op,
                right: rhs
            };
            lhs = MBox::new_range(new_root, left, right)
        } else {
            break;
        }
    }
    Ok(lhs)
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
    if let Ok(ident) = parse_ident(input) {
        let span = ident.span.clone();
        return Ok(MBox::new(Expression::Identifier { ident }, span))
    }
    Err(input.unexpected_token("Parse Leaf"))
}

fn parse_parenthetical(input: &mut ParseInput) -> Result<MBox<Expression>, ParserError> {
    let _left = input.assert_next(Token::LParen, "Left parenthesis '('")?;
    let inner = parse_expression(input)?;
    let _right = input.assert_next(Token::RParen, "Right parenthesis ')'")?;
    Ok(inner)
}

/// Parse an identifier
pub fn parse_ident(input: &mut ParseInput) -> Result<M<String>, ParserError> {
    let checkpoint = input.checkpoint();
    let next = input.next()?;
    let span = next.span.clone();
    match next.token.clone() {
        Token::Identifier(ident) => {
            Ok(M::new(ident, span))
        },
        _ => {
            input.restore(checkpoint);
            Err(input.unexpected_token("Expected identifier"))
        }
    }
}

fn parse_literal(input: &mut ParseInput) -> Result<M<Literal>, ParserError> {
    let next = input.next()?;
    let value = match &next.token {
        Token::StringLiteral(_value) => return Err(input.unsupported_error("StringLiteral")),
        Token::DecIntLiteral(value) => Literal::Integer(*value),
        Token::DecFloatLiteral(value) => Literal::Float(*value),
        Token::BinLiteral(value) => Literal::Integer(*value),
        Token::HexLiteral(value) => Literal::Integer(*value),
        _ => return Err(input.unexpected_token("Parse Literal"))
    };
    Ok(M::new(value, next.span.clone()))
}

fn try_parse_bin_op(input: &mut ParseInput) -> Option<M<BinaryOp>> {
    let next = input.peek().ok()?;
    let span = next.span.clone();
    let op = match &next.token {
        Token::LogicalOr => BinaryOp::LogicalOr,
        Token::LogicalAnd => BinaryOp::LogicalAnd,

        Token::BitOr => BinaryOp::BitOr,

        Token::BitXor => BinaryOp::BitXor,

        Token::BitAnd => BinaryOp::BitAnd,

        Token::EQ => BinaryOp::EQ,
        Token::NEQ => BinaryOp::NEQ,

        Token::LT => BinaryOp::LT,
        Token::LTE => BinaryOp::LTE,
        Token::GT => BinaryOp::GT,
        Token::GTE => BinaryOp::GTE,

        Token::BitShiftL => BinaryOp::BitShiftL,
        Token::BitShiftR => BinaryOp::BitShiftR,
        Token::ArithShiftR => BinaryOp::ArithShiftR,

        Token::Add => BinaryOp::Add,
        Token::Sub => BinaryOp::Sub,

        Token::Mult => BinaryOp::Mult,
        Token::Div => BinaryOp::Div,
        Token::Mod => BinaryOp::Mod,

        _ => return None
    };
    let _ = input.next();
    Some(M::new(op, span))
}

fn infix_binding_power(op: BinaryOp) -> (u8, u8) {
    match op {
        BinaryOp::LogicalOr => (10, 1),
        BinaryOp::LogicalAnd => (20, 21),

        BinaryOp::BitOr => (30, 31),

        BinaryOp::BitXor => (40, 41),

        BinaryOp::BitAnd => (50, 51),

        BinaryOp::EQ
        | BinaryOp::NEQ => (60, 61),

        BinaryOp::LT
        | BinaryOp::LTE
        | BinaryOp::GT
        | BinaryOp::GTE => (70, 71),

        BinaryOp::BitShiftL
        | BinaryOp::BitShiftR
        | BinaryOp::ArithShiftR => (80, 81),

        BinaryOp::Add
        | BinaryOp::Sub => (90, 91),

        BinaryOp::Mult
        | BinaryOp::Div
        | BinaryOp::Mod => (100, 101),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use crate::parser::tests::{make_input, make_span};

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
            let parsed_expression = MBox::new(
                Expression::Identifier {
                    ident: M::new(source.to_string(), span.clone())
                },
                span.clone()
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
        // parenthesized, raw, raw-span
        let cases = [
            ("(foo)", "foo", make_span(1, 3)),
            ("(foobar)", "foobar", make_span(1, 6)),
            ("(asdf)", "asdf", make_span(1, 4)),
            ("(asdf2)", "asdf2", make_span(1, 5))
        ];
        for (source, ident, span) in cases {
            let parsed_expression = MBox::new(
                Expression::Identifier {
                    ident: M::new(ident.to_string(), span.clone())
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

    macro_rules! lit {
        ($val:expr => ($span_l:expr, $span_r:expr)) => {
            MBox::new(
                Expression::Literal {
                    value: M::new(Literal::Integer($val), make_span($span_l, $span_r))
                },
                make_span($span_l, $span_r)
            )
        };
    }

    macro_rules! op {
        ($op_val:expr => ($span_l:expr, $span_r:expr)) => {
            M::new($op_val, make_span($span_l, $span_r))
        };
    }

    macro_rules! bin {
        (($left:expr, $op:expr, $right:expr) => ($span_l:expr, $span_r:expr)) => {
            MBox::new(
                Expression::Binary {
                    left: $left,
                    operator: $op,
                    right: $right
                },
                make_span($span_l, $span_r)
            )
        };
    }

    #[test]
    fn parse_expression_respects_precedence() {
        let source0 = "0 + 1 * 2";
        let expected0 = bin! (
            (
                lit!(0 => (0, 1)),
                op!(BinaryOp::Add => (2, 1)),
                bin!((
                    lit!(1 => (4, 1)),
                    op!(BinaryOp::Mult => (6, 1)),
                    lit!(2 => (8, 1))
                ) => (4, 5))
            ) => (0, 9)
        );

        let source1 = "0 * 1 + 2";
        let expected1 = bin! (
            (
                bin!((
                    lit!(0 => (0, 1)),
                    op!(BinaryOp::Mult => (2, 1)),
                    lit!(1 => (4, 1))
                ) => (0, 5)),
                op!(BinaryOp::Add => (6, 1)),
                lit!(2 => (8, 1))
            ) => (0, 9)
        );

        let cases = [
            (source0, expected0),
            (source1, expected1)
        ];

        for (source, expected) in cases {
            assert_eq!(
                parse_expression(&mut make_input(source)).unwrap(),
                expected
            );
        }
    }

    #[test]
    fn parse_expression_respects_associativity() {
        let source0 = "0 + 1 + 2";
        let expected0 = bin! (
            (
                bin!((
                    lit!(0 => (0, 1)),
                    op!(BinaryOp::Add => (2, 1)),
                    lit!(1 => (4, 1))
                ) => (0, 5)),
                op!(BinaryOp::Add => (6, 1)),
                lit!(2 => (8, 1))
            ) => (0, 9)
        );

        let source1 = "0 * 1 * 2";
        let expected1 = bin! (
            (
                bin!((
                    lit!(0 => (0, 1)),
                    op!(BinaryOp::Mult => (2, 1)),
                    lit!(1 => (4, 1))
                ) => (0, 5)),
                op!(BinaryOp::Mult => (6, 1)),
                lit!(2 => (8, 1))
            ) => (0, 9)
        );

        let cases = [
            (source0, expected0),
            (source1, expected1)
        ];

        for (source, expected) in cases {
            assert_eq!(
                parse_expression(&mut make_input(source)).unwrap(),
                expected
            );
        }
    }
}