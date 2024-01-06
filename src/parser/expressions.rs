use crate::ast::{NameId, Call};
use crate::ast::expressions::ExpressionId;
use crate::lexer::Token;
use crate::ast::{
    M,
    expressions::{Expression, ExpressionData, Literal, BinaryOp}
};
use crate::parser::{ParserError, ParseInput};

pub fn parse_expression(input: &mut ParseInput, data: &mut ExpressionData) -> Result<ExpressionId, ParserError> {
    pratt_parse(input, data, 0)
}

/// Pratt parsing of expressions based on
/// https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
fn pratt_parse(input: &mut ParseInput, data: &mut ExpressionData, min_bp: u8) -> Result<ExpressionId, ParserError> {
    let mut lhs = parse_leaf(input, data)?;
    loop {
        let checkpoint = input.checkpoint();
        if let Some(bin_op) = try_parse_bin_op(input) {
            let (l_bp, r_bp) = infix_binding_power(bin_op.value);

            if l_bp < min_bp {
                input.restore(checkpoint);
                break;
            }

            let rhs = pratt_parse(input, data, r_bp)?;
            let new_root = Expression::Binary {
                left: lhs,
                operator: bin_op,
                right: rhs
            };
            lhs = data.alloc_merge(new_root, lhs, rhs);
        } else {
            break;
        }
    }
    Ok(lhs)
}

fn parse_leaf(input: &mut ParseInput, data: &mut ExpressionData) -> Result<ExpressionId, ParserError> {
    if input.peek()?.token == Token::LParen {
        return parse_parenthetical(input, data);
    }
    if matches!(input.peekn(0), Some(Token::Identifier(_)))
        && matches!(input.peekn(1), Some(Token::LParen))  {
        let call = parse_call(input, data)?;
        let id = data.alloc(Expression::Call { call: call.value.clone() }, call.span.clone());
        return Ok(id);
    }
    if matches!(input.peek()?.token, Token::Identifier(_)) {
        let ident = parse_ident(input)?;
        let span = ident.span.clone();
        let id = data.alloc(Expression::Identifier { ident, name_id: NameId::new() }, span);
        return Ok(id);
    }
    let literal = parse_literal(input)?;
    let span = literal.span.clone();
    let id = data.alloc(Expression::Literal { literal }, span);
    return Ok(id)
}

fn parse_parenthetical(input: &mut ParseInput, data: &mut ExpressionData) -> Result<ExpressionId, ParserError> {
    let _left = input.assert_next(Token::LParen, "Left parenthesis '('")?;
    let inner = parse_expression(input, data)?;
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

fn parse_call(input: &mut ParseInput, data: &mut ExpressionData) -> Result<M<Call>, ParserError> {
    let first = input.next()?;

    let ident = match first.token.clone() {
        Token::Identifier(ident) => ident,
        _ => return Err(input.unexpected_token("Parse expression identifier"))
    };
    let ident = M::new(ident, first.span.clone());
    let name_id = NameId::new();

    let lparen = input.assert_next(Token::LParen, "Function arguments")?;

    let mut args = Vec::new();
    let rparen = loop {
        args.push(parse_expression(input, data)?);

        let token = input.next()?;
        match token.token {
            Token::Comma => continue,
            Token::RParen => break token.span.clone(),
            _ => return Err(input.unexpected_token("Argument list"))
        }
    };

    Ok(M::new_range(Call { ident, name_id, args }, lparen, rparen))
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
        let mut data = ExpressionData::default();
        let cases = [
            ("0", 0, make_span(0, 1)),
            ("1", 1, make_span(0, 1)),
            ("32", 32, make_span(0, 2)),
            ("129", 129, make_span(0, 3))
        ];
        for (source, value, span) in cases {
            let parsed_literal = M::new(Literal::Integer(value), span.clone());
            let expected_expression = data.alloc(Expression::Literal {
                literal: M::new(Literal::Integer(value), span.clone())
            }, span);
            assert_eq!(
                parse_literal(&mut make_input(source)).unwrap(),
                parsed_literal
            );
            let found_leaf = parse_leaf(&mut make_input(source), &mut data).unwrap();
            assert!(data.eq(found_leaf, expected_expression));
            let found_expression = parse_expression(&mut make_input(source), &mut data).unwrap();
            assert!(data.eq(found_expression, expected_expression));
        }
    }

    #[test]
    fn parsing_supports_idents() {
        let mut data = ExpressionData::default();
        let cases = [
            ("foo", make_span(0, 3)),
            ("foobar", make_span(0, 6)),
            ("asdf", make_span(0, 4)),
            ("asdf2", make_span(0, 5))
        ];
        for (source, span) in cases {
            let expected_expression = data.alloc(Expression::Identifier {
                ident: M::new(source.to_string(), span.clone()),
                name_id: NameId::new()
            }, span.clone());
            let found_ident = parse_ident(&mut make_input(source)).unwrap();
            assert_eq!(M::new(source.to_owned(), span.clone()), found_ident);
            let found_leaf = parse_leaf(&mut make_input(source), &mut data).unwrap();
            assert!(data.eq(found_leaf, expected_expression));
            let found_expression = parse_expression(&mut make_input(source), &mut data).unwrap();
            assert!(data.eq(found_expression, expected_expression));
        }
    }

    #[test]
    fn parsing_supports_parenthesized_idents() {
        let mut data = ExpressionData::default();
        // parenthesized, raw, raw-span
        let cases = [
            ("(foo)", "foo", make_span(1, 3)),
            ("(foobar)", "foobar", make_span(1, 6)),
            ("(asdf)", "asdf", make_span(1, 4)),
            ("(asdf2)", "asdf2", make_span(1, 5))
        ];
        for (source, ident, span) in cases {
            let expected_expression = data.alloc(Expression::Identifier {
                ident: M::new(ident.to_string(), span.clone()),
                name_id: NameId::new()
            }, span);
            let found_expression = parse_parenthetical(&mut make_input(source), &mut data).unwrap();
            assert!(data.eq(found_expression, expected_expression));
            let found_expression = parse_leaf(&mut make_input(source), &mut data).unwrap();
            assert!(data.eq(found_expression, expected_expression));
            let found_expression = parse_expression(&mut make_input(source), &mut data).unwrap();
            assert!(data.eq(found_expression, expected_expression));
        }
    }

    #[test]
    fn parse_expression_respects_precedence() {
        let mut data = ExpressionData::default();

        macro_rules! lit {
            ($val:expr => ($span_l:expr, $span_r:expr)) => {
                {
                    let expr = $val;
                    let span = make_span($span_l, $span_r);
                    data.alloc(Expression::Literal { literal: M::new(Literal::Integer(expr), span.clone()) }, span)
                }
            };
        }
    
        macro_rules! op {
            ($op_val:expr => ($span_l:expr, $span_r:expr)) => {
                M::new($op_val, make_span($span_l, $span_r))
            };
        }
    
        macro_rules! bin {
            ($left:expr, $op:expr, $right:expr) => {
                {
                    let lhs = $left;
                    let rhs = $right;
                    let bin_expr = Expression::Binary {
                        left: lhs,
                        operator: $op,
                        right: rhs
                    };
                    data.alloc_merge(bin_expr, lhs, rhs)
                }
            };
        }

        let source0 = "0 + 1 * 2";
        let expected0 = bin!(
            lit!(0 => (0, 1)),
            op!(BinaryOp::Add => (2, 1)),
            bin!(
                lit!(1 => (4, 1)),
                op!(BinaryOp::Mult => (6, 1)),
                lit!(2 => (8, 1))
            )
        );

        let source1 = "0 * 1 + 2";
        let expected1 = bin!(
            bin!(
                lit!(0 => (0, 1)),
                op!(BinaryOp::Mult => (2, 1)),
                lit!(1 => (4, 1))
            ),
            op!(BinaryOp::Add => (6, 1)),
            lit!(2 => (8, 1))
        );

        let cases = [
            (source0, expected0),
            (source1, expected1)
        ];

        for (source, expected) in cases {
            let result = parse_expression(&mut make_input(source), &mut data).unwrap();
            assert!(data.eq(result, expected));
        }
    }

    #[test]
    fn parse_expression_respects_associativity() {
        let mut data = ExpressionData::default();

        macro_rules! lit {
            ($val:expr => ($span_l:expr, $span_r:expr)) => {
                {
                    let expr = $val;
                    let span = make_span($span_l, $span_r);
                    data.alloc(Expression::Literal { literal: M::new(Literal::Integer(expr), span.clone()) }, span)
                }
            };
        }
    
        macro_rules! op {
            ($op_val:expr => ($span_l:expr, $span_r:expr)) => {
                M::new($op_val, make_span($span_l, $span_r))
            };
        }
    
        macro_rules! bin {
            ($left:expr, $op:expr, $right:expr) => {
                {
                    let lhs = $left;
                    let rhs = $right;
                    let bin_expr = Expression::Binary {
                        left: lhs,
                        operator: $op,
                        right: rhs
                    };
                    data.alloc_merge(bin_expr, lhs, rhs)
                }
            };
        }
    
        let source0 = "0 + 1 + 2";
        let expected0 = bin!(
            bin!(
                lit!(0 => (0, 1)),
                op!(BinaryOp::Add => (2, 1)),
                lit!(1 => (4, 1))
            ),
            op!(BinaryOp::Add => (6, 1)),
            lit!(2 => (8, 1))
        );

        let source1 = "0 * 1 * 2";
        let expected1 = bin!(
            bin!(
                lit!(0 => (0, 1)),
                op!(BinaryOp::Mult => (2, 1)),
                lit!(1 => (4, 1))
            ),
            op!(BinaryOp::Mult => (6, 1)),
            lit!(2 => (8, 1))
        );

        let cases = [
            (source0, expected0),
            (source1, expected1)
        ];

        for (source, expected) in cases {
            let result = parse_expression(&mut make_input(source), &mut data).unwrap();
            assert!(data.eq(result, expected));
        }
    }
}