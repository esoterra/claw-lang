use crate::lexer::Token;
use crate::{ParseInput, ParserError};
use ast::{EnumLiteral, Expression};
use claw_ast as ast;
use claw_ast::{self, expressions::BinaryOp, expressions::ExpressionId, merge, Component, UnaryOp};

use crate::names::parse_ident;

pub fn parse_expression(
    input: &mut ParseInput,
    comp: &mut Component,
) -> Result<ExpressionId, ParserError> {
    pratt_parse(input, comp, 0)
}

/// Pratt parsing of expressions based on
/// https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
fn pratt_parse(
    input: &mut ParseInput,
    comp: &mut Component,
    min_bp: u8,
) -> Result<ExpressionId, ParserError> {
    let mut lhs = match peek_unary_op(input) {
        Some(op) => {
            let ((), r_bp) = prefix_binding_power(op);
            let start_span = input.next().unwrap().span;
            let rhs = pratt_parse(input, comp, r_bp)?;
            let end_span = comp.expr().get_span(rhs);
            let span = merge(&start_span, &end_span);
            comp.expr_mut().alloc_unary_op(op, rhs, span)
        }
        None => parse_leaf(input, comp)?,
    };

    loop {
        let bin_op = match peek_bin_op(input) {
            Some(op) => op,
            None => break,
        };

        let (l_bp, r_bp) = infix_binding_power(bin_op);
        if l_bp < min_bp {
            break;
        }

        let _ = input.next(); // Consumes peeked operator
        let rhs = pratt_parse(input, comp, r_bp)?;
        lhs = comp.expr_mut().alloc_bin_op(bin_op, lhs, rhs);
    }
    Ok(lhs)
}

fn parse_leaf(input: &mut ParseInput, comp: &mut Component) -> Result<ExpressionId, ParserError> {
    let peek0 = &input.peek()?.token;
    let peek1 = input.peekn(1);
    match (peek0, peek1) {
        (Token::LParen, _) => parse_parenthetical(input, comp),
        (Token::Identifier(_), Some(Token::LParen)) => parse_call(input, comp),
        (Token::Identifier(_), Some(Token::Colon)) => parse_enum(input, comp),
        (Token::Identifier(_), _) => parse_ident_expr(input, comp),
        _ => parse_literal(input, comp),
    }
}

fn parse_parenthetical(
    input: &mut ParseInput,
    comp: &mut Component,
) -> Result<ExpressionId, ParserError> {
    let _left = input.assert_next(Token::LParen, "Left parenthesis '('")?;
    let inner = parse_expression(input, comp)?;
    let _right = input.assert_next(Token::RParen, "Right parenthesis ')'")?;
    Ok(inner)
}

/// Parse an identifier
pub fn parse_ident_expr(
    input: &mut ParseInput,
    comp: &mut Component,
) -> Result<ExpressionId, ParserError> {
    match &input.peek()?.token {
        Token::Identifier(ident) => {
            let ident = ident.clone();
            let span = input.next().unwrap().span;
            let ident = comp.new_name(ident, span);
            Ok(comp.expr_mut().alloc_ident(ident, span))
        }
        _ => Err(input.unexpected_token("Parsing identifier expression")),
    }
}

fn parse_literal(
    input: &mut ParseInput,
    comp: &mut Component,
) -> Result<ExpressionId, ParserError> {
    let next = input.next()?;
    let span = next.span;
    let literal = match &next.token {
        Token::StringLiteral(value) => ast::Literal::String(value.to_owned()),
        Token::IntLiteral(value) => ast::Literal::Integer(*value),
        Token::FloatLiteral(value) => ast::Literal::Float(*value),
        _ => return Err(input.unexpected_token("Parse Literal")),
    };
    Ok(comp.expr_mut().alloc_literal(literal, span))
}

fn parse_call(input: &mut ParseInput, comp: &mut Component) -> Result<ExpressionId, ParserError> {
    let ident = parse_ident(input, comp)?;
    let start_span = comp.name_span(ident);
    input.assert_next(Token::LParen, "Function arguments")?;

    let mut args = Vec::new();
    let end_span = loop {
        if let Some(span) = input.next_if(Token::RParen) {
            break span;
        }

        args.push(parse_expression(input, comp)?);

        let token = input.next()?;
        match token.token {
            Token::Comma => continue,
            Token::RParen => break token.span,
            _ => return Err(input.unexpected_token("Argument list")),
        }
    };

    let span = merge(&start_span, &end_span);

    Ok(comp.expr_mut().alloc_call(ident, args, span))
}

fn parse_enum(input: &mut ParseInput, comp: &mut Component) -> Result<ExpressionId, ParserError> {
    let enum_name = parse_ident(input, comp)?;
    input.assert_next(
        Token::Colon,
        "Enum type name and case are separated by '::'",
    )?;
    input.assert_next(
        Token::Colon,
        "Enum type name and case are separated by '::'",
    )?;
    let case_name = parse_ident(input, comp)?;

    let enum_lit = Expression::Enum(EnumLiteral {
        enum_name,
        case_name,
    });
    let span = merge(&comp.name_span(enum_name), &comp.name_span(case_name));

    Ok(comp.expr_mut().alloc(enum_lit, span))
}

fn peek_unary_op(input: &mut ParseInput) -> Option<UnaryOp> {
    let next = input.peek().ok()?;
    let op = match &next.token {
        Token::Sub => UnaryOp::Negate,
        _ => return None,
    };
    Some(op)
}

fn prefix_binding_power(op: UnaryOp) -> ((), u8) {
    match op {
        UnaryOp::Negate => ((), 200),
    }
}

fn peek_bin_op(input: &mut ParseInput) -> Option<BinaryOp> {
    let next = input.peek().ok()?;
    let op = match &next.token {
        Token::LogicalOr => BinaryOp::LogicalOr,
        Token::LogicalAnd => BinaryOp::LogicalAnd,

        Token::BitOr => BinaryOp::BitOr,

        Token::BitXor => BinaryOp::BitXor,

        Token::BitAnd => BinaryOp::BitAnd,

        Token::EQ => BinaryOp::Equals,
        Token::NEQ => BinaryOp::NotEquals,

        Token::LT => BinaryOp::LessThan,
        Token::LTE => BinaryOp::LessThanEqual,
        Token::GT => BinaryOp::GreaterThan,
        Token::GTE => BinaryOp::GreaterThanEqual,

        Token::BitShiftL => BinaryOp::BitShiftL,
        Token::BitShiftR => BinaryOp::BitShiftR,
        Token::ArithShiftR => BinaryOp::ArithShiftR,

        Token::Add => BinaryOp::Add,
        Token::Sub => BinaryOp::Subtract,

        Token::Mult => BinaryOp::Multiply,
        Token::Div => BinaryOp::Divide,
        Token::Mod => BinaryOp::Modulo,

        _ => return None,
    };
    Some(op)
}

fn infix_binding_power(op: BinaryOp) -> (u8, u8) {
    match op {
        BinaryOp::LogicalOr => (10, 1),
        BinaryOp::LogicalAnd => (20, 21),

        BinaryOp::BitOr => (30, 31),

        BinaryOp::BitXor => (40, 41),

        BinaryOp::BitAnd => (50, 51),

        BinaryOp::Equals | BinaryOp::NotEquals => (60, 61),

        BinaryOp::LessThan
        | BinaryOp::LessThanEqual
        | BinaryOp::GreaterThan
        | BinaryOp::GreaterThanEqual => (70, 71),

        BinaryOp::BitShiftL | BinaryOp::BitShiftR | BinaryOp::ArithShiftR => (80, 81),

        BinaryOp::Add | BinaryOp::Subtract => (90, 91),

        BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Modulo => (100, 101),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{make_input, make_span};
    use claw_common::UnwrapPretty;

    use claw_ast::expressions::{ContextEq, Literal};

    #[test]
    fn parsing_supports_integers() {
        let cases = [
            // Decimal
            ("0", 0, make_span(0, 1)),
            ("1", 1, make_span(0, 1)),
            ("32", 32, make_span(0, 2)),
            ("129", 129, make_span(0, 3)),
            // Binary
            ("0b0", 0, make_span(0, 3)),
            ("0b1", 1, make_span(0, 3)),
            ("0b100000", 32, make_span(0, 8)),
            ("0b10000001", 129, make_span(0, 10)),
            // Hexadecimal
            ("0x0", 0, make_span(0, 3)),
            ("0x1", 1, make_span(0, 3)),
            ("0x20", 32, make_span(0, 4)),
            ("0x81", 129, make_span(0, 4)),
        ];
        for (source, value, span) in cases {
            let (src, mut input) = make_input(source);
            let mut comp = Component::new(src);
            let expected_expression = comp.expr_mut().alloc_literal(Literal::Integer(value), span);

            let found_literal = parse_literal(&mut input.clone(), &mut comp).unwrap();
            assert!(found_literal.context_eq(&expected_expression, &comp));
            let found_leaf = parse_leaf(&mut input.clone(), &mut comp).unwrap();
            assert!(found_leaf.context_eq(&expected_expression, &comp));
            let found_expression = parse_expression(&mut input, &mut comp).unwrap();
            assert!(found_expression.context_eq(&expected_expression, &comp));
        }
    }

    #[test]
    fn parsing_supports_idents() {
        let cases = [
            ("foo", make_span(0, 3)),
            ("foobar", make_span(0, 6)),
            ("asdf", make_span(0, 4)),
            ("asdf2", make_span(0, 5)),
        ];
        for (source, span) in cases {
            let (src, mut input) = make_input(source);
            let mut comp = Component::new(src);
            let ident = comp.new_name(source.to_owned(), span);
            let expected_expression = comp.expr_mut().alloc_ident(ident, span);
            let found_ident = parse_ident_expr(&mut input.clone(), &mut comp).unwrap();
            assert!(found_ident.context_eq(&expected_expression, &comp));

            let found_leaf = parse_leaf(&mut input.clone(), &mut comp).unwrap();
            assert!(found_leaf.context_eq(&expected_expression, &comp));

            let found_expression = parse_expression(&mut input, &mut comp).unwrap();
            assert!(found_expression.context_eq(&expected_expression, &comp));
        }
    }

    #[test]
    fn parsing_supports_parenthesized_idents() {
        // parenthesized, raw, raw-span
        let cases = [
            ("(foo)", "foo", make_span(1, 3)),
            ("(foobar)", "foobar", make_span(1, 6)),
            ("(asdf)", "asdf", make_span(1, 4)),
            ("(asdf2)", "asdf2", make_span(1, 5)),
        ];
        for (source, ident, span) in cases {
            let (src, mut input) = make_input(source);
            let mut comp = Component::new(src);
            let ident = comp.new_name(ident.to_owned(), span);
            let expected_expression = comp.expr_mut().alloc_ident(ident, span);
            let found_expression = parse_parenthetical(&mut input.clone(), &mut comp).unwrap();
            assert!(found_expression.context_eq(&expected_expression, &comp));
            let found_expression = parse_leaf(&mut input.clone(), &mut comp).unwrap();
            assert!(found_expression.context_eq(&expected_expression, &comp));
            let found_expression = parse_expression(&mut input, &mut comp).unwrap();
            assert!(found_expression.context_eq(&expected_expression, &comp));
        }
    }

    #[test]
    fn parsing_supports_empty_arg_calls() {
        // parenthesized, raw, raw-span
        let cases = ["foo", "foobar", "asdf", "asdf2"];
        for ident in cases {
            // Compute case information
            let ident_span = make_span(0, ident.len());
            let source = format!("{}()", ident);
            let src_span = make_span(0, source.len());
            // Construct ast
            let (src, input) = make_input(source.as_str());
            let mut comp = Component::new(src);
            let ident = comp.new_name(ident.to_owned(), ident_span);
            let expected_expression = comp.expr_mut().alloc_call(ident, vec![], src_span);
            // Test `parse_call`
            let mut case_input = input.clone();
            let found_expression = parse_call(&mut case_input, &mut comp).unwrap();
            assert!(found_expression.context_eq(&expected_expression, &comp));
            assert!(case_input.done());
            // Test `parse_leaf`
            let mut case_input = input.clone();
            let found_expression = parse_leaf(&mut case_input, &mut comp).unwrap();
            assert!(found_expression.context_eq(&expected_expression, &comp));
            assert!(case_input.done());
            // Test `parse_expression`
            let mut case_input = input;
            let found_expression = parse_expression(&mut case_input, &mut comp).unwrap();
            assert!(found_expression.context_eq(&expected_expression, &comp));
            assert!(case_input.done());
        }
    }

    macro_rules! make_ast {
        ($comp:expr, { $left:tt, $op:expr, $right:tt }) => {{
            let lhs = make_ast!($comp, $left);
            let rhs = make_ast!($comp, $right);
            $comp.expr_mut().alloc_bin_op($op, lhs, rhs)
        }};
        ($comp:expr, ($val:expr => $span_l:expr, $span_r:expr)) => {{
            let expr = $val;
            let span = make_span($span_l, $span_r);
            $comp.expr_mut().alloc_literal(Literal::Integer(expr), span)
        }};
    }

    #[test]
    fn parse_expression_respects_precedence() {
        let source0 = "0 + 1 * 2";
        let (src0, input0) = make_input(source0);
        let mut comp0 = Component::new(src0);
        let expected0 = make_ast!(comp0, {
            (0 => 0, 1),
            BinaryOp::Add,
            {
                (1 => 4, 1),
                BinaryOp::Multiply,
                (2 => 8, 1)
            }
        });

        let source1 = "0 * 1 + 2";
        let (src1, input1) = make_input(source1);
        let mut comp1 = Component::new(src1);
        let expected1 = make_ast!(comp1, {
            {
                (0 => 0, 1),
                BinaryOp::Multiply,
                (1 => 4, 1)
            },
            BinaryOp::Add,
            (2 => 8, 1)
        });

        let cases = [(input0, comp0, expected0), (input1, comp1, expected1)];

        for (mut input, mut comp, expected) in cases {
            let expression = parse_expression(&mut input, &mut comp).unwrap_pretty();
            assert!(expression.context_eq(&expected, &comp));
        }
    }

    #[test]
    fn parse_expression_respects_associativity() {
        let source0 = "0 + 1 + 2";
        let (src0, input0) = make_input(source0);
        let mut comp0 = Component::new(src0);
        let expected0 = make_ast!(comp0, {
            { (0 => 0, 1), BinaryOp::Add, (1 => 4, 1) },
            BinaryOp::Add,
            (2 => 8, 1)
        });

        let source1 = "0 * 1 * 2";
        let (src1, input1) = make_input(source1);
        let mut comp1 = Component::new(src1);
        let expected1 = make_ast!(comp1, {
            { (0 => 0, 1), BinaryOp::Multiply, (1 => 4, 1) },
            BinaryOp::Multiply,
            (2 => 8, 1)
        });

        let cases = [(input0, comp0, expected0), (input1, comp1, expected1)];

        for (mut input, mut comp, expected) in cases {
            let expression = parse_expression(&mut input, &mut comp).unwrap_pretty();
            assert!(expression.context_eq(&expected, &comp));
        }
    }
}
