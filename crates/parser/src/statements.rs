use ast::{Call, Statement};

use crate::ast::{self, merge, Component, Span, StatementId};
use crate::lexer::Token;
use crate::names::parse_ident;
use crate::{expressions::parse_expression, types::parse_valtype, ParseInput, ParserError};

pub fn parse_block(
    input: &mut ParseInput,
    comp: &mut Component,
) -> Result<(Vec<StatementId>, Span), ParserError> {
    let start_span = input.assert_next(Token::LBrace, "Left brace '{'")?;

    let mut statements = Vec::new();
    while input.peek()?.token != Token::RBrace {
        statements.push(parse_statement(input, comp)?);
    }

    let end_span = input.assert_next(Token::RBrace, "Right brace '}'")?;

    let span = merge(&start_span, &end_span);
    Ok((statements, span))
}

pub fn parse_statement(
    input: &mut ParseInput,
    comp: &mut Component,
) -> Result<StatementId, ParserError> {
    let peek0 = &input.peek()?.token;
    let peek1 = input.peekn(1);
    match (peek0, peek1) {
        (Token::Return, _) => parse_return(input, comp),
        (Token::Let, _) => parse_let(input, comp),
        (Token::If, _) => parse_if(input, comp),
        (Token::Identifier(_), Some(Token::LParen)) => parse_call(input, comp),
        (Token::Identifier(_), _) => parse_assign(input, comp),
        _ => {
            _ = input.next();
            Err(input.unexpected_token("Invalid statement start"))
        }
    }
}

fn parse_let(input: &mut ParseInput, comp: &mut Component) -> Result<StatementId, ParserError> {
    // Prefix
    let start_span = input.assert_next(Token::Let, "Let keyword 'let'")?;
    let mutable = input.next_if(Token::Mut).is_some();
    let ident = parse_ident(input, comp)?;

    // Annotation
    let annotation = match input.next_if(Token::Colon) {
        Some(_) => Some(parse_valtype(input, comp)?),
        None => None,
    };

    // Suffix
    input.assert_next(Token::Assign, "Assignment '='")?;
    let expression = parse_expression(input, comp)?;
    let end_span = input.assert_next(Token::Semicolon, "Semicolon ';'")?;

    let span = merge(&start_span, &end_span);
    Ok(comp.alloc_let(mutable, ident, annotation, expression, span))
}

fn parse_return(input: &mut ParseInput, comp: &mut Component) -> Result<StatementId, ParserError> {
    let start_span = input.assert_next(Token::Return, "Return keyword 'return'")?;

    let (expression, end_span) = match input.next_if(Token::Semicolon) {
        Some(end_span) => (None, end_span),
        None => {
            let expression = parse_expression(input, comp)?;
            let end_span = input.assert_next(Token::Semicolon, "Semicolon ';'")?;
            (Some(expression), end_span)
        }
    };

    let statement = ast::Return { expression };
    let span = merge(&start_span, &end_span);
    Ok(comp.new_statement(ast::Statement::Return(statement), span))
}

fn parse_call(input: &mut ParseInput, comp: &mut Component) -> Result<StatementId, ParserError> {
    let ident = parse_ident(input, comp)?;
    let start_span = comp.name_span(ident);
    input.assert_next(Token::LParen, "Function arguments")?;

    let mut args = Vec::new();
    loop {
        if let Some(span) = input.next_if(Token::RParen) {
            break;
        }

        args.push(parse_expression(input, comp)?);

        let token = input.next()?;
        match token.token {
            Token::Comma => continue,
            Token::RParen => break,
            _ => return Err(input.unexpected_token("Argument list")),
        }
    }

    let end_span = input.assert_next(Token::Semicolon, "Statements must end with `;`")?;

    let statement = Statement::Call(Call { ident, args });
    let span = merge(&start_span, &end_span);

    Ok(comp.new_statement(statement, span))
}

fn parse_assign(input: &mut ParseInput, comp: &mut Component) -> Result<StatementId, ParserError> {
    let ident = parse_ident(input, comp)?;
    let start_span = comp.name_span(ident);
    let err_no_assign = "Expected '=' when parsing assignment statement";
    input.assert_next(Token::Assign, err_no_assign)?;
    let expression = parse_expression(input, comp)?;
    let end_span = input.assert_next(Token::Semicolon, "Semicolon ';'")?;

    let statement = ast::Assign { ident, expression };
    let span = merge(&start_span, &end_span);
    Ok(comp.new_statement(ast::Statement::Assign(statement), span))
}

fn parse_if(input: &mut ParseInput, comp: &mut Component) -> Result<StatementId, ParserError> {
    let start_span = input.assert_next(Token::If, "If keyword 'if'")?;
    let condition = parse_expression(input, comp)?;
    let (block, end_span) = parse_block(input, comp)?;

    let statement = ast::If { condition, block };
    let span = merge(&start_span, &end_span);
    Ok(comp.new_statement(ast::Statement::If(statement), span))
}

#[cfg(test)]
mod tests {
    use claw_common::UnwrapPretty;

    use super::*;
    use crate::make_input;

    #[test]
    fn test_parse_block_empty() {
        let source = "{}";
        let (src, mut input) = make_input(source);
        let mut comp = Component::new(src);
        let _assign_stmt = parse_block(&mut input, &mut comp).unwrap_pretty();
        assert!(input.done());
    }

    #[test]
    fn test_parse_block() {
        let source = "{a = 0;}";
        let (src, mut input) = make_input(source);
        let mut comp = Component::new(src);
        let _assign_stmt = parse_block(&mut input, &mut comp).unwrap_pretty();
        assert!(input.done());
    }

    #[test]
    fn test_parse_return() {
        let source = "return 0;";
        let (src, mut input) = make_input(source);
        let mut comp = Component::new(src);
        let _return_stmt = parse_return(&mut input, &mut comp).unwrap_pretty();
        assert!(input.done());
    }

    #[test]
    fn test_parse_assign() {
        let source = "a = 0;";
        let (src, mut input) = make_input(source);
        let mut comp = Component::new(src);
        let _assign_stmt = parse_assign(&mut input, &mut comp).unwrap_pretty();
        assert!(input.done());
    }

    #[test]
    fn test_parse_let() {
        let source = "let start = now();";
        let (src, mut input) = make_input(source);
        let mut comp = Component::new(src);
        let _let_stmt = parse_let(&mut input, &mut comp).unwrap_pretty();
        assert!(input.done());
    }
}
