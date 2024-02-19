use crate::ast::{self, Span, merge, NameId, StatementId, Component};
use crate::lexer::Token;
use crate::parser::{expressions::parse_expression, types::parse_valtype, ParseInput, ParserError};

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

/// Parse an identifier
pub fn parse_ident(input: &mut ParseInput, comp: &mut Component) -> Result<NameId, ParserError> {
    match &input.peek()?.token {
        Token::Identifier(ident) => {
            let ident = ident.clone();
            let span = input.next().unwrap().span;
            Ok(comp.new_name(ident, span))
        }
        _ => {
            Err(input.unexpected_token("Expected identifier"))
        }
    }
}

pub fn parse_statement(
    input: &mut ParseInput,
    comp: &mut Component,
) -> Result<StatementId, ParserError> {
    return match input.peek()?.token {
        Token::Return => parse_return(input, comp),
        Token::Let => parse_let(input, comp),
        Token::If => parse_if(input, comp),
        _ => parse_assign(input, comp),
    };
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
    let expression = parse_expression(input, comp)?;
    let end_span = input.assert_next(Token::Semicolon, "Semicolon ';'")?;

    let statement = ast::Return { expression };
    let span = merge(&start_span, &end_span);
    Ok(comp.new_statement(ast::Statement::Return(statement), span))
}

fn parse_assign(input: &mut ParseInput, comp: &mut Component) -> Result<StatementId, ParserError> {
    let ident = parse_ident(input, comp)?;
    let start_span = comp.name_span(ident);
    input.assert_next(Token::Assign, "Assign '='")?;
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
    use super::*;
    use crate::parser::make_input;

    #[test]
    fn test_parse_block_empty() {
        let source = "{}";
        let (src, mut input) = make_input(source);
        let mut comp = Component::new(src);
        let _assign_stmt = parse_block(&mut input, &mut comp).unwrap();
    }

    #[test]
    fn test_parse_block() {
        let source = "{a = 0;}";
        let (src, mut input) = make_input(source);
        let mut comp = Component::new(src);
        let _assign_stmt = parse_block(&mut input, &mut comp).unwrap();
    }

    #[test]
    fn test_parse_return() {
        let source = "return 0;";
        let (src, mut input) = make_input(source);
        let mut comp = Component::new(src);
        let _return_stmt = parse_return(&mut input, &mut comp).unwrap();
    }

    #[test]
    fn test_parse_assign() {
        let source = "a = 0;";
        let (src, mut input) = make_input(source);
        let mut comp = Component::new(src);
        let _assign_stmt = parse_assign(&mut input, &mut comp).unwrap();
    }
}
