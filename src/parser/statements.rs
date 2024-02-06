use crate::ast::{self, Span};
use crate::ast::{merge, Arenas, NameId, StatementId};
use crate::lexer::Token;
use crate::parser::{expressions::parse_expression, types::parse_valtype, ParseInput, ParserError};

pub fn parse_block(
    input: &mut ParseInput,
    arenas: &mut Arenas,
) -> Result<(Vec<StatementId>, Span), ParserError> {
    let start_span = input.assert_next(Token::LBrace, "Left brace '{'")?;

    let mut statements = Vec::new();
    while input.peek()?.token != Token::RBrace {
        statements.push(parse_statement(input, arenas)?);
    }

    let end_span = input.assert_next(Token::RBrace, "Right brace '}'")?;

    let span = merge(&start_span, &end_span);
    Ok((statements, span))
}

/// Parse an identifier
pub fn parse_ident(input: &mut ParseInput, arenas: &mut Arenas) -> Result<NameId, ParserError> {
    let checkpoint = input.checkpoint();
    let next = input.next()?;
    let span = next.span.clone();
    match next.token.clone() {
        Token::Identifier(ident) => Ok(arenas.new_name(ident, span)),
        _ => {
            input.restore(checkpoint);
            Err(input.unexpected_token("Expected identifier"))
        }
    }
}

pub fn parse_statement(
    input: &mut ParseInput,
    arenas: &mut Arenas,
) -> Result<StatementId, ParserError> {
    return match input.peek()?.token {
        Token::Return => parse_return(input, arenas),
        Token::Let => parse_let(input, arenas),
        Token::If => parse_if(input, arenas),
        _ => parse_assign(input, arenas),
    };
}

fn parse_let(input: &mut ParseInput, arenas: &mut Arenas) -> Result<StatementId, ParserError> {
    // Prefix
    let start_span = input.assert_next(Token::Let, "Let keyword 'let'")?;
    let mutable = input.next_if(Token::Mut).is_some();
    let ident = parse_ident(input, arenas)?;

    // Annotation
    let annotation = match input.next_if(Token::Colon) {
        Some(_) => Some(parse_valtype(input, arenas)?),
        None => None,
    };

    // Suffix
    input.assert_next(Token::Assign, "Assignment '='")?;
    let expression = parse_expression(input, arenas)?;
    let end_span = input.assert_next(Token::Semicolon, "Semicolon ';'")?;

    let span = merge(&start_span, &end_span);
    Ok(arenas.alloc_let(mutable, ident, annotation, expression, span))
}

fn parse_return(input: &mut ParseInput, arenas: &mut Arenas) -> Result<StatementId, ParserError> {
    let start_span = input.assert_next(Token::Return, "Return keyword 'return'")?;
    let expression = parse_expression(input, arenas)?;
    let end_span = input.assert_next(Token::Semicolon, "Semicolon ';'")?;

    let statement = ast::Return { expression };
    let span = merge(&start_span, &end_span);
    Ok(arenas.new_statement(ast::Statement::Return(statement), span))
}

fn parse_assign(input: &mut ParseInput, arenas: &mut Arenas) -> Result<StatementId, ParserError> {
    let ident = parse_ident(input, arenas)?;
    let start_span = arenas.name_span(ident);
    input.assert_next(Token::Assign, "Assign '='")?;
    let expression = parse_expression(input, arenas)?;
    let end_span = input.assert_next(Token::Semicolon, "Semicolon ';'")?;

    let statement = ast::Assign { ident, expression };
    let span = merge(&start_span, &end_span);
    Ok(arenas.new_statement(ast::Statement::Assign(statement), span))
}

fn parse_if(input: &mut ParseInput, arenas: &mut Arenas) -> Result<StatementId, ParserError> {
    let start_span = input.assert_next(Token::If, "If keyword 'if'")?;
    let condition = parse_expression(input, arenas)?;
    let (block, end_span) = parse_block(input, arenas)?;

    let statement = ast::If { condition, block };
    let span = merge(&start_span, &end_span);
    Ok(arenas.new_statement(ast::Statement::If(statement), span))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tests::make_input;

    #[test]
    fn test_parse_block_empty() {
        let mut arenas = Arenas::default();
        let source = "{}";
        let _assign_stmt = parse_block(&mut make_input(source), &mut arenas).unwrap();
    }

    #[test]
    fn test_parse_block() {
        let mut arenas = Arenas::default();
        let source = "{a = 0;}";
        let _assign_stmt = parse_block(&mut make_input(source), &mut arenas).unwrap();
    }

    #[test]
    fn test_parse_return() {
        let mut arenas = Arenas::default();
        let source = "return 0;";
        let _return_stmt = parse_return(&mut make_input(source), &mut arenas).unwrap();
    }

    #[test]
    fn test_parse_assign() {
        let mut arenas = Arenas::default();
        let source = "a = 0;";
        let _assign_stmt = parse_assign(&mut make_input(source), &mut arenas).unwrap();
    }
}
