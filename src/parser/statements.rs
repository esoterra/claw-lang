use crate::ast::expressions::ExpressionData;
use crate::ast::NameId;
use crate::ast::{
    component::{Block, Statement},
    M,
};
use crate::lexer::Token;
use crate::parser::{
    expressions::parse_expression,
    types::parse_valtype,
    ParseInput, ParserError,
};

/// Parse an identifier
pub fn parse_ident(
    input: &mut ParseInput,
) -> Result<M<String>, ParserError> {
    let checkpoint = input.checkpoint();
    let next = input.next()?;
    let span = next.span.clone();
    match next.token.clone() {
        Token::Identifier(ident) => Ok(M::new(ident, span)),
        _ => {
            input.restore(checkpoint);
            Err(input.unexpected_token("Expected identifier"))
        }
    }
}

pub fn parse_block(
    input: &mut ParseInput,
    data: &mut ExpressionData,
) -> Result<M<Block>, ParserError> {
    let start_brace = input.assert_next(Token::LBrace, "Left brace '{'")?;
    let start_span = start_brace.clone();

    let mut statements = Vec::new();
    while input.peek()?.token != Token::RBrace {
        statements.push(parse_statement(input, data)?);
    }

    let end_brace = input.assert_next(Token::RBrace, "Right brace '}'")?;
    let end_span = end_brace.clone();
    Ok(M::new_range(
        Block {
            start_brace,
            statements,
            end_brace,
        },
        start_span,
        end_span,
    ))
}

pub fn parse_statement(
    input: &mut ParseInput,
    data: &mut ExpressionData,
) -> Result<M<Statement>, ParserError> {
    return match input.peek()?.token {
        Token::Return => parse_return(input, data),
        Token::Let => parse_let(input, data),
        Token::If => parse_if(input, data),
        _ => parse_assign(input, data),
    };
}

fn parse_let(
    input: &mut ParseInput,
    data: &mut ExpressionData,
) -> Result<M<Statement>, ParserError> {
    // Prefix
    let let_kwd = input.assert_next(Token::Let, "Let keyword 'let'")?;
    let start_span = let_kwd.clone();
    let mut_kwd = input.next_if(Token::Mut);
    let ident = parse_ident(input)?;

    // Annotation
    let annotation = match input.next_if(Token::Colon) {
        Some(_) => Some(parse_valtype(input)?),
        None => None,
    };

    // Suffix
    let assign_op = input.assert_next(Token::Assign, "Assignment '='")?;
    let expression = parse_expression(input, data)?;
    let semicolon = input.assert_next(Token::Semicolon, "Semicolon ';'")?;

    let statement = Statement::Let {
        let_kwd,
        mut_kwd,
        ident,
        name_id: NameId::new(),
        annotation,
        assign_op,
        expression,
    };
    Ok(M::new_range(statement, start_span, semicolon))
}

fn parse_return(
    input: &mut ParseInput,
    data: &mut ExpressionData,
) -> Result<M<Statement>, ParserError> {
    let return_kwd = input.assert_next(Token::Return, "Return keyword 'return'")?;
    let expression = parse_expression(input, data)?;
    let semicolon = input.assert_next(Token::Semicolon, "Semicolon ';'")?;

    let span = return_kwd.clone();
    let statement = Statement::Return {
        return_kwd,
        expression,
    };
    Ok(M::new_range(statement, span, semicolon))
}

fn parse_assign(
    input: &mut ParseInput,
    data: &mut ExpressionData,
) -> Result<M<Statement>, ParserError> {
    let ident = parse_ident(input)?;
    let assign_op = input.assert_next(Token::Assign, "Assign '='")?;
    let expression = parse_expression(input, data)?;
    let semicolon = input.assert_next(Token::Semicolon, "Semicolon ';'")?;

    let span = ident.span.clone();
    let statement = Statement::Assign {
        ident,
        name_id: NameId::new(),
        assign_op,
        expression,
    };
    Ok(M::new_range(statement, span, semicolon))
}

fn parse_if(
    input: &mut ParseInput,
    data: &mut ExpressionData,
) -> Result<M<Statement>, ParserError> {
    let if_kwd = input.assert_next(Token::If, "If keyword 'if'")?;
    let condition = parse_expression(input, data)?;
    let block = parse_block(input, data)?;

    let start_span = if_kwd.clone();
    let end_span = block.span.clone();

    let statement = Statement::If {
        if_kwd,
        condition,
        block,
    };
    Ok(M::new_range(statement, start_span, end_span))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tests::make_input;

    #[test]
    fn test_parse_block_empty() {
        let mut data = ExpressionData::default();
        let source = "{}";
        let _assign_stmt = parse_block(&mut make_input(source), &mut data).unwrap();
    }

    #[test]
    fn test_parse_block() {
        let mut data = ExpressionData::default();
        let source = "{a = 0;}";
        let _assign_stmt = parse_block(&mut make_input(source), &mut data).unwrap();
    }

    #[test]
    fn test_parse_return() {
        let mut data = ExpressionData::default();
        let source = "return 0;";
        let _return_stmt = parse_return(&mut make_input(source), &mut data).unwrap();
    }

    #[test]
    fn test_parse_assign() {
        let mut data = ExpressionData::default();
        let source = "a = 0;";
        let _assign_stmt = parse_assign(&mut make_input(source), &mut data).unwrap();
    }
}
