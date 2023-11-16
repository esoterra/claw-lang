use crate::ast::expressions::ExpressionData;
use crate::lexer::Token;
use crate::ast::{
    M, MBox,
    statements::{
        Block, Statement
    }
};
use crate::parser::{
    ParserError, ParseInput,
    expressions::{parse_expression, parse_ident},
    types::parse_valtype
};

pub fn parse_block(input: &mut ParseInput, data: &mut ExpressionData) -> Result<M<Block>, ParserError> {
    let start_brace = input.assert_next(Token::LBrace, "Left brace '{'")?;
    let start_span = start_brace.clone();

    let root_statement = if input.peek()?.token == Token::RBrace {
        None
    } else {
        Some(parse_statement(input, data)?)
    };

    let end_brace = input.assert_next(Token::RBrace, "Right brace '}'")?;
    let end_span = end_brace.clone();
    Ok(M::new_range(Block { start_brace, root_statement, end_brace }, start_span, end_span))
}

pub fn parse_statement(input: &mut ParseInput, data: &mut ExpressionData) -> Result<MBox<Statement>, ParserError> {
    let checkpoint = input.checkpoint();
    if let Ok(value) = parse_return(input, data) {
        return Ok(value);
    }
    input.restore(checkpoint);
    if let Ok(value) = parse_assign(input, data) {
        return Ok(value)
    }
    input.restore(checkpoint);
    if let Ok(value) = parse_let(input, data) {
        return Ok(value)
    }
    input.restore(checkpoint);
    if let Ok(value) = parse_if(input, data) {
        return Ok(value)
    }
    if input.done() {
        Err(ParserError::EndOfInput)
    } else {
        Err(input.unexpected_token("Parse Statement"))
    }
}

pub fn try_parse_statement(input: &mut ParseInput, data: &mut ExpressionData) -> Option<MBox<Statement>> {
    let checkpoint = input.checkpoint();
    match parse_statement(input, data) {
        Ok(statement) => Some(statement),
        Err(_) => {
            input.restore(checkpoint);
            None
        }
    }
}

fn parse_let(input: &mut ParseInput, data: &mut ExpressionData) -> Result<MBox<Statement>, ParserError> {
    // Prefix
    let let_kwd = input.assert_next(Token::Let, "Let keyword 'let'")?;
    let start_span = let_kwd.clone();
    let mut_kwd = input.next_if(Token::Mut);
    let ident = parse_ident(input)?;

    // Annotation
    let annotation = match input.next_if(Token::Colon) {
        Some(_) => Some(parse_valtype(input)?),
        None => None
    };

    // Suffix
    let assign_op = input.assert_next(Token::Assign, "Assignment '='")?;
    let expression = parse_expression(input, data)?;
    let semicolon = input.assert_next(Token::Semicolon, "Semicolon ';'")?;

    // Next Statement
    let next = try_parse_statement(input, data);

    let statement = Statement::Let {
        let_kwd,
        mut_kwd,
        ident,
        annotation,
        assign_op,
        expression,
        next,
    };
    Ok(MBox::new_range(statement, start_span, semicolon))
}

fn parse_return(input: &mut ParseInput, data: &mut ExpressionData) -> Result<MBox<Statement>, ParserError> {
    let return_kwd = input.assert_next(Token::Return, "Return keyword 'return'")?;
    let expression = parse_expression(input, data)?;
    let semicolon = input.assert_next(Token::Semicolon, "Semicolon ';'")?;

    let span = return_kwd.clone();
    let statement = Statement::Return {
        return_kwd, expression
    };
    Ok(MBox::new_range(statement, span, semicolon))
}

fn parse_assign(input: &mut ParseInput, data: &mut ExpressionData) -> Result<MBox<Statement>, ParserError> {
    let ident = parse_ident(input)?;
    let assign_op = input.assert_next(Token::Assign, "Assign '='")?;
    let expression = parse_expression(input, data)?;
    let semicolon = input.assert_next(Token::Semicolon, "Semicolon ';'")?;

    let next = try_parse_statement(input, data);

    let span = ident.span.clone();
    let statement = Statement::Assign {
        ident, assign_op, expression, next
    };
    Ok(MBox::new_range(statement, span, semicolon))
}

fn parse_if(input: &mut ParseInput, data: &mut ExpressionData) -> Result<MBox<Statement>, ParserError> {
    let if_kwd = input.assert_next(Token::If, "If keyword 'if'")?;
    let condition = parse_expression(input, data)?;
    let block = parse_block(input, data)?;

    let next = try_parse_statement(input, data);

    let start_span = if_kwd.clone();
    let end_span = block.span.clone();

    let statement = Statement::If {
        if_kwd, condition, block, next
    };
    Ok(MBox::new_range(statement, start_span, end_span))
}

#[cfg(test)]
mod tests {
    use crate::parser::tests::make_input;
    use super::*;


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