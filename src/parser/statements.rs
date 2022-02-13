use crate::lexer::Token;
use crate::ast::{
    M, MBox,
    statements::{
        Block, Statement
    }
};
use crate::parser::{
    ParserError, ParseInput,
    expressions::{parse_expression, parse_place},
    types::parse_valtype
};

pub fn parse_block(input: &mut ParseInput) -> Result<Block, ParserError> {
    let start_brace = input.assert_next(Token::LBrace, "Left brace '{'")?;

    let root_statement = if input.peek()?.token == Token::RBrace {
        None
    } else { Some(parse_statement(input)?) };

    let end_brace = input.assert_next(Token::RBrace, "Right brace '}'")?;
    Ok(Block { start_brace, root_statement, end_brace })
}

pub fn parse_statement(input: &mut ParseInput) -> Result<MBox<Statement>, ParserError> {
    let checkpoint = input.checkpoint();
    if let Ok(value) = parse_return(input) {
        return Ok(value);
    }
    input.restore(checkpoint);
    if let Ok(value) = parse_assign(input) {
        return Ok(value)
    }
    input.restore(checkpoint);
    if let Ok(value) = parse_let(input) {
        return Ok(value)
    }
    Err(input.unexpected_token("Parse Statement"))
}

pub fn try_parse_statement(input: &mut ParseInput) -> Option<MBox<Statement>> {
    let checkpoint = input.checkpoint();
    match parse_statement(input) {
        Ok(statement) => Some(statement),
        Err(_) => {
            input.restore(checkpoint);
            None
        }
    }
}

fn parse_let(input: &mut ParseInput) -> Result<MBox<Statement>, ParserError> {
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
    let expression = parse_expression(input)?;
    let semicolon = input.assert_next(Token::Semicolon, "Semicolon ';'")?;

    // Next Statement
    let next = try_parse_statement(input);

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

fn parse_return(input: &mut ParseInput) -> Result<MBox<Statement>, ParserError> {
    let return_kwd = input.assert_next(Token::Return, "Return keyword 'return'")?;
    let expression = parse_expression(input)?;
    let semicolon = input.assert_next(Token::Semicolon, "Semicolon ';'")?;

    let span = return_kwd.clone();
    let statement = Statement::Return {
        return_kwd, expression
    };
    Ok(MBox::new_range(statement, span, semicolon))
}

fn parse_assign(input: &mut ParseInput) -> Result<MBox<Statement>, ParserError> {
    let place = parse_place(input)?;
    let assign_op = input.assert_next(Token::Assign, "Assign '='")?;
    let expression = parse_expression(input)?;
    let semicolon = input.assert_next(Token::Semicolon, "Semicolon ';'")?;

    let next = try_parse_statement(input);

    let span = place.span.clone();
    let statement = Statement::Assign {
        place, assign_op, expression, next
    };
    Ok(MBox::new_range(statement, span, semicolon))
}

fn parse_ident(input: &mut ParseInput) -> Result<M<String>, ParserError> {
    let next = input.peek()?;
    let span = next.span.clone();
    match next.token.clone() {
        Token::Identifier(ident) => {
            let _ = input.next();
            Ok(M::new(ident, span))
        },
        _ => {
            Err(input.unexpected_token("Expected identifier"))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::tests::make_input;
    use super::*;


    #[test]
    fn test_parse_block_empty() {
        let source = "{}";
        let _assign_stmt = parse_block(&mut make_input(source)).unwrap();
    }

    #[test]
    fn test_parse_block() {
        let source = "{a = 0;}";
        let _assign_stmt = parse_block(&mut make_input(source)).unwrap();
    }

    #[test]
    fn test_parse_return() {
        let source = "return 0;";
        let _return_stmt = parse_return(&mut make_input(source)).unwrap();
    }

    #[test]
    fn test_parse_assign() {
        let source = "a = 0;";
        let _assign_stmt = parse_assign(&mut make_input(source)).unwrap();
    }
}