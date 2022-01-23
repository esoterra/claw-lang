use crate::lexer::Token;
use crate::ast::{
    M, MBox,
    statements::{
        Block, Statement
    }
};
use crate::parser::{
    ParserError, ParseInput,
    expressions::{parse_expression, parse_place}
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
    Err(input.unexpected_token("Parse Statement"))
}

fn parse_let(input: &mut ParseInput) -> Result<MBox<Statement>, ParserError> {
    let let_kwd = input.assert_next(Token::Let, "Let keyword 'let'")?;
    let start_span = let_kwd.clone();
    let mut_kwd = input.next_if(Token::Mut);

    let next = input.next()?;
    let ident = if let Token::Identifier(ident) = &next.token {
        M::new(ident.clone(), next.span.clone())
    } else { todo!() };

    let expression = parse_expression(input)?;
    let semicolon = input.assert_next(Token::Semicolon, "Semicolon ';'")?;

    let statement = Statement::Let {
        let_kwd,
        mut_kwd,
        ident,
        annotation: todo!(),
        assign_op: todo!(),
        expression,
        next: todo!(),
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

    let checkpoint = input.checkpoint();
    let next = if let Ok(next) = parse_statement(input) {
        Some(next)
    } else {
        input.restore(checkpoint);
        None
    };

    let span = place.span.clone();
    let statement = Statement::Assign {
        place, assign_op, expression, next
    };
    Ok(MBox::new_range(statement, span, semicolon))
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