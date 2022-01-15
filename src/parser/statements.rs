use crate::lexer::Token;
use crate::ast::{
    MBox,
    statements::{
        Block, Statement, StatementType
    }
};
use crate::parser::{
    ParserError, ParseInput,
    expressions::{parse_expression, parse_place}
};

pub fn parse_block(input: &mut ParseInput) -> Result<Block, ParserError> {
    let start_brace = input.assert_next(Token::LBrace)?;
    let root_statement = parse_statement(input)?;
    let end_brace = input.assert_next(Token::RBrace)?;
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
    Err(ParserError::UnexpectedToken)
}

fn parse_return(input: &mut ParseInput) -> Result<MBox<Statement>, ParserError> {
    let return_kwd = input.assert_next(Token::Return)?;
    let expression = parse_expression(input)?;
    let semicolon = input.assert_next(Token::Semicolon)?;

    let span = return_kwd.clone();
    let statement = Statement {
        inner: StatementType::Return {
            return_kwd, expression
        },
        next: None
    };
    Ok(MBox::new_range(statement, span, semicolon))
}

fn parse_assign(input: &mut ParseInput) -> Result<MBox<Statement>, ParserError> {
    let place = parse_place(input)?;
    let assign_op = input.assert_next(Token::Assign)?;
    let expression = parse_expression(input)?;
    let semicolon = input.assert_next(Token::Semicolon)?;

    let checkpoint = input.checkpoint();
    let next = if let Ok(next) = parse_statement(input) {
        Some(next)
    } else {
        input.restore(checkpoint);
        None
    };

    let span = place.span.clone();
    let statement = Statement {
        inner: StatementType::Assign {
            place, assign_op, expression
        },
        next
    };
    Ok(MBox::new_range(statement, span, semicolon))
}