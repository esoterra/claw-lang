use crate::ast::M;
use crate::lexer::Token;
use crate::ast::{
    Span,
    module::{
        Module, Item, Global, Function
    }
};
use super::expressions::parse_expression;
use super::{ParserError, ParseInput};


pub fn parse_module(input: &mut ParseInput) -> Result<Module, ParserError> {
    let mut items = Vec::new();

    while !input.done() {
        let item = parse_item(input)?;
        items.push(item);
    }

    Ok(Module { items })
}

fn parse_item(input: &mut ParseInput) -> Result<Item, ParserError> {
    // Check for the export keyword
    let export_kwd = input.next_if(Token::Export);

    // Determine the kind of item and parse it
    match input.peek()?.token {
        Token::Let => parse_global(export_kwd, input)
            .map(|global| Item::Global(global)),
        Token::Fn => parse_fn(export_kwd, input)
            .map(|function| Item::Function(function)),
        _ => Err(ParserError::NotYetSupported)
    }
}

fn parse_global(export_kwd: Option<Span>, input: &mut ParseInput) -> Result<Global, ParserError> {
    let let_kwd = input.assert_next(Token::Let)?;
    let mut_kwd = input.next_if(Token::Mut);

    let ident_token = input.next()?;
    let ident = if let Token::Identifier(ident) = ident_token.token.clone() {
        M::new(ident_token.span.clone(), ident)
    } else {
        return Err(ParserError::UnexpectedToken)
    };

    let assign = input.assert_next(Token::Assign)?;
    let init_value = parse_expression(input)?;
    let semicolon = input.assert_next(Token::Semicolon)?;

    Ok(Global {
        export_kwd,
        let_kwd,
        mut_kwd,
        ident,
        assign,
        init_value,
        semicolon
    })
}

fn parse_fn(export_kwd: Option<Span>, input: &mut ParseInput) -> Result<Function, ParserError> {
    unreachable!()
}