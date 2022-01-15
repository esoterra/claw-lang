use crate::lexer::Token;
use crate::ast::{
    M, Span,
    module::{
        Module, Item, Global,
        Function, FunctionSignature 
    },
    types::ValType
};
use crate::parser::{
    ParserError, ParseInput,
    expressions::parse_expression
};

use super::statements::parse_block;
use super::types::parse_valtype;


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
        M::new(ident, ident_token.span.clone())
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
    let signature = parse_fn_signature(input)?;
    let body = parse_block(input)?;

    Ok(Function {
        export_kwd,
        signature,
        body
    })
}

fn parse_fn_signature(input: &mut ParseInput) -> Result<FunctionSignature, ParserError> {
    let next = input.next()?;
    let name = if let Token::Identifier(name) = &next.token {
        let span = next.span.clone();
        M::new(name.clone(), span)
    } else { return Err(ParserError::UnexpectedToken) };

    let _lparen = input.assert_next(Token::LParen)?;
    let arguments = parse_arguments(input)?;
    let _rparen = input.assert_next(Token::RParen)?;
    let arrow = input.assert_next(Token::Arrow)?;
    let result_type = parse_valtype(input)?;

    Ok(FunctionSignature {
        name,
        arguments,
        arrow,
        result_type
    })
}

fn parse_arguments(input: &mut ParseInput) -> Result<Vec<(M<String>, M<ValType>)>, ParserError> {
    let mut arguments = Vec::new();
    while input.peek()?.token != Token::RParen {
        let next = input.next()?;
        let name = if let Token::Identifier(name) = &next.token {
            let span = next.span.clone();
            M::new(name.clone(), span)
        } else { return Err(ParserError::UnexpectedToken) };

        let _colon = input.assert_next(Token::Colon)?;

        let valtype = parse_valtype(input)?;

        arguments.push((name, valtype));
    }
    Ok(arguments)
}