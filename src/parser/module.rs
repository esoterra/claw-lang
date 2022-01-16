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
    statements::parse_block,
    types::parse_valtype,
    expressions::parse_expression
};

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
        _ => Err(input.unsupported_error("Module Items"))
    }
}

fn parse_global(export_kwd: Option<Span>, input: &mut ParseInput) -> Result<Global, ParserError> {
    let let_kwd = input.assert_next(Token::Let, "Let")?;
    let mut_kwd = input.next_if(Token::Mut);

    let ident_token = input.next()?;
    let ident = if let Token::Identifier(ident) = ident_token.token.clone() {
        M::new(ident, ident_token.span.clone())
    } else {
        return Err(input.unexpected_token("Global Ident"))
    };
    let _colon = input.assert_next(Token::Colon, "Colon: ':'");
    let valtype = parse_valtype(input)?;

    let assign = input.assert_next(Token::Assign, "Assign '='")?;
    let init_value = parse_expression(input)?;
    let semicolon = input.assert_next(Token::Semicolon, "Semicolon ';'")?;

    Ok(Global {
        export_kwd,
        let_kwd,
        mut_kwd,
        ident,
        valtype,
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
    let fn_kwd = input.assert_next(Token::Fn, "Function keyword 'fn'")?;
    let next = input.next()?;
    let name = match &next.token {
        Token::Identifier(name) => {
            let span = next.span.clone();
            M::new(name.clone(), span)
        },
        _ => return Err(input.unexpected_token("Parse Fn Signature"))
    };

    let _lparen = input.assert_next(Token::LParen, "Opening parenthesis '('")?;
    let arguments = parse_arguments(input)?;
    let _rparen = input.assert_next(Token::RParen, "Closing parenthesis ')'")?;
    let arrow = input.assert_next(Token::Arrow, "Function arrow '->'")?;
    let result_type = parse_valtype(input)?;

    Ok(FunctionSignature {
        fn_kwd,
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
        } else { return Err(input.unexpected_token("Parse Arguments Identifier")) };

        let _colon = input.assert_next(Token::Colon, "Colon ':'")?;

        let valtype = parse_valtype(input)?;

        arguments.push((name, valtype));
    }
    Ok(arguments)
}

#[cfg(test)]
mod tests {
    use crate::parser::tests::{make_input};
    use super::*;

    #[test]
    fn test_increment() {
        let source = "
        let mut counter: u32 = 0;

        export fn increment() -> u32 {
            counter = counter + 1;
            return counter;
        }";
        let mut input = make_input(source);
        let _module = parse_module(&mut input).unwrap();
    }

    #[test]
    fn test_basic_function() {
        let source = "fn increment() -> u32 {}";
        let _fn = parse_fn(None, &mut make_input(source)).unwrap();
        let _module = parse_module(&mut make_input(source)).unwrap();
    }

    #[test]
    fn parse_function_signature() {
        let source = "fn increment() -> u32";
        let _fn_sig = parse_fn_signature(&mut make_input(source)).unwrap();
    }

    #[test]
    fn test_parse_global() {
        let source = "let mut counter: u32 = 0;";
        let _fn_sig = parse_global(None, &mut make_input(source)).unwrap();
    }
}