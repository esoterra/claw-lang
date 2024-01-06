use crate::ast::expressions::ExpressionData;
use crate::ast::{
    component::{Component, Function, FunctionSignature, Global},
    types::ValType,
    Span, M,
};
use crate::ast::{ExternalType, FnType, Import};
use crate::lexer::Token;
use crate::parser::{
    expressions::parse_expression, statements::parse_block, types::parse_valtype, ParseInput,
    ParserError,
};

use super::expressions::parse_ident;

pub fn parse_component(input: &mut ParseInput) -> Result<Component, ParserError> {
    let mut module = Component::default();

    while !input.done() {
        // Check for the export keyword
        let export_kwd = input.next_if(Token::Export);

        // Determine the kind of item and parse it
        match input.peek()?.token {
            Token::Import => {
                module.imports.alloc(parse_import(input)?);
            }
            Token::Let => {
                module.globals.alloc(parse_global(export_kwd, input)?);
            }
            _ => {
                module.functions.alloc(parse_func(export_kwd, input)?);
            }
        }
    }

    Ok(module)
}

fn parse_import(input: &mut ParseInput) -> Result<Import, ParserError> {
    let import_kwd = input.assert_next(Token::Import, "Import")?;
    let name = parse_ident(input)?;
    let colon = input.assert_next(Token::Colon, "Colon")?;
    let external_type = parse_external_type(input)?;
    let _semicolon = input.assert_next(Token::Semicolon, "Semicolon")?;

    Ok(Import {
        import_kwd,
        name,
        colon,
        external_type,
    })
}

fn parse_global(export_kwd: Option<Span>, input: &mut ParseInput) -> Result<Global, ParserError> {
    let mut data = ExpressionData::default();

    let let_kwd = input.assert_next(Token::Let, "Let")?;
    let mut_kwd = input.next_if(Token::Mut);

    let ident_token = input.next()?;

    let ident = match &ident_token.token {
        Token::Identifier(ident) => M::new(ident.clone(), ident_token.span.clone()),
        _ => return Err(input.unexpected_token("Global Ident")),
    };

    let colon = input.assert_next(Token::Colon, "Colon: ':'")?;
    let valtype = parse_valtype(input)?;

    let assign = input.assert_next(Token::Assign, "Assign '='")?;
    let init_value = parse_expression(input, &mut data)?;
    let semicolon = input.assert_next(Token::Semicolon, "Semicolon ';'")?;

    Ok(Global {
        export_kwd,
        let_kwd,
        mut_kwd,
        ident,
        colon,
        valtype,
        assign,
        init_value,
        semicolon,
        expressions: data,
    })
}

fn parse_func(export_kwd: Option<Span>, input: &mut ParseInput) -> Result<Function, ParserError> {
    let signature = parse_func_signature(input)?;
    let mut data = ExpressionData::default();
    let body = parse_block(input, &mut data)?;

    Ok(Function {
        export_kwd,
        signature,
        body,
        expressions: data,
    })
}

fn parse_func_signature(input: &mut ParseInput) -> Result<FunctionSignature, ParserError> {
    let next = input.next()?;
    let name = match &next.token {
        Token::Identifier(name) => {
            let span = next.span.clone();
            M::new(name.clone(), span)
        }
        _ => return Err(input.unexpected_token("Parse Fn Signature")),
    };

    let colon = input.assert_next(Token::Colon, "Function signature colon")?;

    let fn_type = parse_fn_type(input)?;

    Ok(FunctionSignature {
        name, colon, fn_type
    })
}

fn parse_argument(input: &mut ParseInput) -> Result<(M<String>, M<ValType>), ParserError> {
    let next = input.next()?;
    let span = next.span.clone();
    let name = match &next.token {
        Token::Identifier(name) => M::new(name.clone(), span),
        _ => return Err(input.unexpected_token("Parse Arguments Identifier")),
    };
    let _colon = input.assert_next(Token::Colon, "Colon ':'")?;
    let valtype = parse_valtype(input)?;
    Ok((name, valtype))
}

fn parse_external_type(input: &mut ParseInput) -> Result<ExternalType, ParserError> {
    Ok(ExternalType::Function(parse_fn_type(input)?))
}

fn parse_fn_type(input: &mut ParseInput) -> Result<FnType, ParserError> {
    let _func_kwd = input.assert_next(Token::Func, "Function keyword")?;
    let _lparen = input.assert_next(Token::LParen, "Start function arguments")?;

    let mut arguments = Vec::new();
    while input.peek()?.token != Token::RParen {
        let argument = parse_argument(input)?;
        arguments.push(argument);

        if input.peek()?.token != Token::Comma {
            break;
        }

        let _ = input.next();
    }

    let _rparen = input.assert_next(Token::RParen, "End function arguments")?;
    let arrow = input.assert_next(Token::Arrow, "Results arrow")?;
    let return_type = parse_valtype(input)?;

    Ok(FnType { arguments, arrow, return_type })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tests::make_input;

    #[test]
    fn test_increment() {
        let source = "
        let mut counter: u32 = 0;

        export increment: func() -> u32 {
            counter = counter + 1;
            return counter;
        }";
        let mut input = make_input(source);
        let _component = parse_component(&mut input).unwrap();
    }

    #[test]
    fn test_basic_function() {
        let source = "increment: func() -> u32 {}";
        let _func = parse_func(None, &mut make_input(source)).unwrap();
        let _component = parse_component(&mut make_input(source)).unwrap();
    }

    #[test]
    fn parse_function_signature() {
        let source = "increment: func() -> u32";
        let _func_sig = parse_func_signature(&mut make_input(source)).unwrap();
    }

    #[test]
    fn test_parse_global() {
        let source = "let mut counter: u32 = 0;";
        let _global = parse_global(None, &mut make_input(source)).unwrap();
    }
}
