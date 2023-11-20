use crate::ast::expressions::ExpressionData;
use crate::lexer::Token;
use crate::ast::{
    M, Span,
    component::{
        Component, Global,
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

pub fn parse_component(input: &mut ParseInput) -> Result<Component, ParserError> {
    let mut module = Component::default();

    while !input.done() {
        // Check for the export keyword
        let export_kwd = input.next_if(Token::Export);

        // Determine the kind of item and parse it
        match input.peek()?.token {
            Token::Let => {
                module.globals.alloc(parse_global(export_kwd, input)?);
            },
            Token::Func => {
                module.functions.alloc(parse_func(export_kwd, input)?);
            },
            _ => {
                return Err(input.unsupported_error("Module Items"));
            }
        }
    }

    Ok(module)
}

fn parse_global(export_kwd: Option<Span>, input: &mut ParseInput) -> Result<Global, ParserError> {
    let mut data = ExpressionData::default();

    let let_kwd = input.assert_next(Token::Let, "Let")?;
    let mut_kwd = input.next_if(Token::Mut);

    let ident_token = input.next()?;

    let ident = match &ident_token.token {
        Token::Identifier(ident) => M::new(ident.clone(), ident_token.span.clone()),
        _ => return Err(input.unexpected_token("Global Ident"))
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
        expressions: data
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
        expressions: data
    })
}

fn parse_func_signature(input: &mut ParseInput) -> Result<FunctionSignature, ParserError> {
    let func_kwd = input.assert_next(Token::Func, "Function keyword 'fn'")?;
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
    let return_type = parse_valtype(input)?;

    Ok(FunctionSignature {
        func_kwd,
        name,
        arguments,
        arrow,
        return_type
    })
}

fn parse_arguments(input: &mut ParseInput) -> Result<Vec<(M<String>, M<ValType>)>, ParserError> {
    let mut arguments = Vec::new();
    while input.peek()?.token != Token::RParen {
        let argument = parse_argument(input)?;
        arguments.push(argument);

        if input.peek()?.token != Token::Comma {
            break;
        }

        let _ = input.next();
    }
    Ok(arguments)
}

fn parse_argument(input: &mut ParseInput) -> Result<(M<String>, M<ValType>), ParserError> {
    let next = input.next()?;
    let span = next.span.clone();
    let name = match &next.token {
        Token::Identifier(name) => M::new(name.clone(), span),
        _ => return Err(input.unexpected_token("Parse Arguments Identifier"))
    };
    let _colon = input.assert_next(Token::Colon, "Colon ':'")?;
    let valtype = parse_valtype(input)?;
    Ok((name, valtype))
}

#[cfg(test)]
mod tests {
    use crate::parser::tests::make_input;
    use super::*;

    #[test]
    fn test_increment() {
        let source = "
        let mut counter: u32 = 0;

        export func increment() -> u32 {
            counter = counter + 1;
            return counter;
        }";
        let mut input = make_input(source);
        let _component = parse_component(&mut input).unwrap();
    }

    #[test]
    fn test_basic_function() {
        let source = "func increment() -> u32 {}";
        let _func = parse_func(None, &mut make_input(source)).unwrap();
        let _component = parse_component(&mut make_input(source)).unwrap();
    }

    #[test]
    fn parse_function_signature() {
        let source = "func increment() -> u32";
        let _func_sig = parse_func_signature(&mut make_input(source)).unwrap();
    }

    #[test]
    fn test_parse_global() {
        let source = "let mut counter: u32 = 0;";
        let _global = parse_global(None, &mut make_input(source)).unwrap();
    }
}