use crate::ast::{self, component::FunctionSignature};
use crate::ast::{Component, ExternalType, FnType, FunctionId, GlobalId, Import, ImportId, NameId, TypeId};
use crate::lexer::Token;
use crate::parser::{
    expressions::parse_expression, statements::parse_block, types::parse_valtype, ParseInput,
    ParserError,
};

use super::statements::parse_ident;

pub fn parse_component(src: crate::Source, input: &mut ParseInput) -> Result<ast::Component, ParserError> {
    let mut component = ast::Component::new(src);

    while !input.done() {
        // Check for the export keyword
        let exported = input.next_if(Token::Export).is_some();

        // Determine the kind of item and parse it
        match input.peek()?.token {
            Token::Import => {
                parse_import(input, &mut component)?;
            }
            Token::Let => {
                parse_global(input, &mut component, exported)?;
            }
            Token::Func => {
                parse_func(input, &mut component, exported)?;
            }
            _ => return Err(input.unexpected_token("Top level item (e.g. import, global, function"))
        }
    }

    Ok(component)
}

fn parse_import(input: &mut ParseInput, comp: &mut Component) -> Result<ImportId, ParserError> {
    input.assert_next(Token::Import, "Import")?;
    let ident = parse_ident(input, comp)?;
    input.assert_next(Token::Colon, "Colon")?;
    let external_type = parse_external_type(input, comp)?;
    input.assert_next(Token::Semicolon, "Semicolon")?;

    let import = Import {
        ident,
        external_type,
    };

    Ok(comp.imports.push(import))
}

fn parse_global(
    input: &mut ParseInput,
    comp: &mut Component,
    exported: bool,
) -> Result<GlobalId, ParserError> {
    input.assert_next(Token::Let, "Let")?;
    let mutable = input.next_if(Token::Mut).is_some();

    let ident = parse_ident(input, comp)?;

    input.assert_next(Token::Colon, "Colon: ':'")?;
    let type_id = parse_valtype(input, comp)?;

    input.assert_next(Token::Assign, "Assign '='")?;
    let init_value = parse_expression(input, comp)?;
    input.assert_next(Token::Semicolon, "Semicolon ';'")?;

    let global = ast::Global {
        exported,
        mutable,
        ident,
        type_id,
        init_value,
    };

    Ok(comp.globals.push(global))
}

fn parse_func(
    input: &mut ParseInput,
    comp: &mut Component,
    exported: bool,
) -> Result<FunctionId, ParserError> {
    let signature = parse_func_signature(input, comp)?;
    let (body, _) = parse_block(input, comp)?;

    let function = ast::Function {
        exported,
        signature,
        body,
    };

    Ok(comp.functions.push(function))
}

fn parse_func_signature(
    input: &mut ParseInput,
    comp: &mut Component,
) -> Result<FunctionSignature, ParserError> {
    input.assert_next(Token::Func, "Function signature")?;
    let ident = parse_ident(input, comp)?;
    input.assert_next(Token::LParen, "Function arguments are parenthesized")?;

    let mut arguments = Vec::new();
    while input.peek()?.token != Token::RParen {
        let argument = parse_argument(input, comp)?;
        arguments.push(argument);

        if input.peek()?.token != Token::Comma {
            break;
        }

        let _ = input.next();
    }

    input.assert_next(Token::RParen, "Function argument parenthesis must be closed")?;

    let return_type = match input.next_if(Token::Arrow) {
        Some(_) => Some(parse_valtype(input, comp)?),
        None => None,
    };

    Ok(FunctionSignature { ident, arguments, return_type })
}

fn parse_argument(
    input: &mut ParseInput,
    comp: &mut Component,
) -> Result<(NameId, TypeId), ParserError> {
    let ident = parse_ident(input, comp)?;
    input.assert_next(Token::Colon, "Colon ':'")?;
    let type_id = parse_valtype(input, comp)?;
    Ok((ident, type_id))
}

fn parse_external_type(
    input: &mut ParseInput,
    comp: &mut Component,
) -> Result<ExternalType, ParserError> {
    Ok(ExternalType::Function(parse_fn_type(input, comp)?))
}

fn parse_fn_type(input: &mut ParseInput, comp: &mut Component) -> Result<FnType, ParserError> {
    let _func_kwd = input.assert_next(Token::Func, "Function keyword")?;
    input.assert_next(Token::LParen, "Function arguments are parenthesized")?;

    let mut arguments = Vec::new();
    while input.peek()?.token != Token::RParen {
        let argument = parse_argument(input, comp)?;
        arguments.push(argument);

        if input.peek()?.token != Token::Comma {
            break;
        }

        let _ = input.next();
    }

    input.assert_next(Token::RParen, "Function argument parenthesis must be closed")?;

    let return_type = match input.next_if(Token::Arrow) {
        Some(_) => Some(parse_valtype(input, comp)?),
        None => None,
    };

    Ok(FnType {
        arguments,
        return_type,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use miette::Report;
    use crate::parser::make_input;

    #[test]
    fn test_increment() {
        let source = "
        let mut counter: u32 = 0;

        export func increment() -> u32 {
            counter = counter + 1;
            return counter;
        }";
        let (src, mut input) = make_input(source);
        let result = parse_component(src, &mut input);
        match result {
            Ok(_) => {
                // yay!
            },
            Err(error) => panic!("{:?}", Report::new(error)),
        };
    }

    #[test]
    fn test_basic_function() {
        let source = "func increment() -> u32 { return 0; }";
        let (src, mut input) = make_input(source);
        let mut comp = Component::new(src.clone());
        let _func = parse_func(&mut input.clone(), &mut comp, false).unwrap();
        let _component = parse_component(src, &mut input).unwrap();
    }

    #[test]
    fn parse_function_signature() {
        let source = "func increment() -> u32";
        let (src, mut input) = make_input(source);
        let mut comp = Component::new(src);
        let _func_sig = parse_func_signature(&mut input, &mut comp).unwrap();
    }

    #[test]
    fn test_parse_global() {
        let source = "let mut counter: u32 = 0;";
        let (src, mut input) = make_input(source);
        let mut comp = Component::new(src);
        let _global = parse_global(&mut input, &mut comp, false).unwrap();
    }
}
