use claw_ast as ast;
use ast::{FunctionId, GlobalId, Import, ImportId, NameId, TypeId};
use crate::lexer::Token;
use crate::{
    expressions::parse_expression, statements::parse_block, types::parse_valtype, ParseInput,
    ParserError,
};

use claw_common::Source;

use crate::statements::parse_ident;

pub fn parse_component(
    src: Source,
    input: &mut ParseInput,
) -> Result<ast::Component, ParserError> {
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
            _ => {
                return Err(input.unexpected_token("Top level item (e.g. import, global, function"))
            }
        }
    }

    Ok(component)
}

fn parse_import(
    input: &mut ParseInput,
    comp: &mut ast::Component,
) -> Result<ImportId, ParserError> {
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
    comp: &mut ast::Component,
    exported: bool,
) -> Result<GlobalId, ParserError> {
    let err_no_let = "Global variable definitions must start with 'let'";
    input.assert_next(Token::Let, err_no_let)?;

    let mutable = input.next_if(Token::Mut).is_some();
    let ident = parse_ident(input, comp)?;

    let err_no_colon = "Global variables must have explicit types annotated starting with ':'";
    input.assert_next(Token::Colon, err_no_colon)?;

    let type_id = parse_valtype(input, comp)?;

    let err_no_assign = "Global variables must be initialized starting with '='";
    input.assert_next(Token::Assign, err_no_assign)?;

    let init_value = parse_expression(input, comp)?;

    let err_no_semicolon = "Global variable definitions must end with ';'";
    input.assert_next(Token::Semicolon, err_no_semicolon)?;

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
    comp: &mut ast::Component,
    exported: bool,
) -> Result<FunctionId, ParserError> {
    input.assert_next(Token::Func, "Function signature")?;
    let ident = parse_ident(input, comp)?;
    let arguments = parse_args(input, comp)?;
    let return_type = parse_return_type(input, comp)?;
    let (body, _) = parse_block(input, comp)?;

    let function = ast::Function {
        exported,
        ident,
        arguments,
        return_type,
        body,
    };

    Ok(comp.functions.push(function))
}

fn parse_args(
    input: &mut ParseInput,
    comp: &mut ast::Component,
) -> Result<Vec<(NameId, TypeId)>, ParserError> {
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
    input.assert_next(
        Token::RParen,
        "Function argument parenthesis must be closed",
    )?;

    Ok(arguments)
}

fn parse_argument(
    input: &mut ParseInput,
    comp: &mut ast::Component,
) -> Result<(NameId, TypeId), ParserError> {
    let ident = parse_ident(input, comp)?;
    input.assert_next(Token::Colon, "Colon ':'")?;
    let type_id = parse_valtype(input, comp)?;
    Ok((ident, type_id))
}

fn parse_return_type(
    input: &mut ParseInput,
    comp: &mut ast::Component,
) -> Result<Option<TypeId>, ParserError> {
    let return_type = match input.next_if(Token::Arrow) {
        Some(_) => Some(parse_valtype(input, comp)?),
        None => None,
    };
    Ok(return_type)
}

fn parse_external_type(
    input: &mut ParseInput,
    comp: &mut ast::Component,
) -> Result<ast::ExternalType, ParserError> {
    Ok(ast::ExternalType::Function(parse_fn_type(input, comp)?))
}

fn parse_fn_type(
    input: &mut ParseInput,
    comp: &mut ast::Component,
) -> Result<ast::FnType, ParserError> {
    input.assert_next(Token::Func, "Function keyword")?;
    let arguments = parse_args(input, comp)?;
    let return_type = parse_return_type(input, comp)?;

    Ok(ast::FnType {
        arguments,
        return_type,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use claw_common::UnwrapPretty;
    use crate::make_input;

    #[test]
    fn test_increment() {
        let source = "
        let mut counter: u32 = 0;

        export func increment() -> u32 {
            counter = counter + 1;
            return counter;
        }";
        let (src, mut input) = make_input(source);
        parse_component(src, &mut input).unwrap_pretty();
    }

    #[test]
    fn test_empty_function() {
        let source = "func empty() {}";
        let (src, mut input) = make_input(source);
        let mut comp = ast::Component::new(src.clone());
        parse_func(&mut input.clone(), &mut comp, false).unwrap_pretty();
        parse_component(src, &mut input).unwrap_pretty();
    }

    #[test]
    fn test_basic_function() {
        let source = "func increment() -> u32 { return 0; }";
        let (src, mut input) = make_input(source);
        let mut comp = ast::Component::new(src.clone());
        parse_func(&mut input.clone(), &mut comp, false).unwrap_pretty();
        parse_component(src, &mut input).unwrap_pretty();
    }

    #[test]
    fn test_parse_global() {
        let source = "let mut counter: u32 = 0;";
        let (src, mut input) = make_input(source);
        let mut comp = ast::Component::new(src);
        parse_global(&mut input, &mut comp, false).unwrap_pretty();
    }
}
