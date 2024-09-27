use crate::lexer::Token;
use crate::{
    expressions::parse_expression, statements::parse_block, types::parse_valtype, ParseInput,
    ParserError,
};
use ast::{FunctionId, GlobalId, Import, ImportFrom, ImportId, NameId, PlainImport, TypeId};
use claw_ast as ast;

use claw_common::Source;

use crate::names::{parse_ident, parse_interface_name};

pub fn parse_component(src: Source, input: &mut ParseInput) -> Result<ast::Component, ParserError> {
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
    let token = input.peekn(1).unwrap();
    let import = match token {
        Token::LBrace => Import::ImportFrom(parse_import_from(input, comp)?),
        Token::Identifier(_) => Import::Plain(parse_plain_import(input, comp)?),
        _ => return Err(input.unexpected_token("Invalid import")),
    };

    Ok(comp.push_import(import))
}

fn parse_plain_import(
    input: &mut ParseInput,
    comp: &mut ast::Component,
) -> Result<PlainImport, ParserError> {
    input.assert_next(Token::Import, "Import item")?;
    let ident = parse_ident(input, comp)?;
    let alias = match input.peek()?.token {
        Token::As => {
            // Consume the `as`
            let _ = input.next();
            Some(parse_ident(input, comp)?)
        }
        _ => None,
    };
    input.assert_next(Token::Colon, "Plain imports must annotate their type")?;
    let external_type = parse_external_type(input, comp)?;
    input.assert_next(
        Token::Semicolon,
        "Plain imports must be ended by semicolons",
    )?;

    Ok(PlainImport {
        ident,
        alias,
        external_type,
    })
}

fn parse_import_from(
    input: &mut ParseInput,
    comp: &mut ast::Component,
) -> Result<ImportFrom, ParserError> {
    input.assert_next(Token::Import, "Import")?;
    input.assert_next(Token::LBrace, "Imported items")?;

    let mut items = Vec::new();
    loop {
        if input.peek()?.token == Token::RBrace {
            break;
        }

        items.push(parse_import_item(input, comp)?);

        if input.next_if(Token::Comma).is_none() {
            break;
        }
    }

    input.assert_next(Token::RBrace, "End of imported items")?;
    input.assert_next(Token::From, "Specify the package to import from")?;

    let (package, interface) = parse_interface_name(input)?;

    input.assert_next(Token::Semicolon, "Imports must be ended with a semicolon")?;

    Ok(ImportFrom {
        items,
        package,
        interface,
    })
}

fn parse_import_item(
    input: &mut ParseInput,
    comp: &mut ast::Component,
) -> Result<(NameId, Option<NameId>), ParserError> {
    let ident = parse_ident(input, comp)?;

    let alias = if input.peek()?.token == Token::As {
        input.next()?;
        let alias = parse_ident(input, comp)?;
        Some(alias)
    } else {
        None
    };

    Ok((ident, alias))
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

    Ok(comp.push_global(global))
}

fn parse_func(
    input: &mut ParseInput,
    comp: &mut ast::Component,
    exported: bool,
) -> Result<FunctionId, ParserError> {
    input.assert_next(Token::Func, "Function signature")?;
    let ident = parse_ident(input, comp)?;
    let params = parse_params(input, comp)?;
    let results = parse_results(input, comp)?;
    let (body, _) = parse_block(input, comp)?;

    let function = ast::Function {
        exported,
        ident,
        params,
        results,
        body,
    };

    Ok(comp.push_function(function))
}

fn parse_params(
    input: &mut ParseInput,
    comp: &mut ast::Component,
) -> Result<Vec<(NameId, TypeId)>, ParserError> {
    input.assert_next(Token::LParen, "Function parameters are parenthesized")?;

    let mut arguments = Vec::new();
    while input.peek()?.token != Token::RParen {
        let argument = parse_param(input, comp)?;
        arguments.push(argument);

        if input.peek()?.token != Token::Comma {
            break;
        }

        let _ = input.next();
    }
    input.assert_next(
        Token::RParen,
        "Function parameter parenthesis must be closed",
    )?;

    Ok(arguments)
}

fn parse_param(
    input: &mut ParseInput,
    comp: &mut ast::Component,
) -> Result<(NameId, TypeId), ParserError> {
    let ident = parse_ident(input, comp)?;
    input.assert_next(Token::Colon, "Colon ':'")?;
    let type_id = parse_valtype(input, comp)?;
    Ok((ident, type_id))
}

fn parse_results(
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
    let params = parse_params(input, comp)?;
    let results = parse_results(input, comp)?;

    Ok(ast::FnType { params, results })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::make_input;
    use claw_common::UnwrapPretty;

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
