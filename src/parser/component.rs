use cranelift_entity::PrimaryMap;

use crate::ast::{self, component::FunctionSignature};
use crate::ast::{Arenas, ExternalType, FnType, Import, NameId, TypeId};
use crate::lexer::Token;
use crate::parser::{
    expressions::parse_expression, statements::parse_block, types::parse_valtype, ParseInput,
    ParserError,
};

use super::statements::parse_ident;

pub fn parse_component(input: &mut ParseInput) -> Result<ast::Component, ParserError> {
    let mut arenas = ast::Arenas::default();
    let mut imports = PrimaryMap::default();
    let mut globals = PrimaryMap::default();
    let mut functions = PrimaryMap::default();

    while !input.done() {
        // Check for the export keyword
        let exported = input.next_if(Token::Export).is_some();

        // Determine the kind of item and parse it
        match input.peek()?.token {
            Token::Import => {
                imports.push(parse_import(input, &mut arenas)?);
            }
            Token::Let => {
                globals.push(parse_global(input, &mut arenas, exported)?);
            }
            _ => {
                functions.push(parse_func(input, &mut arenas, exported)?);
            }
        }
    }

    Ok(ast::Component {
        arenas,
        imports,
        globals,
        functions,
    })
}

fn parse_import(input: &mut ParseInput, arenas: &mut Arenas) -> Result<Import, ParserError> {
    input.assert_next(Token::Import, "Import")?;
    let ident = parse_ident(input, arenas)?;
    input.assert_next(Token::Colon, "Colon")?;
    let external_type = parse_external_type(input, arenas)?;
    input.assert_next(Token::Semicolon, "Semicolon")?;

    Ok(Import {
        ident,
        external_type,
    })
}

fn parse_global(
    input: &mut ParseInput,
    arenas: &mut Arenas,
    exported: bool,
) -> Result<ast::Global, ParserError> {
    input.assert_next(Token::Let, "Let")?;
    let mutable = input.next_if(Token::Mut).is_some();

    let ident = parse_ident(input, arenas)?;

    input.assert_next(Token::Colon, "Colon: ':'")?;
    let type_id = parse_valtype(input, arenas)?;

    input.assert_next(Token::Assign, "Assign '='")?;
    let init_value = parse_expression(input, arenas)?;
    input.assert_next(Token::Semicolon, "Semicolon ';'")?;

    Ok(ast::Global {
        exported,
        mutable,
        ident,
        type_id,
        init_value,
    })
}

fn parse_func(
    input: &mut ParseInput,
    arenas: &mut Arenas,
    exported: bool,
) -> Result<ast::Function, ParserError> {
    let signature = parse_func_signature(input, arenas)?;
    let (body, span) = parse_block(input, arenas)?;

    Ok(ast::Function {
        exported,
        signature,
        body,
    })
}

fn parse_func_signature(
    input: &mut ParseInput,
    arenas: &mut Arenas,
) -> Result<FunctionSignature, ParserError> {
    let ident = parse_ident(input, arenas)?;

    let colon = input.assert_next(Token::Colon, "Function signature colon")?;

    let fn_type = parse_fn_type(input, arenas)?;

    Ok(FunctionSignature { ident, fn_type })
}

fn parse_argument(
    input: &mut ParseInput,
    arenas: &mut Arenas,
) -> Result<(NameId, TypeId), ParserError> {
    let ident = parse_ident(input, arenas)?;
    input.assert_next(Token::Colon, "Colon ':'")?;
    let type_id = parse_valtype(input, arenas)?;
    Ok((ident, type_id))
}

fn parse_external_type(
    input: &mut ParseInput,
    arenas: &mut Arenas,
) -> Result<ExternalType, ParserError> {
    Ok(ExternalType::Function(parse_fn_type(input, arenas)?))
}

fn parse_fn_type(input: &mut ParseInput, arenas: &mut Arenas) -> Result<FnType, ParserError> {
    let _func_kwd = input.assert_next(Token::Func, "Function keyword")?;
    let _lparen = input.assert_next(Token::LParen, "Start function arguments")?;

    let mut arguments = Vec::new();
    while input.peek()?.token != Token::RParen {
        let argument = parse_argument(input, arenas)?;
        arguments.push(argument);

        if input.peek()?.token != Token::Comma {
            break;
        }

        let _ = input.next();
    }

    let _rparen = input.assert_next(Token::RParen, "End function arguments")?;
    let arrow = input.assert_next(Token::Arrow, "Results arrow")?;
    let return_type = parse_valtype(input, arenas)?;

    Ok(FnType {
        arguments,
        return_type,
    })
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
        let mut arenas = Arenas::default();
        let source = "increment: func() -> u32 {}";
        let _func = parse_func(&mut make_input(source), &mut arenas, false).unwrap();
        let _component = parse_component(&mut make_input(source)).unwrap();
    }

    #[test]
    fn parse_function_signature() {
        let mut arenas = Arenas::default();
        let source = "increment: func() -> u32";
        let _func_sig = parse_func_signature(&mut make_input(source), &mut arenas).unwrap();
    }

    #[test]
    fn test_parse_global() {
        let mut arenas = Arenas::default();
        let source = "let mut counter: u32 = 0;";
        let _global = parse_global(&mut make_input(source), &mut arenas, false).unwrap();
    }
}
