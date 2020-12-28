use logos::{Logos};

use codespan::Span;
use codespan_reporting::files::SimpleFile;

use crate::ast::M;

/// The Token type for the language.
#[derive(Logos, Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Token {
    #[regex(r"[ \n\t\f]", logos::skip)]
    #[error]
    Error,

    /// Single-line comments starting with "//"
    #[regex("//[^\n]*")]
    Comment,

    /// Double-quoted string literal
    #[regex(r#""(\\.|[^\\"])*""#)]
    StringLiteral,

    /// Character literal
    #[regex(r"'(\\.|[^\\'])'")]
    CharLiteral,

    /// A Decimal number literal
    #[regex(r"[1-9]\d*(\.\d+)")]
    DecLiteral,

    /// A Binary number literal
    #[regex(r"0b[01]+")]
    BinLiteral,

    /// A Hexadecimal number literal
    #[regex(r"0x[0-9a-fA-F]+")]
    HexLiteral,

    /// A standard identifier token.
    /// Can contain alpha characters, digits, and underscore.
    /// May not begin with a digit character.
    #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*")]
    Identifier,

    // Keywords -----------------------------------------

    /// The Import Keyword
    #[token("import")]
    Import,
    
    /// The From Keyword
    #[token("from")]
    From,
    
    /// The Export Keyword
    #[token("export")]
    Export,
    
    /// The Function "fn" Keyword
    #[token("fn")]
    Fn,
    
    /// The If Keyword
    #[token("if")]
    If,
    
    /// The Loop Keyword
    #[token("loop")]
    Loop,
    
    /// The For Keyword
    #[token("for")]
    For,
    
    /// The In Keyword
    #[token("in")]
    In,

    /// The as Keyword
    #[token("as")]
    As,
    
    /// The Let Keyword
    #[token("let")]
    Let,
    
    /// The Mut Keyword
    #[token("mut")]
    Mut,
    
    /// The Unsigned 32-bit Integer Type Keyword 
    #[token("u32")]
    U32,
    
    /// The Unsigned 64-bit Integer Type Keyword 
    #[token("u64")]
    U64,
    
    /// The Signed 32-bit Integer Type Keyword 
    #[token("s32")]
    S32,
    
    /// The Signed 32-bit Integer Type Keyword 
    #[token("s64")]
    S64,
    
    /// The 32-bit Integer Type Keyword 
    #[token("i32")]
    I32,
    
    /// The 32-bit Integer Type Keyword 
    #[token("i64")]
    I64,
    
    /// The 32-bit Floating-point Type Keyword
    #[token("f32")]
    F32,
    
    /// The 64-bit Floating-point Type Keyword
    #[token("f64")]
    F64,

    /// The Pointer Type Keyword
    #[token("Ptr")]
    Ptr,
    
    /// The Slice Type Keyword
    #[token("Slice")]
    Slice,

    // Symbols -----------------------------------------

    /// Left Parenthesis Symbol "("
    #[token("(")]
    LParen,

    /// Right Parenthesis Symbol ")"
    #[token(")")]
    RParen,

    /// Left Brace Symbol "{"
    #[token("{")]
    LBrace,

    /// Right Brace Symbol "}"
    #[token("}")]
    RBrace,

    /// Left Bracket Symbol "["
    #[token("[")]
    LBracket,

    /// Right Bracket Symbol "]"
    #[token("]")]
    RBracket,

    /// Assignment Operator "="
    #[token("=")]
    Assign,

    /// The Right Arrow Symbol "->"
    #[token("->")]
    Arrow,

    /// Less-than Operator "<"
    #[token("<")]
    LT,

    /// Less-than or Equal Operator "<="
    #[token("<=")]
    LTE,

    /// Greater-than Operator ">"
    #[token(">")]
    GT,

    /// Greater-than or Equal Operator ">="
    #[token(">=")]
    GTE,

    /// Equals Operator "=="
    #[token("==")]
    EQ,

    // Not Equals Operator "!="
    #[token("!=")]
    NEQ,

    /// Addition Operator "+"
    #[token("+")]
    Add,

    /// Add and Assign Operator "+="
    #[token("+=")]
    AddAssign,

    /// Subtraction Operator "-"
    #[token("-")]
    Sub,

    /// Subtract and Assign Operator "-="
    #[token("-=")]
    SubAssign,

    /// Star Operator "*" (used for dereference and multiply)
    #[token("*")]
    Star,

    /// Star Operator "*=" (used for multiply)
    #[token("*=")]
    StarAssign,

    /// Division Operator "/"
    #[token("/")]
    Div,

    /// Division Operator "/"
    #[token("/=")]
    DivAssign,

    /// The Period or Dot Operator "."
    #[token(".")]
    Dot,

    /// The Range Operator ".."
    #[token("..")]
    Range,
    
    /// Colon Symbol ":"
    #[token(":")]
    Colon,
    
    /// Double Colon Symbol "::"
    #[token("::")]
    DoubleColon,

    /// Semicolon Symbol ";"
    #[token(";")]
    Semicolon,
}

/// The ID for a Token
#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub struct TokenId(u32);

/// The tokens from a successful parse
#[derive(Debug, Clone)]
pub struct TokenData {
    pub file: SimpleFile<String, String>,
    pub tokens: Vec<M<Token>>
}

/// A collection of locations where the tokenizer failed to match a token
#[derive(Debug, Clone)]
pub struct TokenErrors {
    pub file: SimpleFile<String, String>,
    pub errors: Vec<usize>
}

pub fn tokenize(file: SimpleFile<String, String>) -> Result<TokenData, TokenErrors> {
    let mut index = 0;
    let mut tokens = Vec::new();
    let mut errors = Vec::new();

    let mut lex = Token::lexer(file.source());

    while let Some(item) = lex.next() {
        if item == Token::Error {
            errors.push(lex.span().start);
        } else {
            let id = TokenId(index);
            index += 1;
            let token_entry = M {
                value: item,
                span: Span::new(lex.span().start as u32, lex.span().end as u32)
            };
            tokens.push(token_entry);
        }
    }

    if errors.is_empty() {
        Ok(TokenData { file, tokens })
    } else {
        Err(TokenErrors { file, errors })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test1() {
        let input = SimpleFile::new(
            "test".into(),
            "fn test(a: u32) -> u32".into()
        );
        let output = vec![
            M {
                value: Token::Fn,
                span: Span::new(0, 2)
            },
            M {
                value: Token::Identifier,
                span: Span::new(3, 7)
            },
            M {
                value: Token::LParen,
                span:Span::new(7, 8)
            },
            M {
                value: Token::Identifier,
                span: Span::new(8, 9)
            },
            M {
                value: Token::Colon,
                span: Span::new(9, 10)
            },
            M {
                value: Token::U32,
                span: Span::new(11, 14)
            },
            M {
                value: Token::RParen,
                span: Span::new(14, 15)
            },
            M {
                value: Token::Arrow,
                span: Span::new(16, 18)
            },
            M {
                value: Token::U32,
                span: Span::new(19, 22)
            }
        ];

        match tokenize(input) {
            Ok(TokenData { file: _, tokens }) => assert_eq!(output, tokens),
            Err(_) => panic!("Should not have failed")
        }
    }

    #[test]
    fn test2() {
        let input = SimpleFile::new(
            "test".into(),
            r#"let a = "asdf\"";"#.into()
        );
        let output = vec![
            M {
                value: Token::Let,
                span: Span::new(0, 3)
            },
            M {
                value: Token::Identifier,
                span: Span::new(4, 5)
            },
            M {
                value: Token::Assign,
                span: Span::new(6, 7)
            },
            M {
                value: Token::StringLiteral,
                span: Span::new(8, 16)
            },
            M {
                value: Token::Semicolon,
                span: Span::new(16, 17)
            }
        ];

        match tokenize(input) {
            Ok(TokenData { file: _, tokens }) => assert_eq!(output, tokens),
            Err(_) => panic!("Should not have failed")
        }
    }
}