use logos::Logos;

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use claw_common::Source;

#[derive(Debug, PartialEq, Clone)]
pub struct TokenData {
    pub token: Token,
    pub span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("Unable to tokenize input")]
#[diagnostic()]
pub struct LexerError {
    #[source_code]
    src: Source,
    #[label("Here")]
    span: SourceSpan,
}

pub fn tokenize(src: Source, contents: &str) -> Result<Vec<TokenData>, LexerError> {
    let lexer = Token::lexer(contents);

    lexer
        .spanned()
        .map(|(token, span)| match token {
            Ok(token) => Ok(TokenData {
                token,
                span: SourceSpan::from(span),
            }),
            Err(_error) => Err(LexerError {
                src: src.clone(),
                span: span.into(),
            }),
        })
        .collect()
}

/// The Token type for the language.
#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(error = ())]
#[logos(skip r"[ \t\r\n\f]+")]
#[logos(skip r"//[^\n]*")]
#[logos(subpattern word = r"[a-z][a-z0-9]*|[A-Z][A-Z0-9]*")]
#[logos(subpattern id = r"%?(?&word)(-(?&word))*")]
pub enum Token {
    /// Double-quoted string literal
    #[token("\"", parse_string_literal)]
    #[token("r", parse_raw_string_literal)]
    StringLiteral(String),

    /// A Decimal number literal
    #[regex(r"[0-9][_0-9]*", |lex| parse_decint_literal(lex.slice()))]
    #[regex(r"0b[01][_01]*", |lex| parse_bin_literal(lex.slice()))]
    #[regex(r"0x[0-9a-fA-F][_0-9a-fA-F]*", |lex| parse_hex_literal(lex.slice()))]
    IntLiteral(u64),

    /// A Decimal floating point literal
    #[regex(r"[0-9][_0-9]*\.[0-9][_0-9]*", |lex| parse_decfloat_literal(lex.slice()))]
    FloatLiteral(f64),

    /// An Identifier
    #[regex(r"(?&id)", |lex| lex.slice().to_string())]
    Identifier(String),

    // Keywords -----------------------------------------
    /// The Export Keyword
    #[token("export")]
    Export,

    /// The Import Keyword
    #[token("import")]
    Import,

    /// The From Keyword
    #[token("from")]
    From,

    /// The Function "func" Keyword
    #[token("func")]
    Func,

    /// The If Keyword
    #[token("if")]
    If,

    /// The For Keyword
    #[token("for")]
    For,

    /// The In Keyword
    #[token("in")]
    In,

    /// The Loop Keyword
    #[token("loop")]
    Loop,

    /// The Break Keyword
    #[token("break")]
    Break,

    /// The Continue Keyword
    #[token("continue")]
    Continue,

    /// The Return Keyword
    #[token("return")]
    Return,

    /// The Result Type Keyword
    #[token("result")]
    Result,

    /// The String Type Keyword
    #[token("string")]
    String,

    /// The Unsigned 8-bit Integer Type Keyword
    #[token("u8")]
    U8,

    /// The Unsigned 16-bit Integer Type Keyword
    #[token("u16")]
    U16,

    /// The Unsigned 32-bit Integer Type Keyword
    #[token("u32")]
    U32,

    /// The Unsigned 64-bit Integer Type Keyword
    #[token("u64")]
    U64,

    /// The Signed 8-bit Integer Type Keyword
    #[token("s8")]
    S8,

    /// The Signed 16-bit Integer Type Keyword
    #[token("s16")]
    S16,

    /// The Signed 32-bit Integer Type Keyword
    #[token("s32")]
    S32,

    /// The Signed 32-bit Integer Type Keyword
    #[token("s64")]
    S64,

    /// The 32-bit Floating-point Type Keyword
    #[token("f32")]
    F32,

    /// The 64-bit Floating-point Type Keyword
    #[token("f64")]
    F64,

    /// The As Keyword
    #[token("as")]
    As,

    /// The At Keyword
    #[token("at")]
    At,

    /// The Let Keyword
    #[token("let")]
    Let,

    /// The Mut Keyword
    #[token("mut")]
    Mut,

    /// The Bool Keyword
    #[token("bool")]
    Bool,

    /// The True Keyword
    #[token("true")]
    True,

    /// The False Keyword
    #[token("false")]
    False,

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

    /// The Comma Delimiter ","
    #[token(",")]
    Comma,

    /// The Period or Dot Operator "."
    #[token(".")]
    Dot,

    /// The Range Operator ".."
    #[token("..")]
    Range,

    /// Colon Symbol ":"
    #[token(":")]
    Colon,

    /// Semicolon Symbol ";"
    #[token(";")]
    Semicolon,

    /// Assignment Operator "="
    #[token("=")]
    Assign,

    /// The Right Arrow Symbol "->"
    #[token("->")]
    Arrow,

    /// Addition Operator "+"
    #[token("+")]
    Add,

    /// Subtraction Operator "-"
    #[token("-")]
    Sub,

    /// Multiplication Operator "*"
    #[token("*")]
    Mult,

    /// Division Operator "/"
    #[token("/")]
    Div,

    /// Modulo Operator "%"
    #[token("%")]
    Mod,

    /// Invert Operator "!"
    #[token("!")]
    Invert,

    /// Logical And Operator
    #[token("and")]
    LogicalAnd,

    /// Logical Or Operator
    #[token("or")]
    LogicalOr,

    /// Bitwise Or "|"
    #[token("|")]
    BitOr,

    /// Bitwise And "&"
    #[token("&")]
    BitAnd,

    /// Bitwise XOR "^"
    #[token("^")]
    BitXor,

    /// Bit Shift Left Operator "<<"
    #[token("<<")]
    BitShiftL,

    /// Bit Shift Right Operator ">>"
    #[token(">>")]
    BitShiftR,

    /// Arithmetic Shift Right Operator ">>>"
    #[token(">>>")]
    ArithShiftR,

    /// Bitwise-Or and Assign Operator "+="
    #[token("|=")]
    BitOrAssign,

    /// Bitwise-And and Assign Operator "+="
    #[token("&=")]
    BitAndAssign,

    /// Bitwsie-Xor and Assign Operator "+="
    #[token("^=")]
    BitXorAssign,

    /// Add and Assign Operator "+="
    #[token("+=")]
    AddAssign,

    /// Subtract and Assign Operator "-="
    #[token("-=")]
    SubAssign,

    /// Star Operator "*=" (used for multiply)
    #[token("*=")]
    StarAssign,

    /// Division Operator "/"
    #[token("/=")]
    DivAssign,

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
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::StringLiteral(s) => write!(f, "\"{}\"", s),
            Token::IntLiteral(i) => write!(f, "{}", i),
            Token::FloatLiteral(float) => write!(f, "{:?}", float),
            Token::Identifier(ident) => write!(f, "{}", ident),
            Token::Export => write!(f, "export"),
            Token::Import => write!(f, "import"),
            Token::From => write!(f, "from"),
            Token::Func => write!(f, "func"),
            Token::If => write!(f, "if"),
            Token::For => write!(f, "for"),
            Token::In => write!(f, "in"),
            Token::Loop => write!(f, "loop"),
            Token::Break => write!(f, "break"),
            Token::Continue => write!(f, "continue"),
            Token::Return => write!(f, "return"),
            Token::Result => write!(f, "result"),
            Token::String => write!(f, "string"),
            Token::U8 => write!(f, "u8"),
            Token::U16 => write!(f, "u16"),
            Token::U32 => write!(f, "u32"),
            Token::U64 => write!(f, "u64"),
            Token::S8 => write!(f, "S8"),
            Token::S16 => write!(f, "S16"),
            Token::S32 => write!(f, "S32"),
            Token::S64 => write!(f, "s64"),
            Token::F32 => write!(f, "f32"),
            Token::F64 => write!(f, "f64"),
            Token::As => write!(f, "as"),
            Token::At => write!(f, "at"),
            Token::Let => write!(f, "let"),
            Token::Mut => write!(f, "mut"),
            Token::Bool => write!(f, "bool"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::Comma => write!(f, ","),
            Token::Dot => write!(f, "."),
            Token::Range => write!(f, ".."),
            Token::Colon => write!(f, ":"),
            Token::Semicolon => write!(f, ";"),
            Token::Assign => write!(f, "="),
            Token::Arrow => write!(f, "->"),
            Token::Add => write!(f, "+"),
            Token::Sub => write!(f, "-"),
            Token::Mult => write!(f, "*"),
            Token::Div => write!(f, "/"),
            Token::Mod => write!(f, "%"),
            Token::Invert => write!(f, "!"),
            Token::LogicalAnd => write!(f, "and"),
            Token::LogicalOr => write!(f, "or"),
            Token::BitOr => write!(f, "|"),
            Token::BitAnd => write!(f, "&"),
            Token::BitXor => write!(f, "^"),
            Token::BitShiftL => write!(f, "<<"),
            Token::BitShiftR => write!(f, ">>"),
            Token::ArithShiftR => write!(f, ">>>"),
            Token::BitOrAssign => write!(f, "|="),
            Token::BitAndAssign => write!(f, "&="),
            Token::BitXorAssign => write!(f, "^="),
            Token::AddAssign => write!(f, "+="),
            Token::SubAssign => write!(f, "-="),
            Token::StarAssign => write!(f, "*="),
            Token::DivAssign => write!(f, "/="),
            Token::LT => write!(f, "<"),
            Token::LTE => write!(f, "<="),
            Token::GT => write!(f, ">"),
            Token::GTE => write!(f, ">="),
            Token::EQ => write!(f, "=="),
            Token::NEQ => write!(f, "!="),
        }
    }
}

/// Parses a string according to the JSON string format in ECMA-404.
fn parse_string_literal(lex: &mut logos::Lexer<'_, Token>) -> Option<String> {
    let mut c_iter = lex.remainder().chars();
    let mut buf = String::new();

    while let Some(c) = c_iter.next() {
        // End the parse when you encounter another quote
        if c == '"' {
            lex.bump(1);
            return Some(buf);
        }

        // If slash, then parse an escaped character
        if c == '\\' {
            lex.bump(1);
            if let Some((c_esc, c_len)) = parse_escaped_char(&mut c_iter) {
                lex.bump(c_len);
                buf.push(c_esc);
            }
        } else {
            lex.bump(c.len_utf8());
            buf.push(c);
        }
    }

    None
}

/// Parses an escaped character according to the JSON string format in ECMA-404.
/// Takes in an iterator which starts after the beginning slash.
/// If successful, returns the produced char and the length of input consumed.
fn parse_escaped_char(lex: &mut std::str::Chars) -> Option<(char, usize)> {
    let res = match lex.next()? {
        '\"' => ('\"', 1),
        '\\' => ('\\', 1),
        '/' => ('/', 1),
        'b' => ('\u{0008}', 1),
        'f' => ('\u{000C}', 1),
        'n' => ('\n', 1),
        'r' => ('\r', 1),
        't' => ('\t', 1),
        'u' => {
            // Combine next for characters together, fail if they can't be found
            let next_4: [Option<char>; 4] = [lex.next(), lex.next(), lex.next(), lex.next()];
            let next_4: Option<Vec<char>> = next_4.iter().copied().collect();
            let next_4: String = next_4?.into_iter().collect();

            let code_point = u32::from_str_radix(&next_4, 16).ok()?;
            let new_c: char = std::char::from_u32(code_point)?;

            (new_c, 5)
        }
        _ => return None,
    };

    Some(res)
}

/// Parses a raw string literal
fn parse_raw_string_literal(lex: &mut logos::Lexer<'_, Token>) -> Option<String> {
    let mut c_iter = lex.remainder().chars();
    let mut buf = String::new();

    let mut starting_hashes = 0;
    let mut starting_quote = false;

    while let Some(c) = c_iter.next() {
        lex.bump(c.len_utf8());
        if c == '"' {
            starting_quote = true;
            break;
        }
        if c == '#' {
            starting_hashes += 1;
        } else {
            return None;
        }
    }

    if !starting_quote {
        return None;
    }

    let mut seen_quote = false;
    let mut hash_count = 0;

    while let Some(c) = c_iter.next() {
        lex.bump(c.len_utf8());
        if seen_quote && c == '#' {
            hash_count += 1;

            if hash_count == starting_hashes {
                return Some(buf);
            }
            continue;
        }

        // Append the unused marker quote
        if seen_quote {
            buf.push('"');
        }
        // Reset the seen quote flag
        seen_quote = false;

        // Append the unused marker hashes
        for _ in 0..hash_count {
            buf.push('#');
        }
        // Reset the hash count
        hash_count = 0;

        if c == '"' {
            seen_quote = true;
        } else {
            buf.push(c);
        }
    }

    None
}

fn parse_decint_literal(s: &str) -> Option<u64> {
    s.replace('_', "").parse().ok()
}

fn parse_decfloat_literal(s: &str) -> Option<f64> {
    s.replace('_', "").parse().ok()
}

fn parse_bin_literal(s: &str) -> Option<u64> {
    u64::from_str_radix(&s[2..].replace('_', ""), 2).ok()
}

fn parse_hex_literal(s: &str) -> Option<u64> {
    u64::from_str_radix(&s[2..].replace('_', ""), 16).ok()
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;
    use claw_common::make_source;

    #[test]
    fn tokenize_func_declaration() {
        let contents = "func test(a: u32) -> u32";
        let src = make_source("test", contents);
        let ident_test = Token::Identifier("test".to_owned());
        let ident_a = Token::Identifier("a".to_owned());
        let output = vec![
            (Token::Func, SourceSpan::from(0..4)),
            (ident_test, SourceSpan::from(5..9)),
            (Token::LParen, SourceSpan::from(9..10)),
            (ident_a, SourceSpan::from(10..11)),
            (Token::Colon, SourceSpan::from(11..12)),
            (Token::U32, SourceSpan::from(13..16)),
            (Token::RParen, SourceSpan::from(16..17)),
            (Token::Arrow, SourceSpan::from(18..20)),
            (Token::U32, SourceSpan::from(21..24)),
        ]
        .into_iter()
        .map(to_token_data)
        .collect::<Vec<TokenData>>();

        match tokenize(src, contents) {
            Ok(tokens) => assert_eq!(output, tokens),
            Err(_) => panic!("Should not have failed"),
        }
    }

    #[test]
    fn tokenize_let() {
        let contents = r#"let a = "asdf\"";"#;
        let src = make_source("test", contents);
        let ident_a = Token::Identifier("a".to_owned());
        let string_asdf = Token::StringLiteral(String::from(r#"asdf""#));
        let output = vec![
            (Token::Let, SourceSpan::from(0..3)),
            (ident_a, SourceSpan::from(4..5)),
            (Token::Assign, SourceSpan::from(6..7)),
            (string_asdf, SourceSpan::from(8..16)),
            (Token::Semicolon, SourceSpan::from(16..17)),
        ]
        .into_iter()
        .map(to_token_data)
        .collect::<Vec<TokenData>>();

        match tokenize(src, contents) {
            Ok(tokens) => assert_eq!(output, tokens),
            Err(_) => panic!("Should not have failed"),
        }
    }

    fn to_token_data(d: (Token, SourceSpan)) -> TokenData {
        TokenData {
            token: d.0,
            span: d.1,
        }
    }
}
