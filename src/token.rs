//! Module for source-level syntactic elements.

use std::fmt;

/// The fundamental units of the language.
///
/// Each token represents an atomic element of the language grammar. A few of the literal tokens
/// also contains the value it represents.
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    // Punctuations.
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Comma,
    Semi,
    Dot,

    // Operators.
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Lt,
    Gt,
    Eq,
    LtEq,
    GtEq,
    EqEq,
    NotEq,

    // Keywords.
    Not,
    And,
    Or,
    If,
    Elif,
    Else,
    For,
    While,
    Break,
    Cont,
    Ret,
    Let,
    Fun,
    Class,
    Self_,
    Super,

    // Literals.
    None,
    True,
    False,
    Num(f64),
    Str(String),
    Ident(String),

    // End-of-file marker.
    EOF,
}

impl Token {
    pub fn get_keyword(ident: &str) -> Option<Token> {
        Some(match ident {
            "not" => Token::Not,
            "and" => Token::And,
            "or" => Token::Or,
            "if" => Token::If,
            "elif" => Token::Elif,
            "else" => Token::Else,
            "for" => Token::For,
            "while" => Token::While,
            "break" => Token::Break,
            "continue" => Token::Cont,
            "return" => Token::Ret,
            "let" => Token::Let,
            "fun" => Token::Fun,
            "class" => Token::Class,
            "self" => Token::Self_,
            "super" => Token::Super,
            "none" => Token::None,
            "true" => Token::True,
            "false" => Token::False,
            _ => return None,
        })
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::Lparen => write!(f, "("),
            Token::Rparen => write!(f, ")"),
            Token::Lbrace => write!(f, "{{"),
            Token::Rbrace => write!(f, "}}"),
            Token::Comma => write!(f, ","),
            Token::Semi => write!(f, ";"),
            Token::Dot => write!(f, "."),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Percent => write!(f, "%"),
            Token::Lt => write!(f, "<"),
            Token::Gt => write!(f, ">"),
            Token::Eq => write!(f, "="),
            Token::LtEq => write!(f, "<="),
            Token::GtEq => write!(f, ">="),
            Token::EqEq => write!(f, "=="),
            Token::NotEq => write!(f, "!="),
            Token::Not => write!(f, "not"),
            Token::And => write!(f, "and"),
            Token::Or => write!(f, "or"),
            Token::If => write!(f, "if"),
            Token::Elif => write!(f, "elif"),
            Token::Else => write!(f, "else"),
            Token::For => write!(f, "for"),
            Token::While => write!(f, "while"),
            Token::Break => write!(f, "break"),
            Token::Cont => write!(f, "continue"),
            Token::Ret => write!(f, "return"),
            Token::Let => write!(f, "let"),
            Token::Fun => write!(f, "fun"),
            Token::Class => write!(f, "class"),
            Token::Self_ => write!(f, "self"),
            Token::Super => write!(f, "super"),
            Token::None => write!(f, "none"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Num(n) => write!(f, "{}", n),
            Token::Str(ref s) | Token::Ident(ref s) => write!(f, "{}", s),
            Token::EOF => write!(f, "EOF"),
        }
    }
}

/// A region of the source code.
///
/// Represents a token and metadata that loosely maps it back to the source code. Thinking of
/// source code as a series of lines, a line is then composed of a series of "spans".
#[derive(Clone, Debug)]
pub struct Span {
    pub token: Token,
    pub line: usize,
}

impl Span {
    pub fn new(token: Token, line: usize) -> Self {
        Self { token, line }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn token_symbols_equality() {
        assert!(Token::EOF == Token::EOF);
        assert_eq!(Token::EOF, Token::EOF);
        assert_eq!(Token::Lt, Token::Lt);
        assert_eq!(Token::Plus, Token::Plus);

        assert!(Token::True != Token::EOF);
        assert_ne!(Token::True, Token::EOF);
        assert_ne!(Token::True, Token::False);
        assert_ne!(Token::Lparen, Token::Lbrace);
        assert_ne!(Token::Plus, Token::Star);
    }

    #[test]
    fn token_num_equality() {
        assert!(Token::Num(1.0) == Token::Num(1.0));
        assert_eq!(Token::Num(1.0), Token::Num(1.0));
        assert_eq!(Token::Num(2.3), Token::Num(2.3));
        assert_eq!(Token::Num(5.13), Token::Num(5.130));
        assert_eq!(Token::Num(-42.13), Token::Num(-42.13));

        assert!(Token::Num(1.0) != Token::Num(1.1));
        assert_ne!(Token::Num(1.0), Token::Num(1.1));
        assert_ne!(Token::Num(1.0), Token::Num(1.01));
        assert_ne!(Token::Num(2.3), Token::Num(5.3));
        assert_ne!(Token::Num(2.3), Token::Num(-2.3));
    }

    #[test]
    fn token_str_equality() {
        assert!(Token::Str(String::from("")) == Token::Str(String::from("")));
        assert_eq!(Token::Str(String::from("")), Token::Str(String::from("")));
        assert_eq!(Token::Str(String::from("a")), Token::Str(String::from("a")));

        assert!(Token::Str(String::from("")) != Token::Str(String::from("a")));
        assert_ne!(Token::Str(String::from("")), Token::Str(String::from("a")));
        assert_ne!(Token::Str(String::from("a")), Token::Str(String::from("b")));
    }

    #[test]
    fn token_ident_equality() {
        assert!(Token::Ident(String::from("")) == Token::Ident(String::from("")));
        assert_eq!(
            Token::Ident(String::from("")),
            Token::Ident(String::from(""))
        );
        assert_eq!(
            Token::Ident(String::from("a")),
            Token::Ident(String::from("a"))
        );

        assert!(Token::Ident(String::from("")) != Token::Ident(String::from("a")));
        assert_ne!(
            Token::Ident(String::from("")),
            Token::Ident(String::from("a"))
        );
        assert_ne!(
            Token::Ident(String::from("a")),
            Token::Ident(String::from("b"))
        );
    }
}
