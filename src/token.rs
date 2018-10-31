/// The fundamental units of the language, where each token represents an atomic element of the
/// language grammar.
#[derive(Debug, PartialEq)]
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
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Lt,
    LtEq,
    Gt,
    GtEq,
    Eq_,
    EqEq,
    NotEq,

    // Keywords.
    And,
    Or,
    Not,
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
    None_,
    True,
    False,
    Num(f64),
    Str(String),
    Ident(String),

    // End-of-file marker.
    EOF,
}

impl Token {
    pub fn to_keyword(ident: &str) -> Option<Token> {
        Some(match ident {
            "and" => Token::And,
            "or" => Token::Or,
            "not" => Token::Not,
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
            "none" => Token::None_,
            "true" => Token::True,
            "false" => Token::False,
            _ => return None,
        })
    }
}

/// Represents a token and metadata that maps it back to the source code, indicating its position
/// and the "span" of source text it comes from. Thinking of source code as a series of lines, a
/// line is then composed of a series of "spans".
#[derive(Debug)]
pub struct Span {
    pub token: Token,
    pub line: u64,
}

impl Span {
    pub fn new(token: Token, line: u64) -> Self {
        Span { token, line }
    }
}
