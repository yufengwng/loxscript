#[derive(Debug)]
pub struct Token {
    pub kind: Kind,
    pub line: u64,
}

impl Token {
    pub fn new(kind: Kind, line: u64) -> Token {
        Token { kind, line }
    }
}

#[derive(Debug, PartialEq)]
pub enum Kind {
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
    Eq,
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
    Selfie,
    Super,

    // Literals.
    None,
    True,
    False,
    Num(f64),
    Str(String),
    Ident(String),

    // Special marker.
    EOF
}

impl Kind {
    pub fn to_keyword(ident: &str) -> Option<Kind> {
        Some(match ident {
            "and" => Kind::And,
            "or" => Kind::Or,
            "not" => Kind::Not,
            "if" => Kind::If,
            "elif" => Kind::Elif,
            "else" => Kind::Else,
            "for" => Kind::For,
            "while" => Kind::While,
            "break" => Kind::Break,
            "continue" => Kind::Cont,
            "return" => Kind::Ret,
            "let" => Kind::Let,
            "fun" => Kind::Fun,
            "class" => Kind::Class,
            "self" => Kind::Selfie,
            "super" => Kind::Super,
            "none" => Kind::None,
            "true" => Kind::True,
            "false" => Kind::False,
            _ => return None,
        })
    }
}
