#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Token {
    // Punctuation.
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
    Eq,
    EqEq,
    BangEq,
    Lt,
    LtEq,
    Gt,
    GtEq,

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
    Continue,
    Return,
    Let,
    Fun,
    Class,
    Super,
    Self_,

    // Literals.
    None,
    True,
    False,
    Ident,
    Str,
    Num,

    // Markers.
    Err,
    EOF,
}

pub struct Span {
    pub token: Token,
    pub slice: String,
    pub line: usize,
}

pub struct Scanner {
    src: Vec<u8>,
    head: usize,
    curr: usize,
    line: usize,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Self {
            src: source.into_bytes(),
            head: 0,
            curr: 0,
            line: 1,
        }
    }

    pub fn scan(&mut self) -> Span {
        self.skip_whitespace();
        if self.is_at_end() {
            return self.make_span(Token::EOF);
        }
        match self.advance() {
            b'(' => return self.make_span(Token::Lparen),
            b')' => return self.make_span(Token::Rparen),
            b'{' => return self.make_span(Token::Lbrace),
            b'}' => return self.make_span(Token::Rbrace),
            b',' => return self.make_span(Token::Comma),
            b';' => return self.make_span(Token::Semi),
            b'.' => return self.make_span(Token::Dot),
            b'+' => return self.make_span(Token::Plus),
            b'-' => return self.make_span(Token::Minus),
            b'*' => return self.make_span(Token::Star),
            b'/' => return self.make_span(Token::Slash),
            b'%' => return self.make_span(Token::Percent),
            b'=' => {
                let matched = self.matched(b'=');
                self.make_span(if matched { Token::EqEq } else { Token::Eq })
            }
            b'<' => {
                let matched = self.matched(b'=');
                self.make_span(if matched { Token::LtEq } else { Token::Lt })
            }
            b'>' => {
                let matched = self.matched(b'=');
                self.make_span(if matched { Token::GtEq } else { Token::Gt })
            }
            b'!' => {
                if self.matched(b'=') {
                    self.make_span(Token::BangEq)
                } else {
                    self.error_span("unexpected character")
                }
            }
            b'"' => return self.make_string(),
            b if is_digit(b) => return self.make_number(),
            b if is_alpha_score(b) => return self.make_identifier(),
            _ => self.error_span("unexpected character"),
        }
    }

    fn buf(&self) -> &[u8] {
        &self.src[self.head..]
    }

    fn is_at_end(&self) -> bool {
        self.head + self.curr >= self.src.len()
    }

    fn consume(&mut self) {
        self.curr += 1;
    }

    fn advance(&mut self) -> u8 {
        let byte = self.buf()[self.curr];
        self.curr += 1;
        byte
    }

    fn peek(&self) -> u8 {
        let buf = self.buf();
        if self.curr < buf.len() {
            buf[self.curr]
        } else {
            b'\0'
        }
    }

    fn peek_next(&self) -> u8 {
        let buf = self.buf();
        if self.curr + 1 < buf.len() {
            buf[self.curr + 1]
        } else {
            b'\0'
        }
    }

    fn matched(&mut self, expected: u8) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.buf()[self.curr] != expected {
            return false;
        }
        self.curr += 1;
        return true;
    }

    fn skip_whitespace(&mut self) {
        while !self.is_at_end() {
            match self.peek() {
                b' ' | b'\r' | b'\t' => {
                    self.consume();
                }
                b'\n' => {
                    self.consume();
                    if !self.is_at_end() {
                        self.line += 1;
                    }
                }
                b'#' => {
                    while !self.is_at_end() && self.peek() != b'\n' {
                        self.consume();
                    }
                }
                _ => break,
            }
        }
        self.slide_buffer();
    }

    fn slide_buffer(&mut self) -> &[u8] {
        let end = self.head + self.curr;
        let bytes = &self.src[self.head..end];
        self.head += self.curr;
        self.curr = 0;
        bytes
    }

    fn error_span(&mut self, message: &str) -> Span {
        self.slide_buffer();
        Span {
            token: Token::Err,
            slice: String::from(message),
            line: self.line,
        }
    }

    fn make_span(&mut self, token: Token) -> Span {
        let line = self.line;
        let bytes = self.slide_buffer().to_vec();
        let slice = String::from_utf8(bytes).unwrap();
        Span { token, slice, line }
    }

    fn make_string(&mut self) -> Span {
        while !self.is_at_end() && self.peek() != b'"' {
            let next = self.advance();
            if next == b'\n' && !self.is_at_end() {
                self.line += 1;
            }
        }

        if self.is_at_end() {
            return self.error_span("unterminated string");
        } else {
            self.consume();
        }

        self.make_span(Token::Str)
    }

    fn make_number(&mut self) -> Span {
        while !self.is_at_end() && is_digit(self.peek()) {
            self.consume();
        }

        if self.peek() == b'.' && is_digit(self.peek_next()) {
            self.consume();
            while !self.is_at_end() && is_digit(self.peek()) {
                self.consume();
            }
        }

        self.make_span(Token::Num)
    }

    fn make_identifier(&mut self) -> Span {
        while !self.is_at_end() && is_alpha_num_score(self.peek()) {
            self.consume();
        }
        self.make_span(self.check_keyword())
    }

    fn check_keyword(&self) -> Token {
        let buf = self.buf();
        match buf[0] {
            b'a' => return self.check_rest(1, b"nd", Token::And),
            b'b' => return self.check_rest(1, b"reak", Token::Break),
            b'c' => {
                if self.curr > 1 {
                    match buf[1] {
                        b'l' => return self.check_rest(2, b"ass", Token::Class),
                        b'o' => return self.check_rest(2, b"ntinue", Token::Continue),
                        _ => {}
                    }
                }
            }
            b'e' => {
                if self.curr > 2 && buf[1] == b'l' {
                    match buf[2] {
                        b'i' => return self.check_rest(3, b"f", Token::Elif),
                        b's' => return self.check_rest(3, b"e", Token::Else),
                        _ => {}
                    }
                }
            }
            b'f' => {
                if self.curr > 1 {
                    match buf[1] {
                        b'a' => return self.check_rest(2, b"lse", Token::False),
                        b'o' => return self.check_rest(2, b"r", Token::For),
                        b'u' => return self.check_rest(2, b"n", Token::Fun),
                        _ => {}
                    }
                }
            }
            b'i' => return self.check_rest(1, b"f", Token::If),
            b'l' => return self.check_rest(1, b"et", Token::Let),
            b'n' => {
                if self.curr > 2 && buf[1] == b'o' {
                    match buf[2] {
                        b'n' => return self.check_rest(3, b"e", Token::None),
                        b't' => return Token::Not,
                        _ => {}
                    }
                }
            }
            b'o' => return self.check_rest(1, b"r", Token::Or),
            b'r' => return self.check_rest(1, b"eturn", Token::Return),
            b's' => {
                if self.curr > 1 {
                    match buf[1] {
                        b'e' => return self.check_rest(2, b"lf", Token::Self_),
                        b'u' => return self.check_rest(2, b"per", Token::Super),
                        _ => {}
                    }
                }
            }
            b't' => return self.check_rest(1, b"rue", Token::True),
            b'w' => return self.check_rest(1, b"hile", Token::While),
            _ => {}
        }
        Token::Ident
    }

    fn check_rest(&self, start: usize, rest: &[u8], token: Token) -> Token {
        if self.curr == start + rest.len() {
            if &self.buf()[start..self.curr] == rest {
                return token;
            }
        }
        Token::Ident
    }
}

fn is_digit(byte: u8) -> bool {
    byte.is_ascii_digit()
}

fn is_alpha_score(byte: u8) -> bool {
    byte == b'_' || byte.is_ascii_alphabetic()
}

fn is_alpha_num_score(byte: u8) -> bool {
    byte == b'_' || byte.is_ascii_alphanumeric()
}
