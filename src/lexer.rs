use crate::token::Span;
use crate::token::Token;

pub struct Lexer {
    src: Vec<char>,
    idx: usize,
    line: usize,
}

impl Lexer {
    pub fn new(src: &str) -> Self {
        Self {
            src: src.chars().collect(),
            idx: 0,
            line: 1,
        }
    }

    pub fn scan(&mut self) -> Vec<Span> {
        let mut spans: Vec<Span> = Vec::new();

        while !self.is_at_end() {
            let line = self.line;
            if let Some(token) = self.scan_token() {
                spans.push(Span::new(token, line));
            }
        }

        spans.push(Span::new(Token::EOF, self.line));
        spans
    }

    fn scan_token(&mut self) -> Option<Token> {
        let curr = self.advance();
        let token = match curr {
            '(' => Token::Lparen,
            ')' => Token::Rparen,
            '{' => Token::Lbrace,
            '}' => Token::Rbrace,
            ',' => Token::Comma,
            ';' => Token::Semi,
            '.' => Token::Dot,

            ' ' | '\t' | '\r' => return None,
            '\n' => {
                self.advance_line();
                return None;
            }

            '#' => {
                self.finish_line_comment();
                return None;
            }

            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Star,
            '/' => Token::Slash,
            '%' => Token::Percent,

            '<' => {
                if self.matches('=') {
                    Token::LtEq
                } else {
                    Token::Lt
                }
            }
            '>' => {
                if self.matches('=') {
                    Token::GtEq
                } else {
                    Token::Gt
                }
            }
            '=' => {
                if self.matches('=') {
                    Token::EqEq
                } else {
                    Token::Eq
                }
            }
            '!' => {
                if self.matches('=') {
                    Token::NotEq
                } else {
                    // report error here
                    return None;
                }
            }

            'a'...'z' | 'A'...'Z' | '_' => self.scan_identifier(),
            '0'...'9' => self.scan_number(),
            '"' => self.scan_string(),

            _ => {
                // report error here
                return None;
            }
        };
        Some(token)
    }

    fn is_at_end(&self) -> bool {
        self.idx >= self.src.len()
    }

    fn advance(&mut self) -> char {
        let curr = self.src[self.idx];
        self.idx += 1;
        curr
    }

    fn advance_line(&mut self) {
        self.line += 1;
    }

    fn consume(&mut self) {
        self.idx += 1;
    }

    fn peek(&self) -> char {
        self.src[self.idx]
    }

    fn matches(&mut self, expected: char) -> bool {
        if !self.is_at_end() && self.peek() == expected {
            self.consume();
            true
        } else {
            false
        }
    }

    fn finish_line_comment(&mut self) {
        while !self.is_at_end() {
            let next = self.advance();
            if next == '\n' {
                self.advance_line();
                return;
            }
        }
    }

    fn scan_identifier(&mut self) -> Token {
        let start = self.idx - 1;
        while !self.is_at_end() && is_alphanumeric(self.peek()) {
            self.consume();
        }
        let name = self.src[start..self.idx].iter().collect::<String>();
        Token::get_keyword(&name).unwrap_or_else(|| Token::Ident(name))
    }

    fn scan_number(&mut self) -> Token {
        let start = self.idx - 1;
        while !self.is_at_end() && (self.peek() == '.' || is_digit(self.peek())) {
            self.consume();
        }
        let rep = self.src[start..self.idx].iter().collect::<String>();
        let num = rep.parse::<f64>().unwrap();
        Token::Num(num)
    }

    fn scan_string(&mut self) -> Token {
        let start = self.idx;
        while !self.is_at_end() && self.peek() != '"' {
            let next = self.advance();
            if next == '\n' {
                self.advance_line();
            }
        }
        let inner = self.src[start..self.idx].iter().collect::<String>();
        self.consume();
        Token::Str(inner)
    }
}

fn is_alphanumeric(ch: char) -> bool {
    ch == '_' || ch.is_ascii_alphanumeric()
}

fn is_digit(ch: char) -> bool {
    ch.is_ascii_digit()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::Token::*;

    fn assert_tokens(src: &str, expected: &[Token]) {
        let spans = Lexer::new(src).scan();
        assert_eq!(expected.len(), spans.len());
        for i in 0..expected.len() {
            assert_eq!(expected[i], spans[i].token);
        }
    }

    #[test]
    fn eof_token_when_empty_source() {
        let src = "";
        let spans = Lexer::new(src).scan();
        assert_eq!(1, spans.len());
        assert_eq!(EOF, spans[0].token);
    }

    #[test]
    fn eof_token_has_line_count() {
        let src = "";
        let spans = Lexer::new(src).scan();
        assert_eq!(1, spans.len());
        assert_eq!(1, spans[0].line);
    }

    #[test]
    fn eof_token_always_last() {
        let src = ";";
        let expected = [Semi, EOF];
        assert_tokens(src, &expected);
    }

    #[test]
    fn line_count_starts_at_one() {
        let src = ";";
        let spans = Lexer::new(src).scan();
        assert_eq!(2, spans.len());
        assert_eq!(Semi, spans[0].token);
        assert_eq!(1, spans[0].line);
    }

    #[test]
    fn line_count_increments() {
        let src = ";\n;\n;";
        let spans = Lexer::new(src).scan();
        assert_eq!(4, spans.len());
        assert_eq!(1, spans[0].line);
        assert_eq!(3, spans[3].line);
    }

    #[test]
    fn whitespace_is_ignored() {
        let src = " \t\r\n";
        let expected = [EOF];
        assert_tokens(src, &expected);
    }

    #[test]
    fn line_comment_is_ignored() {
        let src = "#\n\
                   ##\n\
                   #a\n\
                   #a#\n\
                   # abc\n\
                   # abc #\n\
                   ; #\n";
        let expected = [Semi, EOF];
        assert_tokens(src, &expected);
    }

    #[test]
    fn token_punctuations() {
        let src = "(){},;.";
        let expected = [Lparen, Rparen, Lbrace, Rbrace, Comma, Semi, Dot, EOF];
        assert_tokens(src, &expected);
    }

    #[test]
    fn token_operators() {
        let src = "+-*/% < <= > >= = == !=";
        let expected = [
            Plus, Minus, Star, Slash, Percent, Lt, LtEq, Gt, GtEq, Eq, EqEq, NotEq, EOF,
        ];
        assert_tokens(src, &expected);
    }

    #[test]
    fn token_keywords() {
        let src = "and or not \
                   if elif else \
                   for while break continue return \
                   let fun class \
                   self super";
        let expected = [
            And, Or, Not, If, Elif, Else, For, While, Break, Cont, Ret, Let, Fun, Class, Self_,
            Super, EOF,
        ];
        assert_tokens(src, &expected);
    }

    #[test]
    fn literal_keyword_none() {
        let src = "none";
        let expected = [None, EOF];
        assert_tokens(src, &expected);
    }

    #[test]
    fn literal_keyword_booleans() {
        let src = "true false";
        let expected = [True, False, EOF];
        assert_tokens(src, &expected);
    }

    #[test]
    fn literal_number_integers() {
        let src = "0 1 3 10 500";
        let expected = [Num(0.0), Num(1.0), Num(3.0), Num(10.0), Num(500.0), EOF];
        assert_tokens(src, &expected);
    }

    #[test]
    fn literal_number_decimals() {
        let src = "0.0 0.1 3.1415 10.01 500.001";
        let expected = [
            Num(0.0),
            Num(0.1),
            Num(3.1415),
            Num(10.01),
            Num(500.001),
            EOF,
        ];
        assert_tokens(src, &expected);
    }

    #[test]
    fn literal_strings() {
        let src = r#" "" "a" "abc" "#;
        let expected = [
            Str("".to_owned()),
            Str("a".to_owned()),
            Str("abc".to_owned()),
            EOF,
        ];
        assert_tokens(src, &expected);
    }

    #[test]
    fn literal_string_has_starting_line_count() {
        let src = " \"a\nb\" ";
        let spans = Lexer::new(src).scan();
        assert_eq!(2, spans.len());
        assert_eq!(Str("a\nb".to_owned()), spans[0].token);
        assert_eq!(1, spans[0].line);
    }

    #[test]
    fn literal_string_increments_line_count() {
        let src = " \"a\nb\" ";
        let spans = Lexer::new(src).scan();
        assert_eq!(2, spans.len());
        assert_eq!(EOF, spans[1].token);
        assert_eq!(2, spans[1].line);
    }

    #[test]
    fn literal_identifiers() {
        let src = "_ __ _a a_ _a_ a_b a ab A Ab AB";
        let expected = [
            Ident("_".to_owned()),
            Ident("__".to_owned()),
            Ident("_a".to_owned()),
            Ident("a_".to_owned()),
            Ident("_a_".to_owned()),
            Ident("a_b".to_owned()),
            Ident("a".to_owned()),
            Ident("ab".to_owned()),
            Ident("A".to_owned()),
            Ident("Ab".to_owned()),
            Ident("AB".to_owned()),
            EOF,
        ];
        assert_tokens(src, &expected);
    }
}
