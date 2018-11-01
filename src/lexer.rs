use std::vec::Vec;

use token::Span;
use token::Token;

pub struct Lexer;

impl Lexer {
    pub fn new() -> Self {
        Lexer {}
    }

    pub fn scan(&self, src: &str) -> Vec<Span> {
        let mut spans: Vec<Span> = Vec::new();
        let chars = src.chars().collect::<Vec<char>>();
        let end = chars.len();

        let mut line = 1;
        let mut i = 0;
        while i < end {
            let curr = chars[i];
            let token = match curr {
                '(' => Token::Lparen,
                ')' => Token::Rparen,
                '{' => Token::Lbrace,
                '}' => Token::Rbrace,
                ',' => Token::Comma,
                ';' => Token::Semi,
                '.' => Token::Dot,

                ' ' | '\t' | '\r' => {
                    i += 1;
                    continue;
                }
                '\n' => {
                    line += 1;
                    i += 1;
                    continue;
                }

                '#' => {
                    let mut j = i + 1;
                    while j < end {
                        let next = chars[j];
                        if next == '\n' {
                            line += 1;
                            break;
                        } else {
                            j += 1;
                            continue;
                        }
                    }
                    i = j + 1;
                    continue;
                }

                '+' => Token::Add,
                '-' => Token::Sub,
                '*' => Token::Mul,
                '/' => Token::Div,
                '%' => Token::Rem,

                '<' => {
                    let j = i + 1;
                    if j < end && chars[j] == '=' {
                        i = j;
                        Token::LtEq
                    } else {
                        Token::Lt
                    }
                }

                '>' => {
                    let j = i + 1;
                    if j < end && chars[j] == '=' {
                        i = j;
                        Token::GtEq
                    } else {
                        Token::Gt
                    }
                }

                '=' => {
                    let j = i + 1;
                    if j < end && chars[j] == '=' {
                        i = j;
                        Token::EqEq
                    } else {
                        Token::Eq_
                    }
                }

                '!' => {
                    let j = i + 1;
                    if j < end && chars[j] == '=' {
                        i = j;
                        Token::NotEq
                    } else {
                        i = j;
                        continue;
                    }
                }

                'a'...'z' | 'A'...'Z' | '_' => {
                    let mut j = i + 1;
                    while j < end && (chars[j] == '_' || chars[j].is_ascii_alphanumeric()) {
                        j += 1;
                    }
                    let name = chars[i..j].iter().collect::<String>();
                    i = j - 1;
                    Token::to_keyword(&name).unwrap_or(Token::Ident(name))
                }

                '0'...'9' => {
                    let mut j = i + 1;
                    while j < end && (chars[j] == '.' || chars[j].is_ascii_digit()) {
                        j += 1;
                    }
                    let rep = chars[i..j].iter().collect::<String>();
                    let num = rep.parse::<f64>().unwrap();
                    i = j - 1;
                    Token::Num(num)
                }

                '"' => {
                    let mut j = i + 1;
                    while j < end && chars[j] != '"' {
                        j += 1;
                    }
                    let inner = chars[i + 1..j].iter().collect::<String>();
                    i = j;
                    Token::Str(inner)
                }

                _ => Token::EOF,
            };
            spans.push(Span::new(token, line));
            i += 1;
        }

        spans.push(Span::new(Token::EOF, line));
        spans
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use token::Token::*;

    fn assert_tokens(src: &str, expected: &[Token]) {
        let lexer = Lexer::new();
        let spans = lexer.scan(src);
        assert_eq!(expected.len(), spans.len());
        for i in 0..expected.len() {
            assert_eq!(expected[i], spans[i].token);
        }
    }

    #[test]
    fn empty_source_has_eof_token() {
        let src = "";
        let expected = [EOF];
        assert_tokens(src, &expected);
    }

    #[test]
    fn lines_start_at_one() {
        let src = "";
        let lexer = Lexer::new();
        let spans = lexer.scan(src);
        assert_eq!(1, spans.len());
        assert_eq!(1, spans[0].line);
    }

    #[test]
    fn ignores_whitespace() {
        let src = " \t\r\n";
        let expected = [EOF];
        assert_tokens(src, &expected);
    }

    #[test]
    fn ignores_hash_line_comments() {
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
    fn punctuation_tokens() {
        let src = "(){},;.";
        let expected = [Lparen, Rparen, Lbrace, Rbrace, Comma, Semi, Dot, EOF];
        assert_tokens(src, &expected);
    }

    #[test]
    fn operator_tokens() {
        let src = "+-*/% < <= > >= = == !=";
        let expected = [
            Add, Sub, Mul, Div, Rem, Lt, LtEq, Gt, GtEq, Eq_, EqEq, NotEq, EOF,
        ];
        assert_tokens(src, &expected);
    }

    #[test]
    fn keyword_tokens() {
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
    fn literal_keyword_tokens() {
        let src = "none true false";
        let expected = [None_, True, False, EOF];
        assert_tokens(src, &expected);
    }

    #[test]
    fn literal_number_tokens() {
        let src = "0 1 3 5.0 10.01";
        let expected = [Num(0.0), Num(1.0), Num(3.0), Num(5.0), Num(10.01), EOF];
        assert_tokens(src, &expected);
    }

    #[test]
    fn literal_string_tokens() {
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
    fn literal_identifier_tokens() {
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
