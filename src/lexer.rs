use std::vec::Vec;

use token::{Token, Kind};

struct Lexer {
}

impl Lexer {
    fn new() -> Lexer {
        Lexer {}
    }
}

impl Lexer {
    fn scan(&self, src: &str) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();
        let chars = src.chars().collect::<Vec<char>>();
        let end = chars.len();

        let mut line = 1;
        let mut i = 0;
        while i < end {
            let curr = chars[i];
            let kind = match curr {
                '(' => Kind::Lparen,
                ')' => Kind::Rparen,
                '{' => Kind::Lbrace,
                '}' => Kind::Rbrace,
                ',' => Kind::Comma,
                ';' => Kind::Semi,
                '.' => Kind::Dot,

                ' ' | '\t' | '\r' => {
                    i += 1;
                    continue;
                },
                '\n' => {
                    line += 1;
                    i += 1;
                    continue;
                },

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
                },

                '+' => Kind::Add,
                '-' => Kind::Sub,
                '*' => Kind::Mul,
                '/' => Kind::Div,
                '%' => Kind::Rem,

                '<' => {
                    let j = i + 1;
                    if j < end && chars[j] == '=' {
                        i = j;
                        Kind::LtEq
                    } else {
                        Kind::Lt
                    }
                },

                '>' => {
                    let j = i + 1;
                    if j < end && chars[j] == '=' {
                        i = j;
                        Kind::GtEq
                    } else {
                        Kind::Gt
                    }
                },

                '=' => {
                    let j = i + 1;
                    if j < end && chars[j] == '=' {
                        i = j;
                        Kind::EqEq
                    } else {
                        Kind::Eq
                    }
                },

                '!' => {
                    let j = i + 1;
                    if j < end && chars[j] == '=' {
                        i = j;
                        Kind::NotEq
                    } else {
                        i = j;
                        continue;
                    }
                },

                'a' ... 'z' | 'A' ... 'Z' | '_' => {
                    let mut j = i + 1;
                    while j < end && (chars[j] == '_' || chars[j].is_ascii_alphanumeric()) {
                        j += 1;
                    }
                    let name = chars[i..j].iter().collect::<String>();
                    i = j - 1;
                    Kind::to_keyword(&name).unwrap_or(Kind::Ident(name))
                },

                '0' ... '9' => {
                    let mut j = i + 1;
                    while j < end && (chars[j] == '.' || chars[j].is_ascii_digit()) {
                        j += 1;
                    }
                    let rep = chars[i..j].iter().collect::<String>();
                    let num = rep.parse::<f64>().unwrap();
                    i = j - 1;
                    Kind::Num(num)
                },

                '"' => {
                    let mut j = i + 1;
                    while j < end && chars[j] != '"' {
                        j += 1;
                    }
                    let inner = chars[i + 1 .. j].iter().collect::<String>();
                    i = j;
                    Kind::Str(inner)
                },

                _ => Kind::EOF,
            };
            tokens.push(Token::new(kind, line));
            i += 1;
        }

        tokens.push(Token::new(Kind::EOF, line));
        tokens
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_source_has_eof_token() {
        let src = "";
        let lexer = Lexer::new();
        let tokens = lexer.scan(src);
        assert_eq!(1, tokens.len());
        assert_eq!(Kind::EOF, tokens[0].kind);
    }

    #[test]
    fn lines_start_at_one() {
        let src = "";
        let lexer = Lexer::new();
        let tokens = lexer.scan(src);
        assert_eq!(1, tokens.len());
        assert_eq!(1, tokens[0].line);
    }

    #[test]
    fn ignores_whitespace() {
        let src = " \t\r\n";
        let lexer = Lexer::new();
        let tokens = lexer.scan(src);
        assert_eq!(1, tokens.len());
        assert_eq!(Kind::EOF, tokens[0].kind);
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
        let lexer = Lexer::new();
        let tokens = lexer.scan(src);
        assert_eq!(2, tokens.len());
        assert_eq!(Kind::Semi, tokens[0].kind);
        assert_eq!(Kind::EOF, tokens[1].kind);
    }

    #[test]
    fn punctuation_tokens() {
        let src = "(){},;.";
        let lexer = Lexer::new();
        let tokens = lexer.scan(src);
        assert_eq!(8, tokens.len());
        assert_eq!(Kind::Lparen, tokens[0].kind);
        assert_eq!(Kind::Rparen, tokens[1].kind);
        assert_eq!(Kind::Lbrace, tokens[2].kind);
        assert_eq!(Kind::Rbrace, tokens[3].kind);
        assert_eq!(Kind::Comma, tokens[4].kind);
        assert_eq!(Kind::Semi, tokens[5].kind);
        assert_eq!(Kind::Dot, tokens[6].kind);
        assert_eq!(Kind::EOF, tokens[7].kind);
    }

    #[test]
    fn operator_tokens() {
        let src = "+-*/% < <= > >= = == !=";
        let lexer = Lexer::new();
        let tokens = lexer.scan(src);
        assert_eq!(13, tokens.len());
        assert_eq!(Kind::Add, tokens[0].kind);
        assert_eq!(Kind::Sub, tokens[1].kind);
        assert_eq!(Kind::Mul, tokens[2].kind);
        assert_eq!(Kind::Div, tokens[3].kind);
        assert_eq!(Kind::Rem, tokens[4].kind);
        assert_eq!(Kind::Lt, tokens[5].kind);
        assert_eq!(Kind::LtEq, tokens[6].kind);
        assert_eq!(Kind::Gt, tokens[7].kind);
        assert_eq!(Kind::GtEq, tokens[8].kind);
        assert_eq!(Kind::Eq, tokens[9].kind);
        assert_eq!(Kind::EqEq, tokens[10].kind);
        assert_eq!(Kind::NotEq, tokens[11].kind);
        assert_eq!(Kind::EOF, tokens[12].kind);
    }

    #[test]
    fn keyword_tokens() {
        let src = "and or not \
                   if elif else for while break continue \
                   fun let return \
                   class self super";
        let lexer = Lexer::new();
        let tokens = lexer.scan(src);
        assert_eq!(17, tokens.len());
        assert_eq!(Kind::And, tokens[0].kind);
        assert_eq!(Kind::Or, tokens[1].kind);
        assert_eq!(Kind::Not, tokens[2].kind);
        assert_eq!(Kind::If, tokens[3].kind);
        assert_eq!(Kind::Elif, tokens[4].kind);
        assert_eq!(Kind::Else, tokens[5].kind);
        assert_eq!(Kind::For, tokens[6].kind);
        assert_eq!(Kind::While, tokens[7].kind);
        assert_eq!(Kind::Break, tokens[8].kind);
        assert_eq!(Kind::Cont, tokens[9].kind);
        assert_eq!(Kind::Fun, tokens[10].kind);
        assert_eq!(Kind::Let, tokens[11].kind);
        assert_eq!(Kind::Ret, tokens[12].kind);
        assert_eq!(Kind::Class, tokens[13].kind);
        assert_eq!(Kind::Selfie, tokens[14].kind);
        assert_eq!(Kind::Super, tokens[15].kind);
        assert_eq!(Kind::EOF, tokens[16].kind);
    }

    #[test]
    fn literal_tokens() {
        let src = r#"none true false
                     0 1 3 5.0 10.01
                     "" "a" "abc"
                     a _a ab a_b"#;
        let lexer = Lexer::new();
        let tokens = lexer.scan(src);
        assert_eq!(16, tokens.len());
        assert_eq!(Kind::None, tokens[0].kind);
        assert_eq!(Kind::True, tokens[1].kind);
        assert_eq!(Kind::False, tokens[2].kind);
        assert_eq!(Kind::Num(0.), tokens[3].kind);
        assert_eq!(Kind::Num(1.), tokens[4].kind);
        assert_eq!(Kind::Num(3.), tokens[5].kind);
        assert_eq!(Kind::Num(5.0), tokens[6].kind);
        assert_eq!(Kind::Num(10.01), tokens[7].kind);
        assert_eq!(Kind::Str("".to_owned()), tokens[8].kind);
        assert_eq!(Kind::Str("a".to_owned()), tokens[9].kind);
        assert_eq!(Kind::Str("abc".to_owned()), tokens[10].kind);
        assert_eq!(Kind::Ident("a".to_owned()), tokens[11].kind);
        assert_eq!(Kind::Ident("_a".to_owned()), tokens[12].kind);
        assert_eq!(Kind::Ident("ab".to_owned()), tokens[13].kind);
        assert_eq!(Kind::Ident("a_b".to_owned()), tokens[14].kind);
        assert_eq!(Kind::EOF, tokens[15].kind);
    }
}
