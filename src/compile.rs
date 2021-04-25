use crate::scan::Scanner;
use crate::scan::Token;

pub struct Compiler;

impl Compiler {
    pub fn new() -> Self {
        Self
    }

    pub fn compile(&mut self, source: &str) {
        let mut scanner = Scanner::new(source);
        let mut line = 0;
        loop {
            let span = scanner.scan();
            if line == 0 || span.line != line {
                print!("{:4} ", span.line);
                line = span.line;
            } else {
                print!("   | ");
            }
            println!("{:?} '{}'", span.token, span.slice);
            if span.token == Token::EOF {
                break;
            }
        }
    }
}
