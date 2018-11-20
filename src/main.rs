extern crate loxscript;

use std::io;
use std::io::Write;

use loxscript::interpreter::Interpreter;
use loxscript::lexer::Lexer;
use loxscript::parser::Parser;

fn main() {
    repl();
}

fn repl() {
    let mut interpreter = Interpreter::new();
    let mut line = String::new();

    loop {
        print!("> ");
        io::stdout().flush().expect("error flushing print");

        line.clear();
        io::stdin()
            .read_line(&mut line)
            .expect("error reading line");

        if line.is_empty() {
            continue;
        }

        let lexer = Lexer::new();
        let mut parser = Parser::new(lexer.scan(&line));
        let program = parser.parse();
        interpreter.run(&program);
    }
}
