use std::io;
use std::io::Write;

use loxscript::interpreter::Interpreter;
use loxscript::lexer::Lexer;
use loxscript::parser::Parser;

// todo: impl run script file and return error codes
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
            println!();
            continue;
        }

        let lexer = Lexer::new(&line);
        let scan_report = lexer.scan();

        let parser = Parser::new(scan_report.spans);
        let parse_report = parser.parse();

        if scan_report.had_error || parse_report.had_error {
            continue;
        }

        interpreter.run(&parse_report.decls);
    }
}
