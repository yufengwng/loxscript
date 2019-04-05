use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::process;

use loxscript::interpreter::Interpreter;
use loxscript::lexer::Lexer;
use loxscript::parser::{IdGenerator, Parser};
use loxscript::resolver::Resolver;

static NAME: &str = "loxscript";

const EX_OK: i32 = 0;
const EX_USAGE: i32 = 64;
const EX_DATAERR: i32 = 65;
const EX_SOFTWARE: i32 = 70;

fn main() {
    let mut interpreter = Interpreter::new();
    let mut generator = IdGenerator::new();

    let args: Vec<String> = env::args().collect();
    if args.len() > 2 {
        eprintln!("Usage: {} [script]", NAME);
        process::exit(EX_USAGE);
    }

    let status = if args.len() == 2 {
        run_file(&mut interpreter, &mut generator, &args[1])
    } else {
        run_repl(&mut interpreter, &mut generator)
    };

    process::exit(status);
}

fn run_file(interpreter: &mut Interpreter, generator: &mut IdGenerator, path: &str) -> i32 {
    let source = match fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("[lox] error while loading script '{}': {}", path, e);
            return EX_SOFTWARE;
        }
    };

    let (parse_err, runtime_err) = run(interpreter, generator, &source);

    if parse_err {
        EX_DATAERR
    } else if runtime_err {
        EX_SOFTWARE
    } else {
        EX_OK
    }
}

fn run_repl(interpreter: &mut Interpreter, generator: &mut IdGenerator) -> i32 {
    let mut line = String::new();

    loop {
        print!("> ");
        if let Err(e) = io::stdout().flush() {
            eprintln!("[lox] error while flushing prompt output: {}", e);
            return EX_SOFTWARE;
        }

        line.clear();
        match io::stdin().read_line(&mut line) {
            Ok(n) => {
                if n == 0 {
                    break;
                }
            }
            Err(e) => {
                eprintln!("[lox] error while reading input: {}", e);
                return EX_SOFTWARE;
            }
        }

        if line.is_empty() {
            println!();
            continue;
        }

        run(interpreter, generator, &line);
    }

    EX_OK
}

fn run(interpreter: &mut Interpreter, generator: &mut IdGenerator, src: &str) -> (bool, bool) {
    let lexer = Lexer::new(src);
    let scanned = lexer.scan();

    let parser = Parser::new(generator, scanned.spans);
    let parsed = parser.parse();

    let parse_err = scanned.had_error || parsed.had_error;
    if parse_err {
        return (parse_err, false);
    }

    let resolver = Resolver::new();
    let resolved = resolver.resolve(parsed.decls);
    if resolved.had_error {
        return (true, false);
    }

    let runtime_err = interpreter.run(resolved);
    (parse_err, runtime_err)
}
