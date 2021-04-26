use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::process;

use loxscript::runtime as rt;
use loxscript::runtime::InterpretResult as Res;

static NAME: &str = "loxscript";

const EX_OK: i32 = 0;
const EX_USAGE: i32 = 64;
const EX_DATAERR: i32 = 65;
const EX_SOFTWARE: i32 = 70;

fn main() {
    let args = env::args().collect::<Vec<String>>();
    if args.len() > 2 {
        eprintln!("Usage: {} [script]", NAME);
        process::exit(EX_USAGE);
    }

    let mut vm = rt::VM::new();
    let status = if args.len() == 2 {
        run_file(&mut vm, &args[1])
    } else {
        run_repl(&mut vm)
    };

    process::exit(status);
}

fn run_file(vm: &mut rt::VM, path: &str) -> i32 {
    let source = match fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("[lox] error while loading script '{}': {}", path, e);
            return EX_SOFTWARE;
        }
    };
    return match vm.interpret(source) {
        Res::Ok => EX_OK,
        Res::CompileErr => EX_DATAERR,
        Res::RuntimeErr => EX_SOFTWARE,
    };
}

fn run_repl(vm: &mut rt::VM) -> i32 {
    loop {
        print!("> ");
        if let Err(e) = io::stdout().flush() {
            eprintln!("[lox] error while flushing prompt output: {}", e);
            return EX_SOFTWARE;
        }

        let mut line = String::new();
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

        match vm.interpret(line) {
            Res::Ok => EX_OK,
            Res::CompileErr => EX_DATAERR,
            Res::RuntimeErr => EX_SOFTWARE,
        };
    }
    EX_OK
}
