use std::env;
use std::process;

use loxscript::bytecode::Chunk;
use loxscript::bytecode::OpCode;
use loxscript::debug;
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

    let mut chunk = Chunk::new();
    let mut line = 0;
    for i in 0..300 {
        let index = chunk.add_constant(i as f64 + 0.2);
        line += if index % 10 == 0 { 1 } else { 0 };
        chunk.write_load(index, line);
    }
    chunk.write_byte(OpCode::Return as u8 + 3, 123);

    let const1 = chunk.add_constant(1.2);
    let const2 = chunk.add_constant(3.4);
    let const3 = chunk.add_constant(5.6);
    chunk.write_load(const1, 123);
    chunk.write_load(const2, 123);
    chunk.write(OpCode::Add, 123);
    chunk.write_load(const3, 123);
    chunk.write(OpCode::Divide, 123);
    chunk.write(OpCode::Negate, 123);
    chunk.write(OpCode::Return, 123);

    debug::disassemble(&chunk, "test chunk");
    println!();

    let mut vm = rt::VM::new();
    let status = match vm.interpret(chunk) {
        Res::Ok => EX_OK,
        Res::CompileErr => EX_DATAERR,
        Res::RuntimeErr => EX_SOFTWARE,
    };

    process::exit(status);
}
