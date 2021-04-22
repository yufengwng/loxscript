use std::env;
use std::process;

use loxscript::bytecode::Chunk;
use loxscript::bytecode::OpCode;
use loxscript::debug;

static NAME: &str = "loxscript";

// const EX_OK: i32 = 0;
const EX_USAGE: i32 = 64;
// const EX_DATAERR: i32 = 65;
// const EX_SOFTWARE: i32 = 70;

fn main() {
    let args = env::args().collect::<Vec<String>>();
    if args.len() > 2 {
        eprintln!("Usage: {} [script]", NAME);
        process::exit(EX_USAGE);
    }

    let status = 0;
    let mut chunk = Chunk::new();
    let mut line = 0;
    for i in 0..300 {
        let index = chunk.add_constant(i as f64 + 0.2);
        line += if index % 10 == 0 { 1 } else { 0 };
        chunk.write_index(index, line);
    }
    chunk.write(OpCode::Return, 123);
    chunk.write_byte(OpCode::Return as u8 + 3, 123);
    debug::disassemble(&chunk, "test chunk");

    process::exit(status);
}
