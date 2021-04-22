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
    chunk.write(OpCode::Return as u8);
    chunk.write(OpCode::Return as u8 + 3);
    debug::disassemble(&chunk, "test chunk");

    process::exit(status);
}
