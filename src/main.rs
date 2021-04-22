use std::env;
use std::process;

use loxscript::chunk::Chunk;
use loxscript::chunk::OpCode;

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
    chunk.disassemble("test chunk");

    process::exit(status);
}
