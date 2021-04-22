use crate::bytecode::Chunk;
use crate::bytecode::OpCode;

pub fn disassemble(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);
    let mut offset = 0;
    while offset < chunk.code().len() {
        offset = disassemble_at(chunk, offset);
    }
}

pub fn disassemble_at(chunk: &Chunk, offset: usize) -> usize {
    print!("{:04} ", offset);
    let instruction = chunk.code()[offset];
    return match instruction {
        x if x == OpCode::Return as u8 => simple_instruction("OP_RETURN", offset),
        _ => unknown_instruction(instruction, offset),
    };
}

fn unknown_instruction(byte: u8, offset: usize) -> usize {
    println!("unknown opcode: {}", byte);
    return offset + 1;
}

fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{}", name);
    return offset + 1;
}
