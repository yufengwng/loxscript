use std::convert::TryFrom;

use crate::bytecode::Chunk;
use crate::bytecode::OpCode;
use crate::value;

pub fn disassemble(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);
    let mut offset = 0;
    while offset < chunk.code().len() {
        offset = disassemble_at(chunk, offset);
    }
}

pub fn disassemble_at(chunk: &Chunk, offset: usize) -> usize {
    use OpCode::*;
    print!("{:04} ", offset);

    let lines = chunk.lines();
    if offset > 0 && lines[offset] == lines[offset - 1] {
        print!("   | ");
    } else {
        print!("{:4} ", lines[offset]);
    }

    let byte = chunk.code()[offset];
    let opcode = match OpCode::try_from(byte) {
        Err(_) => return unknown_instruction(byte, offset),
        Ok(op) => op,
    };

    return match opcode {
        Constant => constant_instruction("OP_CONSTANT", chunk, offset),
        Return => simple_instruction("OP_RETURN", offset),
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

fn constant_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let idx = chunk.code()[offset + 1] as usize;
    print!("{:<16} {:4} '", name, idx);
    value::print(&chunk.constants()[idx]);
    println!("'");
    return offset + 2;
}
