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
        Constant => constant_instruction(opcode, chunk, offset),
        ConstantLong => constant_instruction(opcode, chunk, offset),
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

fn constant_instruction(opcode: OpCode, chunk: &Chunk, offset: usize) -> usize {
    let code = chunk.code();
    let (name, index, count) = if opcode == OpCode::Constant {
        ("OP_CONSTANT", code[offset + 1] as usize, 2)
    } else {
        let byte1 = code[offset + 1] as usize;
        let byte2 = code[offset + 2] as usize;
        let byte3 = code[offset + 3] as usize;
        let idx = (byte3 << 16) | (byte2 << 8) | byte1;
        ("OP_CONSTANT_LONG", idx as usize, 4)
    };
    print!("{:<16} {:4} '", name, index);
    value::print(&chunk.constants()[index]);
    println!("'");
    return offset + count;
}
