use std::convert::TryFrom;

use crate::bytecode::Chunk;
use crate::bytecode::OpCode;

pub fn disassemble(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);
    let mut offset = 0;
    while offset < chunk.code_len() {
        offset = disassemble_at(chunk, offset);
    }
}

pub fn disassemble_at(chunk: &Chunk, offset: usize) -> usize {
    use OpCode::*;
    print!("{:04} ", offset);

    if offset > 0 && chunk.line(offset) == chunk.line(offset - 1) {
        print!("   | ");
    } else {
        print!("{:4} ", chunk.line(offset));
    }

    let byte = chunk.code(offset);
    let opcode = match OpCode::try_from(byte) {
        Ok(op) => op,
        Err(_) => return unknown_instruction(byte, offset),
    };

    return match opcode {
        Constant => constant_instruction(opcode, chunk, offset),
        ConstantLong => constant_instruction(opcode, chunk, offset),
        None => simple_instruction("OP_NONE", offset),
        True => simple_instruction("OP_TRUE", offset),
        False => simple_instruction("OP_FALSE", offset),
        Add => simple_instruction("OP_ADD", offset),
        Subtract => simple_instruction("OP_SUBTRACT", offset),
        Multiply => simple_instruction("OP_MULTIPLY", offset),
        Divide => simple_instruction("OP_DIVIDE", offset),
        Modulo => simple_instruction("OP_MODULO", offset),
        Negate => simple_instruction("OP_NEGATE", offset),
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
    let (name, index, count) = if opcode == OpCode::Constant {
        ("OP_CONSTANT", chunk.code(offset + 1) as usize, 2)
    } else {
        let byte1 = chunk.code(offset + 1) as usize;
        let byte2 = chunk.code(offset + 2) as usize;
        let byte3 = chunk.code(offset + 3) as usize;
        let idx = (byte3 << 16) | (byte2 << 8) | byte1;
        ("OP_CONSTANT_LONG", idx as usize, 4)
    };
    print!("{:<16} {:4} '", name, index);
    chunk.constant(index).print();
    println!("'");
    return offset + count;
}
