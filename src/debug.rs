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
        Constant => constant_instruction("OP_CONSTANT", chunk, offset),
        ConstantLong => constant_instruction("OP_CONSTANT_LONG", chunk, offset),
        DefineGlobal => constant_instruction("OP_DEFINE_GLOBAL", chunk, offset),
        GetGlobal => constant_instruction("OP_GET_GLOBAL", chunk, offset),
        SetGlobal => constant_instruction("OP_SET_GLOBAL", chunk, offset),
        GetLocal => byte_instruction("OP_GET_LOCAL", chunk, offset),
        SetLocal => byte_instruction("OP_SET_LOCAL", chunk, offset),
        GetUpvalue => byte_instruction("OP_GET_UPVALUE", chunk, offset),
        SetUpvalue => byte_instruction("OP_SET_UPVALUE", chunk, offset),
        GetProperty => constant_instruction("OP_GET_PROPERTY", chunk, offset),
        SetProperty => constant_instruction("OP_SET_PROPERTY", chunk, offset),
        None => simple_instruction("OP_NONE", offset),
        True => simple_instruction("OP_TRUE", offset),
        False => simple_instruction("OP_FALSE", offset),
        Add => simple_instruction("OP_ADD", offset),
        Subtract => simple_instruction("OP_SUBTRACT", offset),
        Multiply => simple_instruction("OP_MULTIPLY", offset),
        Divide => simple_instruction("OP_DIVIDE", offset),
        Modulo => simple_instruction("OP_MODULO", offset),
        Negate => simple_instruction("OP_NEGATE", offset),
        Not => simple_instruction("OP_NOT", offset),
        Equal => simple_instruction("OP_EQUAL", offset),
        NotEq => simple_instruction("OP_NOT_EQUAL", offset),
        Lt => simple_instruction("OP_LESS", offset),
        LtEq => simple_instruction("OP_LESS_EQUAL", offset),
        Gt => simple_instruction("OP_GREATER", offset),
        GtEq => simple_instruction("OP_GREATER_EQUAL", offset),
        Pop => simple_instruction("OP_POP", offset),
        Loop => jump_instruction("OP_LOOP", false, chunk, offset),
        Jump => jump_instruction("OP_JUMP", true, chunk, offset),
        JumpIfFalse => jump_instruction("OP_JUMP_IF_FALSE", true, chunk, offset),
        Call => byte_instruction("OP_CALL", chunk, offset),
        Closure => closure_instruction("OP_CLOSURE", chunk, offset),
        CloseUpvalue => simple_instruction("OP_CLOSE_UPVALUE", offset),
        Class => constant_instruction("OP_CLASS", chunk, offset),
        Method => constant_instruction("OP_METHOD", chunk, offset),
        Invoke => invoke_instruction("OP_INVOKE", chunk, offset),
        Inherit => simple_instruction("OP_INHERIT", offset),
        SuperGet => constant_instruction("OP_SUPER_GET", chunk, offset),
        SuperInvoke => invoke_instruction("OP_SUPER_INVOKE", chunk, offset),
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

fn byte_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let slot = chunk.code(offset + 1);
    println!("{:<16} {:4}", name, slot);
    return offset + 2;
}

fn jump_instruction(name: &str, positive: bool, chunk: &Chunk, offset: usize) -> usize {
    let byte1 = chunk.code(offset + 1) as u16;
    let byte2 = chunk.code(offset + 2) as u16;
    let jump = ((byte2 << 8) | byte1) as isize;
    let jump = if positive { jump } else { jump * -1 };
    let dest = (offset as isize) + 3 + jump;
    println!("{:<16} {:4} -> {}", name, offset, dest);
    return offset + 3;
}

fn closure_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let mut next = offset + 1;
    let idx = chunk.code(next) as usize;
    print!("{:<16} {:4} ", name, idx);
    chunk.constant(idx).print();
    println!();
    next += 1;

    let fn_obj = chunk.constant(idx).clone().into_fn();
    for _ in 0..fn_obj.num_upvalues {
        let is_local = chunk.code(next) == 1_u8;
        let index = chunk.code(next + 1);
        let kind = if is_local { "local" } else { "upvalue" };
        println!("{:04}    |                     {} {}", offset, kind, index);
        next += 2;
    }

    next
}

fn invoke_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let idx = chunk.code(offset + 1) as usize;
    let arg_count = chunk.code(offset + 2) as usize;
    print!("{:<16}    ({} args) {:4} '", name, arg_count, idx);
    chunk.constant(idx).print();
    println!("'");
    offset + 3
}

fn constant_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let (index, count) = if name == "OP_CONSTANT_LONG" {
        let byte1 = chunk.code(offset + 1) as usize;
        let byte2 = chunk.code(offset + 2) as usize;
        let byte3 = chunk.code(offset + 3) as usize;
        let idx = (byte3 << 16) | (byte2 << 8) | byte1;
        (idx as usize, 4)
    } else {
        (chunk.code(offset + 1) as usize, 2)
    };
    print!("{:<16} {:4} '", name, index);
    chunk.constant(index).print();
    println!("'");
    return offset + count;
}
