#[repr(u8)]
pub enum OpCode {
    Return,
}

pub struct Chunk {
    code: Vec<u8>,
}

impl Chunk {
    pub fn new() -> Self {
        Self { code: Vec::new() }
    }

    pub fn write(&mut self, byte: u8) {
        self.code.push(byte);
    }

    pub fn disassemble(&self, name: &str) {
        println!("== {} ==", name);
        let mut offset = 0;
        while offset < self.code.len() {
            offset = self.disassemble_instruction(offset);
        }
    }

    pub fn disassemble_instruction(&self, offset: usize) -> usize {
        print!("{:04} ", offset);
        let instruction = self.code[offset];
        return match instruction {
            x if x == OpCode::Return as u8 => simple_instruction("OP_RETURN", offset),
            _ => unknown_instruction(instruction, offset),
        };
    }
}

fn unknown_instruction(byte: u8, offset: usize) -> usize {
    println!("unknown opcode: {}", byte);
    return offset + 1;
}

fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{}", name);
    return offset + 1;
}
