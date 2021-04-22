use std::convert::TryFrom;

use crate::value::Value;

#[repr(u8)]
pub enum OpCode {
    Constant,
    Return,
}

impl TryFrom<u8> for OpCode {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        use OpCode::*;
        Ok(match value {
            x if x == Constant as u8 => Constant,
            x if x == Return as u8 => Return,
            _ => return Err(()),
        })
    }
}

pub struct Chunk {
    code: Vec<u8>,
    lines: Vec<usize>,
    constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            lines: Vec::new(),
            constants: Vec::new(),
        }
    }

    pub fn code(&self) -> &[u8] {
        &self.code[..]
    }

    pub fn lines(&self) -> &[usize] {
        &self.lines[..]
    }

    pub fn constants(&self) -> &[Value] {
        &self.constants[..]
    }

    pub fn write(&mut self, opcode: OpCode, line: usize) {
        self.code.push(opcode as u8);
        self.lines.push(line);
    }

    pub fn write_byte(&mut self, byte: u8, line: usize) {
        self.code.push(byte);
        self.lines.push(line);
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        let index = self.constants.len();
        self.constants.push(value);
        return index;
    }
}
