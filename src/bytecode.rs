use std::convert::TryFrom;

use crate::value::Value;

#[repr(u8)]
#[derive(Debug, PartialEq, Eq)]
pub enum OpCode {
    Constant,
    ConstantLong,
    Return,
}

impl TryFrom<u8> for OpCode {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        use OpCode::*;
        Ok(match value {
            x if x == Constant as u8 => Constant,
            x if x == ConstantLong as u8 => ConstantLong,
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

    pub fn write_index(&mut self, index: usize, line: usize) {
        if index <= u8::MAX as usize {
            self.write(OpCode::Constant, line);
            self.write_byte(index as u8, line);
        } else {
            let byte1 = (index & 0xFF) as u8;
            let byte2 = ((index >> 8) & 0xFF) as u8;
            let byte3 = ((index >> 16) & 0xFF) as u8;
            self.write(OpCode::ConstantLong, line);
            self.write_byte(byte1, line);
            self.write_byte(byte2, line);
            self.write_byte(byte3, line);
        }
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        let index = self.constants.len();
        self.constants.push(value);
        return index;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn opcode_try_from() {
        let opcode = OpCode::try_from(OpCode::Return as u8);
        assert!(opcode.is_ok());
        assert_eq!(OpCode::Return, opcode.unwrap());
    }

    #[test]
    fn opcode_try_from_out_of_range() {
        let opcode = OpCode::try_from(255);
        assert!(opcode.is_err());
    }

    #[test]
    fn chunk_write_index() {
        let mut chunk = Chunk::new();
        assert_eq!(0, chunk.code().len());
        chunk.write_index(u8::MAX as usize, 123);
        assert_eq!(2, chunk.code().len());
        assert_eq!(OpCode::Constant as u8, chunk.code()[0]);
    }

    #[test]
    fn chunk_write_index_long() {
        let mut chunk = Chunk::new();
        assert_eq!(0, chunk.code().len());
        chunk.write_index(u8::MAX as usize + 1, 123);
        assert_eq!(4, chunk.code().len());
        assert_eq!(OpCode::ConstantLong as u8, chunk.code()[0]);
    }

    #[test]
    fn chunk_add_constant() {
        let mut chunk = Chunk::new();
        assert_eq!(0, chunk.constants().len());
        let index = chunk.add_constant(1.2);
        assert_eq!(1, chunk.constants().len());
        assert_eq!(0, index);
    }
}
