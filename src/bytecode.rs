use std::convert::TryFrom;

use crate::value::Value;

#[repr(u8)]
#[derive(Debug, PartialEq, Eq)]
pub enum OpCode {
    Constant,
    ConstantLong,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
    GetUpvalue,
    SetUpvalue,
    GetProperty,
    SetProperty,
    None,
    True,
    False,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Negate,
    Not,
    Equal,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    Pop,
    Loop,
    Jump,
    JumpIfFalse,
    Call,
    Closure,
    CloseUpvalue,
    Class,
    Method,
    Invoke,
    Inherit,
    SuperGet,
    SuperInvoke,
    Return,
}

impl TryFrom<u8> for OpCode {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        use OpCode::*;
        Ok(match value {
            b if b == Constant as u8 => Constant,
            b if b == ConstantLong as u8 => ConstantLong,
            b if b == DefineGlobal as u8 => DefineGlobal,
            b if b == GetGlobal as u8 => GetGlobal,
            b if b == SetGlobal as u8 => SetGlobal,
            b if b == GetLocal as u8 => GetLocal,
            b if b == SetLocal as u8 => SetLocal,
            b if b == GetUpvalue as u8 => GetUpvalue,
            b if b == SetUpvalue as u8 => SetUpvalue,
            b if b == GetProperty as u8 => GetProperty,
            b if b == SetProperty as u8 => SetProperty,
            b if b == None as u8 => None,
            b if b == True as u8 => True,
            b if b == False as u8 => False,
            b if b == Add as u8 => Add,
            b if b == Subtract as u8 => Subtract,
            b if b == Multiply as u8 => Multiply,
            b if b == Divide as u8 => Divide,
            b if b == Modulo as u8 => Modulo,
            b if b == Negate as u8 => Negate,
            b if b == Not as u8 => Not,
            b if b == Equal as u8 => Equal,
            b if b == NotEq as u8 => NotEq,
            b if b == Lt as u8 => Lt,
            b if b == LtEq as u8 => LtEq,
            b if b == Gt as u8 => Gt,
            b if b == GtEq as u8 => GtEq,
            b if b == Pop as u8 => Pop,
            b if b == Loop as u8 => Loop,
            b if b == Jump as u8 => Jump,
            b if b == JumpIfFalse as u8 => JumpIfFalse,
            b if b == Call as u8 => Call,
            b if b == Closure as u8 => Closure,
            b if b == CloseUpvalue as u8 => CloseUpvalue,
            b if b == Class as u8 => Class,
            b if b == Method as u8 => Method,
            b if b == Invoke as u8 => Invoke,
            b if b == Inherit as u8 => Inherit,
            b if b == SuperGet as u8 => SuperGet,
            b if b == SuperInvoke as u8 => SuperInvoke,
            b if b == Return as u8 => Return,
            _ => return Err(()),
        })
    }
}

pub const MAX_CONST_INDEX: usize = 0xFF_FF_FF;

pub struct Chunk {
    code: Vec<u8>,
    line_nums: Vec<usize>,
    line_runs: Vec<usize>,
    constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            line_nums: Vec::new(),
            line_runs: Vec::new(),
            constants: Vec::new(),
        }
    }

    pub fn code_len(&self) -> usize {
        self.code.len()
    }

    pub fn code(&self, offset: usize) -> u8 {
        self.code[offset]
    }

    pub fn patch(&mut self, offset: usize, byte: u8) {
        self.code[offset] = byte;
    }

    pub fn constant(&self, index: usize) -> &Value {
        &self.constants[index]
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        let index = self.constants.len();
        if value.is_str() {
            let target = value.clone().into_str();
            for (i, val) in self.constants.iter().enumerate() {
                if val.is_str() && val.clone().into_str() == target {
                    return i;
                }
            }
        }
        self.constants.push(value);
        return index;
    }

    pub fn line(&self, offset: usize) -> usize {
        let target_run = offset + 1;
        let mut current_runs = 0;
        for i in 0..self.line_nums.len() {
            current_runs += self.line_runs[i];
            if target_run <= current_runs {
                return self.line_nums[i];
            }
        }
        panic!("[lox] no line info for bytecode offset: {}", offset);
    }

    fn add_next_line(&mut self, line: usize) {
        let len = self.line_nums.len();
        if len == 0 || self.line_nums[len - 1] != line {
            self.line_nums.push(line);
            self.line_runs.push(1);
        } else {
            self.line_runs[len - 1] += 1;
        }
    }

    pub fn write(&mut self, opcode: OpCode, line: usize) {
        self.code.push(opcode as u8);
        self.add_next_line(line);
    }

    pub fn write_byte(&mut self, byte: u8, line: usize) {
        self.code.push(byte);
        self.add_next_line(line);
    }

    pub fn write_load(&mut self, index: usize, line: usize) {
        if index <= u8::MAX as usize {
            self.write(OpCode::Constant, line);
            self.write_byte(index as u8, line);
        } else {
            // little-endian
            let byte1 = (index & 0xFF) as u8;
            let byte2 = ((index >> 8) & 0xFF) as u8;
            let byte3 = ((index >> 16) & 0xFF) as u8;
            self.write(OpCode::ConstantLong, line);
            self.write_byte(byte1, line);
            self.write_byte(byte2, line);
            self.write_byte(byte3, line);
        }
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
    fn chunk_write_load() {
        let mut chunk = Chunk::new();
        assert_eq!(0, chunk.code_len());
        chunk.write_load(u8::MAX as usize, 123);
        assert_eq!(2, chunk.code_len());
        assert_eq!(OpCode::Constant as u8, chunk.code(0));
    }

    #[test]
    fn chunk_write_load_long() {
        let mut chunk = Chunk::new();
        assert_eq!(0, chunk.code_len());
        chunk.write_load(u8::MAX as usize + 1, 123);
        assert_eq!(4, chunk.code_len());
        assert_eq!(OpCode::ConstantLong as u8, chunk.code(0));
    }

    #[test]
    fn chunk_add_constant() {
        let mut chunk = Chunk::new();
        assert_eq!(0, chunk.constants.len());
        let index = chunk.add_constant(Value::Num(1.2));
        assert_eq!(1, chunk.constants.len());
        assert_eq!(0, index);
    }

    #[test]
    fn chunk_line() {
        let mut chunk = Chunk::new();
        chunk.write(OpCode::Return, 123);
        chunk.write(OpCode::Return, 123);
        chunk.write(OpCode::Return, 123);
        chunk.write(OpCode::Return, 456);
        chunk.write(OpCode::Return, 789);
        assert_eq!(123, chunk.line(0));
        assert_eq!(123, chunk.line(1));
        assert_eq!(123, chunk.line(2));
        assert_eq!(456, chunk.line(3));
        assert_eq!(789, chunk.line(4));
    }

    #[test]
    #[should_panic]
    fn chunk_line_out_of_range() {
        Chunk::new().line(1000);
    }
}
