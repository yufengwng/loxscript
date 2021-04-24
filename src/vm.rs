use std::convert::TryFrom;

use crate::bytecode::Chunk;
use crate::bytecode::OpCode;
use crate::debug;
use crate::value;
use crate::value::Value;

pub enum InterpretResult {
    Ok,
    CompileErr,
    RuntimeErr,
}

pub fn interpret(chunk: Chunk) -> InterpretResult {
    run(chunk)
}

fn run(chunk: Chunk) -> InterpretResult {
    use OpCode::*;
    let mut vm = VM::new(chunk);
    loop {
        debug::disassemble_at(&vm.chunk, vm.ip);
        let byte = vm.read_byte();
        let opcode = match OpCode::try_from(byte) {
            Ok(op) => op,
            Err(_) => {
                eprintln!("[lox] invalid opcode: {}", byte);
                continue;
            }
        };
        match opcode {
            Constant => {
                let val = vm.read_constant();
                value::print(val);
                println!();
            }
            ConstantLong => {
                let val = vm.read_constant_long();
                value::print(val);
                println!();
            }
            Return => {
                return InterpretResult::Ok;
            }
        }
    }
}

struct VM {
    chunk: Chunk,
    ip: usize,
}

impl VM {
    fn new(chunk: Chunk) -> Self {
        Self { chunk, ip: 0 }
    }

    fn read_byte(&mut self) -> u8 {
        let byte = self.chunk.code(self.ip);
        self.ip += 1;
        return byte;
    }

    fn read_constant(&mut self) -> &Value {
        let index = self.read_byte() as usize;
        self.chunk.constant(index)
    }

    fn read_constant_long(&mut self) -> &Value {
        let byte1 = self.read_byte() as usize;
        let byte2 = self.read_byte() as usize;
        let byte3 = self.read_byte() as usize;
        let index = (byte3 << 16) | (byte2 << 8) | byte1;
        self.chunk.constant(index)
    }
}
