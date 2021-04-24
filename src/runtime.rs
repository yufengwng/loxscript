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

pub struct VM {
    stack: Vec<Value>,
}

impl VM {
    pub fn new() -> Self {
        Self { stack: Vec::new() }
    }

    pub fn interpret(&mut self, chunk: Chunk) -> InterpretResult {
        self.run(chunk)
    }

    fn run(&mut self, chunk: Chunk) -> InterpretResult {
        use OpCode::*;
        let mut frame = CallFrame::new(chunk);
        loop {
            self.trace_stack();
            debug::disassemble_at(&frame.chunk, frame.ip);
            let byte = frame.read_byte();
            let opcode = match OpCode::try_from(byte) {
                Ok(op) => op,
                Err(_) => {
                    eprintln!("[lox] invalid opcode: {}", byte);
                    continue;
                }
            };
            match opcode {
                Constant => {
                    let val = frame.read_constant();
                    self.stack.push(*val);
                }
                ConstantLong => {
                    let val = frame.read_constant_long();
                    self.stack.push(*val);
                }
                Return => {
                    let val = self.stack.pop().unwrap();
                    value::print(&val);
                    println!();
                    return InterpretResult::Ok;
                }
            }
        }
    }

    fn trace_stack(&self) {
        print!("        ");
        for val in self.stack.iter() {
            print!("[ ");
            value::print(val);
            print!(" ]");
        }
        println!();
    }
}

struct CallFrame {
    chunk: Chunk,
    ip: usize,
}

impl CallFrame {
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
