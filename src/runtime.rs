use std::convert::TryFrom;

use crate::bytecode::Chunk;
use crate::bytecode::OpCode;
use crate::compile::Compiler;
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

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        let mut compiler = Compiler::new();
        compiler.compile(source);
        return InterpretResult::Ok;
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
                Constant => self.load_const(&mut frame),
                ConstantLong => self.load_const_long(&mut frame),
                Add => self.bin_add(),
                Subtract => self.bin_subtract(),
                Multiply => self.bin_multiply(),
                Divide => {
                    if !self.bin_divide() {
                        return InterpretResult::RuntimeErr;
                    }
                }
                Modulo => {
                    if !self.bin_modulo() {
                        return InterpretResult::RuntimeErr;
                    }
                }
                Negate => self.negate(),
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

    fn load_const(&mut self, frame: &mut CallFrame) {
        let val = frame.read_constant();
        self.stack.push(*val);
    }

    fn load_const_long(&mut self, frame: &mut CallFrame) {
        let val = frame.read_constant_long();
        self.stack.push(*val);
    }

    fn bin_add(&mut self) {
        let rhs = self.stack.pop().unwrap();
        let lhs = self.stack.pop().unwrap();
        self.stack.push(lhs + rhs);
    }

    fn bin_subtract(&mut self) {
        let rhs = self.stack.pop().unwrap();
        let lhs = self.stack.pop().unwrap();
        self.stack.push(lhs - rhs);
    }

    fn bin_multiply(&mut self) {
        let rhs = self.stack.pop().unwrap();
        let lhs = self.stack.pop().unwrap();
        self.stack.push(lhs * rhs);
    }

    fn bin_divide(&mut self) -> bool {
        let rhs = self.stack.pop().unwrap();
        let lhs = self.stack.pop().unwrap();
        if rhs == 0.0 {
            eprintln!("[lox] runtime error: divide-by-zero");
            return false;
        } else {
            self.stack.push(lhs / rhs);
            return true;
        }
    }

    fn bin_modulo(&mut self) -> bool {
        let rhs = self.stack.pop().unwrap();
        let lhs = self.stack.pop().unwrap();
        if rhs == 0.0 {
            eprintln!("[lox] runtime error: divide-by-zero");
            return false;
        } else {
            self.stack.push(lhs % rhs);
            return true;
        }
    }

    fn negate(&mut self) {
        let val = self.stack.pop().unwrap();
        self.stack.push(-val);
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
