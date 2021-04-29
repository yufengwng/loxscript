use std::convert::TryFrom;

use crate::bytecode::Chunk;
use crate::bytecode::OpCode;
use crate::compile;
use crate::debug;
use crate::value::Value;

macro_rules! runtime_err {
    ( $this:ident, $frame:ident, $($args:tt)+ ) => ({
        println!($($args)*);
        println!("[line {}] in script", $frame.curr_line());
        $this.stack_reset();
    });
}

macro_rules! bin_op {
    ( $e:expr ) => ({
        $e
    });
}

macro_rules! bin_linear {
    ( $this:ident, $frame:ident, $op:tt ) => ({
        let top = $this.stack_peek(0);
        let under = $this.stack_peek(1);
        if let (Value::Num(lhs), Value::Num(rhs)) = (under, top) {
            $this.stack_pop();
            $this.stack_set(0, Value::Num(bin_op!(lhs $op rhs)));
            true
        } else {
            runtime_err!($this, $frame, "operands must be numbers");
            false
        }
    });
}

macro_rules! bin_inverse {
    ( $this:ident, $frame:ident, $op:tt ) => ({
        let top = $this.stack_peek(0);
        let under = $this.stack_peek(1);
        if let (Value::Num(lhs), Value::Num(rhs)) = (under, top) {
            if rhs == 0.0 {
                eprintln!("[lox] runtime error: divide-by-zero");
                return false;
            }
            $this.stack_pop();
            $this.stack_set(0, Value::Num(bin_op!(lhs $op rhs)));
            true
        } else {
            runtime_err!($this, $frame, "operands must be numbers");
            false
        }
    });
}

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

    pub fn interpret(&mut self, source: String) -> InterpretResult {
        match compile::compile(source) {
            Some(chunk) => self.run(chunk),
            None => InterpretResult::CompileErr,
        }
    }

    fn run(&mut self, chunk: Chunk) -> InterpretResult {
        use OpCode::*;
        let mut frame = CallFrame::new(chunk);
        loop {
            self.stack_print();
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
                Add => {
                    if !self.bin_add(&frame) {
                        return InterpretResult::RuntimeErr;
                    }
                }
                Subtract => {
                    if !self.bin_subtract(&frame) {
                        return InterpretResult::RuntimeErr;
                    }
                }
                Multiply => {
                    if !self.bin_multiply(&frame) {
                        return InterpretResult::RuntimeErr;
                    }
                }
                Divide => {
                    if !self.bin_divide(&frame) {
                        return InterpretResult::RuntimeErr;
                    }
                }
                Modulo => {
                    if !self.bin_modulo(&frame) {
                        return InterpretResult::RuntimeErr;
                    }
                }
                Negate => {
                    if !self.negate(&frame) {
                        return InterpretResult::RuntimeErr;
                    }
                }
                Return => {
                    let value = self.stack_pop();
                    value.print();
                    println!();
                    return InterpretResult::Ok;
                }
            }
        }
    }

    fn stack_print(&self) {
        print!("        ");
        for value in self.stack.iter() {
            print!("[ ");
            value.print();
            print!(" ]");
        }
        println!();
    }

    fn stack_peek(&mut self, distance: usize) -> Value {
        self.stack[self.stack.len() - distance - 1]
    }

    fn stack_set(&mut self, distance: usize, value: Value) {
        let len = self.stack.len();
        self.stack[len - distance - 1] = value;
    }

    fn stack_push(&mut self, val: Value) {
        self.stack.push(val);
    }

    fn stack_pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    fn stack_reset(&mut self) {
        self.stack.clear();
    }

    fn load_const(&mut self, frame: &mut CallFrame) {
        let value = frame.read_constant();
        self.stack_push(*value);
    }

    fn load_const_long(&mut self, frame: &mut CallFrame) {
        let value = frame.read_constant_long();
        self.stack_push(*value);
    }

    fn bin_add(&mut self, frame: &CallFrame) -> bool {
        bin_linear!(self, frame, +)
    }

    fn bin_subtract(&mut self, frame: &CallFrame) -> bool {
        bin_linear!(self, frame, -)
    }

    fn bin_multiply(&mut self, frame: &CallFrame) -> bool {
        bin_linear!(self, frame, *)
    }

    fn bin_divide(&mut self, frame: &CallFrame) -> bool {
        bin_inverse!(self, frame, /)
    }

    fn bin_modulo(&mut self, frame: &CallFrame) -> bool {
        bin_inverse!(self, frame, %)
    }

    fn negate(&mut self, frame: &CallFrame) -> bool {
        if let Value::Num(num) = self.stack_peek(0) {
            self.stack_set(0, Value::Num(-num));
            true
        } else {
            runtime_err!(self, frame, "operand must be a number");
            false
        }
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

    fn curr_line(&self) -> usize {
        self.chunk.line(self.ip)
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
