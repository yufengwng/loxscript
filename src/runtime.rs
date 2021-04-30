use std::convert::TryFrom;

use crate::bytecode::Chunk;
use crate::bytecode::OpCode;
use crate::compile;
use crate::debug;
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

    pub fn interpret(&mut self, source: String) -> InterpretResult {
        match compile::compile(source) {
            Some(chunk) => self.run(chunk),
            None => InterpretResult::CompileErr,
        }
    }

    fn run(&mut self, chunk: Chunk) -> InterpretResult {
        use OpCode::*;
        let mut frame = CallFrame::new(chunk);

        macro_rules! runtime_err {
            ( $($args:tt)+ ) => ({
                println!($($args)*);
                println!("[line {}] in script", frame.curr_line());
                self.stack_reset();
            });
        }

        macro_rules! bin_op {
            ( $e:expr ) => {{
                $e
            }};
        }

        macro_rules! bin_linear {
            ( $op:tt ) => ({
                let top = self.stack_peek(0);
                let under = self.stack_peek(1);
                if let (Value::Num(lhs), Value::Num(rhs)) = (under, top) {
                    self.stack_pop();
                    self.stack_set(0, Value::Num(bin_op!(lhs $op rhs)));
                } else {
                    runtime_err!("operands must be numbers");
                    return InterpretResult::RuntimeErr;
                }
            });
        }

        macro_rules! bin_inverse {
            ( $op:tt ) => ({
                let top = self.stack_peek(0);
                let under = self.stack_peek(1);
                if let (Value::Num(lhs), Value::Num(rhs)) = (under, top) {
                    if rhs == 0.0 {
                        eprintln!("[lox] runtime error: divide-by-zero");
                        return InterpretResult::RuntimeErr;
                    }
                    self.stack_pop();
                    self.stack_set(0, Value::Num(bin_op!(lhs $op rhs)));
                } else {
                    runtime_err!("operands must be numbers");
                    return InterpretResult::RuntimeErr;
                }
            });
        }

        macro_rules! unary_negate {
            () => ({
                if let Value::Num(num) = self.stack_peek(0) {
                    self.stack_set(0, Value::Num(-num));
                } else {
                    runtime_err!("operand must be a number");
                    return InterpretResult::RuntimeErr;
                }
            });
        }

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
                None => self.stack_push(Value::None),
                True => self.stack_push(Value::Bool(true)),
                False => self.stack_push(Value::Bool(false)),
                Add => bin_linear!(+),
                Subtract => bin_linear!(-),
                Multiply => bin_linear!(*),
                Divide => bin_inverse!(/),
                Modulo => bin_inverse!(%),
                Negate => unary_negate!(),
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
