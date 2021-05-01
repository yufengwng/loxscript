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

        macro_rules! expand {
            ( $e:expr ) => ({
                $e
            });
        }

        macro_rules! runtime_err {
            ( $($args:tt)+ ) => ({
                println!($($args)*);
                println!("[line {}] in script", frame.curr_line());
                self.stack_reset();
            });
        }

        macro_rules! bin_op {
            ( $ctor:expr, $op:tt ) => ({
                let rhs_is_num = self.stack_peek(0).is_num();
                let lhs_is_num = self.stack_peek(1).is_num();
                if lhs_is_num && rhs_is_num {
                    let rhs = self.stack_pop().into_num();
                    let lhs = self.stack_pop().into_num();
                    self.stack_push($ctor(expand!(lhs $op rhs)));
                } else {
                    runtime_err!("operands must be numbers");
                    return InterpretResult::RuntimeErr;
                }
            });
        }

        macro_rules! bin_op_checked {
            ( $op:tt ) => ({
                let rhs_is_num = self.stack_peek(0).is_num();
                let lhs_is_num = self.stack_peek(1).is_num();
                if lhs_is_num && rhs_is_num {
                    let rhs = self.stack_pop().into_num();
                    let lhs = self.stack_pop().into_num();
                    if rhs == 0.0 {
                        eprintln!("[lox] runtime error: divide-by-zero");
                        return InterpretResult::RuntimeErr;
                    }
                    self.stack_push(Value::Num(expand!(lhs $op rhs)));
                } else {
                    runtime_err!("operands must be numbers");
                    return InterpretResult::RuntimeErr;
                }
            });
        }

        macro_rules! unary_negate {
            () => ({
                if self.stack_peek(0).is_num() {
                    let num = self.stack_pop().into_num();
                    self.stack_push(Value::Num(-num));
                } else {
                    runtime_err!("operand must be a number");
                    return InterpretResult::RuntimeErr;
                }
            });
        }

        macro_rules! unary_not {
            () => ({
                let value = self.stack_pop().is_falsey();
                self.stack_push(Value::Bool(value));
            });
        }

        macro_rules! equality {
            ( $op:tt ) => ({
                let rhs = self.stack_pop();
                let lhs = self.stack_pop();
                self.stack_push(Value::Bool(expand!(lhs $op rhs)));
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
                Add => bin_op!(Value::Num, +),
                Subtract => bin_op!(Value::Num, -),
                Multiply => bin_op!(Value::Num, *),
                Divide => bin_op_checked!(/),
                Modulo => bin_op_checked!(%),
                Negate => unary_negate!(),
                Not => unary_not!(),
                Equal => equality!(==),
                NotEq => equality!(!=),
                Lt => bin_op!(Value::Bool, <),
                LtEq => bin_op!(Value::Bool, <=),
                Gt => bin_op!(Value::Bool, >),
                GtEq => bin_op!(Value::Bool, >=),
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

    fn stack_peek(&mut self, distance: usize) -> &Value {
        &self.stack[self.stack.len() - distance - 1]
    }

    fn stack_push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn stack_pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    fn stack_reset(&mut self) {
        self.stack.clear();
    }

    fn load_const(&mut self, frame: &mut CallFrame) {
        let value = frame.read_constant();
        self.stack_push(value.clone());
    }

    fn load_const_long(&mut self, frame: &mut CallFrame) {
        let value = frame.read_constant_long();
        self.stack_push(value.clone());
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
