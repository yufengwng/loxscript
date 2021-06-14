use std::collections::HashMap;
use std::convert::TryFrom;
use std::rc::Rc;
use std::time::SystemTime;

use crate::bytecode::OpCode;
use crate::compile;
use crate::debug;
use crate::value::NativeFn;
use crate::value::ObjFn;
use crate::value::ObjNative;
use crate::value::Value;

const FRAMES_MAX: usize = 64;

pub enum InterpretResult {
    Ok,
    CompileErr,
    RuntimeErr,
}

pub struct VM {
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    globals: HashMap<String, Value>,
}

macro_rules! runtime_err {
    ( $self:ident, $($args:tt)+ ) => ({
        eprintln!($($args)*);
        for i in (0..$self.frames.len()).rev() {
            let frame = &$self.frames[i];
            let fn_obj = &frame.function;
            eprint!("[line {}] in ", frame.prev_line());
            if fn_obj.name.is_empty() {
                eprintln!("script");
            } else {
                eprintln!("{}()", &fn_obj.name);
            }
        }
        $self.stack_reset();
    });
}

impl VM {
    pub fn new() -> Self {
        let mut this = Self {
            stack: Vec::new(),
            frames: Vec::new(),
            globals: HashMap::new(),
        };
        this.define_native("print", native_print, 1);
        this.define_native("clock", native_clock, 0);
        this
    }

    pub fn interpret(&mut self, source: String) -> InterpretResult {
        let fn_rc = match compile::compile(source) {
            Some(fn_obj) => Rc::new(fn_obj),
            None => return InterpretResult::CompileErr,
        };

        self.stack_push(Value::Fun(fn_rc.clone()));
        self.call(fn_rc, 0);

        let result = self.run();
        self.stack_print();
        result
    }

    fn run(&mut self) -> InterpretResult {
        use OpCode::*;

        macro_rules! expand {
            ( $e:expr ) => {{
                $e
            }};
        }

        macro_rules! bin_add {
            () => {{
                let rhs_is_str = self.stack_peek(0).is_str();
                let lhs_is_str = self.stack_peek(1).is_str();
                let rhs_is_num = self.stack_peek(0).is_num();
                let lhs_is_num = self.stack_peek(1).is_num();
                if lhs_is_str && rhs_is_str {
                    let rhs = self.stack_pop().into_str();
                    let mut lhs = self.stack_pop().into_str();
                    lhs.push_str(&rhs);
                    self.stack_push(Value::Str(lhs));
                } else if lhs_is_num && rhs_is_num {
                    let rhs = self.stack_pop().into_num();
                    let lhs = self.stack_pop().into_num();
                    self.stack_push(Value::Num(lhs + rhs));
                } else {
                    runtime_err!(self, "operands must be two numbers or two strings");
                    return InterpretResult::RuntimeErr;
                }
            }};
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
                    runtime_err!(self, "operands must be numbers");
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
                    runtime_err!(self, "operands must be numbers");
                    return InterpretResult::RuntimeErr;
                }
            });
        }

        macro_rules! unary_negate {
            () => {{
                if self.stack_peek(0).is_num() {
                    let num = self.stack_pop().into_num();
                    self.stack_push(Value::Num(-num));
                } else {
                    runtime_err!(self, "operand must be a number");
                    return InterpretResult::RuntimeErr;
                }
            }};
        }

        macro_rules! unary_not {
            () => {{
                let value = self.stack_pop().is_falsey();
                self.stack_push(Value::Bool(value));
            }};
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
            if cfg!(feature = "debug") {
                debug::disassemble_at(&self.frame().function.chunk, self.frame().ip);
            }
            let byte = self.frame_mut().read_byte();
            let opcode = match OpCode::try_from(byte) {
                Ok(op) => op,
                Err(_) => {
                    eprintln!("[lox] invalid opcode: {}", byte);
                    continue;
                }
            };
            match opcode {
                Constant => self.load_const(),
                ConstantLong => self.load_const_long(),
                DefineGlobal => {
                    let name = self.frame_mut().read_constant().clone().into_str();
                    let value = self.stack_pop();
                    self.globals.insert(name, value);
                }
                GetGlobal => {
                    let name = self.frame_mut().read_constant().clone().into_str();
                    let entry = self.globals.get(&name);
                    if entry.is_none() {
                        runtime_err!(self, "undefined variable '{}'", name);
                        return InterpretResult::RuntimeErr;
                    }
                    let value = entry.unwrap().clone();
                    self.stack_push(value);
                }
                SetGlobal => {
                    let name = self.frame_mut().read_constant().clone().into_str();
                    if !self.globals.contains_key(&name) {
                        runtime_err!(self, "undefined variable '{}'", name);
                        return InterpretResult::RuntimeErr;
                    }
                    let value = self.stack_pop();
                    self.stack_push(Value::None);
                    self.globals.insert(name, value);
                }
                GetLocal => {
                    let mut slot = self.frame_mut().read_byte() as usize;
                    slot += self.frame().base_slot;
                    let local = self.stack[slot].clone();
                    self.stack_push(local);
                }
                SetLocal => {
                    let mut slot = self.frame_mut().read_byte() as usize;
                    slot += self.frame().base_slot;
                    let value = self.stack_pop();
                    self.stack_push(Value::None);
                    self.stack[slot] = value;
                }
                None => self.stack_push(Value::None),
                True => self.stack_push(Value::Bool(true)),
                False => self.stack_push(Value::Bool(false)),
                Add => bin_add!(),
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
                Pop => {
                    self.stack_pop();
                }
                Loop => {
                    let mut frame = self.frame_mut();
                    let offset = frame.read_short() as usize;
                    frame.ip -= offset;
                }
                Jump => {
                    let mut frame = self.frame_mut();
                    let amount = frame.read_short() as usize;
                    frame.ip += amount;
                }
                JumpIfFalse => {
                    let amount = self.frame_mut().read_short() as usize;
                    if self.stack_peek(0).is_falsey() {
                        let mut frame = self.frame_mut();
                        frame.ip += amount;
                    }
                }
                Call => {
                    let arg_count = self.frame_mut().read_byte() as usize;
                    let value = self.stack_peek(arg_count).clone();
                    if !self.call_value(value, arg_count) {
                        return InterpretResult::RuntimeErr;
                    }
                }
                Return => {
                    let value = self.stack_pop();
                    self.frame_pop();
                    if self.frames.is_empty() {
                        return InterpretResult::Ok;
                    }
                    self.stack_push(value);
                }
            }
        }
    }

    fn stack_print(&self) {
        if cfg!(feature = "debug") {
            print!("        ");
            for value in self.stack.iter() {
                print!("[ ");
                value.print();
                print!(" ]");
            }
            if self.stack.is_empty() {
                print!("[ ]");
            }
            println!();
        }
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

    fn load_const(&mut self) {
        let value = self.frame_mut().read_constant().clone();
        self.stack_push(value);
    }

    fn load_const_long(&mut self) {
        let value = self.frame_mut().read_constant_long().clone();
        self.stack_push(value);
    }

    fn frame(&self) -> &CallFrame {
        self.frames.last().unwrap()
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().unwrap()
    }

    fn frame_pop(&mut self) {
        let frame = self.frames.pop().unwrap();
        for _ in 0..frame.function.arity {
            self.stack_pop();
        }
        self.stack_pop();
    }

    fn define_native(&mut self, name: &str, function: NativeFn, arity: usize) {
        let native = ObjNative::new(function, arity);
        let value = Value::Native(Rc::new(native));
        self.globals.insert(name.to_owned(), value);
    }

    fn call_value(&mut self, value: Value, arg_count: usize) -> bool {
        match value {
            Value::Fun(_) => self.call(value.into_fn(), arg_count),
            Value::Native(_) => self.call_native(value.into_native(), arg_count),
            _ => {
                runtime_err!(self, "can only call functions and classes");
                false
            }
        }
    }

    fn call_native(&mut self, native: Rc<ObjNative>, arg_count: usize) -> bool {
        if arg_count != native.arity {
            runtime_err!(
                self,
                "expected {} arguments but got {}",
                native.arity,
                arg_count
            );
            return false;
        }

        let start = self.stack.len() - arg_count;
        let args = self.stack.split_off(start);
        let result = (native.function)(args);

        self.stack_pop();
        self.stack_push(result);
        true
    }

    fn call(&mut self, fn_obj: Rc<ObjFn>, arg_count: usize) -> bool {
        if arg_count != fn_obj.arity {
            runtime_err!(
                self,
                "expected {} arguments but got {}",
                fn_obj.arity,
                arg_count
            );
            return false;
        } else if self.frames.len() == FRAMES_MAX {
            runtime_err!(self, "stack overflow");
            return false;
        }
        let mut frame = CallFrame::new(fn_obj);
        frame.base_slot = self.stack.len() - arg_count - 1;
        self.frames.push(frame);
        true
    }
}

struct CallFrame {
    function: Rc<ObjFn>,
    base_slot: usize,
    ip: usize,
}

impl CallFrame {
    fn new(fn_obj: Rc<ObjFn>) -> Self {
        Self {
            function: fn_obj,
            base_slot: 0,
            ip: 0,
        }
    }

    fn prev_line(&self) -> usize {
        self.function.chunk.line(self.ip - 1)
    }

    fn read_byte(&mut self) -> u8 {
        let byte = self.function.chunk.code(self.ip);
        self.ip += 1;
        byte
    }

    fn read_short(&mut self) -> u16 {
        let byte1 = self.function.chunk.code(self.ip) as u16;
        let byte2 = self.function.chunk.code(self.ip + 1) as u16;
        self.ip += 2;
        (byte2 << 8) | byte1
    }

    fn read_constant(&mut self) -> &Value {
        let index = self.read_byte() as usize;
        self.function.chunk.constant(index)
    }

    fn read_constant_long(&mut self) -> &Value {
        let byte1 = self.read_byte() as usize;
        let byte2 = self.read_byte() as usize;
        let byte3 = self.read_byte() as usize;
        let index = (byte3 << 16) | (byte2 << 8) | byte1;
        self.function.chunk.constant(index)
    }
}

fn native_print(args: Vec<Value>) -> Value {
    args[0].print();
    println!();
    Value::None
}

fn native_clock(_args: Vec<Value>) -> Value {
    let seconds = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_secs();
    Value::Num(seconds as f64)
}
