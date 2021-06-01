use std::rc::Rc;

use crate::bytecode::Chunk;

#[derive(Clone)]
pub enum Value {
    None,
    Bool(bool),
    Num(f64),
    Str(String),
    Fun(Rc<ObjFn>),
}

pub struct ObjFn {
    pub name: String,
    pub arity: usize,
    pub chunk: Chunk,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::None, Value::None) => true,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Num(n), Value::Num(m)) => n == m,
            (Value::Str(s), Value::Str(t)) => s == t,
            (Value::Fun(f), Value::Fun(g)) => Rc::ptr_eq(f, g),
            _ => false,
        }
    }
}

impl Value {
    pub fn is_falsey(&self) -> bool {
        match self {
            Value::None => true,
            Value::Bool(b) => !b,
            _ => false,
        }
    }

    pub fn is_num(&self) -> bool {
        matches!(self, Self::Num(..))
    }

    pub fn is_str(&self) -> bool {
        matches!(self, Self::Str(..))
    }

    pub fn into_num(self) -> f64 {
        match self {
            Self::Num(n) => n,
            _ => panic!(),
        }
    }

    pub fn into_str(self) -> String {
        match self {
            Self::Str(s) => s,
            _ => panic!(),
        }
    }

    pub fn print(&self) {
        match self {
            Value::None => print!("none"),
            Value::Bool(b) => print!("{}", b),
            Value::Num(n) => print!("{}", n),
            Value::Str(s) => print!("{}", s),
            Value::Fun(f) => f.print(),
        }
    }
}

impl ObjFn {
    pub fn new() -> Self {
        Self {
            name: String::new(),
            arity: 0,
            chunk: Chunk::new(),
        }
    }

    pub fn print(&self) {
        if self.name.is_empty() {
            print!("<script>");
        } else {
            print!("<fn {}>", self.name);
        }
    }
}
