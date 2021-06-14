use std::rc::Rc;

use crate::bytecode::Chunk;

#[derive(Clone)]
pub enum Value {
    None,
    Bool(bool),
    Num(f64),
    Str(String),
    Fun(Rc<ObjFn>),
    Native(Rc<ObjNative>),
}

pub struct ObjFn {
    pub name: String,
    pub arity: usize,
    pub chunk: Chunk,
}

pub type NativeFn = fn(Vec<Value>) -> Value;

pub struct ObjNative {
    pub function: NativeFn,
    pub arity: usize,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::None, Self::None) => true,
            (Self::Bool(a), Self::Bool(b)) => a == b,
            (Self::Num(n), Self::Num(m)) => n == m,
            (Self::Str(s), Self::Str(t)) => s == t,
            (Self::Fun(f), Self::Fun(g)) => Rc::ptr_eq(f, g),
            (Self::Native(f), Self::Native(g)) => Rc::ptr_eq(f, g),
            _ => false,
        }
    }
}

impl Value {
    pub fn is_falsey(&self) -> bool {
        match self {
            Self::None => true,
            Self::Bool(b) => !b,
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

    pub fn into_fn(self) -> Rc<ObjFn> {
        match self {
            Self::Fun(rc) => rc,
            _ => panic!(),
        }
    }

    pub fn into_native(self) -> Rc<ObjNative> {
        match self {
            Self::Native(rc) => rc,
            _ => panic!(),
        }
    }

    pub fn print(&self) {
        match self {
            Self::None => print!("none"),
            Self::Bool(b) => print!("{}", b),
            Self::Num(n) => print!("{}", n),
            Self::Str(s) => print!("{}", s),
            Self::Fun(f) => f.print(),
            Self::Native(f) => f.print(),
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

impl ObjNative {
    pub fn new(function: NativeFn, arity: usize) -> Self {
        Self { function, arity }
    }

    pub fn print(&self) {
        print!("<native fn>");
    }
}
