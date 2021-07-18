use std::cell::RefCell;
use std::collections::HashMap;
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
    Closure(Rc<ObjClosure>),
    Class(Rc<ObjClass>),
    Instance(Rc<ObjInstance>),
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
            (Self::Closure(f), Self::Closure(g)) => Rc::ptr_eq(f, g),
            (Self::Class(c), Self::Class(k)) => Rc::ptr_eq(c, k),
            (Self::Instance(i), Self::Instance(j)) => Rc::ptr_eq(i, j),
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

    pub fn is_instance(&self) -> bool {
        matches!(self, Self::Instance(..))
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

    pub fn into_closure(self) -> Rc<ObjClosure> {
        match self {
            Self::Closure(rc) => rc,
            _ => panic!(),
        }
    }

    pub fn into_class(self) -> Rc<ObjClass> {
        match self {
            Self::Class(rc) => rc,
            _ => panic!(),
        }
    }

    pub fn into_instance(self) -> Rc<ObjInstance> {
        match self {
            Self::Instance(rc) => rc,
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
            Self::Closure(f) => f.print(),
            Self::Class(c) => c.print(),
            Value::Instance(i) => i.print(),
        }
    }
}

pub struct ObjFn {
    pub name: String,
    pub arity: usize,
    pub chunk: Chunk,
    pub num_upvalues: usize,
}

impl ObjFn {
    pub fn new() -> Self {
        Self {
            name: String::new(),
            arity: 0,
            chunk: Chunk::new(),
            num_upvalues: 0,
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

pub type NativeFn = fn(Vec<Value>) -> Value;

pub struct ObjNative {
    pub function: NativeFn,
    pub arity: usize,
}

impl ObjNative {
    pub fn new(function: NativeFn, arity: usize) -> Self {
        Self { function, arity }
    }

    pub fn print(&self) {
        print!("<native fn>");
    }
}

pub struct ObjClosure {
    pub function: Rc<ObjFn>,
    pub upvalues: Vec<Rc<ObjUpvalue>>,
}

impl ObjClosure {
    pub fn new(function: Rc<ObjFn>) -> Self {
        Self {
            function,
            upvalues: Vec::new(),
        }
    }

    pub fn print(&self) {
        self.function.print();
    }
}

pub struct ObjUpvalue {
    pub location: usize,
    closed: RefCell<Option<Value>>,
}

impl ObjUpvalue {
    pub fn new(location: usize) -> Self {
        Self {
            location,
            closed: RefCell::new(None),
        }
    }

    pub fn close(&self, value: Value) {
        self.closed.replace(Some(value));
    }

    pub fn is_closed(&self) -> bool {
        self.closed.borrow().is_some()
    }

    pub fn get_value(&self) -> Value {
        self.closed.borrow().clone().unwrap()
    }

    pub fn set_value(&self, value: Value) {
        self.close(value);
    }
}

pub struct ObjClass {
    pub name: String,
}

impl ObjClass {
    pub fn new(name: String) -> Self {
        Self { name }
    }

    pub fn print(&self) {
        print!("<class {}>", self.name);
    }
}

pub struct ObjInstance {
    pub class: Rc<ObjClass>,
    pub fields: RefCell<HashMap<String, Value>>,
}

impl ObjInstance {
    pub fn new(class: Rc<ObjClass>) -> Self {
        Self {
            class,
            fields: RefCell::new(HashMap::new()),
        }
    }

    pub fn print(&self) {
        print!("<{} instance>", self.class.name);
    }

    pub fn get_field(&self, name: &str) -> Option<Value> {
        self.fields.borrow().get(name).map(|v| v.clone())
    }

    pub fn set_field(&self, name: String, value: Value) {
        self.fields.borrow_mut().insert(name, value);
    }
}
