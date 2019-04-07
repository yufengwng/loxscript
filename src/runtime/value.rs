use std::fmt;
use std::rc::Rc;

use crate::ast::Decl;
use crate::runtime::Callable;

#[derive(Clone, Debug)]
pub enum Value {
    None,
    Bool(bool),
    Num(f64),
    Str(String),
    Instance(Rc<Instance>),
    Callable(Rc<Callable>),
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match *self {
            Value::None => false,
            Value::Bool(b) => b,
            _ => true,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::None, Value::None) => true,
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
            (Value::Num(lhs), Value::Num(rhs)) => lhs == rhs,
            (Value::Str(lhs), Value::Str(rhs)) => lhs == rhs,
            (Value::Instance(lhs), Value::Instance(rhs)) => Rc::ptr_eq(lhs, rhs),
            (Value::Callable(lhs), Value::Callable(rhs)) => Rc::ptr_eq(lhs, rhs),
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::None => write!(f, "none"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Num(n) => write!(f, "{}", n),
            Value::Str(s) => write!(f, "{}", s),
            Value::Instance(inst) => write!(f, "{}", inst),
            Value::Callable(fun) => write!(f, "{}", fun),
        }
    }
}

pub struct Instance {
    class: String,
    methods: Rc<Vec<Decl>>,
}

impl Instance {
    pub fn new(class: String, methods: &Rc<Vec<Decl>>) -> Self {
        Self {
            class,
            methods: Rc::clone(methods),
        }
    }
}

impl fmt::Debug for Instance {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Instance {{ class: {:?}, methods: [{}] }}",
            self.class,
            self.methods.len()
        )
    }
}

impl fmt::Display for Instance {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{} instance>", self.class)
    }
}
