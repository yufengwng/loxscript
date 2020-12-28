use std::fmt;
use std::rc::Rc;

use crate::runtime::{Call, LoxFunction};
use crate::runtime::{LoxClass, LoxInstance};

#[derive(Clone)]
pub enum Value {
    None,
    Bool(bool),
    Num(f64),
    Str(String),
    Fun(LoxFunction),
    Call(Rc<dyn Call>),
    Class(LoxClass),
    Instance(LoxInstance),
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
            (Value::Fun(lhs), Value::Fun(rhs)) => lhs == rhs,
            (Value::Call(lhs), Value::Call(rhs)) => Rc::ptr_eq(lhs, rhs),
            (Value::Class(lhs), Value::Class(rhs)) => lhs == rhs,
            (Value::Instance(lhs), Value::Instance(rhs)) => lhs == rhs,
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
            Value::Fun(fun) => write!(f, "{}", fun),
            Value::Call(fun) => write!(f, "{}", fun),
            Value::Class(class) => write!(f, "{}", class),
            Value::Instance(inst) => write!(f, "{}", inst),
        }
    }
}
