use std::fmt;
use std::rc::Rc;

use crate::runtime::Callable;

#[derive(Clone, Debug)]
pub enum Value {
    None,
    Bool(bool),
    Num(f64),
    Str(String),
    Callable(Rc<Callable>),
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match *self {
            Value::Bool(b) => b,
            Value::None => false,
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
            Value::Callable(fun) => write!(f, "{}", fun),
        }
    }
}
