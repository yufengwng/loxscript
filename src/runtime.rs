//! Module for runtime representation of language items.

#[derive(Debug, PartialEq)]
pub enum Value {
    None,
    Bool(bool),
    Num(f64),
    Str(String),
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
