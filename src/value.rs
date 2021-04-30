#[derive(Copy, Clone)]
pub enum Value {
    None,
    Bool(bool),
    Num(f64),
}

impl Value {
    pub fn is_falsey(&self) -> bool {
        match self {
            Value::None => true,
            Value::Bool(b) => !b,
            _ => false,
        }
    }

    pub fn equal(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (Value::None, Value::None) => true,
            (Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
            (Value::Num(n1), Value::Num(n2)) => n1 == n2,
            _ => false,
        }
    }

    pub fn print(&self) {
        match self {
            Value::None => print!("none"),
            Value::Bool(b) => print!("{}", b),
            Value::Num(n) => print!("{}", n),
        }
    }
}
