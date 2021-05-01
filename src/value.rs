#[derive(Clone, PartialEq)]
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

    pub fn is_num(&self) -> bool {
        matches!(self, Self::Num(..))
    }

    pub fn into_num(self) -> f64 {
        match self {
            Self::Num(n) => n,
            _ => panic!(),
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
