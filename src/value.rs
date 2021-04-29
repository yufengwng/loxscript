#[derive(Copy, Clone)]
pub enum Value {
    None,
    Bool(bool),
    Num(f64),
}

impl Value {
    pub fn print(&self) {
        match self {
            Value::None => print!("none"),
            Value::Bool(b) => print!("{}", b),
            Value::Num(n) => print!("{}", n),
        }
    }
}
