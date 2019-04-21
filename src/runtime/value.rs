use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::runtime::Callable;
use crate::runtime::{Class, LoxClass};

#[derive(Clone, Debug)]
pub enum Value {
    None,
    Bool(bool),
    Num(f64),
    Str(String),
    Class(Rc<LoxClass>),
    Instance(Rc<LoxInstance>),
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
            (Value::Class(lhs), Value::Class(rhs)) => Rc::ptr_eq(lhs, rhs),
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
            Value::Class(class) => write!(f, "{}", class),
            Value::Instance(inst) => write!(f, "{}", inst),
            Value::Callable(fun) => write!(f, "{}", fun),
        }
    }
}

struct Instance {
    class: Rc<Class>,
    fields: HashMap<String, Value>,
}

pub struct LoxInstance(Rc<RefCell<Instance>>);

impl LoxInstance {
    pub fn new(class: &Rc<Class>) -> Self {
        Self(Rc::new(RefCell::new(Instance {
            class: Rc::clone(class),
            fields: HashMap::new(),
        })))
    }

    pub fn share(&self) -> Self {
        Self(Rc::clone(&self.0))
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        let inst = self.0.borrow();
        if inst.fields.contains_key(name) {
            inst.fields.get(name).cloned()
        } else {
            inst.class.find_method(name).map(|fun| {
                let fun = fun.bind(self.share());
                let fun: Rc<Callable> = Rc::new(fun);
                Value::Callable(fun)
            })
        }
    }

    pub fn set(&self, name: String, value: Value) {
        let mut inst = self.0.borrow_mut();
        inst.fields.insert(name, value);
    }
}

impl fmt::Debug for LoxInstance {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Instance {{ class: {:?}, fields: [{}] }}",
            self.0.borrow().class.name,
            self.0.borrow().fields.len()
        )
    }
}

impl fmt::Display for LoxInstance {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{} instance>", self.0.borrow().class.name)
    }
}
