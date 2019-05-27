use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::runtime::{Call, LoxFunction, RunResult, Value};
use crate::Interpreter;

#[derive(Clone)]
pub struct LoxClass(Rc<Class>);

impl LoxClass {
    pub fn new(
        name: String,
        superclass: Option<LoxClass>,
        methods: HashMap<String, LoxFunction>,
    ) -> Self {
        Self(Rc::new(Class {
            name,
            superclass,
            methods,
        }))
    }

    pub fn find_method(&self, name: &str) -> Option<LoxFunction> {
        self.0.find_method(name)
    }
}

impl Call for LoxClass {
    fn name(&self) -> String {
        self.0.name.to_owned()
    }

    fn arity(&self) -> usize {
        self.0
            .find_method("init")
            .map(|fun| fun.arity())
            .unwrap_or(0)
    }

    fn call(&self, rt: &mut Interpreter, args: Vec<Value>) -> RunResult<Value> {
        let instance = LoxInstance::new(self.clone());
        if let Some(fun) = self.0.find_method("init") {
            fun.bind(instance.clone()).call(rt, args)?;
        }
        Ok(Value::Instance(instance))
    }
}

impl PartialEq for LoxClass {
    fn eq(&self, other: &LoxClass) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl fmt::Display for LoxClass {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<class {}>", self.0.name)
    }
}

struct Class {
    name: String,
    superclass: Option<LoxClass>,
    methods: HashMap<String, LoxFunction>,
}

impl Class {
    fn find_method(&self, name: &str) -> Option<LoxFunction> {
        self.methods.get(name).cloned().or_else(|| {
            if let Some(parent) = &self.superclass {
                parent.0.find_method(name)
            } else {
                None
            }
        })
    }
}

#[derive(Clone)]
pub struct LoxInstance(Rc<RefCell<Instance>>);

impl LoxInstance {
    pub fn new(class: LoxClass) -> Self {
        Self(Rc::new(RefCell::new(Instance {
            class,
            fields: HashMap::new(),
        })))
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        let this = self.0.borrow();
        if this.fields.contains_key(name) {
            this.fields.get(name).cloned()
        } else {
            this.class
                .find_method(name)
                .map(|fun| Value::Fun(fun.bind(self.clone())))
        }
    }

    pub fn set(&self, name: String, value: Value) {
        let mut this = self.0.borrow_mut();
        this.fields.insert(name, value);
    }
}

impl PartialEq for LoxInstance {
    fn eq(&self, other: &LoxInstance) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl fmt::Display for LoxInstance {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let this = self.0.borrow();
        write!(f, "<{} instance>", this.class.name())
    }
}

struct Instance {
    class: LoxClass,
    fields: HashMap<String, Value>,
}
