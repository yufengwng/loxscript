use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::runtime::LoxFunction;
use crate::runtime::RunResult;
use crate::runtime::{Call, LoxInstance, Value};
use crate::Interpreter;

pub struct Class {
    pub name: String,
    pub superclass: Option<Rc<LoxClass>>,
    pub methods: HashMap<String, LoxFunction>,
}

impl Class {
    pub fn find_method(&self, name: &str) -> Option<LoxFunction> {
        self.methods.get(name).cloned().or_else(|| {
            if let Some(parent) = &self.superclass {
                parent.0.find_method(name)
            } else {
                None
            }
        })
    }
}

pub struct LoxClass(Rc<Class>);

impl LoxClass {
    pub fn new(
        name: String,
        superclass: Option<Rc<LoxClass>>,
        methods: HashMap<String, LoxFunction>,
    ) -> Self {
        Self(Rc::new(Class {
            name,
            superclass,
            methods,
        }))
    }

    pub fn inner(&self) -> &Class {
        &self.0
    }
}

impl Call for LoxClass {
    fn call(&self, rt: &mut Interpreter, args: Vec<Value>) -> RunResult<Value> {
        let instance = LoxInstance::new(&self.0);
        if let Some(fun) = self.0.find_method("init") {
            fun.bind(instance.share()).call(rt, args)?;
        }
        Ok(Value::Instance(Rc::new(instance)))
    }

    fn arity(&self) -> usize {
        self.0
            .find_method("init")
            .map(|fun| fun.arity())
            .unwrap_or(0)
    }

    fn name(&self) -> String {
        self.0.name.to_owned()
    }
}

impl fmt::Debug for LoxClass {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Class {{ name: {:?}, methods: [{}] }}",
            self.0.name,
            self.0.methods.len()
        )
    }
}

impl fmt::Display for LoxClass {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<class {}>", self.0.name)
    }
}
