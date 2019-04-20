use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::ast::Decl;
use crate::interpreter::Interpreter;
use crate::interpreter::RuntimeError;
use crate::runtime::Env;
use crate::runtime::Signal;
use crate::runtime::{LoxInstance, Value};

pub trait Callable: fmt::Debug + fmt::Display {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, RuntimeError>;
    fn arity(&self) -> usize;
    fn name(&self) -> String;
}

pub struct Class {
    pub name: String,
    pub methods: HashMap<String, Rc<Function>>,
}

impl Class {
    pub fn find_method(&self, name: &str) -> Option<Rc<Function>> {
        self.methods.get(name).map(|fun| Rc::clone(fun))
    }
}

pub struct LoxClass(Rc<Class>);

impl LoxClass {
    pub fn new(name: String, methods: HashMap<String, Rc<Function>>) -> Self {
        Self(Rc::new(Class { name, methods }))
    }
}

impl Callable for LoxClass {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, RuntimeError> {
        let instance = LoxInstance::new(&self.0);
        if let Some(fun) = self.0.find_method("init") {
            fun.bind(instance.share()).call(interpreter, args)?;
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

pub struct Function {
    name: String,
    params: Vec<(String, usize)>,
    body: Rc<Vec<Decl>>,
    closure: Rc<RefCell<Env>>,
    is_init: bool,
}

impl Function {
    pub fn new(
        name: String,
        params: Vec<(String, usize)>,
        body: &Rc<Vec<Decl>>,
        env: &Rc<RefCell<Env>>,
        is_init: bool,
    ) -> Self {
        Self {
            name,
            params,
            body: Rc::clone(body),
            closure: Rc::clone(env),
            is_init,
        }
    }

    pub fn bind(&self, instance: LoxInstance) -> Self {
        let mut env = Env::enclosing(&self.closure);
        env.define(String::from("self"), Value::Instance(Rc::new(instance)));
        Self {
            name: self.name.to_owned(),
            params: self.params.to_vec(),
            body: Rc::clone(&self.body),
            closure: Rc::new(RefCell::new(env)),
            is_init: self.is_init,
        }
    }
}

impl Callable for Function {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, RuntimeError> {
        let mut env = Env::enclosing(&self.closure);
        self.params.iter().zip(args).for_each(|(param, arg)| {
            env.define(param.0.to_owned(), arg);
        });

        let sig = interpreter.exec_block(&self.body, env)?;
        let result = match sig {
            Signal::Ret(value) => value,
            _ => Value::None,
        };

        Ok(if self.is_init {
            self.closure.borrow().get_at(0, "self").unwrap()
        } else {
            result
        })
    }

    fn arity(&self) -> usize {
        self.params.len()
    }

    fn name(&self) -> String {
        self.name.to_owned()
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Function {{ name: {:?}, params: {:?}, body: [{}] }}",
            self.name,
            self.params,
            self.body.len()
        )
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<fn {}>", self.name)
    }
}
