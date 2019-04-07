use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::ast::Decl;
use crate::interpreter::Interpreter;
use crate::interpreter::RuntimeError;
use crate::runtime::Env;
use crate::runtime::Signal;
use crate::runtime::{Instance, Value};

pub trait Callable: fmt::Debug + fmt::Display {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, RuntimeError>;
    fn arity(&self) -> usize;
    fn name(&self) -> String;
}

pub struct Class {
    name: String,
    methods: Rc<Vec<Decl>>,
}

impl Class {
    pub fn new(name: String, methods: &Rc<Vec<Decl>>) -> Self {
        Self {
            name,
            methods: Rc::clone(methods),
        }
    }
}

impl Callable for Class {
    fn call(
        &self,
        _interpreter: &mut Interpreter,
        _args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        Ok(Value::Instance(Rc::new(Instance::new(
            self.name.to_owned(),
            &self.methods,
        ))))
    }

    fn arity(&self) -> usize {
        0
    }

    fn name(&self) -> String {
        self.name.to_owned()
    }
}

impl fmt::Debug for Class {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Class {{ name: {:?} }}", self.name,)
    }
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<class {}>", self.name)
    }
}

pub struct Function {
    name: String,
    params: Vec<(String, usize)>,
    body: Rc<Vec<Decl>>,
    closure: Rc<RefCell<Env>>,
}

impl Function {
    pub fn new(
        name: String,
        params: Vec<(String, usize)>,
        body: &Rc<Vec<Decl>>,
        env: &Rc<RefCell<Env>>,
    ) -> Self {
        Self {
            name,
            params,
            body: Rc::clone(body),
            closure: Rc::clone(env),
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
        match sig {
            Signal::Ret(value) => Ok(value),
            _ => Ok(Value::None),
        }
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
