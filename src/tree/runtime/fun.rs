use std::fmt;
use std::rc::Rc;

use crate::tree::ast::FunDecl;
use crate::tree::runtime::Call;
use crate::tree::runtime::Env;
use crate::tree::runtime::LoxInstance;
use crate::tree::runtime::RunResult;
use crate::tree::runtime::Signal;
use crate::tree::runtime::Value;
use crate::tree::Interpreter;

#[derive(Clone)]
pub struct LoxFunction(Rc<Function>);

impl LoxFunction {
    pub fn new(decl: Rc<FunDecl>, closure: Env, is_init: bool) -> Self {
        Self(Rc::new(Function::new(decl, closure, is_init)))
    }

    pub fn bind(&self, instance: LoxInstance) -> Self {
        Self(Rc::new(self.0.bind(instance)))
    }
}

impl Call for LoxFunction {
    fn name(&self) -> String {
        self.0.decl.name.to_owned()
    }

    fn arity(&self) -> usize {
        self.0.decl.params.len()
    }

    fn call(&self, rt: &mut Interpreter, args: Vec<Value>) -> RunResult<Value> {
        self.0.call(rt, args)
    }
}

impl PartialEq for LoxFunction {
    fn eq(&self, other: &LoxFunction) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl fmt::Display for LoxFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<fn {}>", self.0.decl.name)
    }
}

struct Function {
    decl: Rc<FunDecl>,
    closure: Env,
    is_init: bool,
}

impl Function {
    fn new(decl: Rc<FunDecl>, closure: Env, is_init: bool) -> Self {
        Self {
            decl,
            closure,
            is_init,
        }
    }

    fn bind(&self, instance: LoxInstance) -> Self {
        let mut env = Env::wrap(&self.closure);
        env.define(String::from("self"), Value::Instance(instance));
        Self {
            decl: Rc::clone(&self.decl),
            closure: env,
            is_init: self.is_init,
        }
    }

    fn call(&self, rt: &mut Interpreter, args: Vec<Value>) -> RunResult<Value> {
        let mut env = Env::wrap(&self.closure);
        self.decl.params.iter().zip(args).for_each(|(param, arg)| {
            env.define(param.name.to_owned(), arg);
        });

        let sig = rt.run_with(&self.decl.body, env)?;
        let result = if self.is_init {
            self.closure.get_at(0, "self").unwrap()
        } else {
            match sig {
                Signal::Ret(value) => value,
                _ => Value::None,
            }
        };

        Ok(result)
    }
}
