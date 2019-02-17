use std::fmt;

use crate::interpreter::Interpreter;
use crate::interpreter::RuntimeError;
use crate::runtime::Value;

pub trait Callable: fmt::Debug + fmt::Display {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, RuntimeError>;
    fn arity(&self) -> usize;
    fn name(&self) -> String;
}
