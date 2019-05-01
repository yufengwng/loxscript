//! Module for runtime representation of language items.

use std::error;
use std::fmt;

use crate::interpreter::Interpreter;

mod call;
mod env;
mod fun;
mod value;

pub use self::call::{Class, LoxClass};
pub use self::env::Env;
pub use self::fun::LoxFunction;
pub use self::value::{LoxInstance, Value};

pub trait Call: fmt::Display {
    fn name(&self) -> String;
    fn arity(&self) -> usize;
    fn call(&self, rt: &mut Interpreter, args: Vec<Value>) -> RunResult<Value>;
}

pub type RunResult<T> = Result<T, RuntimeError>;

#[derive(Debug)]
pub enum RuntimeError {
    /// (line, expected, actual)
    ArityMismatch(usize, usize, usize),
    /// (line)
    BinAddUnsupportedType(usize),
    /// (line)
    BinNonNumeric(usize),
    /// (line)
    NoFields(usize),
    /// (line)
    NotCallable(usize),
    /// (line)
    NotInstance(usize),
    /// (line)
    NotSuperclass(usize),
    /// (line)
    UniNonNumeric(usize),
    /// (line, name)
    UndefinedProp(usize, String),
    /// (line, name)
    UndefinedVar(usize, String),
}

impl error::Error for RuntimeError {}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use RuntimeError::*;
        match self {
            ArityMismatch(line, expected, actual) => write!(
                f,
                "[line {}] runtime error: expected {} arguments but got {}",
                line, expected, actual
            ),
            BinAddUnsupportedType(line) => write!(
                f,
                "[line {}] runtime error: operands must be two numbers or two strings",
                line
            ),
            BinNonNumeric(line) => {
                write!(f, "[line {}] runtime error: operands must be numbers", line)
            }
            NoFields(line) => write!(
                f,
                "[line {}] runtime error: only instances have fields",
                line
            ),
            NotCallable(line) => write!(
                f,
                "[line {}] runtime error: can only call functions and classes",
                line
            ),
            NotInstance(line) => write!(
                f,
                "[line {}] runtime error: only instances have properties",
                line
            ),
            NotSuperclass(line) => write!(
                f,
                "[line {}] runtime error: superclass must be a class",
                line
            ),
            UniNonNumeric(line) => {
                write!(f, "[line {}] runtime error: operand must be a number", line)
            }
            UndefinedProp(line, name) => write!(
                f,
                "[line {}] runtime error: undefined property '{}'",
                line, name
            ),
            UndefinedVar(line, name) => write!(
                f,
                "[line {}] runtime error: undefined variable '{}'",
                line, name
            ),
        }
    }
}

pub enum Signal {
    Ret(Value),
    Break,
    Cont,
    None,
}
