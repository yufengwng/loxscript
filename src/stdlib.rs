use std::fmt;
use std::time::SystemTime;

use crate::runtime::Call;
use crate::runtime::RunResult;
use crate::runtime::Value;
use crate::Interpreter;

#[derive(Debug)]
pub struct Print;

impl Call for Print {
    fn name(&self) -> String {
        String::from("print")
    }

    fn arity(&self) -> usize {
        1
    }

    fn call(&self, _: &mut Interpreter, args: Vec<Value>) -> RunResult<Value> {
        println!("{}", args[0]);
        Ok(Value::None)
    }
}

impl fmt::Display for Print {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<native fn print>")
    }
}

#[derive(Debug)]
pub struct Clock;

impl Call for Clock {
    fn name(&self) -> String {
        String::from("clock")
    }

    fn arity(&self) -> usize {
        0
    }

    fn call(&self, _: &mut Interpreter, _: Vec<Value>) -> RunResult<Value> {
        let seconds = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .map(|d| d.as_secs() as f64)
            .unwrap_or(0.0);
        Ok(Value::Num(seconds))
    }
}

impl fmt::Display for Clock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<native fn clock>")
    }
}
