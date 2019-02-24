use std::cell::RefCell;
use std::error;
use std::fmt;
use std::rc::Rc;

use crate::ast::{BinOp, LogOp, UniOp};
use crate::ast::{Decl, Expr, Primitive, Stmt};
use crate::runtime::Env;
use crate::runtime::Value;
use crate::runtime::{Callable, Function};

#[derive(Debug)]
pub enum RuntimeError {
    Arity(usize, usize, usize),
    BinAddUnsupportedType(usize),
    BinNonNumeric(usize),
    NotCallable(usize),
    UniNonNumeric(usize),
    UndefinedVar(usize, String),
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RuntimeError::Arity(line, expected, actual) => write!(
                f,
                "[line {}] runtime error: expected {} arguments but got {}",
                line, expected, actual
            ),
            RuntimeError::BinAddUnsupportedType(line) => write!(
                f,
                "[line {}] runtime error: operands must be two numbers or two strings",
                line
            ),
            RuntimeError::BinNonNumeric(line) => {
                write!(f, "[line {}] runtime error: operands must be numbers", line)
            }
            RuntimeError::NotCallable(line) => write!(
                f,
                "[line {}] runtime error: can only call functions and classes",
                line
            ),
            RuntimeError::UniNonNumeric(line) => {
                write!(f, "[line {}] runtime error: operand must be a number", line)
            }
            RuntimeError::UndefinedVar(line, name) => write!(
                f,
                "[line {}] runtime error: undefined variable '{}'",
                line, name
            ),
        }
    }
}

impl error::Error for RuntimeError {}

#[derive(Debug)]
struct Print;

impl Callable for Print {
    fn call(&self, _: &mut Interpreter, args: Vec<Value>) -> Result<Value, RuntimeError> {
        println!("{}", args[0]);
        Ok(Value::None)
    }

    fn arity(&self) -> usize {
        1
    }

    fn name(&self) -> String {
        "print".to_owned()
    }
}

impl fmt::Display for Print {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<native fn print>")
    }
}

pub enum Signal {
    Ret(Value),
    None,
}

#[derive(Default)]
pub struct Interpreter {
    env: Rc<RefCell<Env>>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut globals = Env::new();
        globals.define(Print.name(), Value::Callable(Rc::new(Print)));
        Self {
            env: Rc::new(RefCell::new(globals)),
        }
    }

    pub fn run(&mut self, program: &[Decl]) {
        if let Err(err) = self.interpret(program) {
            eprintln!("{}", err);
        }
    }

    fn interpret(&mut self, program: &[Decl]) -> Result<(), RuntimeError> {
        for decl in program {
            self.declare(decl)?;
        }
        Ok(())
    }

    fn declare(&mut self, decl: &Decl) -> Result<Signal, RuntimeError> {
        match decl {
            Decl::Function(name, params, body) => {
                let fun = Function::new(name.to_owned(), params.to_vec(), body, &self.env);
                self.env
                    .borrow_mut()
                    .define(fun.name(), Value::Callable(Rc::new(fun)));
            }
            Decl::Let(name, init) => {
                let value = match init {
                    Some(expr) => self.eval(expr)?,
                    None => Value::None,
                };
                self.env.borrow_mut().define(name.clone(), value);
            }
            Decl::Statement(stmt) => return self.exec(stmt),
        }
        Ok(Signal::None)
    }

    fn exec(&mut self, stmt: &Stmt) -> Result<Signal, RuntimeError> {
        match stmt {
            Stmt::Return(expr, _) => {
                let value = match expr {
                    Some(e) => self.eval(e)?,
                    None => Value::None,
                };
                return Ok(Signal::Ret(value));
            }
            Stmt::Assignment(name, expr, line) => {
                let value = self.eval(expr)?;
                if !self.env.borrow_mut().assign(name.clone(), value) {
                    return Err(RuntimeError::UndefinedVar(*line, name.clone()));
                }
            }
            Stmt::Expression(expr) => {
                println!("{:?}", self.eval(expr)?);
            }
            Stmt::Block(decls) => return self.exec_block(decls, Env::enclosing(&self.env)),
        }
        Ok(Signal::None)
    }

    pub fn exec_block(&mut self, decls: &[Decl], env: Env) -> Result<Signal, RuntimeError> {
        let prev = Rc::clone(&self.env);
        self.env = Rc::new(RefCell::new(env));

        let mut res = Ok(Signal::None);
        for decl in decls {
            res = self.declare(decl);
            if res.is_err() {
                break;
            }
            if let Ok(Signal::Ret(_)) = res {
                break;
            }
        }

        self.env = prev;
        res
    }

    fn eval(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        Ok(match expr {
            Expr::Literal(ref prim) => match prim {
                Primitive::None(_) => Value::None,
                Primitive::Bool(b, _) => Value::Bool(*b),
                Primitive::Num(n, _) => Value::Num(*n),
                Primitive::Str(s, _) => Value::Str(s.clone()),
            },
            Expr::Unary(ref op, expr) => {
                let value = self.eval(expr)?;
                match op {
                    UniOp::Neg(line) => {
                        if let Value::Num(n) = value {
                            Value::Num(-n)
                        } else {
                            return Err(RuntimeError::UniNonNumeric(*line));
                        }
                    }
                    UniOp::Not(_) => Value::Bool(!value.is_truthy()),
                }
            }
            Expr::Binary(ref lhs, ref op, ref rhs) => {
                let lval = self.eval(lhs)?;
                let rval = self.eval(rhs)?;
                match op {
                    BinOp::Add(line) => match (lval, rval) {
                        (Value::Num(lv), Value::Num(rv)) => Value::Num(lv + rv),
                        (Value::Str(lv), Value::Str(rv)) => Value::Str(lv + &rv),
                        _ => return Err(RuntimeError::BinAddUnsupportedType(*line)),
                    },
                    BinOp::Sub(line) => match (lval, rval) {
                        (Value::Num(lv), Value::Num(rv)) => Value::Num(lv - rv),
                        _ => return Err(RuntimeError::BinNonNumeric(*line)),
                    },
                    BinOp::Mul(line) => match (lval, rval) {
                        (Value::Num(lv), Value::Num(rv)) => Value::Num(lv * rv),
                        _ => return Err(RuntimeError::BinNonNumeric(*line)),
                    },
                    BinOp::Div(line) => match (lval, rval) {
                        (Value::Num(lv), Value::Num(rv)) => Value::Num(lv / rv),
                        _ => return Err(RuntimeError::BinNonNumeric(*line)),
                    },
                    BinOp::Rem(line) => match (lval, rval) {
                        (Value::Num(lv), Value::Num(rv)) => Value::Num(lv % rv),
                        _ => return Err(RuntimeError::BinNonNumeric(*line)),
                    },
                    BinOp::Lt(line) => match (lval, rval) {
                        (Value::Num(lv), Value::Num(rv)) => Value::Bool(lv < rv),
                        _ => return Err(RuntimeError::BinNonNumeric(*line)),
                    },
                    BinOp::LtEq(line) => match (lval, rval) {
                        (Value::Num(lv), Value::Num(rv)) => Value::Bool(lv <= rv),
                        _ => return Err(RuntimeError::BinNonNumeric(*line)),
                    },
                    BinOp::Gt(line) => match (lval, rval) {
                        (Value::Num(lv), Value::Num(rv)) => Value::Bool(lv > rv),
                        _ => return Err(RuntimeError::BinNonNumeric(*line)),
                    },
                    BinOp::GtEq(line) => match (lval, rval) {
                        (Value::Num(lv), Value::Num(rv)) => Value::Bool(lv >= rv),
                        _ => return Err(RuntimeError::BinNonNumeric(*line)),
                    },
                    BinOp::EqEq(_) => Value::Bool(lval == rval),
                    BinOp::NotEq(_) => Value::Bool(lval != rval),
                }
            }
            Expr::Logical(ref lhs, ref op, ref rhs) => {
                let lval = self.eval(lhs)?;
                match op {
                    LogOp::And(_) => {
                        if lval.is_truthy() {
                            self.eval(rhs)?
                        } else {
                            lval
                        }
                    }
                    LogOp::Or(_) => {
                        if lval.is_truthy() {
                            lval
                        } else {
                            self.eval(rhs)?
                        }
                    }
                }
            }
            Expr::Call(expr, arguments, line) => {
                let callee = self.eval(expr)?;

                let mut args = Vec::new();
                for arg in arguments {
                    args.push(self.eval(arg)?);
                }

                let fun = match callee {
                    Value::Callable(fun) => fun,
                    _ => {
                        return Err(RuntimeError::NotCallable(*line));
                    }
                };

                if args.len() != fun.arity() {
                    return Err(RuntimeError::Arity(*line, fun.arity(), args.len()));
                }

                fun.call(self, args)?
            }
            Expr::Variable(name, line) => self
                .env
                .borrow()
                .search(name)
                .ok_or_else(|| RuntimeError::UndefinedVar(*line, name.clone()))?,
            Expr::Group(ref inner) => self.eval(inner)?,
        })
    }
}
