use std::cell::RefCell;
use std::error;
use std::fmt;
use std::rc::Rc;

use crate::ast::{BinOp, LogOp, UniOp};
use crate::ast::{Decl, Expr, Primitive, Stmt};
use crate::runtime::Env;
use crate::runtime::Value;

#[derive(Debug)]
enum RuntimeError {
    BinAddUnsupportedType(usize),
    BinNonNumeric(usize),
    UniNonNumeric(usize),
    UndefinedVar(String, usize),
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RuntimeError::BinAddUnsupportedType(line) => write!(
                f,
                "[line {}] runtime error: operands must be two numbers or two strings",
                line
            ),
            RuntimeError::BinNonNumeric(line) => {
                write!(f, "[line {}] runtime error: operands must be numbers", line)
            }
            RuntimeError::UniNonNumeric(line) => {
                write!(f, "[line {}] runtime error: operand must be a number", line)
            }
            RuntimeError::UndefinedVar(name, line) => write!(
                f,
                "[line {}] runtime error: undefined variable '{}'",
                line, name
            ),
        }
    }
}

impl error::Error for RuntimeError {}

#[derive(Default)]
pub struct Interpreter {
    env: Rc<RefCell<Env>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Rc::new(RefCell::new(Env::new())),
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

    fn declare(&mut self, decl: &Decl) -> Result<(), RuntimeError> {
        match decl {
            Decl::Let(name, init, _) => {
                let value = match init {
                    Some(expr) => self.eval(expr)?,
                    None => Value::None,
                };
                self.env.borrow_mut().define(name.clone(), value);
            }
            Decl::Statement(stmt) => self.exec(stmt)?,
        }
        Ok(())
    }

    fn exec(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::Expression(ref expr) => {
                println!("{:?}", self.eval(expr)?);
            }
            Stmt::Assignment(ref name, ref expr, ref line) => {
                let value = self.eval(expr)?;
                if !self.env.borrow_mut().assign(name.clone(), value) {
                    return Err(RuntimeError::UndefinedVar(name.clone(), *line));
                }
            }
            Stmt::Block(decls) => self.exec_block(decls, Env::enclosing(Rc::clone(&self.env)))?,
        }
        Ok(())
    }

    fn exec_block(&mut self, decls: &[Decl], env: Env) -> Result<(), RuntimeError> {
        let prev = Rc::clone(&self.env);
        self.env = Rc::new(RefCell::new(env));

        let mut res = Ok(());
        for decl in decls {
            res = self.declare(decl);
            if res.is_err() {
                break;
            }
        }

        self.env = prev;
        res
    }

    fn eval(&self, expr: &Expr) -> Result<Value, RuntimeError> {
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
            Expr::Variable(name, line) => self
                .env
                .borrow()
                .search(name)
                .ok_or_else(|| RuntimeError::UndefinedVar(name.clone(), *line))?,
            Expr::Group(ref inner) => self.eval(inner)?,
        })
    }
}
