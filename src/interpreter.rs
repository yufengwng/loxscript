use std::cell::RefCell;
use std::collections::HashMap;
use std::error;
use std::fmt;
use std::rc::Rc;

use crate::ast::{BinOp, LogOp, UniOp};
use crate::ast::{Decl, Expr, Primitive, Stmt, Var};
use crate::runtime::Env;
use crate::runtime::Value;
use crate::runtime::{Callable, Class, Function, Signal};
use crate::ResolvedProgram;

#[derive(Debug)]
pub enum RuntimeError {
    Arity(usize, usize, usize),
    BinAddUnsupportedType(usize),
    BinNonNumeric(usize),
    NoFields(usize),
    NotCallable(usize),
    NotInstance(usize),
    UniNonNumeric(usize),
    UndefinedProp(usize, String),
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
            RuntimeError::NoFields(line) => write!(
                f,
                "[line {}] runtime error: only instances have fields",
                line
            ),
            RuntimeError::NotCallable(line) => write!(
                f,
                "[line {}] runtime error: can only call functions and classes",
                line
            ),
            RuntimeError::NotInstance(line) => write!(
                f,
                "[line {}] runtime error: only instances have properties",
                line
            ),
            RuntimeError::UniNonNumeric(line) => {
                write!(f, "[line {}] runtime error: operand must be a number", line)
            }
            RuntimeError::UndefinedProp(line, name) => write!(
                f,
                "[line {}] runtime error: undefined property '{}'",
                line, name
            ),
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

#[derive(Default)]
pub struct Interpreter {
    env: Rc<RefCell<Env>>,
    globals: Rc<RefCell<Env>>,
    hops: HashMap<usize, usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut globals = Env::new();
        globals.define(Print.name(), Value::Callable(Rc::new(Print)));
        let globals = Rc::new(RefCell::new(globals));
        Self {
            env: Rc::clone(&globals),
            globals: Rc::clone(&globals),
            hops: HashMap::new(),
        }
    }

    pub fn run(&mut self, program: ResolvedProgram) -> bool {
        self.hops.extend(program.hops);
        if let Err(err) = self.interpret(&program.decls) {
            eprintln!("{}", err);
            true
        } else {
            false
        }
    }

    pub fn exec_block(&mut self, decls: &[Decl], env: Env) -> Result<Signal, RuntimeError> {
        self.with_scope(env, |this| {
            for decl in decls {
                let sig = this.declare(decl)?;
                match sig {
                    Signal::Ret(_) => return Ok(sig),
                    Signal::Break => return Ok(sig),
                    Signal::Cont => return Ok(sig),
                    Signal::None => continue,
                }
            }
            Ok(Signal::None)
        })
    }

    fn with_scope<F>(&mut self, env: Env, mut f: F) -> Result<Signal, RuntimeError>
    where
        F: FnMut(&mut Self) -> Result<Signal, RuntimeError>,
    {
        let prev = Rc::clone(&self.env);
        self.env = Rc::new(RefCell::new(env));

        let res = f(self);

        self.env = prev;
        res
    }

    fn interpret(&mut self, program: &[Decl]) -> Result<(), RuntimeError> {
        for decl in program {
            self.declare(decl)?;
        }
        Ok(())
    }

    fn declare(&mut self, decl: &Decl) -> Result<Signal, RuntimeError> {
        match decl {
            Decl::Class(name, methods, _) => {
                self.env.borrow_mut().define(name.to_owned(), Value::None);
                let class = Class::new(name.to_owned(), &methods);
                self.env
                    .borrow_mut()
                    .assign(name.to_owned(), Value::Callable(Rc::new(class)));
            }
            Decl::Function(name, params, body, _) => {
                let fun = Function::new(name.to_owned(), params.to_vec(), body, &self.env);
                self.env
                    .borrow_mut()
                    .define(fun.name(), Value::Callable(Rc::new(fun)));
            }
            Decl::Let(name, init, _) => {
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
            Stmt::For(init, cond, post, body) => {
                return self.with_scope(Env::enclosing(&self.env), |this| {
                    if let Some(decl) = init {
                        this.declare(decl)?;
                    }

                    while this.eval(cond)?.is_truthy() {
                        let sig = this.exec_block(body, Env::enclosing(&this.env))?;
                        match sig {
                            Signal::Ret(_) => return Ok(sig),
                            Signal::Break => break,
                            Signal::Cont => {}
                            Signal::None => {}
                        }

                        if let Some(stmt) = post {
                            this.exec(stmt)?;
                        }
                    }

                    Ok(Signal::None)
                });
            }
            Stmt::If(branches, otherwise) => {
                for (cond, then) in branches {
                    if self.eval(cond)?.is_truthy() {
                        return self.exec_block(then, Env::enclosing(&self.env));
                    }
                }
                if let Some(body) = otherwise {
                    return self.exec_block(body, Env::enclosing(&self.env));
                }
            }
            Stmt::While(cond, body) => {
                while self.eval(cond)?.is_truthy() {
                    let sig = self.exec_block(&body, Env::enclosing(&self.env))?;
                    match sig {
                        Signal::Ret(_) => return Ok(sig),
                        Signal::Break => return Ok(Signal::None),
                        Signal::Cont => continue,
                        Signal::None => continue,
                    }
                }
            }
            Stmt::Break(_) => {
                return Ok(Signal::Break);
            }
            Stmt::Continue(_) => {
                return Ok(Signal::Cont);
            }
            Stmt::Return(expr, _) => {
                let value = match expr {
                    Some(e) => self.eval(e)?,
                    None => Value::None,
                };
                return Ok(Signal::Ret(value));
            }
            Stmt::Assignment(var, expr, line) => {
                let value = self.eval(expr)?;

                let success = match self.hops.get(&var.id) {
                    Some(dist) => self
                        .env
                        .borrow_mut()
                        .assign_at(*dist, var.name.clone(), value),
                    None => self.globals.borrow_mut().assign(var.name.clone(), value),
                };

                if !success {
                    return Err(RuntimeError::UndefinedVar(*line, var.name.clone()));
                }
            }
            Stmt::Set(object, name, value, line) => match self.eval(object)? {
                Value::Instance(obj) => {
                    let value = self.eval(value)?;
                    obj.borrow_mut().set(name.to_owned(), value);
                }
                _ => return Err(RuntimeError::NoFields(*line)),
            },
            Stmt::Expression(expr) => {
                self.eval(expr)?;
            }
            Stmt::Block(decls) => return self.exec_block(decls, Env::enclosing(&self.env)),
        }
        Ok(Signal::None)
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
            Expr::Logical(lhs, op, rhs) => {
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
            Expr::Get(object, name, line) => {
                return match self.eval(object)? {
                    Value::Instance(obj) => obj
                        .borrow()
                        .get(&name)
                        .ok_or_else(|| RuntimeError::UndefinedProp(*line, name.to_owned())),
                    _ => Err(RuntimeError::NotInstance(*line)),
                };
            }
            Expr::Variable(var, line) => self.lookup_var(var, *line)?,
            Expr::Group(ref inner) => self.eval(inner)?,
        })
    }

    fn lookup_var(&mut self, var: &Var, line: usize) -> Result<Value, RuntimeError> {
        let value = match self.hops.get(&var.id) {
            Some(dist) => self.env.borrow().get_at(*dist, &var.name),
            None => self.globals.borrow().search(&var.name),
        };
        value.ok_or_else(|| RuntimeError::UndefinedVar(line, var.name.clone()))
    }
}
