use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{BinOp, LogOp, UniOp};
use crate::ast::{Decl, Expr, Primitive, Stmt, Var};
use crate::runtime::Env;
use crate::runtime::Value;
use crate::runtime::{Call, Function, LoxClass, Signal};
use crate::runtime::{RunResult, RuntimeError};
use crate::stdlib::{Clock, Print};
use crate::ResolvedProgram;

#[derive(Default)]
pub struct Interpreter {
    env: Rc<RefCell<Env>>,
    globals: Rc<RefCell<Env>>,
    hops: HashMap<usize, usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut globals = Env::new();
        globals.define(Print.name(), Value::Call(Rc::new(Print)));
        globals.define(Clock.name(), Value::Call(Rc::new(Clock)));
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

    pub fn exec_block(&mut self, decls: &[Decl], env: Env) -> RunResult<Signal> {
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

    fn with_scope<F>(&mut self, env: Env, mut f: F) -> RunResult<Signal>
    where
        F: FnMut(&mut Self) -> RunResult<Signal>,
    {
        let prev = Rc::clone(&self.env);
        self.env = Rc::new(RefCell::new(env));

        let res = f(self);

        self.env = prev;
        res
    }

    fn interpret(&mut self, program: &[Decl]) -> RunResult<()> {
        for decl in program {
            self.declare(decl)?;
        }
        Ok(())
    }

    fn declare(&mut self, decl: &Decl) -> RunResult<Signal> {
        match decl {
            Decl::Class(name, superclass, method_decls, _) => {
                let superclass = if let Some(super_expr) = superclass {
                    let value = self.eval(super_expr)?;
                    match value {
                        Value::Class(ref class_ptr) => Some(Rc::clone(class_ptr)),
                        _ => {
                            return Err(RuntimeError::NotSuperclass(match super_expr {
                                Expr::Variable(_, line) => *line,
                                _ => 0,
                            }));
                        }
                    }
                } else {
                    None
                };

                self.env.borrow_mut().define(name.to_owned(), Value::None);

                if let Some(ref parent) = superclass {
                    self.env = Rc::new(RefCell::new(Env::enclosing(&self.env)));
                    self.env
                        .borrow_mut()
                        .define(String::from("super"), Value::Class(Rc::clone(parent)));
                }

                let mut methods = HashMap::new();
                for method in method_decls.iter() {
                    if let Decl::Function(fun_name, params, body, _) = method {
                        let fun = Function::new(
                            fun_name.to_owned(),
                            params.to_vec(),
                            &body,
                            &self.env,
                            fun_name == "init",
                        );
                        methods.insert(fun_name.to_owned(), Rc::new(fun));
                    }
                }

                let has_superclass = superclass.is_some();
                let class = LoxClass::new(name.to_owned(), superclass, methods);
                if has_superclass {
                    let inner = self.env.borrow().inner_env();
                    self.env = inner;
                }
                self.env
                    .borrow_mut()
                    .assign(name.to_owned(), Value::Class(Rc::new(class)));
            }
            Decl::Function(name, params, body, _) => {
                let fun = Function::new(name.to_owned(), params.to_vec(), body, &self.env, false);
                self.env
                    .borrow_mut()
                    .define(fun.name(), Value::Call(Rc::new(fun)));
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

    fn exec(&mut self, stmt: &Stmt) -> RunResult<Signal> {
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
                    obj.set(name.to_owned(), value);
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

    fn eval(&mut self, expr: &Expr) -> RunResult<Value> {
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
                    Value::Class(class) => class,
                    Value::Call(fun) => fun,
                    _ => {
                        return Err(RuntimeError::NotCallable(*line));
                    }
                };

                if args.len() != fun.arity() {
                    return Err(RuntimeError::ArityMismatch(*line, fun.arity(), args.len()));
                }

                fun.call(self, args)?
            }
            Expr::Get(object, name, line) => {
                return match self.eval(object)? {
                    Value::Instance(obj) => obj
                        .get(&name)
                        .ok_or_else(|| RuntimeError::UndefinedProp(*line, name.to_owned())),
                    _ => Err(RuntimeError::NotInstance(*line)),
                };
            }
            Expr::Variable(var, line) => self.lookup_var(var, *line)?,
            Expr::Self_(var, line) => self.lookup_var(var, *line)?,
            Expr::Super(var, _, method, line) => {
                let dist = self.hops.get(&var.id).cloned().unwrap();
                let superclass = match self.env.borrow().get_at(dist, &var.name).unwrap() {
                    Value::Class(clz) => clz,
                    _ => panic!(),
                };
                let instance = match self.env.borrow().get_at(dist - 1, "self").unwrap() {
                    Value::Instance(inst) => inst,
                    _ => panic!(),
                };
                return superclass
                    .inner()
                    .find_method(method)
                    .map(|fun| {
                        let fun = fun.bind(instance.share());
                        let fun: Rc<Call> = Rc::new(fun);
                        Value::Call(fun)
                    })
                    .ok_or_else(|| RuntimeError::UndefinedProp(*line, method.to_owned()));
            }
            Expr::Group(ref inner) => self.eval(inner)?,
        })
    }

    fn lookup_var(&mut self, var: &Var, line: usize) -> RunResult<Value> {
        let value = match self.hops.get(&var.id) {
            Some(dist) => self.env.borrow().get_at(*dist, &var.name),
            None => self.globals.borrow().search(&var.name),
        };
        value.ok_or_else(|| RuntimeError::UndefinedVar(line, var.name.clone()))
    }
}
