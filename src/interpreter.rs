use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{BinOp, LogOp, UniOp};
use crate::ast::{Body, Decl, Expr, FunDecl, Primitive, Stmt, Var};
use crate::runtime::Env;
use crate::runtime::Value;
use crate::runtime::{Call, LoxClass, LoxFunction, Signal};
use crate::runtime::{RunResult, RuntimeError};
use crate::stdlib::{Clock, Print};
use crate::ResolvedProgram;

pub struct Interpreter {
    env: Env,
    globals: Env,
    hops: HashMap<usize, usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut globals = Env::new();
        globals.define(Print.name(), Value::Call(Rc::new(Print)));
        globals.define(Clock.name(), Value::Call(Rc::new(Clock)));
        Self {
            env: globals.clone(),
            globals,
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

    pub fn run_with(&mut self, body: &[Decl], env: Env) -> RunResult<Signal> {
        self.exec_block(body, env)
    }

    fn interpret(&mut self, program: &[Decl]) -> RunResult<()> {
        for decl in program {
            self.declare(decl)?;
        }
        Ok(())
    }

    fn declare(&mut self, decl: &Decl) -> RunResult<Signal> {
        match decl {
            Decl::Class(_, name, superclass_var, method_decls) => {
                self.declare_class(name, superclass_var, method_decls)?;
            }
            Decl::Function(decl) => {
                let fun = LoxFunction::new(Rc::clone(decl), self.env.clone(), false);
                self.env.define(fun.name(), Value::Fun(fun));
            }
            Decl::Let(_, name, value) => {
                let value = match value {
                    Some(expr) => self.eval(expr)?,
                    None => Value::None,
                };
                self.env.define(name.clone(), value);
            }
            Decl::Statement(stmt) => return self.exec(stmt),
        }
        Ok(Signal::None)
    }

    fn declare_class(
        &mut self,
        name: &str,
        superclass: &Option<Var>,
        signatures: &[Rc<FunDecl>],
    ) -> RunResult<()> {
        let superclass = match superclass {
            Some(var) => match self.lookup_var(&var)? {
                Value::Class(parent) => Some(parent),
                _ => return Err(RuntimeError::NotSuperclass(var.line)),
            },
            None => None,
        };

        self.env.define(name.to_owned(), Value::None);

        if let Some(ref parent) = superclass {
            self.env = Env::wrap(&self.env);
            let sup = Value::Class(parent.clone());
            self.env.define(String::from("super"), sup);
        }

        let mut methods = HashMap::new();
        for decl in signatures {
            let is_init = decl.name == "init";
            let fun = LoxFunction::new(Rc::clone(decl), self.env.clone(), is_init);
            methods.insert(fun.name(), fun);
        }

        if superclass.is_some() {
            let inner = self.env.unwrap();
            self.env = inner;
        }

        let class = LoxClass::new(name.to_owned(), superclass, methods);
        self.env.assign(name.to_owned(), Value::Class(class));
        Ok(())
    }

    fn exec(&mut self, stmt: &Stmt) -> RunResult<Signal> {
        match stmt {
            Stmt::For(init, cond, post, body) => self.exec_for_stmt(init, cond, post, body),
            Stmt::If(branches, otherwise) => self.exec_if_stmt(branches, otherwise),
            Stmt::While(cond, body) => self.exec_while_stmt(cond, body),
            Stmt::Break(_) => Ok(Signal::Break),
            Stmt::Continue(_) => Ok(Signal::Cont),
            Stmt::Return(_, value) => {
                let value = match value {
                    Some(expr) => self.eval(expr)?,
                    None => Value::None,
                };
                Ok(Signal::Ret(value))
            }
            Stmt::Assignment(var, value) => self.exec_assign_stmt(var, value),
            Stmt::Set(line, object, name, value) => self.exec_set_stmt(object, name, value, *line),
            Stmt::Expression(expr) => {
                self.eval(expr)?;
                Ok(Signal::None)
            }
            Stmt::Block(body) => self.exec_block(body, Env::wrap(&self.env)),
        }
    }

    fn exec_block(&mut self, body: &[Decl], env: Env) -> RunResult<Signal> {
        let prev = self.env.clone();
        self.env = env;
        let res = (|| {
            for decl in body {
                let sig = self.declare(decl)?;
                match sig {
                    Signal::Ret(_) => return Ok(sig),
                    Signal::Break => return Ok(sig),
                    Signal::Cont => return Ok(sig),
                    Signal::None => continue,
                }
            }
            Ok(Signal::None)
        })();
        self.env = prev;
        res
    }

    fn exec_for_stmt(
        &mut self,
        init: &Option<Box<Decl>>,
        cond: &Expr,
        post: &Option<Box<Stmt>>,
        body: &[Decl],
    ) -> RunResult<Signal> {
        let prev = self.env.clone();
        self.env = Env::wrap(&self.env);
        let res = (|| {
            if let Some(decl) = init {
                self.declare(decl)?;
            }
            while self.eval(cond)?.is_truthy() {
                let sig = self.exec_block(body, Env::wrap(&self.env))?;
                match sig {
                    Signal::Ret(_) => return Ok(sig),
                    Signal::Break => break,
                    Signal::Cont => {}
                    Signal::None => {}
                }
                if let Some(stmt) = post {
                    self.exec(stmt)?;
                }
            }
            Ok(Signal::None)
        })();
        self.env = prev;
        res
    }

    fn exec_if_stmt(
        &mut self,
        branches: &[(Expr, Body)],
        otherwise: &Option<Body>,
    ) -> RunResult<Signal> {
        for (cond, then) in branches {
            if self.eval(cond)?.is_truthy() {
                return self.exec_block(then, Env::wrap(&self.env));
            }
        }
        if let Some(body) = otherwise {
            return self.exec_block(body, Env::wrap(&self.env));
        }
        Ok(Signal::None)
    }

    fn exec_while_stmt(&mut self, cond: &Expr, body: &[Decl]) -> RunResult<Signal> {
        while self.eval(cond)?.is_truthy() {
            let sig = self.exec_block(body, Env::wrap(&self.env))?;
            match sig {
                Signal::Ret(_) => return Ok(sig),
                Signal::Break => return Ok(Signal::None),
                Signal::Cont => continue,
                Signal::None => continue,
            }
        }
        Ok(Signal::None)
    }

    fn exec_assign_stmt(&mut self, var: &Var, value: &Expr) -> RunResult<Signal> {
        let value = self.eval(value)?;
        let success = match self.hops.get(&var.id) {
            Some(dist) => self.env.assign_at(*dist, var.name.clone(), value),
            None => self.globals.assign(var.name.clone(), value),
        };
        if success {
            Ok(Signal::None)
        } else {
            Err(RuntimeError::UndefinedVar(var.line, var.name.clone()))
        }
    }

    fn exec_set_stmt(
        &mut self,
        object: &Expr,
        name: &str,
        value: &Expr,
        line: usize,
    ) -> RunResult<Signal> {
        let obj = match self.eval(object)? {
            Value::Instance(inst) => inst,
            _ => return Err(RuntimeError::NoFields(line)),
        };
        let value = self.eval(value)?;
        obj.set(name.to_owned(), value);
        Ok(Signal::None)
    }

    fn eval(&mut self, expr: &Expr) -> RunResult<Value> {
        match expr {
            Expr::Literal(_, value) => self.eval_literal_expr(value),
            Expr::Unary(line, op, expr) => self.eval_unary_expr(op, expr, *line),
            Expr::Binary(line, lhs, op, rhs) => self.eval_binary_expr(lhs, op, rhs, *line),
            Expr::Logical(_, lhs, op, rhs) => self.eval_logical_expr(lhs, op, rhs),
            Expr::Call(line, callee, arguments) => self.eval_call_expr(callee, arguments, *line),
            Expr::Get(line, object, name) => self.eval_get_expr(object, name, *line),
            Expr::Variable(var) => self.lookup_var(var),
            Expr::Self_(var) => self.lookup_var(var),
            Expr::Super(var, line, method) => self.eval_super_expr(var, method, *line),
            Expr::Group(inner) => self.eval(inner),
        }
    }

    fn eval_literal_expr(&mut self, value: &Primitive) -> RunResult<Value> {
        Ok(match value {
            Primitive::None => Value::None,
            Primitive::Bool(b) => Value::Bool(*b),
            Primitive::Num(n) => Value::Num(*n),
            Primitive::Str(s) => Value::Str(s.clone()),
        })
    }

    fn eval_unary_expr(&mut self, op: &UniOp, rhs: &Expr, line: usize) -> RunResult<Value> {
        let value = self.eval(rhs)?;
        Ok(match op {
            UniOp::Neg => {
                if let Value::Num(n) = value {
                    Value::Num(-n)
                } else {
                    return Err(RuntimeError::UniNonNumeric(line));
                }
            }
            UniOp::Not => Value::Bool(!value.is_truthy()),
        })
    }

    fn eval_binary_expr(
        &mut self,
        lhs: &Expr,
        op: &BinOp,
        rhs: &Expr,
        line: usize,
    ) -> RunResult<Value> {
        let lval = self.eval(lhs)?;
        let rval = self.eval(rhs)?;
        Ok(match op {
            BinOp::Add => match (lval, rval) {
                (Value::Num(lv), Value::Num(rv)) => Value::Num(lv + rv),
                (Value::Str(lv), Value::Str(rv)) => Value::Str(lv + &rv),
                _ => return Err(RuntimeError::BinAddUnsupportedType(line)),
            },
            BinOp::Sub => match (lval, rval) {
                (Value::Num(lv), Value::Num(rv)) => Value::Num(lv - rv),
                _ => return Err(RuntimeError::BinNonNumeric(line)),
            },
            BinOp::Mul => match (lval, rval) {
                (Value::Num(lv), Value::Num(rv)) => Value::Num(lv * rv),
                _ => return Err(RuntimeError::BinNonNumeric(line)),
            },
            BinOp::Div => match (lval, rval) {
                (Value::Num(lv), Value::Num(rv)) => {
                    if rv == 0.0 {
                        return Err(RuntimeError::DivByZero(line));
                    }
                    Value::Num(lv / rv)
                }
                _ => return Err(RuntimeError::BinNonNumeric(line)),
            },
            BinOp::Rem => match (lval, rval) {
                (Value::Num(lv), Value::Num(rv)) => {
                    if rv == 0.0 {
                        return Err(RuntimeError::DivByZero(line));
                    }
                    Value::Num(lv % rv)
                }
                _ => return Err(RuntimeError::BinNonNumeric(line)),
            },
            BinOp::Lt => match (lval, rval) {
                (Value::Num(lv), Value::Num(rv)) => Value::Bool(lv < rv),
                _ => return Err(RuntimeError::BinNonNumeric(line)),
            },
            BinOp::LtEq => match (lval, rval) {
                (Value::Num(lv), Value::Num(rv)) => Value::Bool(lv <= rv),
                _ => return Err(RuntimeError::BinNonNumeric(line)),
            },
            BinOp::Gt => match (lval, rval) {
                (Value::Num(lv), Value::Num(rv)) => Value::Bool(lv > rv),
                _ => return Err(RuntimeError::BinNonNumeric(line)),
            },
            BinOp::GtEq => match (lval, rval) {
                (Value::Num(lv), Value::Num(rv)) => Value::Bool(lv >= rv),
                _ => return Err(RuntimeError::BinNonNumeric(line)),
            },
            BinOp::EqEq => Value::Bool(lval == rval),
            BinOp::NotEq => Value::Bool(lval != rval),
        })
    }

    fn eval_logical_expr(&mut self, lhs: &Expr, op: &LogOp, rhs: &Expr) -> RunResult<Value> {
        let lval = self.eval(lhs)?;
        Ok(match op {
            LogOp::And => {
                if lval.is_truthy() {
                    self.eval(rhs)?
                } else {
                    lval
                }
            }
            LogOp::Or => {
                if lval.is_truthy() {
                    lval
                } else {
                    self.eval(rhs)?
                }
            }
        })
    }

    fn eval_call_expr(
        &mut self,
        callee: &Expr,
        arguments: &[Expr],
        line: usize,
    ) -> RunResult<Value> {
        let callee = self.eval(callee)?;

        let mut args = Vec::new();
        for arg in arguments {
            args.push(self.eval(arg)?);
        }

        let arity = match callee {
            Value::Class(ref class) => class.arity(),
            Value::Call(ref fun) => fun.arity(),
            Value::Fun(ref fun) => fun.arity(),
            _ => return Err(RuntimeError::NotCallable(line)),
        };

        if args.len() != arity {
            return Err(RuntimeError::ArityMismatch(line, arity, args.len()));
        }

        match callee {
            Value::Class(class) => class.call(self, args),
            Value::Call(fun) => fun.call(self, args),
            Value::Fun(fun) => fun.call(self, args),
            _ => Err(RuntimeError::NotCallable(line)),
        }
    }

    fn eval_get_expr(&mut self, object: &Expr, name: &str, line: usize) -> RunResult<Value> {
        match self.eval(object)? {
            Value::Instance(inst) => inst
                .get(&name)
                .ok_or_else(|| RuntimeError::UndefinedProp(line, name.to_owned())),
            _ => Err(RuntimeError::NotInstance(line)),
        }
    }

    fn eval_super_expr(&mut self, var: &Var, method: &str, line: usize) -> RunResult<Value> {
        let dist = self.hops.get(&var.id).cloned().unwrap();
        let superclass = match self.env.get_at(dist, &var.name).unwrap() {
            Value::Class(class) => class,
            _ => panic!(),
        };
        let instance = match self.env.get_at(dist - 1, "self").unwrap() {
            Value::Instance(inst) => inst,
            _ => panic!(),
        };
        superclass
            .find_method(method)
            .map(|fun| Value::Fun(fun.bind(instance)))
            .ok_or_else(|| RuntimeError::UndefinedProp(line, method.to_owned()))
    }

    fn lookup_var(&mut self, var: &Var) -> RunResult<Value> {
        let value = match self.hops.get(&var.id) {
            Some(dist) => self.env.get_at(*dist, &var.name),
            None => self.globals.get(&var.name),
        };
        value.ok_or_else(|| RuntimeError::UndefinedVar(var.line, var.name.clone()))
    }
}
