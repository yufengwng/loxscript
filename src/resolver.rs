use std::collections::HashMap;
use std::error;
use std::fmt;

use crate::ast::{Decl, Expr, Stmt};
use crate::ResolvedProgram;

#[derive(Debug)]
enum ResolveError {
    AlreadyDeclared(usize, String),
    OwnInitializer(usize, String),
    TopReturn(usize),
}

impl error::Error for ResolveError {}

impl fmt::Display for ResolveError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ResolveError::AlreadyDeclared(line, name) => write!(
                f,
                "[line {}] resolve error at '{}': variable with this name already declared in this scope",
                line, name
            ),
            ResolveError::OwnInitializer(line, name) => write!(
                f,
                "[line {}] resolve error at '{}': cannot read local variable in its own initializer",
                line, name
            ),
            ResolveError::TopReturn(line) => write!(
                f,
                "[line {}] resolve error at 'return': cannot return from top-level code",
                line
            ),
        }
    }
}

#[derive(Clone, PartialEq)]
enum FunType {
    None,
    Function,
}

pub struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
    hops: HashMap<usize, usize>,
    had_error: bool,
    curr_fun: FunType,
}

impl Default for Resolver {
    fn default() -> Self {
        Self::new()
    }
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            scopes: Vec::new(),
            hops: HashMap::new(),
            had_error: false,
            curr_fun: FunType::None,
        }
    }

    pub fn resolve(mut self, program: Vec<Decl>) -> ResolvedProgram {
        self.resolve_all(&program);
        ResolvedProgram {
            errored: self.had_error,
            decls: program,
            hops: self.hops,
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &str, line: usize) {
        if self
            .scopes
            .last_mut()
            .and_then(|map| map.insert(name.to_owned(), false))
            .is_some()
        {
            self.log_err(ResolveError::AlreadyDeclared(line, name.to_owned()));
        }
    }

    fn define(&mut self, name: &str) {
        self.scopes
            .last_mut()
            .and_then(|map| map.insert(name.to_owned(), true));
    }

    fn save_hops(&mut self, id: usize, name: &str) {
        let index = self
            .scopes
            .iter()
            .rev()
            .enumerate()
            .find(|(_, map)| map.contains_key(name))
            .map(|(idx, _)| idx);
        if let Some(idx) = index {
            self.hops.insert(id, idx);
        }
        // Not found, assume global.
    }

    fn resolve_all(&mut self, decls: &[Decl]) {
        for decl in decls {
            self.resolve_declare(decl);
        }
    }

    fn resolve_function(&mut self, params: &[(String, usize)], body: &[Decl], kind: FunType) {
        let prev = self.curr_fun.clone();
        self.curr_fun = kind;

        self.begin_scope();
        for (param, line) in params {
            self.declare(param, *line);
            self.define(param);
        }
        self.resolve_all(body);
        self.end_scope();

        self.curr_fun = prev;
    }

    fn resolve_block(&mut self, body: &[Decl]) {
        self.begin_scope();
        self.resolve_all(body);
        self.end_scope();
    }

    fn resolve_declare(&mut self, decl: &Decl) {
        match decl {
            Decl::Class(name, _methods, line) => {
                self.declare(name, *line);
                self.define(name);
            }
            Decl::Function(name, params, body, line) => {
                self.declare(name, *line);
                self.define(name);
                self.resolve_function(params, body, FunType::Function);
            }
            Decl::Let(name, init, line) => {
                self.declare(name, *line);
                if let Some(expr) = init {
                    self.resolve_expression(expr);
                }
                self.define(name);
            }
            Decl::Statement(stmt) => self.resolve_statement(stmt),
        }
    }

    fn resolve_statement(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::For(init, cond, post, body) => {
                self.begin_scope();
                if let Some(decl) = init {
                    self.resolve_declare(decl);
                }
                self.resolve_expression(cond);
                if let Some(stmt) = post {
                    self.resolve_statement(stmt);
                }
                self.resolve_block(body);
                self.end_scope();
            }
            Stmt::If(branches, otherwise) => {
                for (cond, body) in branches {
                    self.resolve_expression(cond);
                    self.resolve_block(body);
                }
                if let Some(body) = otherwise {
                    self.resolve_block(body);
                }
            }
            Stmt::While(cond, body) => {
                self.resolve_expression(cond);
                self.resolve_block(body);
            }
            Stmt::Break(_) => {}
            Stmt::Continue(_) => {}
            Stmt::Return(expr, line) => {
                if self.curr_fun == FunType::None {
                    self.log_err(ResolveError::TopReturn(*line));
                }
                if let Some(e) = expr {
                    self.resolve_expression(e);
                }
            }
            Stmt::Assignment(var, expr, _) => {
                self.resolve_expression(expr);
                self.save_hops(var.id, &var.name);
            }
            Stmt::Set(object, _, value, _) => {
                self.resolve_expression(value);
                self.resolve_expression(object);
            }
            Stmt::Expression(expr) => self.resolve_expression(expr),
            Stmt::Block(decls) => self.resolve_block(decls),
        }
    }

    fn resolve_expression(&mut self, expr: &Expr) {
        match expr {
            Expr::Literal(_) => {}
            Expr::Unary(_, expr) => {
                self.resolve_expression(expr);
            }
            Expr::Binary(lhs, _, rhs) => {
                self.resolve_expression(lhs);
                self.resolve_expression(rhs);
            }
            Expr::Logical(lhs, _, rhs) => {
                self.resolve_expression(lhs);
                self.resolve_expression(rhs);
            }
            Expr::Call(expr, arguments, _) => {
                self.resolve_expression(expr);
                for arg in arguments {
                    self.resolve_expression(arg);
                }
            }
            Expr::Get(object, _, _) => {
                self.resolve_expression(object);
            }
            Expr::Variable(var, line) => {
                if let Some(false) = self.scopes.last().and_then(|map| map.get(&var.name)) {
                    self.log_err(ResolveError::OwnInitializer(*line, var.name.clone()));
                }
                self.save_hops(var.id, &var.name);
            }
            Expr::Group(inner) => self.resolve_expression(inner),
        }
    }

    fn log_err(&mut self, err: ResolveError) {
        eprintln!("{}", err);
        self.had_error = true;
    }
}
