use std::collections::HashMap;
use std::error;
use std::fmt;

use crate::ast::FunDecl;
use crate::ast::{Decl, Expr, Stmt};
use crate::ResolvedProgram;

#[derive(Debug)]
enum ResolveError {
    AlreadyDeclared(usize, String),
    InheritSelf(usize, String),
    InitReturn(usize),
    InvalidSelf(usize),
    OwnInitializer(usize, String),
    SuperNotClass(usize),
    SuperNotSub(usize),
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
            ResolveError::InheritSelf(line, name) => write!(
                f,
                "[line {}] resolve error at '{}': a class cannot inherit from itself",
                line, name
            ),
            ResolveError::InitReturn(line) => write!(
                f,
                "[line {}] resolve error at 'return': cannot return a value from an initializer",
                line
            ),
            ResolveError::InvalidSelf(line) => write!(
                f,
                "[line {}] resolve error at 'self': cannot use 'self' outside of a class",
                line
            ),
            ResolveError::OwnInitializer(line, name) => write!(
                f,
                "[line {}] resolve error at '{}': cannot read local variable in its own initializer",
                line, name
            ),
            ResolveError::SuperNotClass(line) => write!(
                f,
                "[line {}] resolve error at 'super': cannot use 'super' outside of a class",
                line
            ),
            ResolveError::SuperNotSub(line) => write!(
                f,
                "[line {}] resolve error at 'super': cannot use 'super' in a class with no superclass",
                line
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
    Method,
    Init,
}

#[derive(Clone, PartialEq)]
enum ClassType {
    None,
    Class,
    Subclass,
}

pub struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
    hops: HashMap<usize, usize>,
    had_error: bool,
    curr_fun: FunType,
    curr_class: ClassType,
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
            curr_class: ClassType::None,
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

    fn resolve_function(&mut self, decl: &FunDecl, kind: FunType) {
        self.declare(&decl.name, decl.line);
        self.define(&decl.name);

        let prev = self.curr_fun.clone();
        self.curr_fun = kind;

        self.begin_scope();
        for param in &decl.params {
            self.declare(&param.name, param.line);
            self.define(&param.name);
        }
        self.resolve_all(&decl.body);
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
            Decl::Class(name, superclass, methods, line) => {
                let prev = self.curr_class.clone();
                self.curr_class = ClassType::Class;

                self.declare(name, *line);
                self.define(name);

                if let Some(super_expr) = superclass {
                    if let Expr::Variable(var, _) = super_expr {
                        if var.name == *name {
                            self.log_err(ResolveError::InheritSelf(*line, var.name.to_owned()));
                        }
                    }
                    self.curr_class = ClassType::Subclass;
                    self.resolve_expression(super_expr);
                    self.begin_scope();
                    self.define("super");
                }

                self.begin_scope();
                self.define("self");

                for method in methods {
                    let kind = if method.name == "init" {
                        FunType::Init
                    } else {
                        FunType::Method
                    };
                    self.resolve_function(method, kind);
                }

                self.end_scope();
                if superclass.is_some() {
                    self.end_scope();
                }
                self.curr_class = prev;
            }
            Decl::Function(decl) => {
                self.resolve_function(decl, FunType::Function);
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
                    if self.curr_fun == FunType::Init {
                        self.log_err(ResolveError::InitReturn(*line));
                    }
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
            Expr::Self_(var, line) => {
                if ClassType::None == self.curr_class {
                    self.log_err(ResolveError::InvalidSelf(*line));
                } else {
                    self.save_hops(var.id, &var.name);
                }
            }
            Expr::Super(var, line, _, _) => {
                match self.curr_class {
                    ClassType::None => self.log_err(ResolveError::SuperNotClass(*line)),
                    ClassType::Class => self.log_err(ResolveError::SuperNotSub(*line)),
                    ClassType::Subclass => {}
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
