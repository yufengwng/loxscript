//! Module for higher-level syntactic language items.

use std::rc::Rc;

pub enum LogOp {
    And,
    Or,
}

pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Lt,
    LtEq,
    Gt,
    GtEq,
    EqEq,
    NotEq,
}

pub enum UniOp {
    Neg,
    Not,
}

pub enum Primitive {
    None,
    Bool(bool),
    Num(f64),
    Str(String),
}

pub struct Var {
    pub id: usize,
    pub name: String,
    pub line: usize,
}

impl Var {
    pub fn new(id: usize, name: String, line: usize) -> Self {
        Self { id, name, line }
    }
}

/// Language items that evaluate to values.
pub enum Expr {
    /// (line, lhs, op, rhs)
    Logical(usize, Box<Expr>, LogOp, Box<Expr>),
    /// (line, lhs, op, rhs)
    Binary(usize, Box<Expr>, BinOp, Box<Expr>),
    /// (line, op, expr)
    Unary(usize, UniOp, Box<Expr>),
    /// (line, callee, args)
    Call(usize, Box<Expr>, Vec<Expr>),
    /// (line, obj, name)
    Get(usize, Box<Expr>, String),
    /// (line, value)
    Literal(usize, Primitive),
    /// (var)
    Variable(Var),
    /// (var)
    Self_(Var),
    /// (var, method_line, method_name)
    Super(Var, usize, String),
    /// (inner)
    Group(Box<Expr>),
}

/// Language items for control flow or side effects.
pub enum Stmt {
    /// (init, condition, post, body)
    For(Option<Box<Decl>>, Expr, Option<Box<Stmt>>, Body),
    /// (branches, otherwise)
    If(Vec<(Expr, Body)>, Option<Body>),
    /// (condition, body)
    While(Expr, Body),
    /// (line)
    Break(usize),
    /// (line)
    Continue(usize),
    /// (line, value)
    Return(usize, Option<Expr>),
    /// (var, value)
    Assignment(Var, Expr),
    /// (line, object, name, value)
    Set(usize, Expr, String, Expr),
    /// (expr)
    Expression(Expr),
    /// (body)
    Block(Body),
}

pub type Body = Vec<Decl>;

pub struct Param {
    pub name: String,
    pub line: usize,
}

pub struct FunDecl {
    pub name: String,
    pub params: Vec<Param>,
    pub body: Body,
    pub line: usize,
}

/// Language items that introduce name bindings.
pub enum Decl {
    /// (line, name, superclass, methods)
    Class(usize, String, Option<Var>, Vec<Rc<FunDecl>>),
    /// (decl)
    Function(Rc<FunDecl>),
    /// (line, name, value)
    Let(usize, String, Option<Expr>),
    /// (stmt)
    Statement(Stmt),
}
