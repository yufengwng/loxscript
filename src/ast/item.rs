//! Module for higher-level syntactic language items.

use std::rc::Rc;

#[derive(Debug)]
pub enum LogOp {
    And(usize),
    Or(usize),
}

#[derive(Debug)]
pub enum BinOp {
    Add(usize),
    Sub(usize),
    Mul(usize),
    Div(usize),
    Rem(usize),
    Lt(usize),
    LtEq(usize),
    Gt(usize),
    GtEq(usize),
    EqEq(usize),
    NotEq(usize),
}

#[derive(Debug)]
pub enum UniOp {
    Neg(usize),
    Not(usize),
}

#[derive(Debug)]
pub enum Primitive {
    None(usize),
    Bool(bool, usize),
    Num(f64, usize),
    Str(String, usize),
}

#[derive(Debug)]
pub struct Var {
    pub id: usize,
    pub name: String,
}

impl Var {
    pub fn new(id: usize, name: String) -> Self {
        Self { id, name }
    }
}

/// Language items that evaluate to values.
pub enum Expr {
    Logical(Box<Expr>, LogOp, Box<Expr>),
    Binary(Box<Expr>, BinOp, Box<Expr>),
    Unary(UniOp, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>, usize),
    Get(Box<Expr>, String, usize),
    Literal(Primitive),
    Variable(Var, usize),
    Self_(Var, usize),
    Super(Var, usize, String, usize),
    Group(Box<Expr>),
}

/// Language items for control flow or side effects.
pub enum Stmt {
    For(Option<Box<Decl>>, Expr, Option<Box<Stmt>>, Vec<Decl>),
    If(Vec<(Expr, Vec<Decl>)>, Option<Vec<Decl>>),
    While(Expr, Vec<Decl>),
    Break(usize),
    Continue(usize),
    Return(Option<Expr>, usize),
    Assignment(Var, Expr, usize),
    Set(Expr, String, Expr, usize),
    Expression(Expr),
    Block(Vec<Decl>),
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
    Class(String, Option<Expr>, Vec<Rc<FunDecl>>, usize),
    Function(Rc<FunDecl>),
    Let(String, Option<Expr>, usize),
    Statement(Stmt),
}
