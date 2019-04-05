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
    Literal(Primitive),
    Variable(Var, usize),
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
    Expression(Expr),
    Block(Vec<Decl>),
}

/// Language items that introduce name bindings.
pub enum Decl {
    Function(String, Vec<(String, usize)>, Rc<Vec<Decl>>, usize),
    Let(String, Option<Expr>, usize),
    Statement(Stmt),
}
