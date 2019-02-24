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

/// Language items that evaluate to values.
pub enum Expr {
    Logical(Box<Expr>, LogOp, Box<Expr>),
    Binary(Box<Expr>, BinOp, Box<Expr>),
    Unary(UniOp, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>, usize),
    Literal(Primitive),
    Variable(String, usize),
    Group(Box<Expr>),
}

/// Language items for control flow or side effects.
pub enum Stmt {
    Return(Option<Expr>, usize),
    Assignment(String, Expr, usize),
    Expression(Expr),
    Block(Vec<Decl>),
}

/// Language items that introduce name bindings.
pub enum Decl {
    Function(String, Vec<String>, Rc<Vec<Decl>>),
    Let(String, Option<Expr>),
    Statement(Stmt),
}
