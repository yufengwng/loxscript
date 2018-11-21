//! Module for higher-level syntactic language items.

use token::Span;

#[derive(Debug)]
pub enum BinOp {
    Add(Span),
    Sub(Span),
    Mul(Span),
    Div(Span),
    Rem(Span),
    Lt(Span),
    LtEq(Span),
    Gt(Span),
    GtEq(Span),
    EqEq(Span),
    NotEq(Span),
}

#[derive(Debug)]
pub enum LogOp {
    And(Span),
    Or(Span),
}

#[derive(Debug)]
pub enum UniOp {
    Neg(Span),
    Not(Span),
}

/// Mostly language items that evaluate to values.
pub enum Expr {
    Logical(Box<Expr>, LogOp, Box<Expr>),
    Binary(Box<Expr>, BinOp, Box<Expr>),
    Unary(UniOp, Box<Expr>),
    Literal(Span),
}

/// Mostly language items for control flow.
pub enum Stmt {
    Expression(Expr),
}

/// Mostly language items that introduce name bindings.
pub enum Decl {
    Statement(Stmt),
}
