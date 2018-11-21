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
pub enum UniOp {
    Neg(Span),
    Not(Span),
}

/// Mostly language items that evaluate to values.
pub enum Expr {
    Binary(Box<Expr>, BinOp, Box<Expr>),
    Literal(Span),
    Unary(UniOp, Box<Expr>),
}

/// Mostly language items for control flow.
pub enum Stmt {
    Expression(Expr),
}

/// Mostly language items that introduce name bindings.
pub enum Decl {
    Statement(Stmt),
}
