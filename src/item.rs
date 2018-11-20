//! Module for higher-level syntactic language items.

use token::Span;

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
    Eq_,
    EqEq,
    NotEq,
}

/// Mostly language items that evaluate to values.
pub enum Expr {
    // Binary(Box<Expr>, BinOp, Box<Expr>),
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
