use std::error;
use std::fmt;

use crate::ast::{BinOp, LogOp, UniOp};
use crate::ast::{Decl, Expr, Primitive, Stmt};
use crate::ast::{Span, Token};

#[derive(Debug)]
enum ParseError {
    MissingExpr(Span),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ParseError::MissingExpr(ref span) => write!(
                f,
                "[line {}] parse error at '{}': expected an expression",
                span.line, span.token
            ),
        }
    }
}

impl error::Error for ParseError {}

pub struct ParseReport {
    pub decls: Vec<Decl>,
    pub had_error: bool,
}

pub struct Parser {
    spans: Vec<Span>,
    idx: usize,
    had_error: bool,
}

impl Parser {
    pub fn new(spans: Vec<Span>) -> Self {
        Self {
            spans,
            idx: 0,
            had_error: false,
        }
    }

    pub fn parse(mut self) -> ParseReport {
        let mut decls = Vec::new();

        while !self.is_at_end() {
            match self.declaration() {
                Ok(decl) => decls.push(decl),
                Err(err) => {
                    eprintln!("{}", err);
                    self.had_error = true;
                    self.synchronize();
                }
            }
        }

        ParseReport {
            decls,
            had_error: self.had_error,
        }
    }

    fn declaration(&mut self) -> Result<Decl, ParseError> {
        self.statement().map(Decl::Statement)
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        self.expression().map(Stmt::Expression)
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.logical_or()
    }

    fn logical_or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.logical_and()?;

        while let Some(op) = self.consume_or_op() {
            let rhs = self.logical_and()?;
            expr = Expr::Logical(Box::new(expr), op, Box::new(rhs));
        }

        Ok(expr)
    }

    fn logical_and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.equality()?;

        while let Some(op) = self.consume_and_op() {
            let rhs = self.equality()?;
            expr = Expr::Logical(Box::new(expr), op, Box::new(rhs));
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison()?;

        while let Some(op) = self.consume_equality_op() {
            let rhs = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(rhs));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.addition()?;

        while let Some(op) = self.consume_compare_op() {
            let rhs = self.addition()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(rhs));
        }

        Ok(expr)
    }

    fn addition(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.multiply()?;

        while let Some(op) = self.consume_additive_op() {
            let rhs = self.multiply()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(rhs));
        }

        Ok(expr)
    }

    fn multiply(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;

        while let Some(op) = self.consume_multiplicative_op() {
            let rhs = self.unary()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(rhs));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if let Some(op) = self.consume_unary_op() {
            let rhs = self.unary()?;
            return Ok(Expr::Unary(op, Box::new(rhs)));
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        let curr = self.peek();

        let expr = match &curr.token {
            Token::None => Expr::Literal(Primitive::None(curr.line)),
            Token::True => Expr::Literal(Primitive::Bool(true, curr.line)),
            Token::False => Expr::Literal(Primitive::Bool(false, curr.line)),
            Token::Num(n) => Expr::Literal(Primitive::Num(*n, curr.line)),
            Token::Str(s) => Expr::Literal(Primitive::Str(s.clone(), curr.line)),
            _ => return Err(ParseError::MissingExpr(curr.clone())),
        };

        self.consume();
        Ok(expr)
    }

    fn is_at_end(&self) -> bool {
        self.idx >= self.spans.len() || self.spans[self.idx].token == Token::EOF
    }

    fn advance(&mut self) -> &Span {
        let curr = &self.spans[self.idx];
        self.idx += 1;
        curr
    }

    fn peek(&self) -> &Span {
        &self.spans[self.idx]
    }

    fn consume(&mut self) {
        self.idx += 1;
    }

    fn consume_unary_op(&mut self) -> Option<UniOp> {
        if self.is_at_end() {
            return None;
        }

        let curr = self.peek();
        let op = match &curr.token {
            Token::Minus => UniOp::Neg(curr.line),
            Token::Not => UniOp::Not(curr.line),
            _ => return None,
        };

        self.consume();
        Some(op)
    }

    fn consume_multiplicative_op(&mut self) -> Option<BinOp> {
        if self.is_at_end() {
            return None;
        }

        let curr = self.peek();
        let op = match &curr.token {
            Token::Star => BinOp::Mul(curr.line),
            Token::Slash => BinOp::Div(curr.line),
            Token::Percent => BinOp::Rem(curr.line),
            _ => return None,
        };

        self.consume();
        Some(op)
    }

    fn consume_additive_op(&mut self) -> Option<BinOp> {
        if self.is_at_end() {
            return None;
        }

        let curr = self.peek();
        let op = match &curr.token {
            Token::Plus => BinOp::Add(curr.line),
            Token::Minus => BinOp::Sub(curr.line),
            _ => return None,
        };

        self.consume();
        Some(op)
    }

    fn consume_compare_op(&mut self) -> Option<BinOp> {
        if self.is_at_end() {
            return None;
        }

        let curr = self.peek();
        let op = match &curr.token {
            Token::Lt => BinOp::Lt(curr.line),
            Token::LtEq => BinOp::LtEq(curr.line),
            Token::Gt => BinOp::Gt(curr.line),
            Token::GtEq => BinOp::GtEq(curr.line),
            _ => return None,
        };

        self.consume();
        Some(op)
    }

    fn consume_equality_op(&mut self) -> Option<BinOp> {
        if self.is_at_end() {
            return None;
        }

        let curr = self.peek();
        let op = match &curr.token {
            Token::EqEq => BinOp::EqEq(curr.line),
            Token::NotEq => BinOp::NotEq(curr.line),
            _ => return None,
        };

        self.consume();
        Some(op)
    }

    fn consume_and_op(&mut self) -> Option<LogOp> {
        if self.is_at_end() {
            return None;
        }

        let curr = self.peek();
        let op = match &curr.token {
            Token::And => LogOp::And(curr.line),
            _ => return None,
        };

        self.consume();
        Some(op)
    }

    fn consume_or_op(&mut self) -> Option<LogOp> {
        if self.is_at_end() {
            return None;
        }

        let curr = self.peek();
        let op = match &curr.token {
            Token::Or => LogOp::Or(curr.line),
            _ => return None,
        };

        self.consume();
        Some(op)
    }

    fn synchronize(&mut self) {
        self.advance();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_literal_expression_statement() {
        let spans = [Token::None, Token::True, Token::False, Token::EOF]
            .iter()
            .map(|t| Span::new(t.clone(), 1))
            .collect::<Vec<_>>();
        let actual = Parser::new(spans).parse();

        assert_eq!(3, actual.decls.len());
    }
}
