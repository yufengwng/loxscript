use std::error;
use std::fmt;
use std::rc::Rc;

use crate::ast::{BinOp, LogOp, UniOp};
use crate::ast::{Decl, Expr, Primitive, Stmt, Var};
use crate::ast::{Span, Token};
use crate::ParsedProgram;

const MAX_FN_ARITY: usize = 8;

static mut VAR_ID: usize = 0;

fn next_var_id() -> usize {
    unsafe {
        let id = VAR_ID;
        VAR_ID += 1;
        id
    }
}

#[derive(Debug)]
enum ParseError {
    InvalidAssignTarget(Span),
    MaxArgs(Span),
    MaxParams(Span),
    MissingExpr(Span),
    NotConsumed(Span, String),
}

impl error::Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::InvalidAssignTarget(span) => write!(
                f,
                "[line {}] parse error at '{}': invalid assignment target",
                span.line, span.token
            ),
            ParseError::MaxArgs(span) => write!(
                f,
                "[line {}] parse error at '{}': cannot have more than {} arguments",
                span.line, span.token, MAX_FN_ARITY
            ),
            ParseError::MaxParams(span) => write!(
                f,
                "[line {}] parse error at '{}': cannot have more than {} parameters",
                span.line, span.token, MAX_FN_ARITY
            ),
            ParseError::MissingExpr(span) => write!(
                f,
                "[line {}] parse error at '{}': expected an expression",
                span.line, span.token
            ),
            ParseError::NotConsumed(span, msg) => write!(
                f,
                "[line {}] parse error at '{}': {}",
                span.line, span.token, msg
            ),
        }
    }
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

    pub fn parse(mut self) -> ParsedProgram {
        let mut decls = Vec::new();

        while !self.is_at_end() {
            match self.declaration() {
                Ok(decl) => decls.push(decl),
                Err(err) => {
                    self.log_err(err);
                    self.synchronize();
                }
            }
        }

        ParsedProgram {
            errored: self.had_error,
            decls,
        }
    }

    fn declaration(&mut self) -> Result<Decl, ParseError> {
        if self.matches(&Token::Class) {
            self.class_declaration()
        } else if self.matches(&Token::Fun) {
            self.fun_declaration("function")
        } else if self.matches(&Token::Let) {
            self.let_declaration()
        } else {
            self.statement().map(Decl::Statement)
        }
    }

    fn class_declaration(&mut self) -> Result<Decl, ParseError> {
        let (name, line) = self.consume_ident("expected class name")?;

        let superclass = if self.matches(&Token::Lt) {
            let (name, line) = self.consume_ident("expected superclass name")?;
            Some(Expr::Variable(Var::new(next_var_id(), name), line))
        } else {
            None
        };

        self.consume(&Token::Lbrace, "expected '{' before class body")?;

        let mut methods = Vec::new();
        while !self.is_at_end() && !self.check(&Token::Rbrace) {
            methods.push(self.fun_declaration("method")?);
        }

        self.consume(&Token::Rbrace, "expected '}' after class body")?;
        Ok(Decl::Class(name, superclass, Rc::new(methods), line))
    }

    fn fun_declaration(&mut self, kind: &'static str) -> Result<Decl, ParseError> {
        let msg = format!("expected {} name", kind);
        let (name, line) = self.consume_ident(&msg)?;

        let msg = format!("expected '(' after {} name", kind);
        self.consume(&Token::Lparen, &msg)?;

        let mut params = Vec::new();
        if !self.check(&Token::Rparen) {
            let param = self.consume_ident("expected parameter name")?;
            params.push(param);
            while self.matches(&Token::Comma) {
                if params.len() >= MAX_FN_ARITY {
                    self.log_err(ParseError::MaxParams(self.peek().clone()));
                }
                let param = self.consume_ident("expected parameter name")?;
                params.push(param);
            }
        }

        self.consume(&Token::Rparen, "expected ')' after parameters")?;

        let msg = format!("expected '{{' before {} body", kind);
        self.consume(&Token::Lbrace, &msg)?;

        let body = self.block()?;
        Ok(Decl::Function(name, params, Rc::new(body), line))
    }

    fn let_declaration(&mut self) -> Result<Decl, ParseError> {
        let (name, line) = self.consume_ident("expected variable name")?;

        let init = if self.matches(&Token::Eq) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(&Token::Semi, "expected ';' after variable declaration")?;
        Ok(Decl::Let(name, init, line))
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        if self.matches(&Token::For) {
            return self.for_statement();
        } else if self.matches(&Token::If) {
            return self.if_statement();
        } else if self.matches(&Token::While) {
            return self.while_statement();
        } else if self.matches(&Token::Break) {
            let keyword = self.consume(&Token::Semi, "expected ';' after break")?;
            return Ok(Stmt::Break(keyword.line));
        } else if self.matches(&Token::Cont) {
            let keyword = self.consume(&Token::Semi, "expected ';' after continue")?;
            return Ok(Stmt::Continue(keyword.line));
        } else if self.check(&Token::Ret) {
            return self.return_statement();
        } else if self.matches(&Token::Lbrace) {
            return Ok(Stmt::Block(self.block()?));
        }

        let expr = self.expression()?;
        if self.check(&Token::Eq) {
            self.finish_assignment(expr, true)
        } else {
            self.consume(&Token::Semi, "expected ';' after expression")?;
            Ok(Stmt::Expression(expr))
        }
    }

    fn finish_assignment(&mut self, expr: Expr, has_semi: bool) -> Result<Stmt, ParseError> {
        let equals = self.advance().clone();
        let value = self.expression()?;
        match expr {
            Expr::Variable(var, line) => {
                if has_semi {
                    self.consume(&Token::Semi, "expected ';' after assignment")?;
                }
                Ok(Stmt::Assignment(var, value, line))
            }
            Expr::Get(object, name, line) => {
                if has_semi {
                    self.consume(&Token::Semi, "expected ';' after assignment")?;
                }
                Ok(Stmt::Set(*object, name, value, line))
            }
            _ => {
                self.log_err(ParseError::InvalidAssignTarget(equals));
                if has_semi {
                    self.consume(&Token::Semi, "expected ';' after expression")?;
                }
                Ok(Stmt::Expression(expr))
            }
        }
    }

    fn for_statement(&mut self) -> Result<Stmt, ParseError> {
        let init_decl = if self.matches(&Token::Semi) {
            None
        } else if self.matches(&Token::Let) {
            Some(Box::new(self.let_declaration()?))
        } else {
            let expr = self.expression()?;
            if self.check(&Token::Eq) {
                Some(Box::new(Decl::Statement(
                    self.finish_assignment(expr, true)?,
                )))
            } else {
                self.consume(&Token::Semi, "expected ';' after expression")?;
                Some(Box::new(Decl::Statement(Stmt::Expression(expr))))
            }
        };

        let cond_expr = if self.check(&Token::Semi) {
            let line = self.advance().line;
            Expr::Literal(Primitive::Bool(true, line))
        } else {
            let expr = self.expression()?;
            self.consume(&Token::Semi, "expected ';' after loop condition")?;
            expr
        };

        let post_stmt = if self.check(&Token::Lbrace) {
            None
        } else {
            let expr = self.expression()?;
            if self.check(&Token::Eq) {
                Some(Box::new(self.finish_assignment(expr, false)?))
            } else {
                Some(Box::new(Stmt::Expression(expr)))
            }
        };

        self.consume(&Token::Lbrace, "expected '{' after for clauses")?;
        let body = self.block()?;

        Ok(Stmt::For(init_decl, cond_expr, post_stmt, body))
    }

    fn if_statement(&mut self) -> Result<Stmt, ParseError> {
        let mut branches = Vec::new();

        let cond = self.expression()?;
        self.consume(&Token::Lbrace, "expected '{' after if condition")?;
        let then = self.block()?;
        branches.push((cond, then));

        while self.matches(&Token::Elif) {
            let elif_cond = self.expression()?;
            self.consume(&Token::Lbrace, "expected '{' after elif condition")?;
            let elif_then = self.block()?;
            branches.push((elif_cond, elif_then));
        }

        let otherwise = if self.matches(&Token::Else) {
            self.consume(&Token::Lbrace, "expected '{' after else")?;
            Some(self.block()?)
        } else {
            None
        };

        Ok(Stmt::If(branches, otherwise))
    }

    fn while_statement(&mut self) -> Result<Stmt, ParseError> {
        let cond = self.expression()?;
        self.consume(&Token::Lbrace, "expected '{' after while condition")?;
        let body = self.block()?;
        Ok(Stmt::While(cond, body))
    }

    fn return_statement(&mut self) -> Result<Stmt, ParseError> {
        let line = self.advance().line;
        let value = if !self.check(&Token::Semi) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(&Token::Semi, "expected ';' after return value")?;
        Ok(Stmt::Return(value, line))
    }

    fn block(&mut self) -> Result<Vec<Decl>, ParseError> {
        let mut decls = Vec::new();
        while !self.is_at_end() && self.peek().token != Token::Rbrace {
            decls.push(self.declaration()?);
        }
        self.consume(&Token::Rbrace, "expected '}' after block")?;
        Ok(decls)
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
            Ok(Expr::Unary(op, Box::new(rhs)))
        } else {
            self.parse_call()
        }
    }

    fn parse_call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;
        loop {
            if self.matches(&Token::Lparen) {
                expr = self.finish_call(expr)?;
            } else if self.matches(&Token::Dot) {
                let (name, line) = self.consume_ident("expected property name after '.'")?;
                expr = Expr::Get(Box::new(expr), name, line);
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, expr: Expr) -> Result<Expr, ParseError> {
        let mut args = Vec::new();
        if !self.check(&Token::Rparen) {
            args.push(self.expression()?);
            while self.matches(&Token::Comma) {
                if args.len() >= MAX_FN_ARITY {
                    self.log_err(ParseError::MaxArgs(self.peek().clone()));
                }
                args.push(self.expression()?);
            }
        }
        let paren = self.consume(&Token::Rparen, "expected ')' after arguments")?;
        Ok(Expr::Call(Box::new(expr), args, paren.line))
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        let curr = self.peek().clone();

        let expr = match curr.token {
            Token::None => Expr::Literal(Primitive::None(curr.line)),
            Token::True => Expr::Literal(Primitive::Bool(true, curr.line)),
            Token::False => Expr::Literal(Primitive::Bool(false, curr.line)),
            Token::Num(n) => Expr::Literal(Primitive::Num(n, curr.line)),
            Token::Str(s) => Expr::Literal(Primitive::Str(s, curr.line)),
            Token::Ident(s) => Expr::Variable(Var::new(next_var_id(), s), curr.line),
            Token::Self_ => Expr::Self_(Var::new(next_var_id(), String::from("self")), curr.line),
            Token::Super => {
                self.advance();
                self.consume(&Token::Dot, "expected '.' after 'super'")?;
                let (method, line) = self.consume_ident("expected superclass method name")?;
                return Ok(Expr::Super(
                    Var::new(next_var_id(), String::from("super")),
                    curr.line,
                    method,
                    line,
                ));
            }
            Token::Lparen => {
                self.advance();
                let expr = self.expression()?;
                self.consume(&Token::Rparen, "expected ')' after expression")?;
                return Ok(Expr::Group(Box::new(expr)));
            }
            _ => return Err(ParseError::MissingExpr(curr)),
        };

        self.advance();
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

    fn check(&self, token: &Token) -> bool {
        !self.is_at_end() && self.peek().token == *token
    }

    fn matches(&mut self, token: &Token) -> bool {
        if !self.is_at_end() && self.peek().token == *token {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume(&mut self, token: &Token, msg: &str) -> Result<Span, ParseError> {
        if !self.is_at_end() && self.peek().token == *token {
            Ok(self.advance().clone())
        } else {
            Err(ParseError::NotConsumed(self.peek().clone(), msg.to_owned()))
        }
    }

    fn consume_ident(&mut self, msg: &str) -> Result<(String, usize), ParseError> {
        if !self.is_at_end() {
            let curr = self.peek().clone();
            if let Token::Ident(name) = curr.token {
                self.advance();
                return Ok((name, curr.line));
            }
        }
        Err(ParseError::NotConsumed(self.peek().clone(), msg.to_owned()))
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

        self.advance();
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

        self.advance();
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

        self.advance();
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

        self.advance();
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

        self.advance();
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

        self.advance();
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

        self.advance();
        Some(op)
    }

    fn log_err(&mut self, err: ParseError) {
        eprintln!("{}", err);
        self.had_error = true;
    }

    fn synchronize(&mut self) {
        let mut curr = self.advance();
        if curr.token == Token::Semi {
            return;
        }
        while !self.is_at_end() {
            curr = self.peek();
            match curr.token {
                Token::Class
                | Token::Fun
                | Token::Let
                | Token::For
                | Token::If
                | Token::While
                | Token::Ret => return,
                Token::Semi => {
                    self.advance();
                    return;
                }
                _ => {}
            }
            self.advance();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_literal_expression_statement() {
        let spans = [
            Token::None,
            Token::Semi,
            Token::True,
            Token::Semi,
            Token::False,
            Token::Semi,
            Token::EOF,
        ]
        .iter()
        .map(|t| Span::new(t.clone(), 1))
        .collect::<Vec<_>>();
        let actual = Parser::new(spans).parse();

        assert_eq!(3, actual.decls.len());
    }
}
