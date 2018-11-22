use item::{BinOp, LogOp, UniOp};
use item::{Decl, Expr, Primitive, Stmt};
use token::{Span, Token};

pub struct Parser {
    spans: Vec<Span>,
    idx: usize,
}

impl Parser {
    pub fn new(spans: Vec<Span>) -> Self {
        Self { spans, idx: 0 }
    }

    pub fn parse(&mut self) -> Vec<Decl> {
        let mut decls = Vec::new();

        while !self.is_at_end() {
            match self.declaration() {
                Some(decl) => decls.push(decl),
                None => {}
            }
        }

        decls
    }

    fn is_at_end(&self) -> bool {
        self.idx >= self.spans.len() || self.spans[self.idx].token == Token::EOF
    }

    fn advance(&mut self) -> &Span {
        let curr = &self.spans[self.idx];
        self.idx += 1;
        curr
    }

    fn consume_unary_op(&mut self) -> Option<UniOp> {
        if self.is_at_end() {
            return None;
        }

        let curr = &self.spans[self.idx];
        let op = match curr.token {
            Token::Minus => UniOp::Neg(curr.line),
            Token::Not => UniOp::Not(curr.line),
            _ => return None,
        };

        self.idx += 1;
        Some(op)
    }

    fn consume_multiplicative_op(&mut self) -> Option<BinOp> {
        if self.is_at_end() {
            return None;
        }

        let curr = &self.spans[self.idx];
        let op = match &curr.token {
            Token::Star => BinOp::Mul(curr.line),
            Token::Slash => BinOp::Div(curr.line),
            Token::Percent => BinOp::Rem(curr.line),
            _ => return None,
        };

        self.idx += 1;
        Some(op)
    }

    fn consume_additive_op(&mut self) -> Option<BinOp> {
        if self.is_at_end() {
            return None;
        }

        let curr = &self.spans[self.idx];
        let op = match &curr.token {
            Token::Plus => BinOp::Add(curr.line),
            Token::Minus => BinOp::Sub(curr.line),
            _ => return None,
        };

        self.idx += 1;
        Some(op)
    }

    fn consume_compare_op(&mut self) -> Option<BinOp> {
        if self.is_at_end() {
            return None;
        }

        let curr = &self.spans[self.idx];
        let op = match &curr.token {
            Token::Lt => BinOp::Lt(curr.line),
            Token::LtEq => BinOp::LtEq(curr.line),
            Token::Gt => BinOp::Gt(curr.line),
            Token::GtEq => BinOp::GtEq(curr.line),
            _ => return None,
        };

        self.idx += 1;
        Some(op)
    }

    fn consume_equality_op(&mut self) -> Option<BinOp> {
        if self.is_at_end() {
            return None;
        }

        let curr = &self.spans[self.idx];
        let op = match &curr.token {
            Token::EqEq => BinOp::EqEq(curr.line),
            Token::NotEq => BinOp::NotEq(curr.line),
            _ => return None,
        };

        self.idx += 1;
        Some(op)
    }

    fn consume_and_op(&mut self) -> Option<LogOp> {
        if self.is_at_end() {
            return None;
        }

        let curr = &self.spans[self.idx];
        if curr.token == Token::And {
            self.idx += 1;
            Some(LogOp::And(curr.line))
        } else {
            None
        }
    }

    fn consume_or_op(&mut self) -> Option<LogOp> {
        if self.is_at_end() {
            return None;
        }

        let curr = &self.spans[self.idx];
        if curr.token == Token::Or {
            self.idx += 1;
            Some(LogOp::Or(curr.line))
        } else {
            None
        }
    }

    fn declaration(&mut self) -> Option<Decl> {
        self.statement().map(|s| Decl::Statement(s))
    }

    fn statement(&mut self) -> Option<Stmt> {
        self.expression().map(|e| Stmt::Expression(e))
    }

    fn expression(&mut self) -> Option<Expr> {
        self.logical_or()
    }

    fn logical_or(&mut self) -> Option<Expr> {
        let mut expr = match self.logical_and() {
            Some(e) => e,
            None => return None,
        };

        while let Some(op) = self.consume_or_op() {
            let rhs = match self.logical_and() {
                Some(e) => e,
                None => return None,
            };
            expr = Expr::Logical(Box::new(expr), op, Box::new(rhs));
        }

        Some(expr)
    }

    fn logical_and(&mut self) -> Option<Expr> {
        let mut expr = match self.equality() {
            Some(e) => e,
            None => return None,
        };

        while let Some(op) = self.consume_and_op() {
            let rhs = match self.equality() {
                Some(e) => e,
                None => return None,
            };
            expr = Expr::Logical(Box::new(expr), op, Box::new(rhs));
        }

        Some(expr)
    }

    fn equality(&mut self) -> Option<Expr> {
        let mut expr = match self.comparison() {
            Some(e) => e,
            None => return None,
        };

        while let Some(op) = self.consume_equality_op() {
            let rhs = match self.comparison() {
                Some(e) => e,
                None => return None,
            };
            expr = Expr::Binary(Box::new(expr), op, Box::new(rhs));
        }

        Some(expr)
    }

    fn comparison(&mut self) -> Option<Expr> {
        let mut expr = match self.addition() {
            Some(e) => e,
            None => return None,
        };

        while let Some(op) = self.consume_compare_op() {
            let rhs = match self.addition() {
                Some(e) => e,
                None => return None,
            };
            expr = Expr::Binary(Box::new(expr), op, Box::new(rhs));
        }

        Some(expr)
    }

    fn addition(&mut self) -> Option<Expr> {
        let mut expr = match self.multiply() {
            Some(e) => e,
            None => return None,
        };

        while let Some(op) = self.consume_additive_op() {
            let rhs = match self.multiply() {
                Some(e) => e,
                None => return None,
            };
            expr = Expr::Binary(Box::new(expr), op, Box::new(rhs));
        }

        Some(expr)
    }

    fn multiply(&mut self) -> Option<Expr> {
        let mut expr = match self.unary() {
            Some(e) => e,
            None => return None,
        };

        while let Some(op) = self.consume_multiplicative_op() {
            let rhs = match self.unary() {
                Some(e) => e,
                None => return None,
            };
            expr = Expr::Binary(Box::new(expr), op, Box::new(rhs));
        }

        Some(expr)
    }

    fn unary(&mut self) -> Option<Expr> {
        if let Some(op) = self.consume_unary_op() {
            let rhs = match self.unary() {
                Some(e) => e,
                None => return None,
            };
            return Some(Expr::Unary(op, Box::new(rhs)));
        }

        self.primary()
    }

    fn primary(&mut self) -> Option<Expr> {
        let curr = self.advance();

        let expr = match &curr.token {
            Token::None => Expr::Literal(Primitive::None(curr.line)),
            Token::True => Expr::Literal(Primitive::Bool(true, curr.line)),
            Token::False => Expr::Literal(Primitive::Bool(false, curr.line)),
            Token::Num(n) => Expr::Literal(Primitive::Num(n.clone(), curr.line)),
            Token::Str(s) => Expr::Literal(Primitive::Str(s.clone(), curr.line)),
            _ => return None,
        };

        Some(expr)
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

        assert_eq!(3, actual.len());
    }
}
