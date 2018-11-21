use item::{Decl, Expr, Stmt};
use item::{BinOp, LogOp, UniOp};
use token::{Span, Token};

pub struct Parser {
    spans: Vec<Span>,
    pos: usize,
}

impl Parser {
    pub fn new(spans: Vec<Span>) -> Self {
        Self { spans, pos: 0 }
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
        self.pos >= self.spans.len() || self.spans[self.pos].token == Token::EOF
    }

    fn advance(&mut self) -> Span {
        let curr = self.spans[self.pos].clone();
        self.pos += 1;
        curr
    }

    fn matches(&mut self, token: &Token) -> Option<Span> {
        if self.is_at_end() {
            return None;
        }

        if self.spans[self.pos].token == *token {
            Some(self.advance())
        } else {
            None
        }
    }

    fn matches_any(&mut self, tokens: &[Token]) -> Option<Span> {
        for token in tokens {
            if let Some(span) = self.matches(token) {
                return Some(span);
            }
        }
        None
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

        while let Some(curr) = self.matches(&Token::Or) {
            let right = match self.logical_and() {
                Some(e) => e,
                None => return None,
            };
            expr = Expr::Logical(Box::new(expr), LogOp::Or(curr), Box::new(right));
        }

        Some(expr)
    }

    fn logical_and(&mut self) -> Option<Expr> {
        let mut expr = match self.equality() {
            Some(e) => e,
            None => return None,
        };

        while let Some(curr) = self.matches(&Token::And) {
            let right = match self.equality() {
                Some(e) => e,
                None => return None,
            };
            expr = Expr::Logical(Box::new(expr), LogOp::And(curr), Box::new(right));
        }

        Some(expr)
    }

    fn equality(&mut self) -> Option<Expr> {
        let mut expr = match self.comparison() {
            Some(e) => e,
            None => return None,
        };

        while let Some(curr) = self.matches_any(&[Token::EqEq, Token::NotEq]) {
            let right = match self.comparison() {
                Some(e) => e,
                None => return None,
            };
            let op = match curr.token {
                Token::EqEq => BinOp::EqEq(curr),
                Token::NotEq => BinOp::NotEq(curr),
                _ => return None,
            };
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        Some(expr)
    }

    fn comparison(&mut self) -> Option<Expr> {
        let mut expr = match self.addition() {
            Some(e) => e,
            None => return None,
        };

        while let Some(curr) = self.matches_any(&[Token::Lt, Token::LtEq, Token::Gt, Token::GtEq]) {
            let right = match self.addition() {
                Some(e) => e,
                None => return None,
            };
            let op = match curr.token {
                Token::Lt => BinOp::Lt(curr),
                Token::LtEq => BinOp::LtEq(curr),
                Token::Gt => BinOp::Gt(curr),
                Token::GtEq => BinOp::GtEq(curr),
                _ => return None,
            };
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        Some(expr)
    }

    fn addition(&mut self) -> Option<Expr> {
        let mut expr = match self.multiply() {
            Some(e) => e,
            None => return None,
        };

        while let Some(curr) = self.matches_any(&[Token::Plus, Token::Minus]) {
            let right = match self.multiply() {
                Some(e) => e,
                None => return None,
            };
            let op = match curr.token {
                Token::Plus => BinOp::Add(curr),
                Token::Minus => BinOp::Sub(curr),
                _ => return None,
            };
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        Some(expr)
    }

    fn multiply(&mut self) -> Option<Expr> {
        let mut expr = match self.unary() {
            Some(e) => e,
            None => return None,
        };

        while let Some(curr) = self.matches_any(&[Token::Star, Token::Slash, Token::Percent]) {
            let right = match self.unary() {
                Some(e) => e,
                None => return None,
            };
            let op = match curr.token {
                Token::Star => BinOp::Mul(curr),
                Token::Slash => BinOp::Div(curr),
                Token::Percent => BinOp::Rem(curr),
                _ => return None,
            };
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        Some(expr)
    }

    fn unary(&mut self) -> Option<Expr> {
        if let Some(curr) = self.matches_any(&[Token::Minus, Token::Not]) {
            let right = match self.unary() {
                Some(e) => e,
                None => return None,
            };
            let op = match curr.token {
                Token::Minus => UniOp::Neg(curr),
                Token::Not => UniOp::Not(curr),
                _ => return None,
            };
            return Some(Expr::Unary(op, Box::new(right)));
        }

        self.primary()
    }

    fn primary(&mut self) -> Option<Expr> {
        let curr = self.advance();

        let expr = match curr.token {
            Token::None_ | Token::True | Token::False => Expr::Literal(curr),
            Token::Num(_) | Token::Str(_) | Token::Ident(_) => Expr::Literal(curr),
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
        let spans = [Token::None_, Token::True, Token::False, Token::EOF]
            .iter()
            .map(|t| Span::new(t.clone(), 1))
            .collect::<Vec<_>>();
        let actual = Parser::new(spans).parse();

        assert_eq!(3, actual.len());
    }
}
