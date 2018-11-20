use item::{Decl, Expr, Stmt};
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

    fn declaration(&mut self) -> Option<Decl> {
        self.statement().map(|s| Decl::Statement(s))
    }

    fn statement(&mut self) -> Option<Stmt> {
        self.expression().map(|e| Stmt::Expression(e))
    }

    fn expression(&mut self) -> Option<Expr> {
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
