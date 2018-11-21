use item::{Decl, Expr, Stmt};

pub struct Interpreter;

impl Interpreter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run(&mut self, program: &[Decl]) {
        for decl in program {
            match decl {
                Decl::Statement(ref stmt) => match stmt {
                    Stmt::Expression(ref expr) => self.eval(expr),
                },
            }
        }
    }

    fn eval(&self, expr: &Expr) {
        match expr {
            Expr::Literal(ref span) => {
                println!("{:?}", span);
            }
            Expr::Unary(ref op, expr) => {
                println!("{:?}", op);
                self.eval(expr);
            }
            Expr::Binary(ref lhs, ref op, ref rhs) => {
                self.eval(lhs);
                println!("{:?}", op);
                self.eval(rhs);
            }
            Expr::Logical(ref lhs, ref op, ref rhs) => {
                self.eval(lhs);
                println!("{:?}", op);
                self.eval(rhs);
            }
        }
    }
}
