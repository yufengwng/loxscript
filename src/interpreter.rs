use std::io::{self, Write};

use item::{Decl, Expr, Stmt};
use token::Token;

pub struct Interpreter;

impl Interpreter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run(&mut self, program: &[Decl]) {
        for decl in program {
            match decl {
                Decl::Statement(ref stmt) => match stmt {
                    Stmt::Expression(ref expr) => match expr {
                        Expr::Literal(ref span) => {
                            println!("{:?}", span.token);
                            match span.token {
                                Token::None_ => println!("none"),
                                Token::True => println!("true"),
                                Token::False => println!("false"),
                                Token::Num(n) => println!("{}", n),
                                Token::Str(ref s) => println!("{}", s),
                                Token::Ident(ref name) => println!("{}", name),
                                _ => {}
                            }
                            io::stdout().flush().unwrap();
                        }
                    },
                },
            }
        }
    }
}
