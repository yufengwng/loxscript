use std::collections::HashMap;

pub mod ast;
pub mod runtime;
pub mod stdlib;

mod interpreter;
mod lexer;
mod parser;
mod resolver;

pub use interpreter::Interpreter;
pub use lexer::Lexer;
pub use parser::Parser;
pub use resolver::Resolver;

pub struct LexedProgram {
    pub errored: bool,
    pub spans: Vec<ast::Span>,
}

pub struct ParsedProgram {
    pub errored: bool,
    pub decls: Vec<ast::Decl>,
}

pub struct ResolvedProgram {
    pub errored: bool,
    pub decls: Vec<ast::Decl>,
    pub hops: HashMap<usize, usize>,
}
