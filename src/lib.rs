mod ast;
mod runtime;
mod stdlib;

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
    pub hops: std::collections::HashMap<usize, usize>,
}
