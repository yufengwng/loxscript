use crate::bytecode::Chunk;
use crate::bytecode::OpCode;
use crate::bytecode::MAX_CONST_INDEX;
use crate::debug;
use crate::scan::Scanner;
use crate::scan::Span;
use crate::scan::Token;
use crate::value::Value;

pub fn compile(source: String) -> Option<Chunk> {
    Compiler::new(source).compile()
}

struct Compiler {
    scanner: Scanner,
    chunk: Chunk,
    curr: Option<Span>,
    prev: Option<Span>,
    had_error: bool,
    panic_mode: bool,
}

impl Compiler {
    pub fn new(source: String) -> Self {
        Self {
            scanner: Scanner::new(source),
            chunk: Chunk::new(),
            curr: None,
            prev: None,
            had_error: false,
            panic_mode: false,
        }
    }

    pub fn compile(mut self) -> Option<Chunk> {
        self.advance();
        self.expression();
        self.consume(Token::EOF, "expected end of expression");
        self.end();
        if !self.had_error {
            debug::disassemble(&self.chunk, "code");
            Some(self.chunk)
        } else {
            None
        }
    }

    fn curr(&self) -> &Span {
        self.curr.as_ref().unwrap()
    }

    fn prev(&self) -> &Span {
        self.prev.as_ref().unwrap()
    }

    fn error_at(&mut self, at_curr: bool, message: &str) {
        if self.panic_mode {
            return;
        }

        let span = if at_curr { self.curr() } else { self.prev() };
        eprint!("[line {}] Error", span.line);
        if span.token == Token::EOF {
            eprint!(" at end");
        } else if span.token != Token::Err {
            eprint!(" at '{}'", &span.slice);
        }
        eprintln!(": {}", message);

        self.had_error = true;
        self.panic_mode = true;
    }

    fn error_curr(&mut self, message: &str) {
        self.error_at(true, message);
    }

    fn error(&mut self, message: &str) {
        self.error_at(false, message);
    }

    fn advance(&mut self) {
        self.prev = self.curr.take();
        loop {
            self.curr = Some(self.scanner.scan());
            let span = self.curr();
            if span.token != Token::Err {
                break;
            }
            let msg = span.slice.to_owned();
            self.error_curr(&msg);
        }
    }

    fn consume(&mut self, token: Token, message: &str) {
        if self.curr().token == token {
            self.advance();
            return;
        }
        self.error_curr(message);
    }

    fn parse_precedence(&mut self, prec: Prec) {
        self.advance();

        let prefix_fn = self.op_prefix(self.prev().token);
        if prefix_fn.is_none() {
            self.error("expect expression");
            return;
        }

        prefix_fn.unwrap()(self);

        while prec.power() <= self.op_prec(self.curr().token).power() {
            self.advance();
            let infix_fn = self.op_infix(self.prev().token);
            infix_fn.unwrap()(self);
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Prec::Or);
    }

    fn binary(&mut self) {
        let operator = self.prev().token;

        let prec = self.op_prec(operator);
        self.parse_precedence(prec.stronger());

        match operator {
            Token::Plus => self.emit(OpCode::Add),
            Token::Minus => self.emit(OpCode::Subtract),
            Token::Star => self.emit(OpCode::Multiply),
            Token::Slash => self.emit(OpCode::Divide),
            Token::Percent => self.emit(OpCode::Modulo),
            Token::EqEq => self.emit(OpCode::Equal),
            Token::BangEq => self.emit(OpCode::NotEq),
            Token::Lt => self.emit(OpCode::Lt),
            Token::LtEq => self.emit(OpCode::LtEq),
            Token::Gt => self.emit(OpCode::Gt),
            Token::GtEq => self.emit(OpCode::GtEq),
            _ => unreachable!(),
        }
    }

    fn unary(&mut self) {
        let operator = self.prev().token;
        self.parse_precedence(Prec::Unary);
        match operator {
            Token::Minus => self.emit(OpCode::Negate),
            Token::Not => self.emit(OpCode::Not),
            _ => unreachable!(),
        }
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(Token::Rparen, "expect ')' after expression");
    }

    fn literal(&mut self) {
        match self.prev().token {
            Token::None => self.emit(OpCode::None),
            Token::True => self.emit(OpCode::True),
            Token::False => self.emit(OpCode::False),
            _ => unreachable!(),
        }
    }

    fn string(&mut self) {
        let lexeme = &self.prev().slice;
        let end = lexeme.len() - 1;
        let copy = lexeme[1..end].to_owned();
        self.emit_constant(Value::Str(copy));
    }

    fn number(&mut self) {
        let value: f64 = self.prev().slice.parse().unwrap();
        self.emit_constant(Value::Num(value));
    }

    fn make_constant(&mut self, value: Value) -> usize {
        let index = self.chunk.add_constant(value);
        if index > MAX_CONST_INDEX {
            self.error("too many constants in one chunk");
            return 0;
        }
        index
    }

    fn emit(&mut self, opcode: OpCode) {
        self.chunk.write(opcode, self.prev().line);
    }

    fn emit_byte(&mut self, byte: u8) {
        self.chunk.write_byte(byte, self.prev().line);
    }

    fn emit_pair(&mut self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn emit_constant(&mut self, value: Value) {
        let index = self.make_constant(value);
        self.chunk.write_load(index, self.prev().line);
    }

    fn emit_return(&mut self) {
        self.emit(OpCode::Return);
    }

    fn end(&mut self) {
        self.emit_return();
    }

    fn op_prefix(&self, token: Token) -> Option<Box<ParseFn>> {
        Some(Box::new(match token {
            Token::Lparen => Compiler::grouping,
            Token::Minus => Compiler::unary,
            Token::Not => Compiler::unary,
            Token::None => Compiler::literal,
            Token::True => Compiler::literal,
            Token::False => Compiler::literal,
            Token::Str => Compiler::string,
            Token::Num => Compiler::number,
            _ => return None,
        }))
    }

    fn op_infix(&self, token: Token) -> Option<Box<ParseFn>> {
        Some(Box::new(match token {
            Token::Plus => Compiler::binary,
            Token::Minus => Compiler::binary,
            Token::Star => Compiler::binary,
            Token::Slash => Compiler::binary,
            Token::Percent => Compiler::binary,
            Token::EqEq => Compiler::binary,
            Token::BangEq => Compiler::binary,
            Token::Lt => Compiler::binary,
            Token::LtEq => Compiler::binary,
            Token::Gt => Compiler::binary,
            Token::GtEq => Compiler::binary,
            _ => return None,
        }))
    }

    fn op_prec(&self, token: Token) -> Prec {
        match token {
            Token::Plus => Prec::Term,
            Token::Minus => Prec::Term,
            Token::Star => Prec::Factor,
            Token::Slash => Prec::Factor,
            Token::Percent => Prec::Factor,
            Token::EqEq => Prec::Equality,
            Token::BangEq => Prec::Equality,
            Token::Lt => Prec::Comparison,
            Token::LtEq => Prec::Comparison,
            Token::Gt => Prec::Comparison,
            Token::GtEq => Prec::Comparison,
            _ => Prec::None,
        }
    }
}

type ParseFn = dyn FnMut(&mut Compiler) -> ();

#[repr(u8)]
#[derive(Copy, Clone)]
enum Prec {
    None,
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * / %
    Unary,      // - not
    Call,       // . ()
    Primary,
}

impl Prec {
    fn power(&self) -> u8 {
        *self as u8
    }

    fn stronger(&self) -> Self {
        match self {
            Prec::None => Prec::Or,
            Prec::Or => Prec::And,
            Prec::And => Prec::Equality,
            Prec::Equality => Prec::Comparison,
            Prec::Comparison => Prec::Term,
            Prec::Term => Prec::Factor,
            Prec::Factor => Prec::Unary,
            Prec::Unary => Prec::Call,
            Prec::Call => Prec::Primary,
            Prec::Primary => Prec::Primary,
        }
    }
}
