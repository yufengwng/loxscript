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

    fn curr_span(&self) -> &Span {
        self.curr.as_ref().unwrap()
    }

    fn prev_span(&self) -> &Span {
        self.prev.as_ref().unwrap()
    }

    fn error_at(&mut self, at_curr: bool, message: &str) {
        if self.panic_mode {
            return;
        }

        let span = if at_curr {
            self.curr_span()
        } else {
            self.prev_span()
        };

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
            let span = self.curr_span();
            if span.token != Token::Err {
                break;
            }
            let msg = span.slice.to_owned();
            self.error_curr(&msg);
        }
    }

    fn consume(&mut self, token: Token, message: &str) {
        if self.curr_span().token == token {
            self.advance();
            return;
        }
        self.error_curr(message);
    }

    fn parse_precedence(&mut self, prec: Prec) {
        self.advance();

        let rule = get_rule(self.prev_span().token);
        if rule.prefix().is_none() {
            self.error("expect expression");
            return;
        }

        let mut prefix_fn = self.rule_fn(rule.prefix().unwrap());
        prefix_fn(self);

        while prec as u8 <= get_rule(self.curr_span().token).prec() as u8 {
            self.advance();
            let rule = get_rule(self.prev_span().token);
            let mut infix_fn = self.rule_fn(rule.infix().unwrap());
            infix_fn(self);
        }
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(Token::Rparen, "expect ')' after expression");
    }

    fn expression(&mut self) {
        self.parse_precedence(Prec::Assignment);
    }

    fn binary(&mut self) {
        let operator = self.prev_span().token;

        let rule = get_rule(operator);
        self.parse_precedence(rule.prec().stronger());

        match operator {
            Token::Plus => self.emit(OpCode::Add),
            Token::Minus => self.emit(OpCode::Subtract),
            Token::Star => self.emit(OpCode::Multiply),
            Token::Slash => self.emit(OpCode::Divide),
            Token::Percent => self.emit(OpCode::Modulo),
            _ => panic!("[lox] should be unreachable in binary"),
        }
    }

    fn unary(&mut self) {
        let operator = self.prev_span().token;
        self.parse_precedence(Prec::Unary);
        match operator {
            Token::Minus => self.emit(OpCode::Negate),
            _ => panic!("[lox] should be unreachable in unary"),
        }
    }

    fn number(&mut self) {
        let val = self.prev_span().slice.parse::<f64>().unwrap();
        self.emit_constant(val);
    }

    fn make_constant(&mut self, val: Value) -> usize {
        let index = self.chunk.add_constant(val);
        if index > MAX_CONST_INDEX {
            self.error("too many constants in one chunk");
            return 0;
        }
        index
    }

    fn emit(&mut self, opcode: OpCode) {
        self.chunk.write(opcode, self.prev_span().line);
    }

    fn emit_byte(&mut self, byte: u8) {
        self.chunk.write_byte(byte, self.prev_span().line);
    }

    fn emit_pair(&mut self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn emit_constant(&mut self, val: Value) {
        let index = self.make_constant(val);
        self.chunk.write_load(index, self.prev_span().line);
    }

    fn emit_return(&mut self) {
        self.emit(OpCode::Return);
    }

    fn end(&mut self) {
        self.emit_return();
    }

    fn rule_fn(&mut self, id: usize) -> Box<dyn FnMut(&mut Compiler) -> ()> {
        Box::new(match id {
            0 => Compiler::grouping,
            1 => Compiler::unary,
            2 => Compiler::binary,
            3 => Compiler::number,
            _ => panic!("[lox] invalid rule fn id {}", id),
        })
    }
}

#[repr(u8)]
#[derive(Copy, Clone)]
enum Prec {
    None,
    Assignment, // =
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
    fn stronger(&self) -> Self {
        match self {
            Prec::None => Prec::Assignment,
            Prec::Assignment => Prec::Or,
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

struct Rule(Option<usize>, Option<usize>, Prec);

impl Rule {
    fn prefix(&self) -> &Option<usize> {
        &self.0
    }

    fn infix(&self) -> &Option<usize> {
        &self.1
    }

    fn prec(&self) -> Prec {
        self.2
    }
}

fn get_rule(token: Token) -> &'static Rule {
    &RULES[token as usize]
}

static RULES: [Rule; 43] = [
    /* Lparen   */ Rule(Some(0), None, Prec::None),
    /* Rparen   */ Rule(None, None, Prec::None),
    /* Lbrace   */ Rule(None, None, Prec::None),
    /* Rbrace   */ Rule(None, None, Prec::None),
    /* Comma    */ Rule(None, None, Prec::None),
    /* Semi     */ Rule(None, None, Prec::None),
    /* Dot      */ Rule(None, None, Prec::None),
    /* Plus     */ Rule(None, Some(2), Prec::Term),
    /* Minus    */ Rule(Some(1), Some(2), Prec::Term),
    /* Star     */ Rule(None, Some(2), Prec::Factor),
    /* Slash    */ Rule(None, Some(2), Prec::Factor),
    /* Percent  */ Rule(None, Some(2), Prec::Factor),
    /* Eq       */ Rule(None, None, Prec::None),
    /* EqEq     */ Rule(None, None, Prec::None),
    /* BangEq   */ Rule(None, None, Prec::None),
    /* Lt       */ Rule(None, None, Prec::None),
    /* LtEq     */ Rule(None, None, Prec::None),
    /* Gt       */ Rule(None, None, Prec::None),
    /* GtEq     */ Rule(None, None, Prec::None),
    /* Not      */ Rule(None, None, Prec::None),
    /* And      */ Rule(None, None, Prec::None),
    /* Or       */ Rule(None, None, Prec::None),
    /* If       */ Rule(None, None, Prec::None),
    /* Elif     */ Rule(None, None, Prec::None),
    /* Else     */ Rule(None, None, Prec::None),
    /* For      */ Rule(None, None, Prec::None),
    /* While    */ Rule(None, None, Prec::None),
    /* Break    */ Rule(None, None, Prec::None),
    /* Continue */ Rule(None, None, Prec::None),
    /* Return   */ Rule(None, None, Prec::None),
    /* Let      */ Rule(None, None, Prec::None),
    /* Fun      */ Rule(None, None, Prec::None),
    /* Class    */ Rule(None, None, Prec::None),
    /* Super    */ Rule(None, None, Prec::None),
    /* Self_    */ Rule(None, None, Prec::None),
    /* None     */ Rule(None, None, Prec::None),
    /* True     */ Rule(None, None, Prec::None),
    /* False    */ Rule(None, None, Prec::None),
    /* Ident    */ Rule(None, None, Prec::None),
    /* Str      */ Rule(None, None, Prec::None),
    /* Num      */ Rule(Some(3), None, Prec::None),
    /* Err      */ Rule(None, None, Prec::None),
    /* EOF      */ Rule(None, None, Prec::None),
];
