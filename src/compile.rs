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
    scope: Scope,
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
            scope: Scope::new(),
            curr: None,
            prev: None,
            had_error: false,
            panic_mode: false,
        }
    }

    pub fn compile(mut self) -> Option<Chunk> {
        self.advance();
        while !self.matches(Token::EOF) {
            self.declaration();
        }
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

    fn matches(&mut self, token: Token) -> bool {
        if self.check(token) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn check(&self, token: Token) -> bool {
        self.curr().token == token
    }

    fn synchronize(&mut self) {
        self.panic_mode = false;
        while self.curr().token != Token::EOF {
            if self.prev().token == Token::Semi {
                return;
            }
            match self.curr().token {
                Token::Class
                | Token::Fun
                | Token::Let
                | Token::If
                | Token::For
                | Token::While
                | Token::Break
                | Token::Continue
                | Token::Return => return,
                _ => {}
            }
            self.advance();
        }
    }

    fn declaration(&mut self) {
        if self.matches(Token::Let) {
            self.decl_let();
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn decl_let(&mut self) {
        let global_idx = self.parse_variable("expect variable name");
        if self.matches(Token::Eq) {
            self.expression();
        } else {
            self.emit(OpCode::None);
        }
        self.consume(Token::Semi, "expect ';' after variable declaration");
        self.define_variable(global_idx);
    }

    fn statement(&mut self) {
        if self.matches(Token::If) {
            self.stmt_if();
        } else if self.matches(Token::Lbrace) {
            self.scope_begin();
            self.block();
            self.scope_end();
        } else {
            self.stmt_expression();
        }
    }

    fn stmt_if(&mut self) {
        self.expression();
        self.consume(Token::Lbrace, "expect '{' after condition");

        let then_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit(OpCode::Pop);
        self.block();

        let else_jump = self.emit_jump(OpCode::Jump);
        self.patch_jump(then_jump);
        self.emit(OpCode::Pop);

        if self.matches(Token::Else) {
            self.consume(Token::Lbrace, "expect '{' after else");
            self.block();
        }

        self.patch_jump(else_jump);
    }

    fn stmt_expression(&mut self) {
        self.expression();
        self.consume(Token::Semi, "expect ';' after expression");
        self.emit(OpCode::Pop);
    }

    fn block(&mut self) {
        while !self.check(Token::Rbrace) && !self.check(Token::EOF) {
            self.declaration();
        }
        self.consume(Token::Rbrace, "expect '}' after block");
    }

    fn parse_variable(&mut self, message: &str) -> usize {
        self.consume(Token::Ident, message);
        self.declare_variable();
        if self.scope.depth > 0 {
            return 0;
        }
        let name = self.prev().slice.to_owned();
        self.make_ident_constant(name)
    }

    fn parse_precedence(&mut self, prec: Prec) {
        self.advance();

        let prefix_fn = self.op_prefix(self.prev().token);
        if prefix_fn.is_none() {
            self.error("expect expression");
            return;
        }

        let assignable = prec.power() <= Prec::Assign.power();
        prefix_fn.unwrap()(self, assignable);

        while prec.power() <= self.op_prec(self.curr().token).power() {
            self.advance();
            let infix_fn = self.op_infix(self.prev().token);
            infix_fn.unwrap()(self, assignable);
        }

        if assignable && self.matches(Token::Eq) {
            self.error("invalid assignment target");
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Prec::Assign);
    }

    fn logical_and(&mut self, _assignable: bool) {
        let end_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit(OpCode::Pop);
        self.parse_precedence(Prec::And);
        self.patch_jump(end_jump);
    }

    fn logical_or(&mut self, _assignable: bool) {
        let else_jump = self.emit_jump(OpCode::JumpIfFalse);
        let end_jump = self.emit_jump(OpCode::Jump);
        self.patch_jump(else_jump);
        self.emit(OpCode::Pop);
        self.parse_precedence(Prec::Or);
        self.patch_jump(end_jump);
    }

    fn binary(&mut self, _assignable: bool) {
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

    fn unary(&mut self, _assignable: bool) {
        let operator = self.prev().token;
        self.parse_precedence(Prec::Unary);
        match operator {
            Token::Minus => self.emit(OpCode::Negate),
            Token::Not => self.emit(OpCode::Not),
            _ => unreachable!(),
        }
    }

    fn grouping(&mut self, _assignable: bool) {
        self.expression();
        self.consume(Token::Rparen, "expect ')' after expression");
    }

    fn literal(&mut self, _assignable: bool) {
        match self.prev().token {
            Token::None => self.emit(OpCode::None),
            Token::True => self.emit(OpCode::True),
            Token::False => self.emit(OpCode::False),
            _ => unreachable!(),
        }
    }

    fn variable(&mut self, assignable: bool) {
        let name = self.prev().slice.to_owned();
        self.named_variable(name, assignable);
    }

    fn string(&mut self, _assignable: bool) {
        let lexeme = &self.prev().slice;
        let end = lexeme.len() - 1;
        let copy = lexeme[1..end].to_owned();
        self.emit_constant(Value::Str(copy));
    }

    fn number(&mut self, _assignable: bool) {
        let value: f64 = self.prev().slice.parse().unwrap();
        self.emit_constant(Value::Num(value));
    }

    fn make_ident_constant(&mut self, name: String) -> usize {
        self.make_constant(Value::Str(name))
    }

    fn make_constant(&mut self, value: Value) -> usize {
        let index = self.chunk.add_constant(value);
        if index > MAX_CONST_INDEX {
            self.error("too many constants in one chunk");
            return 0;
        }
        index
    }

    fn named_variable(&mut self, name: String, assignable: bool) {
        let (idx, get_op, set_op) = if let Some(arg) = self.resolve_local(&name) {
            (arg, OpCode::GetLocal, OpCode::SetLocal)
        } else {
            (
                self.make_ident_constant(name),
                OpCode::GetGlobal,
                OpCode::SetGlobal,
            )
        };

        if assignable && self.matches(Token::Eq) {
            self.expression();
            self.emit(set_op);
            self.emit_byte(idx as u8);
        } else {
            self.emit(get_op);
            self.emit_byte(idx as u8);
        }
    }

    fn declare_variable(&mut self) {
        if self.scope.depth == 0 {
            return;
        }
        let name = self.prev().slice.to_owned();
        let mut already_exists = false;
        for local in self.scope.locals.iter().rev() {
            if local.initialized && local.depth < self.scope.depth {
                break;
            }
            if local.name == name {
                already_exists = true;
                break;
            }
        }
        if already_exists {
            self.error("already variable with this name in this scope");
        }
        self.add_local(name);
    }

    fn define_variable(&mut self, global: usize) {
        if self.scope.depth > 0 {
            self.initialize_local();
            return;
        }
        self.emit(OpCode::DefineGlobal);
        self.emit_byte(global as u8);
    }

    fn add_local(&mut self, name: String) {
        if self.scope.locals.len() > u8::MAX as usize {
            self.error("too many local variables in function");
            return;
        }
        let local = Local::new(name, self.scope.depth);
        self.scope.locals.push(local);
    }

    fn initialize_local(&mut self) {
        self.scope.locals.last_mut().unwrap().initialized = true;
    }

    fn resolve_local(&mut self, name: &str) -> Option<usize> {
        for (idx, local) in self.scope.locals.iter().enumerate().rev() {
            if local.name == name {
                if !local.initialized {
                    self.error("can't read local variable in its own initializer");
                }
                return Some(idx);
            }
        }
        None
    }

    fn emit(&mut self, opcode: OpCode) {
        self.chunk.write(opcode, self.prev().line);
    }

    fn emit_byte(&mut self, byte: u8) {
        self.chunk.write_byte(byte, self.prev().line);
    }

    fn emit_constant(&mut self, value: Value) {
        let index = self.make_constant(value);
        self.chunk.write_load(index, self.prev().line);
    }

    fn emit_return(&mut self) {
        self.emit(OpCode::Return);
    }

    fn emit_jump(&mut self, opcode: OpCode) -> usize {
        self.emit(opcode);
        self.emit_byte(0xFF);
        self.emit_byte(0xFF);
        self.chunk.code_len() - 2
    }

    fn patch_jump(&mut self, offset: usize) {
        let amount = self.chunk.code_len() - offset - 2;
        if amount > u16::MAX as usize {
            self.error("too much code to jump over");
        }
        // little-endian
        self.chunk.patch(offset, (amount & 0xFF) as u8);
        self.chunk.patch(offset + 1, ((amount >> 8) & 0xFF) as u8);
    }

    fn scope_begin(&mut self) {
        self.scope.depth += 1;
    }

    fn scope_end(&mut self) {
        self.scope.depth -= 1;
        while !self.scope.locals.is_empty() {
            let local_depth = self.scope.locals.last().unwrap().depth;
            if local_depth > self.scope.depth {
                self.scope.locals.pop();
                self.emit(OpCode::Pop);
            } else {
                break;
            }
        }
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
            Token::Ident => Compiler::variable,
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
            Token::And => Compiler::logical_and,
            Token::Or => Compiler::logical_or,
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
            Token::And => Prec::And,
            Token::Or => Prec::Or,
            _ => Prec::None,
        }
    }
}

type ParseFn = dyn FnMut(&mut Compiler, bool) -> ();

#[repr(u8)]
#[derive(Copy, Clone)]
enum Prec {
    None,
    Assign,     // =
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
            Prec::None => Prec::Assign,
            Prec::Assign => Prec::Or,
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

struct Scope {
    locals: Vec<Local>,
    depth: usize,
}

impl Scope {
    fn new() -> Self {
        Self {
            locals: Vec::new(),
            depth: 0,
        }
    }
}

struct Local {
    name: String,
    depth: usize,
    initialized: bool,
}

impl Local {
    fn new(name: String, depth: usize) -> Self {
        Self {
            name,
            depth,
            initialized: false,
        }
    }
}
