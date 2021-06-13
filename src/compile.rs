use std::rc::Rc;

use crate::bytecode::Chunk;
use crate::bytecode::OpCode;
use crate::bytecode::MAX_CONST_INDEX;
use crate::debug;
use crate::scan::Scanner;
use crate::scan::Span;
use crate::scan::Token;
use crate::value::ObjFn;
use crate::value::Value;

pub fn compile(source: String) -> Option<ObjFn> {
    Compiler::new(source).compile()
}

struct Compiler {
    scanner: Scanner,
    contexts: Vec<Context>,
    curr: Option<Span>,
    prev: Option<Span>,
    had_error: bool,
    panic_mode: bool,
}

impl Compiler {
    pub fn new(source: String) -> Self {
        let mut this = Self {
            scanner: Scanner::new(source),
            contexts: Vec::new(),
            curr: None,
            prev: None,
            had_error: false,
            panic_mode: false,
        };
        this.ctx_init(FnKind::Script);
        this
    }

    pub fn compile(mut self) -> Option<ObjFn> {
        self.advance();
        while !self.matches(Token::EOF) {
            self.declaration();
        }
        let fn_obj = self.ctx_end();
        if !self.had_error {
            Some(fn_obj)
        } else {
            None
        }
    }

    fn ctx(&self) -> &Context {
        self.contexts.last().unwrap()
    }

    fn ctx_mut(&mut self) -> &mut Context {
        self.contexts.last_mut().unwrap()
    }

    fn chunk(&self) -> &Chunk {
        &self.ctx().function.chunk
    }

    fn chunk_mut(&mut self) -> &mut Chunk {
        &mut self.ctx_mut().function.chunk
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
        } else if self.matches(Token::Fun) {
            self.decl_function();
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

    fn decl_function(&mut self) {
        let global_idx = self.parse_variable("expect function name");
        if self.ctx().depth > 0 {
            self.initialize_local();
        }
        self.function(FnKind::Function);
        self.define_variable(global_idx);
    }

    fn statement(&mut self) {
        if self.matches(Token::If) {
            self.stmt_if();
        } else if self.matches(Token::For) {
            self.stmt_for();
        } else if self.matches(Token::While) {
            self.stmt_while();
        } else if self.matches(Token::Break) {
            self.stmt_break();
        } else if self.matches(Token::Continue) {
            self.stmt_continue();
        } else if self.matches(Token::Return) {
            self.stmt_return();
        } else if self.matches(Token::Lbrace) {
            self.stmt_block();
        } else {
            self.stmt_expression();
        }
    }

    fn stmt_block(&mut self) {
        self.scope_begin();
        self.block();
        self.scope_end();
    }

    fn stmt_return(&mut self) {
        if self.ctx().fn_kind == FnKind::Script {
            self.error("can't return from top-level code");
        }

        if self.matches(Token::Semi) {
            self.emit_return();
        } else {
            self.expression();
            self.consume(Token::Semi, "expect ';' after return value");
            self.emit(OpCode::Return);
        }
    }

    fn stmt_break(&mut self) {
        if self.ctx().loop_breaks.is_empty() {
            self.error("cannot use 'break' outside of a loop");
            return;
        }

        self.consume(Token::Semi, "expect ';' after break");
        let mut num_pops = 0;
        let ctx = self.ctx();
        let loop_depth = ctx.loop_depths.last().unwrap().clone();
        for i in (0..ctx.locals.len()).rev() {
            let local_depth = ctx.locals[i].depth;
            if local_depth > loop_depth {
                num_pops += 1;
            } else {
                break;
            }
        }
        for _ in 0..num_pops {
            self.emit(OpCode::Pop);
        }

        let jump = self.emit_jump(OpCode::Jump);
        let breaks = self.ctx_mut().loop_breaks.last_mut().unwrap();
        breaks.push(jump);
    }

    fn stmt_continue(&mut self) {
        if self.ctx().loop_starts.is_empty() {
            self.error("cannot use 'continue' outside of a loop");
            return;
        }

        self.consume(Token::Semi, "expect ';' after continue");
        let mut num_pops = 0;
        let ctx = self.ctx();
        let loop_depth = ctx.loop_depths.last().unwrap().clone();
        for i in (0..ctx.locals.len()).rev() {
            let local_depth = ctx.locals[i].depth;
            if local_depth > loop_depth {
                num_pops += 1;
            } else {
                break;
            }
        }
        for _ in 0..num_pops {
            self.emit(OpCode::Pop);
        }

        let start = self.ctx().loop_starts.last().unwrap().clone();
        self.emit_loop(start);
    }

    fn stmt_if(&mut self) {
        self.expression();
        self.consume(Token::Lbrace, "expect '{' after condition");

        let mut then_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit(OpCode::Pop);
        self.stmt_block();

        let mut exits = Vec::new();
        exits.push(self.emit_jump(OpCode::Jump));
        self.patch_jump(then_jump);
        self.emit(OpCode::Pop);

        while self.matches(Token::Elif) {
            self.expression();
            self.consume(Token::Lbrace, "expect '{' after condition");

            then_jump = self.emit_jump(OpCode::JumpIfFalse);
            self.emit(OpCode::Pop);
            self.stmt_block();

            exits.push(self.emit_jump(OpCode::Jump));
            self.patch_jump(then_jump);
            self.emit(OpCode::Pop);
        }

        if self.matches(Token::Else) {
            self.consume(Token::Lbrace, "expect '{' after else");
            self.stmt_block();
        }

        for exit in exits {
            self.patch_jump(exit);
        }
    }

    fn stmt_while(&mut self) {
        let loop_start = self.chunk().code_len();
        self.loop_begin(loop_start);

        self.expression();
        self.consume(Token::Lbrace, "expect '{' after condition");

        let exit_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit(OpCode::Pop);
        self.stmt_block();
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit(OpCode::Pop);

        let breaks = self.loop_end();
        for jump in breaks {
            self.patch_jump(jump);
        }
    }

    fn stmt_for(&mut self) {
        self.scope_begin();
        if self.matches(Token::Semi) {
            // No initializer clause
        } else if self.matches(Token::Let) {
            self.decl_let();
        } else {
            self.stmt_expression();
        }

        let mut loop_start = self.chunk().code_len();
        let exit_jump = if !self.matches(Token::Semi) {
            self.expression();
            self.consume(Token::Semi, "expect ';' after loop condition");
            let exit = self.emit_jump(OpCode::JumpIfFalse);
            self.emit(OpCode::Pop);
            Some(exit)
        } else {
            None
        };

        if !self.matches(Token::Lbrace) {
            let body_jump = self.emit_jump(OpCode::Jump);

            let incr_start = self.chunk().code_len();
            self.expression();
            self.emit(OpCode::Pop);
            self.consume(Token::Lbrace, "expect '{' after for clauses");

            self.emit_loop(loop_start);
            self.patch_jump(body_jump);
            loop_start = incr_start;
        }

        self.loop_begin(loop_start);
        self.stmt_block();
        self.emit_loop(loop_start);

        if let Some(exit) = exit_jump {
            self.patch_jump(exit);
            self.emit(OpCode::Pop);
        }

        let breaks = self.loop_end();
        for jump in breaks {
            self.patch_jump(jump);
        }

        self.scope_end();
    }

    fn stmt_expression(&mut self) {
        self.expression();
        self.consume(Token::Semi, "expect ';' after expression");
        self.emit(OpCode::Pop);
    }

    fn function(&mut self, fn_kind: FnKind) {
        self.ctx_init(fn_kind);
        self.scope_begin();

        self.consume(Token::Lparen, "expect '(' after function name");
        self.parameter_list();
        self.consume(Token::Lbrace, "expect '{' before function body");
        self.block();

        let fn_obj = self.ctx_end();
        self.emit_constant(Value::Fun(Rc::new(fn_obj)));
    }

    fn parameter_list(&mut self) {
        if !self.check(Token::Rparen) {
            loop {
                let ctx = self.ctx_mut();
                ctx.function.arity += 1;
                if ctx.function.arity > 255 {
                    self.error_curr("can't have more than 255 parameters");
                }
                let const_idx = self.parse_variable("expect parameter name");
                self.define_variable(const_idx);
                if !self.matches(Token::Comma) {
                    break;
                }
            }
        }
        self.consume(Token::Rparen, "expect ')' after parameters");
    }

    fn argument_list(&mut self) -> usize {
        let mut arg_count = 0;
        if !self.check(Token::Rparen) {
            loop {
                self.expression();
                if arg_count == 255 {
                    self.error("can't have more than 255 arguments");
                }
                arg_count += 1;
                if !self.matches(Token::Comma) {
                    break;
                }
            }
        }
        self.consume(Token::Rparen, "expect ')' after arguments");
        arg_count
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
        if self.ctx().depth > 0 {
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

    fn call(&mut self, _assignable: bool) {
        let arg_count = self.argument_list();
        self.emit(OpCode::Call);
        self.emit_byte(arg_count as u8);
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
        let index = self.chunk_mut().add_constant(value);
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
        let ctx = self.ctx();
        if ctx.depth == 0 {
            return;
        }
        let name = self.prev().slice.to_owned();
        let mut already_exists = false;
        for local in ctx.locals.iter().rev() {
            if local.initialized && local.depth < ctx.depth {
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
        if self.ctx().depth > 0 {
            self.initialize_local();
            return;
        }
        self.emit(OpCode::DefineGlobal);
        self.emit_byte(global as u8);
    }

    fn add_local(&mut self, name: String) {
        let ctx = self.ctx();
        if ctx.locals.len() > u8::MAX as usize {
            self.error("too many local variables in function");
            return;
        }
        let local = Local::new(name, ctx.depth);
        self.ctx_mut().locals.push(local);
    }

    fn initialize_local(&mut self) {
        self.ctx_mut().locals.last_mut().unwrap().initialized = true;
    }

    fn resolve_local(&mut self, name: &str) -> Option<usize> {
        for (idx, local) in self.ctx().locals.iter().enumerate().rev() {
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
        let line = self.prev().line;
        self.chunk_mut().write(opcode, line);
    }

    fn emit_byte(&mut self, byte: u8) {
        let line = self.prev().line;
        self.chunk_mut().write_byte(byte, line);
    }

    fn emit_constant(&mut self, value: Value) {
        let index = self.make_constant(value);
        let line = self.prev().line;
        self.chunk_mut().write_load(index, line);
    }

    fn emit_return(&mut self) {
        self.emit(OpCode::None);
        self.emit(OpCode::Return);
    }

    fn emit_loop(&mut self, loop_start: usize) {
        self.emit(OpCode::Loop);

        let offset = self.chunk().code_len() - loop_start + 2;
        if offset > u16::MAX as usize {
            self.error("loop body too large");
        }

        // little-endian
        self.emit_byte((offset & 0xFF) as u8);
        self.emit_byte(((offset >> 8) & 0xFF) as u8);
    }

    fn emit_jump(&mut self, opcode: OpCode) -> usize {
        self.emit(opcode);
        self.emit_byte(0xFF);
        self.emit_byte(0xFF);
        self.chunk().code_len() - 2
    }

    fn patch_jump(&mut self, offset: usize) {
        let amount = self.chunk().code_len() - offset - 2;
        if amount > u16::MAX as usize {
            self.error("too much code to jump over");
        }
        // little-endian
        let chunk = self.chunk_mut();
        chunk.patch(offset, (amount & 0xFF) as u8);
        chunk.patch(offset + 1, ((amount >> 8) & 0xFF) as u8);
    }

    fn ctx_init(&mut self, fn_kind: FnKind) {
        let mut ctx = Context::new();
        if fn_kind != FnKind::Script {
            let name = &self.prev().slice;
            ctx.function.name = name.to_owned();
        }
        ctx.fn_kind = fn_kind;
        ctx.locals.push(Local::new(String::new(), 0));
        self.contexts.push(ctx);
    }

    fn ctx_end(&mut self) -> ObjFn {
        self.emit_return();
        let ctx = self.contexts.pop().unwrap();
        let fn_obj = ctx.function;
        if !self.had_error {
            let name = if !fn_obj.name.is_empty() {
                fn_obj.name.to_owned()
            } else {
                "<script>".to_owned()
            };
            debug::disassemble(&fn_obj.chunk, &name);
        }
        fn_obj
    }

    fn scope_begin(&mut self) {
        self.ctx_mut().depth += 1;
    }

    fn scope_end(&mut self) {
        let ctx = self.ctx_mut();
        ctx.depth -= 1;

        let mut num_pops = 0;
        while !ctx.locals.is_empty() {
            let local_depth = ctx.locals.last().unwrap().depth;
            if local_depth > ctx.depth {
                ctx.locals.pop();
                num_pops += 1;
            } else {
                break;
            }
        }

        for _ in 0..num_pops {
            self.emit(OpCode::Pop);
        }
    }

    fn loop_begin(&mut self, start: usize) {
        let ctx = self.ctx_mut();
        ctx.loop_breaks.push(Vec::new());
        ctx.loop_depths.push(ctx.depth);
        ctx.loop_starts.push(start);
    }

    fn loop_end(&mut self) -> Vec<usize> {
        let ctx = self.ctx_mut();
        ctx.loop_starts.pop();
        ctx.loop_depths.pop();
        ctx.loop_breaks.pop().unwrap()
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
            Token::Lparen => Compiler::call,
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
            Token::Lparen => Prec::Call,
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

#[derive(PartialEq)]
enum FnKind {
    Script,
    Function,
}

struct Context {
    function: ObjFn,
    fn_kind: FnKind,
    locals: Vec<Local>,
    loop_breaks: Vec<Vec<usize>>,
    loop_depths: Vec<usize>,
    loop_starts: Vec<usize>,
    depth: usize,
}

impl Context {
    fn new() -> Self {
        Self {
            function: ObjFn::new(),
            fn_kind: FnKind::Script,
            locals: Vec::new(),
            loop_breaks: Vec::new(),
            loop_depths: Vec::new(),
            loop_starts: Vec::new(),
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
