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
    classes: Vec<ClassCtx>,
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
            classes: Vec::new(),
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
        let fn_obj = self.ctx_end().function;
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
        } else if self.matches(Token::Class) {
            self.decl_class();
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn decl_let(&mut self) {
        let idx = self.parse_variable("expected variable name");
        if self.matches(Token::Eq) {
            self.expression();
        } else {
            self.emit(OpCode::None);
        }
        self.consume(Token::Semi, "expected ';' after variable declaration");
        self.define_variable(idx);
    }

    fn decl_function(&mut self) {
        let idx = self.parse_variable("expected function name");
        if self.ctx().depth > 0 {
            self.initialize_local();
        }
        self.function(FnKind::Function);
        self.define_variable(idx);
    }

    fn decl_class(&mut self) {
        self.consume(Token::Ident, "expected class name");
        let name = self.prev().slice.to_owned();
        let idx = self.make_ident_constant(name.clone());
        self.declare_variable();

        self.emit(OpCode::Class);
        self.emit_byte(idx as u8);
        self.define_variable(idx);

        self.classes.push(ClassCtx::new());

        if self.matches(Token::Lt) {
            self.consume(Token::Ident, "expected superclass name");
            let superclass_name = self.prev().slice.to_owned();
            if superclass_name == name {
                self.error("a class cannot inherit from itself");
            }

            self.named_variable(superclass_name, false);
            self.named_variable(name.clone(), false);
            self.emit(OpCode::Inherit);

            self.scope_begin();
            self.add_local("super".to_owned());
            self.initialize_local();
            self.classes.last_mut().unwrap().has_superclass = true;
        } else {
            self.named_variable(name.clone(), false);
        }

        self.consume(Token::Lbrace, "expected '{' before class body");
        while !self.check(Token::EOF) && !self.check(Token::Rbrace) {
            self.method();
        }
        self.consume(Token::Rbrace, "expected '}' after class body");

        self.emit(OpCode::Pop);
        if self.classes.last().unwrap().has_superclass {
            self.scope_end();
        }
        self.classes.pop();
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
            self.error("cannot return from top-level code");
        }

        if self.matches(Token::Semi) {
            self.emit_return();
        } else {
            if self.ctx().fn_kind == FnKind::Initializer {
                self.error("cannot return a value from an initializer");
            }
            self.expression();
            self.consume(Token::Semi, "expected ';' after return value");
            self.emit(OpCode::Return);
        }
    }

    fn stmt_break(&mut self) {
        if self.ctx().loop_breaks.is_empty() {
            self.error("cannot use 'break' outside of a loop");
            return;
        }

        self.consume(Token::Semi, "expected ';' after break");
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

        self.consume(Token::Semi, "expected ';' after continue");
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
        self.consume(Token::Lbrace, "expected '{' after condition");

        let mut then_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit(OpCode::Pop);
        self.stmt_block();

        let mut exits = Vec::new();
        exits.push(self.emit_jump(OpCode::Jump));
        self.patch_jump(then_jump);
        self.emit(OpCode::Pop);

        while self.matches(Token::Elif) {
            self.expression();
            self.consume(Token::Lbrace, "expected '{' after condition");

            then_jump = self.emit_jump(OpCode::JumpIfFalse);
            self.emit(OpCode::Pop);
            self.stmt_block();

            exits.push(self.emit_jump(OpCode::Jump));
            self.patch_jump(then_jump);
            self.emit(OpCode::Pop);
        }

        if self.matches(Token::Else) {
            self.consume(Token::Lbrace, "expected '{' after else");
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
        self.consume(Token::Lbrace, "expected '{' after condition");

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
            self.consume(Token::Semi, "expected ';' after loop condition");
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
            self.consume(Token::Lbrace, "expected '{' after for clauses");

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
        self.consume(Token::Semi, "expected ';' after expression");
        self.emit(OpCode::Pop);
    }

    fn method(&mut self) {
        self.consume(Token::Ident, "expected method name");
        let name = self.prev().slice.to_owned();
        let fn_kind = if name == "init" {
            FnKind::Initializer
        } else {
            FnKind::Method
        };
        let idx = self.make_ident_constant(name);
        self.function(fn_kind);

        self.emit(OpCode::Method);
        self.emit_byte(idx as u8);
    }

    fn function(&mut self, fn_kind: FnKind) {
        self.ctx_init(fn_kind);
        self.scope_begin();

        self.consume(Token::Lparen, "expected '(' after function name");
        self.parameter_list();
        self.consume(Token::Lbrace, "expected '{' before function body");
        self.block();

        let ctx = self.ctx_end();
        let index = self.make_constant(Value::Fun(Rc::new(ctx.function)));
        self.emit(OpCode::Closure);
        self.emit_byte(index as u8);

        for upvalue in ctx.upvalues {
            self.emit_byte(if upvalue.is_local { 1_u8 } else { 0_u8 });
            self.emit_byte(upvalue.index as u8);
        }
    }

    fn parameter_list(&mut self) {
        if !self.check(Token::Rparen) {
            loop {
                let ctx = self.ctx_mut();
                ctx.function.arity += 1;
                if ctx.function.arity > 255 {
                    self.error_curr("cannot have more than 255 parameters");
                }
                let const_idx = self.parse_variable("expected parameter name");
                self.define_variable(const_idx);
                if !self.matches(Token::Comma) {
                    break;
                }
            }
        }
        self.consume(Token::Rparen, "expected ')' after parameters");
    }

    fn argument_list(&mut self) -> usize {
        let mut arg_count = 0;
        if !self.check(Token::Rparen) {
            loop {
                self.expression();
                if arg_count == 255 {
                    self.error("cannot have more than 255 arguments");
                }
                arg_count += 1;
                if !self.matches(Token::Comma) {
                    break;
                }
            }
        }
        self.consume(Token::Rparen, "expected ')' after arguments");
        arg_count
    }

    fn block(&mut self) {
        while !self.check(Token::Rbrace) && !self.check(Token::EOF) {
            self.declaration();
        }
        self.consume(Token::Rbrace, "expected '}' after block");
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
            self.error("expected an expression");
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

    fn dot(&mut self, assignable: bool) {
        self.consume(Token::Ident, "expected property name after '.'");
        let name = self.prev().slice.to_owned();
        let idx = self.make_ident_constant(name);
        if assignable && self.matches(Token::Eq) {
            self.expression();
            self.emit(OpCode::SetProperty);
            self.emit_byte(idx as u8);
        } else if self.matches(Token::Lparen) {
            let arg_count = self.argument_list();
            self.emit(OpCode::Invoke);
            self.emit_byte(idx as u8);
            self.emit_byte(arg_count as u8);
        } else {
            self.emit(OpCode::GetProperty);
            self.emit_byte(idx as u8);
        }
    }

    fn grouping(&mut self, _assignable: bool) {
        self.expression();
        self.consume(Token::Rparen, "expected ')' after expression");
    }

    fn literal(&mut self, _assignable: bool) {
        match self.prev().token {
            Token::None => self.emit(OpCode::None),
            Token::True => self.emit(OpCode::True),
            Token::False => self.emit(OpCode::False),
            _ => unreachable!(),
        }
    }

    fn self_(&mut self, _assignable: bool) {
        if self.classes.is_empty() {
            self.error("cannot use 'self' outside of a class");
            return;
        }
        self.variable(false);
    }

    fn super_(&mut self, _assignable: bool) {
        if self.classes.is_empty() {
            self.error("cannot use 'super' outside of a class");
        } else if !self.classes.last().unwrap().has_superclass {
            self.error("cannot use 'super' in a class with no superclass");
        }

        self.consume(Token::Dot, "expected '.' after 'super'");
        self.consume(Token::Ident, "expected superclass method name");
        let name = self.prev().slice.to_owned();
        let idx = self.make_ident_constant(name);

        self.named_variable("self".to_owned(), false);
        if self.matches(Token::Lparen) {
            let arg_count = self.argument_list();
            self.named_variable("super".to_owned(), false);
            self.emit(OpCode::SuperInvoke);
            self.emit_byte(idx as u8);
            self.emit_byte(arg_count as u8);
        } else {
            self.named_variable("super".to_owned(), false);
            self.emit(OpCode::SuperGet);
            self.emit_byte(idx as u8);
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
        let ctx_idx = self.contexts.len() - 1;
        let (idx, get_op, set_op) = if let Some(arg) = self.resolve_local(ctx_idx, &name) {
            (arg, OpCode::GetLocal, OpCode::SetLocal)
        } else if let Some(arg) = self.resolve_upvalue(ctx_idx, &name) {
            (arg, OpCode::GetUpvalue, OpCode::SetUpvalue)
        } else {
            let arg = self.make_ident_constant(name);
            (arg, OpCode::GetGlobal, OpCode::SetGlobal)
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
            self.error("variable with this name already declared in this scope");
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

    fn initialize_local(&mut self) {
        self.ctx_mut().locals.last_mut().unwrap().initialized = true;
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

    fn add_upvalue(&mut self, ctx_idx: usize, local_idx: usize, is_local: bool) -> usize {
        let ctx = &mut self.contexts[ctx_idx];
        let count = ctx.upvalues.len();
        for (idx, upvalue) in ctx.upvalues.iter().enumerate() {
            if upvalue.index == local_idx && upvalue.is_local == is_local {
                return idx;
            }
        }
        if count == (u8::MAX as usize + 1) {
            self.error("too many closure variables in function");
            return 0;
        }
        ctx.upvalues.push(Upvalue::new(local_idx, is_local));
        count
    }

    fn resolve_local(&mut self, ctx_idx: usize, name: &str) -> Option<usize> {
        let ctx = &self.contexts[ctx_idx];
        for (idx, local) in ctx.locals.iter().enumerate().rev() {
            if local.name == name {
                if !local.initialized {
                    self.error("cannot read local variable in its own initializer");
                }
                return Some(idx);
            }
        }
        None
    }

    fn resolve_upvalue(&mut self, ctx_idx: usize, name: &str) -> Option<usize> {
        if ctx_idx == 0 {
            return None;
        }

        let local_idx = self.resolve_local(ctx_idx - 1, name);
        if let Some(idx) = local_idx {
            self.contexts[ctx_idx - 1].locals[idx].is_captured = true;
            return Some(self.add_upvalue(ctx_idx, idx, true));
        }

        let upvalue = self.resolve_upvalue(ctx_idx - 1, name);
        if let Some(idx) = upvalue {
            return Some(self.add_upvalue(ctx_idx, idx, false));
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
        if self.ctx().fn_kind == FnKind::Initializer {
            self.emit(OpCode::GetLocal);
            self.emit_byte(0);
        } else {
            self.emit(OpCode::None);
        }
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
        let name = if fn_kind.is_method() {
            "self".to_string()
        } else {
            String::new()
        };
        let mut local = Local::new(name, 0);
        local.initialized = true;
        ctx.locals.push(local);
        ctx.fn_kind = fn_kind;
        self.contexts.push(ctx);
    }

    fn ctx_end(&mut self) -> Context {
        self.emit_return();
        let mut ctx = self.contexts.pop().unwrap();
        let mut fn_obj = &mut ctx.function;
        fn_obj.num_upvalues = ctx.upvalues.len();
        if !self.had_error {
            let name = if !fn_obj.name.is_empty() {
                fn_obj.name.to_owned()
            } else {
                "<script>".to_owned()
            };
            if cfg!(feature = "debug") {
                debug::disassemble(&fn_obj.chunk, &name);
            }
        }
        ctx
    }

    fn scope_begin(&mut self) {
        self.ctx_mut().depth += 1;
    }

    fn scope_end(&mut self) {
        let ctx = self.ctx_mut();
        ctx.depth -= 1;

        let mut opcodes = Vec::new();
        while !ctx.locals.is_empty() {
            let local_depth = ctx.locals.last().unwrap().depth;
            if local_depth > ctx.depth {
                let local = ctx.locals.pop().unwrap();
                opcodes.push(if local.is_captured {
                    OpCode::CloseUpvalue
                } else {
                    OpCode::Pop
                });
            } else {
                break;
            }
        }

        for opcode in opcodes {
            self.emit(opcode);
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
            Token::Self_ => Compiler::self_,
            Token::Super => Compiler::super_,
            Token::Ident => Compiler::variable,
            Token::Str => Compiler::string,
            Token::Num => Compiler::number,
            _ => return None,
        }))
    }

    fn op_infix(&self, token: Token) -> Option<Box<ParseFn>> {
        Some(Box::new(match token {
            Token::Lparen => Compiler::call,
            Token::Dot => Compiler::dot,
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
            Token::Dot => Prec::Call,
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

type ParseFn = fn(&mut Compiler, bool) -> ();

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
    Method,
    Initializer,
}

impl FnKind {
    fn is_method(&self) -> bool {
        match self {
            Self::Method | Self::Initializer => true,
            _ => false,
        }
    }
}

struct Context {
    function: ObjFn,
    fn_kind: FnKind,
    locals: Vec<Local>,
    upvalues: Vec<Upvalue>,
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
            upvalues: Vec::new(),
            loop_breaks: Vec::new(),
            loop_depths: Vec::new(),
            loop_starts: Vec::new(),
            depth: 0,
        }
    }
}

struct ClassCtx {
    has_superclass: bool,
}

impl ClassCtx {
    fn new() -> Self {
        Self {
            has_superclass: false,
        }
    }
}

struct Local {
    name: String,
    depth: usize,
    initialized: bool,
    is_captured: bool,
}

impl Local {
    fn new(name: String, depth: usize) -> Self {
        Self {
            name,
            depth,
            initialized: false,
            is_captured: false,
        }
    }
}

struct Upvalue {
    index: usize,
    is_local: bool,
}

impl Upvalue {
    fn new(index: usize, is_local: bool) -> Self {
        Self { index, is_local }
    }
}
