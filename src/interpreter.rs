use crate::item::{BinOp, Decl, Expr, LogOp, Primitive, Stmt, UniOp};
use crate::runtime::Value;

#[derive(Default)]
pub struct Interpreter;

impl Interpreter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run(&mut self, program: &[Decl]) {
        for decl in program {
            match decl {
                Decl::Statement(ref stmt) => self.exec(stmt),
            }
        }
    }

    fn exec(&self, stmt: &Stmt) {
        match stmt {
            Stmt::Expression(ref expr) => {
                println!("{:?}", self.eval(expr));
            }
        }
    }

    fn eval(&self, expr: &Expr) -> Value {
        match expr {
            Expr::Literal(ref prim) => match prim {
                Primitive::None(_) => Value::None,
                Primitive::Bool(b, _) => Value::Bool(*b),
                Primitive::Num(n, _) => Value::Num(*n),
                Primitive::Str(s, _) => Value::Str(s.clone()),
            },
            Expr::Unary(ref op, expr) => {
                let value = self.eval(expr);
                match op {
                    UniOp::Neg(_) => {
                        if let Value::Num(n) = value {
                            Value::Num(-n)
                        } else {
                            Value::None
                        }
                    }
                    UniOp::Not(_) => Value::Bool(!value.is_truthy()),
                }
            }
            Expr::Binary(ref lhs, ref op, ref rhs) => {
                let lval = self.eval(lhs);
                let rval = self.eval(rhs);
                match op {
                    BinOp::Add(_) => match (lval, rval) {
                        (Value::Num(lv), Value::Num(rv)) => Value::Num(lv + rv),
                        (Value::Str(lv), Value::Str(rv)) => Value::Str(lv + &rv),
                        _ => Value::None,
                    },
                    BinOp::Sub(_) => match (lval, rval) {
                        (Value::Num(lv), Value::Num(rv)) => Value::Num(lv - rv),
                        _ => Value::None,
                    },
                    BinOp::Mul(_) => match (lval, rval) {
                        (Value::Num(lv), Value::Num(rv)) => Value::Num(lv * rv),
                        _ => Value::None,
                    },
                    BinOp::Div(_) => match (lval, rval) {
                        (Value::Num(lv), Value::Num(rv)) => Value::Num(lv / rv),
                        _ => Value::None,
                    },
                    BinOp::Rem(_) => match (lval, rval) {
                        (Value::Num(lv), Value::Num(rv)) => Value::Num(lv % rv),
                        _ => Value::None,
                    },
                    BinOp::Lt(_) => match (lval, rval) {
                        (Value::Num(lv), Value::Num(rv)) => Value::Bool(lv < rv),
                        _ => Value::None,
                    },
                    BinOp::LtEq(_) => match (lval, rval) {
                        (Value::Num(lv), Value::Num(rv)) => Value::Bool(lv <= rv),
                        _ => Value::None,
                    },
                    BinOp::Gt(_) => match (lval, rval) {
                        (Value::Num(lv), Value::Num(rv)) => Value::Bool(lv > rv),
                        _ => Value::None,
                    },
                    BinOp::GtEq(_) => match (lval, rval) {
                        (Value::Num(lv), Value::Num(rv)) => Value::Bool(lv >= rv),
                        _ => Value::None,
                    },
                    BinOp::EqEq(_) => Value::Bool(lval == rval),
                    BinOp::NotEq(_) => Value::Bool(lval != rval),
                }
            }
            Expr::Logical(ref lhs, ref op, ref rhs) => {
                let lval = self.eval(lhs);
                match op {
                    LogOp::And(_) => {
                        if lval.is_truthy() {
                            self.eval(rhs)
                        } else {
                            lval
                        }
                    }
                    LogOp::Or(_) => {
                        if lval.is_truthy() {
                            lval
                        } else {
                            self.eval(rhs)
                        }
                    }
                }
            }
        }
    }
}
