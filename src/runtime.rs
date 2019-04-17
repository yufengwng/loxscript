//! Module for runtime representation of language items.

mod callable;
mod env;
mod value;

pub use self::callable::{Callable, Class, Function, LoxClass};
pub use self::env::Env;
pub use self::value::{LoxInstance, Value};

pub enum Signal {
    Ret(Value),
    Break,
    Cont,
    None,
}
