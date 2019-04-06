//! Module for runtime representation of language items.

mod callable;
mod env;
mod value;

pub use self::callable::{Callable, Function};
pub use self::env::Env;
pub use self::value::Value;

pub enum Signal {
    Ret(Value),
    Break,
    Cont,
    None,
}
