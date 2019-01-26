use std::collections::HashMap;

use crate::runtime::Value;

#[derive(Debug, Default)]
pub struct Env {
    values: HashMap<String, Value>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        self.values.get(name).cloned()
    }
}
