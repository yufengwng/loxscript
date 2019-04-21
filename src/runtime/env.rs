use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::runtime::Value;

#[derive(Debug, Default)]
pub struct Env {
    values: HashMap<String, Value>,
    inner: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            inner: None,
        }
    }

    pub fn enclosing(inner: &Rc<RefCell<Env>>) -> Self {
        Self {
            values: HashMap::new(),
            inner: Some(Rc::clone(inner)),
        }
    }

    pub fn inner_env(&self) -> Rc<RefCell<Env>> {
        self.inner.as_ref().cloned().unwrap()
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn search(&self, name: &str) -> Option<Value> {
        if self.values.contains_key(name) {
            return self.values.get(name).cloned();
        }

        let mut curr = self.inner.as_ref().cloned();
        while let Some(ptr) = curr {
            let env = ptr.borrow();
            if env.values.contains_key(name) {
                return env.values.get(name).cloned();
            }
            curr = env.inner.as_ref().cloned();
        }

        None
    }

    pub fn assign(&mut self, name: String, value: Value) -> bool {
        if self.values.contains_key(&name) {
            self.values.insert(name, value);
            return true;
        }

        let mut curr = self.inner.as_ref().cloned();
        while let Some(ptr) = curr {
            let mut env = ptr.borrow_mut();
            if env.values.contains_key(&name) {
                env.values.insert(name, value);
                return true;
            }
            curr = env.inner.as_ref().cloned();
        }

        false
    }

    pub fn get_at(&self, distance: usize, name: &str) -> Option<Value> {
        if distance == 0 {
            return self.values.get(name).cloned();
        }
        self.find_inner_at(distance)
            .and_then(|ptr| ptr.borrow().values.get(name).cloned())
    }

    pub fn assign_at(&mut self, distance: usize, name: String, value: Value) -> bool {
        if distance == 0 {
            match self.values.insert(name, value) {
                Some(_) => return true,
                None => return false,
            }
        }
        self.find_inner_at(distance)
            .and_then(|ptr| ptr.borrow_mut().values.insert(name, value))
            .is_some()
    }

    fn find_inner_at(&self, distance: usize) -> Option<Rc<RefCell<Self>>> {
        let mut i = 1;
        let mut curr = self.inner.as_ref().cloned();
        while i < distance {
            if let Some(ptr) = curr {
                curr = ptr.borrow().inner.as_ref().cloned();
                i += 1;
            } else {
                break;
            }
        }
        curr
    }
}
