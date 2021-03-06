use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::tree::runtime::Value;

#[derive(Clone)]
pub struct Env(Rc<RefCell<Scope>>);

impl Env {
    pub fn new() -> Self {
        Self(Rc::new(RefCell::new(Scope::new())))
    }

    pub fn wrap(env: &Env) -> Self {
        Self(Rc::new(RefCell::new(Scope::wrap(&env.0))))
    }

    pub fn unwrap(&self) -> Env {
        match self.0.borrow().inner {
            Some(ref ptr) => Self(Rc::clone(ptr)),
            None => panic!("fatal error: cannot unwrap global root environment"),
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.0.borrow_mut().set(name, value);
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        let mut curr = Some(Rc::clone(&self.0));
        while let Some(ptr) = curr {
            let scope = ptr.borrow();
            if scope.has(name) {
                return scope.get(name);
            }
            curr = scope.next();
        }
        None
    }

    pub fn assign(&mut self, name: String, value: Value) -> bool {
        let mut curr = Some(Rc::clone(&self.0));
        while let Some(ptr) = curr {
            let mut scope = ptr.borrow_mut();
            if scope.has(&name) {
                return scope.set(name, value).is_some();
            }
            curr = scope.next();
        }
        false
    }

    pub fn get_at(&self, distance: usize, name: &str) -> Option<Value> {
        self.find_at(distance)
            .and_then(|ptr| ptr.borrow().get(name))
    }

    pub fn assign_at(&mut self, distance: usize, name: String, value: Value) -> bool {
        self.find_at(distance)
            .and_then(|ptr| ptr.borrow_mut().set(name, value))
            .is_some()
    }

    fn find_at(&self, distance: usize) -> Option<Rc<RefCell<Scope>>> {
        let mut i = 0;
        let mut curr = Some(Rc::clone(&self.0));
        while i < distance {
            if let Some(ptr) = curr {
                curr = ptr.borrow().next();
            } else {
                break;
            }
            i += 1;
        }
        curr
    }
}

struct Scope {
    values: HashMap<String, Value>,
    inner: Option<Rc<RefCell<Scope>>>,
}

impl Scope {
    fn new() -> Self {
        Scope {
            values: HashMap::new(),
            inner: None,
        }
    }

    fn wrap(ptr: &Rc<RefCell<Scope>>) -> Self {
        Scope {
            values: HashMap::new(),
            inner: Some(Rc::clone(ptr)),
        }
    }

    fn next(&self) -> Option<Rc<RefCell<Scope>>> {
        self.inner.as_ref().cloned()
    }

    fn has(&self, name: &str) -> bool {
        self.values.contains_key(name)
    }

    fn get(&self, name: &str) -> Option<Value> {
        self.values.get(name).cloned()
    }

    fn set(&mut self, name: String, value: Value) -> Option<Value> {
        self.values.insert(name, value)
    }
}
