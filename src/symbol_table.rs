use crate::{ast::Stmt, prelude};
use std::collections::HashMap;

pub struct SymbolTable {
    hashmap: HashMap<String, Stmt>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            hashmap: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: &str, value: Stmt) {
        self.hashmap.insert(key.into(), value);
    }

    pub fn get(&self, key: &str) -> Option<&Stmt> {
        self.hashmap.get(key)
    }

    pub fn contains_var(&self, identifier: &str) -> bool {
        prelude::CONSTANTS.contains_key(identifier) || self.hashmap.contains_key(identifier)
    }

    pub fn contains_func(&self, identifier: &str) -> bool {
        prelude::UNARY_FUNCS.contains_key(identifier)
            || prelude::UNARY_FUNCS.contains_key(identifier)
            || self.hashmap.contains_key(&format!("{}()", identifier))
    }
}
