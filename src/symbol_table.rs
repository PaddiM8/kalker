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

    pub fn contains_func(&self, key: &str) -> bool {
        prelude::UNARY_FUNCS.contains_key(key)
            || prelude::UNARY_FUNCS.contains_key(key)
            || self.hashmap.contains_key(&format!("{}()", key))
    }
}
