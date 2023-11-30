use crate::{ast::Expr, ast::Identifier, ast::Stmt, prelude};
use std::collections::HashMap;

#[derive(Debug)]
pub struct SymbolTable {
    pub(crate) hashmap: HashMap<String, Stmt>,
    pub(crate) unit_types: HashMap<String, ()>,
}

impl SymbolTable {
    pub fn new() -> Self {
        let mut symbol_table = SymbolTable {
            hashmap: HashMap::new(),
            unit_types: HashMap::new(),
        };

        // i = sqrt(-1)
        symbol_table.insert(Stmt::VarDecl(
            Identifier::from_full_name("i"),
            Box::new(Expr::FnCall(
                Identifier::from_full_name("sqrt"),
                vec![Expr::Literal(crate::float!(-1f64))],
            )),
        ));

        symbol_table
    }

    pub fn insert(&mut self, value: Stmt) -> &mut Self {
        match &value {
            Stmt::VarDecl(identifier, _) => {
                self.hashmap
                    .insert(format!("var.{}", identifier.full_name), value);
            }
            Stmt::UnitDecl(identifier, to_unit, _) => {
                self.unit_types.insert(identifier.to_string(), ());
                self.unit_types.insert(to_unit.to_string(), ());
                self.hashmap
                    .insert(format!("unit.{}.{}", identifier, to_unit), value);
            }
            Stmt::FnDecl(identifier, _, _) => {
                self.hashmap
                    .insert(format!("fn.{}", identifier.full_name), value);
            }
            _ => panic!("Can only insert VarDecl, UnitDecl and FnDecl into symbol table."),
        }

        self
    }

    pub fn get_var(&self, key: &str) -> Option<&Stmt> {
        self.hashmap.get(&format!("var.{}", key))
    }

    pub fn get_unit(&self, key: &str, to_unit: &str) -> Option<&Stmt> {
        self.hashmap.get(&format!("unit.{}.{}", key, to_unit))
    }

    pub fn get_fn(&self, key: &str) -> Option<&Stmt> {
        self.hashmap.get(&format!("fn.{}", key))
    }

    pub fn set(&mut self, value: Stmt) {
        let existing_item = match &value {
            Stmt::VarDecl(identifier, _) => self
                .hashmap
                .get_mut(&format!("var.{}", identifier.full_name)),
            Stmt::UnitDecl(identifier, to_unit, _) => self
                .hashmap
                .get_mut(&format!("unit.{}.{}", identifier, to_unit)),
            Stmt::FnDecl(identifier, _, _) => self
                .hashmap
                .get_mut(&format!("fn.{}", identifier.full_name)),
            _ => panic!("Can only set VarDecl, UnitDecl and FnDecl in symbol table."),
        };

        if let Some(stmt) = existing_item {
            *stmt = value;
        } else {
            self.insert(value);
        }
    }

    pub fn get_and_remove_fn(&mut self, identifier: &str) -> Option<Stmt> {
        self.hashmap.remove(&format!("fn.{}", identifier))
    }

    pub fn get_and_remove_var(&mut self, identifier: &str) -> Option<Stmt> {
        self.hashmap.remove(&format!("var.{}", identifier))
    }

    pub fn contains_var(&self, identifier: &str) -> bool {
        prelude::is_constant(identifier)
            || identifier == "i"
            || self.hashmap.contains_key(&format!("var.{}", identifier))
    }

    pub fn contains_unit(&self, identifier: &str) -> bool {
        self.unit_types.contains_key(identifier)
    }

    pub fn contains_fn(&self, identifier: &str) -> bool {
        prelude::is_prelude_func(identifier)
            || self.hashmap.contains_key(&format!("fn.{}", identifier))
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}
