use crate::parser::{Expr, Stmt};

pub trait Visitor<T, U> {
    fn visit_stmt(&mut self, stmt: &Stmt) -> T;
    fn visit_expr(&mut self, expr: &Expr) -> U;
}
