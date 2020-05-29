use std::mem;

use crate::lexer::TokenKind;
use crate::parser::{Expr, Stmt, Unit};
use crate::prelude::{self};
use crate::{symbol_table::SymbolTable, visitor::Visitor};

pub struct Interpreter<'a> {
    symbol_table: &'a mut SymbolTable,
    angle_unit: Unit,
}

impl<'a> Interpreter<'a> {
    pub fn new(angle_unit: Unit, symbol_table: &'a mut SymbolTable) -> Self {
        for constant in prelude::CONSTANTS {
            symbol_table.insert(
                constant.0,
                Stmt::VarDecl(
                    constant.0.to_string(),
                    Box::new(Expr::Literal(constant.1.to_string())),
                ),
            );
        }

        Interpreter {
            angle_unit: angle_unit.clone(),
            symbol_table,
        }
    }

    pub fn interpret(&mut self, statements: Vec<Stmt>) -> Option<f64> {
        for (i, stmt) in statements.iter().enumerate() {
            let value = self.visit_stmt(stmt);

            if i == statements.len() - 1 {
                if let Stmt::Expr(_) = stmt {
                    return Some(value);
                }
            }
        }

        return None;
    }
}

impl TokenKind {
    fn to_unit(&self) -> Unit {
        match self {
            TokenKind::Deg => Unit::Degrees,
            TokenKind::Rad => Unit::Radians,
            _ => panic!("Invalid unit."),
        }
    }
}

impl Unit {
    // TODO: Something more generic
    fn compare(&self, second_token: &Unit) -> bool {
        mem::discriminant(self) == mem::discriminant(second_token)
    }
}

impl<'a> Visitor<f64, f64> for Interpreter<'a> {
    fn visit_stmt(&mut self, stmt: &Stmt) -> f64 {
        match stmt {
            Stmt::VarDecl(identifier, _) => {
                self.symbol_table.insert(&identifier, stmt.clone());
                0f64
            }
            Stmt::FnDecl(_, _, _) => 0f64, // Nothing needs to happen here, since the parser will already have added the FnDecl's to the symbol table.
            Stmt::Expr(expr) => self.visit_expr(&expr),
        }
    }

    fn visit_expr(&mut self, expr: &Expr) -> f64 {
        match expr {
            Expr::Binary(left, op, right) => {
                let left = self.visit_expr(&left);
                let right = self.visit_expr(&right);

                match op {
                    TokenKind::Plus => left + right,
                    TokenKind::Minus => left - right,
                    TokenKind::Star => left * right,
                    TokenKind::Slash => left / right,
                    TokenKind::Power => left.powf(right),
                    _ => 0f64,
                }
            }
            Expr::Unary(_, expr) => self.visit_expr(&expr).clone(),
            Expr::Unit(expr, kind) => {
                let x = self.visit_expr(&expr);

                // Don't do any angle conversions if the defauly angle unit is the same as the unit kind
                if (kind.compare(&TokenKind::Deg) || kind.compare(&TokenKind::Rad))
                    && self.angle_unit.compare(&kind.to_unit())
                {
                    return x;
                }

                match kind {
                    TokenKind::Deg => x.to_radians(),
                    TokenKind::Rad => x.to_degrees(),
                    _ => panic!("Invalid unit."),
                }
            }
            Expr::Var(identifier) => {
                let value = self
                    .symbol_table
                    .get(identifier)
                    .expect("Undefined variable.")
                    .clone();
                if let Stmt::VarDecl(_, expr) = value {
                    return self.visit_expr(&expr);
                }

                panic!("Unknown error.");
            }
            Expr::Literal(value) => value.parse().unwrap(),
            Expr::Group(expr) => self.visit_expr(&expr),
            Expr::FnCall(identifier, expressions) => {
                let prelude_func = match expressions.len() {
                    1 => {
                        let x = self.visit_expr(&expressions[0]);
                        prelude::call_unary_func(identifier, x, &self.angle_unit)
                    }
                    2 => {
                        let x = self.visit_expr(&expressions[0]);
                        let y = self.visit_expr(&expressions[1]);
                        prelude::call_binary_func(identifier, x, y, &self.angle_unit)
                    }
                    _ => None,
                };

                if let Some(result) = prelude_func {
                    result
                } else {
                    let stmt = self
                        .symbol_table
                        .get(&format!("{}()", identifier))
                        .expect("Undefined function")
                        .clone();

                    if let Stmt::FnDecl(_, arguments, fn_body) = stmt {
                        if arguments.len() != expressions.len() {
                            panic!("Incorrect amount of arguments.");
                        }

                        // Initialise the arguments as their own variables.
                        for (i, argument) in arguments.iter().enumerate() {
                            self.visit_stmt(&Stmt::VarDecl(
                                argument.clone(),
                                Box::new(expressions[i].clone()),
                            ));
                        }

                        return self.visit_expr(&*fn_body);
                    }

                    panic!("Unexpected error.");
                }
            }
        }
    }
}
