use std::{collections::HashMap, mem};

use crate::lexer::TokenKind;
use crate::parser::{Expr, Stmt, Unit};
use crate::prelude::{self, Prelude};
use crate::visitor::Visitor;

pub struct Interpreter {
    pub symbol_table: HashMap<String, Stmt>,
    angle_unit: Unit,
    prelude: Prelude,
}

impl Interpreter {
    pub fn new(angle_unit: Unit) -> Self {
        let mut hashmap: HashMap<String, Stmt> = HashMap::new();
        for constant in prelude::CONSTANTS {
            hashmap.insert(
                constant.0.to_string(),
                Stmt::VarDecl(
                    constant.0.to_string(),
                    Box::new(Expr::Literal(constant.1.to_string())),
                ),
            );
        }

        Interpreter {
            angle_unit,
            symbol_table: hashmap,
            prelude: Prelude::new(),
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

    pub fn set_angle_unit(&mut self, angle_unit: Unit) {
        self.prelude.angle_unit = angle_unit.clone();
        self.angle_unit = angle_unit;
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

impl Visitor<f64, f64> for Interpreter {
    fn visit_stmt(&mut self, stmt: &Stmt) -> f64 {
        match stmt {
            Stmt::VarDecl(identifier, _) => {
                self.symbol_table.insert(identifier.clone(), stmt.clone());
                0f64
            }
            Stmt::FnDecl(identifier, argument, _) => {
                self.visit_stmt(&Stmt::VarDecl(
                    argument.clone(),
                    Box::new(Expr::Literal(String::from("0"))),
                ));
                self.symbol_table
                    .insert(format!("{}()", identifier.clone()), stmt.clone());
                0f64
            }
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
            Expr::FnCall(identifier, expr) => {
                let x = self.visit_expr(&expr);
                if let Some(result) = self.prelude.call_unary_func(identifier, x) {
                    result
                } else {
                    let stmt = self
                        .symbol_table
                        .get(&format!("{}()", identifier))
                        .expect("Undefined function")
                        .clone();
                    match stmt {
                        Stmt::FnDecl(_, argument, fn_body) => {
                            self.visit_stmt(&Stmt::VarDecl(argument.clone(), expr.clone()));
                            self.visit_expr(&*fn_body)
                        }
                        _ => panic!(),
                    }
                }
            }
        }
    }
}
