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
            Stmt::VarDecl(identifier, _) => self.eval_var_decl_stmt(stmt, identifier),
            Stmt::FnDecl(_, _, _) => self.eval_fn_decl_stmt(),
            Stmt::Expr(expr) => self.eval_expr_stmt(&expr),
        }
    }

    fn visit_expr(&mut self, expr: &Expr) -> f64 {
        match expr {
            Expr::Binary(left, op, right) => self.eval_binary_expr(&left, op, &right),
            Expr::Unary(_, expr) => self.eval_unary_expr(expr),
            Expr::Unit(expr, kind) => self.eval_unit_expr(expr, kind),
            Expr::Var(identifier) => self.eval_var_expr(identifier),
            Expr::Literal(value) => self.eval_literal_expr(value),
            Expr::Group(expr) => self.eval_group_expr(&expr),
            Expr::FnCall(identifier, expressions) => {
                self.eval_fn_call_expr(identifier, expressions)
            }
        }
    }
}

impl<'a> Interpreter<'a> {
    fn eval_var_decl_stmt(&mut self, stmt: &Stmt, identifier: &str) -> f64 {
        self.symbol_table.insert(&identifier, stmt.clone());
        0f64
    }

    fn eval_fn_decl_stmt(&mut self) -> f64 {
        0f64 // Nothing needs to happen here, since the parser will already have added the FnDecl's to the symbol table.
    }

    fn eval_expr_stmt(&mut self, expr: &Expr) -> f64 {
        self.visit_expr(&expr)
    }
}

impl<'a> Interpreter<'a> {
    fn eval_binary_expr(&mut self, left: &Expr, op: &TokenKind, right: &Expr) -> f64 {
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

    fn eval_unary_expr(&mut self, expr: &Expr) -> f64 {
        self.visit_expr(&expr).clone()
    }

    fn eval_unit_expr(&mut self, expr: &Expr, kind: &TokenKind) -> f64 {
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

    fn eval_var_expr(&mut self, identifier: &str) -> f64 {
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

    fn eval_literal_expr(&mut self, value: &str) -> f64 {
        value.parse().unwrap()
    }

    fn eval_group_expr(&mut self, expr: &Expr) -> f64 {
        self.visit_expr(expr)
    }

    fn eval_fn_call_expr(&mut self, identifier: &str, expressions: &Vec<Expr>) -> f64 {
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
            return result;
        }

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
