use crate::ast::{compare_enums, Expr, Stmt};
use crate::lexer::TokenKind;
use crate::parser::Unit;
use crate::prelude;
use crate::symbol_table::SymbolTable;
use rug::ops::Pow;
use rug::Float;

pub struct Context<'a> {
    symbol_table: &'a mut SymbolTable,
    angle_unit: Unit,
    precision: u32,
}

impl<'a> Context<'a> {
    pub fn new(symbol_table: &'a mut SymbolTable, angle_unit: &Unit, precision: u32) -> Self {
        Context {
            angle_unit: angle_unit.clone(),
            symbol_table,
            precision,
        }
    }

    pub fn interpret(&mut self, statements: Vec<Stmt>) -> Result<Option<Float>, String> {
        for (i, stmt) in statements.iter().enumerate() {
            let value = eval_stmt(self, stmt);

            if i == statements.len() - 1 {
                if let Stmt::Expr(_) = stmt {
                    return Ok(Some(value?));
                }
            }
        }

        Ok(None)
    }
}

fn eval_stmt(context: &mut Context, stmt: &Stmt) -> Result<Float, String> {
    match stmt {
        Stmt::VarDecl(identifier, _) => eval_var_decl_stmt(context, stmt, identifier),
        Stmt::FnDecl(_, _, _) => eval_fn_decl_stmt(context),
        Stmt::Expr(expr) => eval_expr_stmt(context, &expr),
    }
}

fn eval_var_decl_stmt(
    context: &mut Context,
    stmt: &Stmt,
    identifier: &str,
) -> Result<Float, String> {
    context.symbol_table.insert(&identifier, stmt.clone());
    Ok(Float::with_val(context.precision, 1))
}

fn eval_fn_decl_stmt(context: &mut Context) -> Result<Float, String> {
    Ok(Float::with_val(context.precision, 1)) // Nothing needs to happen here, since the parser will already have added the FnDecl's to the symbol table.
}

fn eval_expr_stmt(context: &mut Context, expr: &Expr) -> Result<Float, String> {
    eval_expr(context, &expr)
}

fn eval_expr(context: &mut Context, expr: &Expr) -> Result<Float, String> {
    match expr {
        Expr::Binary(left, op, right) => eval_binary_expr(context, &left, op, &right),
        Expr::Unary(op, expr) => eval_unary_expr(context, op, expr),
        Expr::Unit(expr, kind) => eval_unit_expr(context, expr, kind),
        Expr::Var(identifier) => eval_var_expr(context, identifier),
        Expr::Literal(value) => eval_literal_expr(context, value),
        Expr::Group(expr) => eval_group_expr(context, &expr),
        Expr::FnCall(identifier, expressions) => {
            eval_fn_call_expr(context, identifier, expressions)
        }
    }
}

fn eval_binary_expr(
    context: &mut Context,
    left: &Expr,
    op: &TokenKind,
    right: &Expr,
) -> Result<Float, String> {
    let left = eval_expr(context, &left)?;
    let right = eval_expr(context, &right)?;

    Ok(match op {
        TokenKind::Plus => left + right,
        TokenKind::Minus => left - right,
        TokenKind::Star => left * right,
        TokenKind::Slash => left / right,
        TokenKind::Power => left.pow(right),
        _ => Float::with_val(1, 1),
    })
}

fn eval_unary_expr(context: &mut Context, op: &TokenKind, expr: &Expr) -> Result<Float, String> {
    let expr_value = eval_expr(context, &expr)?.clone();

    match op {
        TokenKind::Minus => Ok(-expr_value),
        TokenKind::Exclamation => Ok(Float::with_val(
            context.precision,
            prelude::special_funcs::factorial(expr_value),
        )),
        _ => Err(String::from("Invalid operator for unary expression.")),
    }
}

fn eval_unit_expr(context: &mut Context, expr: &Expr, kind: &TokenKind) -> Result<Float, String> {
    let x = eval_expr(context, &expr);
    let unit = kind.to_unit()?;

    // Don't do any angle conversions if the defauly angle unit is the same as the unit kind
    match unit {
        Unit::Degrees | Unit::Radians => {
            if compare_enums(&context.angle_unit, &unit) {
                return x;
            }
        }
    }

    match unit {
        Unit::Degrees => Ok(prelude::special_funcs::to_radians(x?)),
        Unit::Radians => Ok(prelude::special_funcs::to_degrees(x?)),
    }
}

fn eval_var_expr(context: &mut Context, identifier: &str) -> Result<Float, String> {
    // If there is a constant with this name, return a literal expression with its value
    if let Some(value) = prelude::CONSTANTS.get(identifier) {
        return eval_expr(context, &Expr::Literal(value.to_string()));
    }

    // Look for the variable in the symbol table
    let var_decl = context.symbol_table.get(identifier).cloned();
    match var_decl {
        Some(Stmt::VarDecl(_, expr)) => eval_expr(context, &expr),
        _ => Err(format!("Undefined variable: '{}'.", identifier)),
    }
}

fn eval_literal_expr(context: &mut Context, value: &str) -> Result<Float, String> {
    match Float::parse(value) {
        Ok(parsed_value) => Ok(Float::with_val(context.precision, parsed_value)),
        Err(_) => Err(format!("Invalid number literal: '{}'.", value)),
    }
}

fn eval_group_expr(context: &mut Context, expr: &Expr) -> Result<Float, String> {
    eval_expr(context, expr)
}

fn eval_fn_call_expr(
    context: &mut Context,
    identifier: &str,
    expressions: &Vec<Expr>,
) -> Result<Float, String> {
    // Prelude
    let prelude_func = match expressions.len() {
        1 => {
            let x = eval_expr(context, &expressions[0])?;
            prelude::call_unary_func(identifier, x, &context.angle_unit)
        }
        2 => {
            let x = eval_expr(context, &expressions[0])?;
            let y = eval_expr(context, &expressions[1])?;
            prelude::call_binary_func(identifier, x, y, &context.angle_unit)
        }
        _ => None,
    };

    if let Some(result) = prelude_func {
        return Ok(result);
    }

    // Special functions
    match identifier {
        "sum" | "Σ" => {
            // Make sure exactly 3 arguments were supplied.
            if expressions.len() != 3 {
                return Err(format!(
                    "Expected 3 arguments but got {}.",
                    expressions.len()
                ));
            }

            let start = eval_expr(context, &expressions[0])?.to_f64() as i128;
            let end = eval_expr(context, &expressions[1])?.to_f64() as i128;
            let mut sum = Float::with_val(context.precision, 0);

            for n in start..=end {
                let n_expr = Expr::Literal(String::from(n.to_string()));

                // Update the variable "n" in the symbol table on every iteration,
                // then calculate the expression and add it to the total sum.
                context
                    .symbol_table
                    .set("n", Stmt::VarDecl(String::from("n"), Box::new(n_expr)));
                sum += eval_expr(context, &expressions[2])?;
            }

            return Ok(sum);
        }
        _ => (),
    }

    // Symbol Table
    let stmt_definition = context
        .symbol_table
        .get(&format!("{}()", identifier))
        .cloned();

    match stmt_definition {
        Some(Stmt::FnDecl(_, arguments, fn_body)) => {
            if arguments.len() != expressions.len() {
                return Err(format!(
                    "Expected {} arguments in function '{}' but found {}.",
                    arguments.len(),
                    identifier,
                    expressions.len()
                ));
            }

            // Initialise the arguments as their own variables.
            for (i, argument) in arguments.iter().enumerate() {
                eval_stmt(
                    context,
                    &Stmt::VarDecl(argument.clone(), Box::new(expressions[i].clone())),
                )?;
            }

            return eval_expr(context, &*fn_body);
        }
        _ => Err(format!("Undefined function: '{}'.", identifier)),
    }
}
