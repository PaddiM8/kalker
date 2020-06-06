use crate::ast::{Expr, Stmt};
use crate::lexer::TokenKind;
use crate::parser::CalcError;
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

    pub fn interpret(&mut self, statements: Vec<Stmt>) -> Result<Option<Float>, CalcError> {
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

fn eval_stmt(context: &mut Context, stmt: &Stmt) -> Result<Float, CalcError> {
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
) -> Result<Float, CalcError> {
    context.symbol_table.insert(&identifier, stmt.clone());
    Ok(Float::with_val(context.precision, 1))
}

fn eval_fn_decl_stmt(context: &mut Context) -> Result<Float, CalcError> {
    Ok(Float::with_val(context.precision, 1)) // Nothing needs to happen here, since the parser will already have added the FnDecl's to the symbol table.
}

fn eval_expr_stmt(context: &mut Context, expr: &Expr) -> Result<Float, CalcError> {
    eval_expr(context, &expr)
}

fn eval_expr(context: &mut Context, expr: &Expr) -> Result<Float, CalcError> {
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
) -> Result<Float, CalcError> {
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

fn eval_unary_expr(context: &mut Context, op: &TokenKind, expr: &Expr) -> Result<Float, CalcError> {
    let expr_value = eval_expr(context, &expr)?;

    match op {
        TokenKind::Minus => Ok(-expr_value),
        TokenKind::Exclamation => Ok(Float::with_val(
            context.precision,
            prelude::special_funcs::factorial(expr_value),
        )),
        _ => Err(CalcError::InvalidOperator),
    }
}

fn eval_unit_expr(
    context: &mut Context,
    expr: &Expr,
    kind: &TokenKind,
) -> Result<Float, CalcError> {
    let x = eval_expr(context, &expr);
    let unit = kind.to_unit()?;

    // Don't do any angle conversions if the defauly angle unit is the same as the unit kind
    match unit {
        Unit::Degrees | Unit::Radians => {
            if context.angle_unit == unit {
                return x;
            }
        }
    }

    match unit {
        Unit::Degrees => Ok(prelude::special_funcs::to_radians(x?)),
        Unit::Radians => Ok(prelude::special_funcs::to_degrees(x?)),
    }
}

fn eval_var_expr(context: &mut Context, identifier: &str) -> Result<Float, CalcError> {
    // If there is a constant with this name, return a literal expression with its value
    if let Some(value) = prelude::CONSTANTS.get(identifier) {
        return eval_expr(context, &Expr::Literal((*value).to_string()));
    }

    // Look for the variable in the symbol table
    let var_decl = context.symbol_table.get(identifier).cloned();
    match var_decl {
        Some(Stmt::VarDecl(_, expr)) => eval_expr(context, &expr),
        _ => Err(CalcError::UndefinedVar(identifier.into())),
    }
}

fn eval_literal_expr(context: &mut Context, value: &str) -> Result<Float, CalcError> {
    match Float::parse(value) {
        Ok(parsed_value) => Ok(Float::with_val(context.precision, parsed_value)),
        Err(_) => Err(CalcError::InvalidNumberLiteral(value.into())),
    }
}

fn eval_group_expr(context: &mut Context, expr: &Expr) -> Result<Float, CalcError> {
    eval_expr(context, expr)
}

fn eval_fn_call_expr(
    context: &mut Context,
    identifier: &str,
    expressions: &[Expr],
) -> Result<Float, CalcError> {
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
                return Err(CalcError::IncorrectAmountOfArguments(
                    3,
                    "sum".into(),
                    expressions.len(),
                ));
            }

            let start = eval_expr(context, &expressions[0])?.to_f64() as i128;
            let end = eval_expr(context, &expressions[1])?.to_f64() as i128;
            let mut sum = Float::with_val(context.precision, 0);

            for n in start..=end {
                let n_expr = Expr::Literal(n.to_string());

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
                return Err(CalcError::IncorrectAmountOfArguments(
                    arguments.len(),
                    identifier.into(),
                    expressions.len(),
                ));
            }

            // Initialise the arguments as their own variables.
            for (i, argument) in arguments.iter().enumerate() {
                eval_stmt(
                    context,
                    &Stmt::VarDecl(argument.clone(), Box::new(expressions[i].clone())),
                )?;
            }

            eval_expr(context, &*fn_body)
        }
        _ => Err(CalcError::UndefinedFn(identifier.into())),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::TokenKind::*;
    use crate::test_helpers::*;

    const PRECISION: u32 = 53;

    fn interpret(stmt: Stmt) -> Result<Option<Float>, CalcError> {
        let mut symbol_table = SymbolTable::new();
        let mut context = Context::new(&mut symbol_table, &Unit::Radians, PRECISION);
        context.interpret(vec![stmt])
    }

    #[test]
    fn test_literal() {
        let stmt = Stmt::Expr(literal("1"));

        assert_eq!(
            interpret(stmt).unwrap().unwrap(),
            Float::with_val(PRECISION, 1)
        );
    }

    #[test]
    fn test_binary() {
        let add = Stmt::Expr(binary(literal("2"), Plus, literal("3")));
        let sub = Stmt::Expr(binary(literal("2"), Minus, literal("3")));
        let mul = Stmt::Expr(binary(literal("2"), Star, literal("3")));
        let div = Stmt::Expr(binary(literal("2"), Slash, literal("4")));
        let pow = Stmt::Expr(binary(literal("2"), Power, literal("3")));

        assert_eq!(interpret(add).unwrap().unwrap(), 5);
        assert_eq!(interpret(sub).unwrap().unwrap(), -1);
        assert_eq!(interpret(mul).unwrap().unwrap(), 6);
        assert_eq!(interpret(div).unwrap().unwrap(), 0.5);
        assert_eq!(interpret(pow).unwrap().unwrap(), 8);
    }

    #[test]
    fn test_unary() {
        let neg = Stmt::Expr(unary(Minus, literal("1")));
        let fact = Stmt::Expr(unary(Exclamation, literal("5")));
        let fact_dec = Stmt::Expr(unary(Exclamation, literal("5.2")));

        assert_eq!(interpret(neg).unwrap().unwrap(), -1);
        assert_eq!(interpret(fact).unwrap().unwrap(), 120);

        let fact_dec_result = interpret(fact_dec).unwrap().unwrap();
        assert!(fact_dec_result > 169.406 && fact_dec_result < 169.407);
    }

    #[test]
    fn test_unit() {
        let rad = Stmt::Expr(Box::new(Expr::Unit(literal("1"), Rad)));
        let deg = Stmt::Expr(Box::new(Expr::Unit(literal("1"), Deg)));

        assert_eq!(interpret(rad).unwrap().unwrap(), 1);
        assert_eq!(
            interpret(deg).unwrap().unwrap(),
            Float::with_val(10, 0.017456)
        );
    }

    #[test]
    fn test_var() {
        let stmt = Stmt::Expr(Box::new(Expr::Var(String::from("x"))));

        // Prepare by inserting a variable declaration in the symbol table.
        let mut symbol_table = SymbolTable::new();
        symbol_table.insert("x", Stmt::VarDecl(String::from("x"), literal("1")));

        let mut context = Context::new(&mut symbol_table, &Unit::Radians, PRECISION);
        assert_eq!(context.interpret(vec![stmt]).unwrap().unwrap(), 1);
    }

    #[test]
    fn test_undefined_var() {
        let stmt = Stmt::Expr(Box::new(Expr::Var(String::from("x"))));

        assert_eq!(
            interpret(stmt),
            Err(CalcError::UndefinedVar(String::from("x")))
        );
    }

    #[test]
    fn test_var_decl() {
        let stmt = Stmt::VarDecl(String::from("x"), literal("1"));
        let mut symbol_table = SymbolTable::new();
        Context::new(&mut symbol_table, &Unit::Radians, PRECISION)
            .interpret(vec![stmt])
            .unwrap();

        assert!(symbol_table.contains_var("x"));
    }
}
