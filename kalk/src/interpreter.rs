use crate::ast::{Expr, Stmt};
use crate::lexer::TokenKind;
use crate::parser::CalcError;
use crate::parser::DECL_UNIT;
use crate::prelude;
use crate::symbol_table::SymbolTable;
use rug::ops::Pow;
use rug::Float;

pub struct Context<'a> {
    symbol_table: &'a mut SymbolTable,
    angle_unit: String,
    precision: u32,
}

impl<'a> Context<'a> {
    pub fn new(symbol_table: &'a mut SymbolTable, angle_unit: &str, precision: u32) -> Self {
        Context {
            angle_unit: angle_unit.into(),
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
        Stmt::VarDecl(_, _) => eval_var_decl_stmt(context, stmt),
        Stmt::FnDecl(_, _, _) => eval_fn_decl_stmt(context),
        Stmt::UnitDecl(_, _, _) => eval_unit_decl_stmt(context),
        Stmt::Expr(expr) => eval_expr_stmt(context, &expr),
    }
}

fn eval_var_decl_stmt(context: &mut Context, stmt: &Stmt) -> Result<Float, CalcError> {
    context.symbol_table.insert(stmt.clone());
    Ok(Float::with_val(context.precision, 1))
}

fn eval_fn_decl_stmt(context: &mut Context) -> Result<Float, CalcError> {
    Ok(Float::with_val(context.precision, 1)) // Nothing needs to happen here, since the parser will already have added the FnDecl's to the symbol table.
}

fn eval_unit_decl_stmt(context: &mut Context) -> Result<Float, CalcError> {
    Ok(Float::with_val(context.precision, 1))
}

fn eval_expr_stmt(context: &mut Context, expr: &Expr) -> Result<Float, CalcError> {
    eval_expr(context, &expr)
}

fn eval_expr(context: &mut Context, expr: &Expr) -> Result<Float, CalcError> {
    match expr {
        Expr::Binary(left, op, right) => eval_binary_expr(context, &left, op, &right),
        Expr::Unary(op, expr) => eval_unary_expr(context, op, expr),
        Expr::Unit(identifier, expr) => eval_unit_expr(context, identifier, expr),
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
    left_expr: &Expr,
    op: &TokenKind,
    right_expr: &Expr,
) -> Result<Float, CalcError> {
    let left = eval_expr(context, left_expr)?;
    let right = if let Expr::Unit(left_unit, _) = left_expr {
        if let Expr::Unit(right_unit, right_unit_expr) = right_expr {
            convert_unit(context, right_unit_expr, right_unit, &left_unit)?
        } else {
            eval_expr(context, right_expr)?
        }
    } else {
        eval_expr(context, right_expr)?
    };

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
    identifier: &str,
    expr: &Expr,
) -> Result<Float, CalcError> {
    let angle_unit = &context.angle_unit.clone();
    if (identifier == "rad" || identifier == "deg") && angle_unit != identifier {
        return convert_unit(context, expr, identifier, angle_unit);
    }

    eval_expr(context, expr)
}

pub fn convert_unit(
    context: &mut Context,
    expr: &Expr,
    from_unit: &str,
    to_unit: &str,
) -> Result<Float, CalcError> {
    if let Some(Stmt::UnitDecl(_, _, unit_def)) =
        context.symbol_table.get_unit(to_unit, from_unit).cloned()
    {
        context
            .symbol_table
            .insert(Stmt::VarDecl(DECL_UNIT.into(), Box::new(expr.clone())));

        eval_expr(context, &unit_def)
    } else {
        Err(CalcError::InvalidUnit)
    }
}

fn eval_var_expr(context: &mut Context, identifier: &str) -> Result<Float, CalcError> {
    // If there is a constant with this name, return a literal expression with its value
    if let Some(value) = prelude::CONSTANTS.get(identifier) {
        return eval_expr(context, &Expr::Literal((*value).to_string()));
    }

    // Look for the variable in the symbol table
    let var_decl = context.symbol_table.get_var(identifier).cloned();
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
            prelude::call_unary_func(context, identifier, x, &context.angle_unit.clone())
        }
        2 => {
            let x = eval_expr(context, &expressions[0])?;
            let y = eval_expr(context, &expressions[1])?;
            prelude::call_binary_func(context, identifier, x, y, &context.angle_unit.clone())
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
                    .set(Stmt::VarDecl(String::from("n"), Box::new(n_expr)));
                sum += eval_expr(context, &expressions[2])?;
            }

            return Ok(sum);
        }
        _ => (),
    }

    // Symbol Table
    let stmt_definition = context.symbol_table.get_fn(identifier).cloned();

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
    use test_case::test_case;

    const PRECISION: u32 = 53;

    lazy_static::lazy_static! {
        static ref DEG_RAD_UNIT: Stmt = unit_decl(
            "deg",
            "rad",
            binary(
                binary(
                    var(crate::parser::DECL_UNIT),
                    TokenKind::Star,
                    literal("180"),
                ),
                TokenKind::Slash,
                var("pi"),
            ),
        );
        static ref RAD_DEG_UNIT: Stmt = unit_decl(
            "rad",
            "deg",
            binary(
                binary(var(crate::parser::DECL_UNIT), TokenKind::Star, var("pi")),
                TokenKind::Slash,
                literal("180"),
            ),
        );
    }

    fn interpret(stmt: Stmt) -> Result<Option<Float>, CalcError> {
        let mut symbol_table = SymbolTable::new();
        symbol_table
            .insert(DEG_RAD_UNIT.clone())
            .insert(RAD_DEG_UNIT.clone());

        let mut context = Context::new(&mut symbol_table, "rad", PRECISION);
        context.interpret(vec![stmt])
    }

    fn cmp(x: Float, y: f64) -> bool {
        println!("{} = {}", x.to_f64(), y);
        (x.to_f64() - y).abs() < 0.0001
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

        assert_eq!(interpret(neg).unwrap().unwrap(), -1);
        assert_eq!(interpret(fact).unwrap().unwrap(), 120);
    }

    #[test]
    fn test_angle_units() {
        let rad_explicit = Stmt::Expr(fn_call("sin", vec![*unit("rad", literal("1"))]));
        let deg_explicit = Stmt::Expr(fn_call("sin", vec![*unit("deg", literal("1"))]));
        let implicit = Stmt::Expr(fn_call("sin", vec![*literal("1")]));

        assert!(cmp(interpret(rad_explicit).unwrap().unwrap(), 0.84147098));
        assert!(cmp(interpret(deg_explicit).unwrap().unwrap(), 0.01745240));

        let mut rad_symbol_table = SymbolTable::new();
        rad_symbol_table
            .insert(DEG_RAD_UNIT.clone())
            .insert(RAD_DEG_UNIT.clone());
        let mut deg_symbol_table = SymbolTable::new();
        deg_symbol_table
            .insert(DEG_RAD_UNIT.clone())
            .insert(RAD_DEG_UNIT.clone());
        let mut rad_context = Context::new(&mut rad_symbol_table, "rad", PRECISION);
        let mut deg_context = Context::new(&mut deg_symbol_table, "deg", PRECISION);

        assert!(cmp(
            rad_context
                .interpret(vec![implicit.clone()])
                .unwrap()
                .unwrap(),
            0.84147098
        ));
        assert!(cmp(
            deg_context.interpret(vec![implicit]).unwrap().unwrap(),
            0.01745240
        ));
    }

    #[test]
    fn test_var() {
        let stmt = Stmt::Expr(var("x"));

        // Prepare by inserting a variable declaration in the symbol table.
        let mut symbol_table = SymbolTable::new();
        symbol_table.insert(var_decl("x", literal("1")));

        let mut context = Context::new(&mut symbol_table, "rad", PRECISION);
        assert_eq!(context.interpret(vec![stmt]).unwrap().unwrap(), 1);
    }

    #[test]
    fn test_undefined_var() {
        let stmt = Stmt::Expr(var("x"));

        assert_eq!(
            interpret(stmt),
            Err(CalcError::UndefinedVar(String::from("x")))
        );
    }

    #[test]
    fn test_var_decl() {
        let stmt = var_decl("x", literal("1"));
        let mut symbol_table = SymbolTable::new();
        Context::new(&mut symbol_table, "rad", PRECISION)
            .interpret(vec![stmt])
            .unwrap();

        assert!(symbol_table.contains_var("x"));
    }

    #[test]
    fn test_fn() {
        let stmt = Stmt::Expr(fn_call("f", vec![*literal("1")]));

        // Prepare by inserting a variable declaration in the symbol table.
        let mut symbol_table = SymbolTable::new();
        symbol_table.insert(fn_decl(
            "f",
            vec![String::from("x")],
            binary(var("x"), TokenKind::Plus, literal("2")),
        ));

        let mut context = Context::new(&mut symbol_table, "rad", PRECISION);
        assert_eq!(context.interpret(vec![stmt]).unwrap().unwrap(), 3);
    }

    #[test]
    fn test_undefined_fn() {
        let stmt = Stmt::Expr(fn_call("f", vec![*literal("1")]));

        assert_eq!(
            interpret(stmt),
            Err(CalcError::UndefinedFn(String::from("f")))
        );
    }

    #[test_case("1", "2", 9f64)]
    #[test_case("1.2", "2.3", 9f64)]
    fn test_sum_fn(start: &str, to: &str, result: f64) {
        let stmt = Stmt::Expr(fn_call(
            "sum",
            vec![
                *literal(start),
                *literal(to),
                *binary(var("n"), TokenKind::Plus, literal("3")),
            ],
        ));

        assert_eq!(interpret(stmt).unwrap().unwrap(), result);
    }
}
