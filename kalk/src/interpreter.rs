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

    pub fn interpret(
        &mut self,
        statements: Vec<Stmt>,
    ) -> Result<Option<(Float, String)>, CalcError> {
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

fn eval_stmt(context: &mut Context, stmt: &Stmt) -> Result<(Float, String), CalcError> {
    match stmt {
        Stmt::VarDecl(_, _) => eval_var_decl_stmt(context, stmt),
        Stmt::FnDecl(_, _, _) => eval_fn_decl_stmt(context),
        Stmt::UnitDecl(_, _, _) => eval_unit_decl_stmt(context),
        Stmt::Expr(expr) => eval_expr_stmt(context, &expr),
    }
}

fn eval_var_decl_stmt(context: &mut Context, stmt: &Stmt) -> Result<(Float, String), CalcError> {
    context.symbol_table.insert(stmt.clone());
    Ok((Float::with_val(context.precision, 1), String::new()))
}

fn eval_fn_decl_stmt(context: &mut Context) -> Result<(Float, String), CalcError> {
    Ok((Float::with_val(context.precision, 1), String::new())) // Nothing needs to happen here, since the parser will already have added the FnDecl's to the symbol table.
}

fn eval_unit_decl_stmt(context: &mut Context) -> Result<(Float, String), CalcError> {
    Ok((Float::with_val(context.precision, 1), String::new()))
}

fn eval_expr_stmt(context: &mut Context, expr: &Expr) -> Result<(Float, String), CalcError> {
    eval_expr(context, &expr, "")
}

fn eval_expr(context: &mut Context, expr: &Expr, unit: &str) -> Result<(Float, String), CalcError> {
    match expr {
        Expr::Binary(left, op, right) => eval_binary_expr(context, &left, op, &right, unit),
        Expr::Unary(op, expr) => eval_unary_expr(context, op, expr, unit),
        Expr::Unit(identifier, expr) => eval_unit_expr(context, identifier, expr),
        Expr::Var(identifier) => eval_var_expr(context, identifier, unit),
        Expr::Literal(value) => eval_literal_expr(context, value, unit),
        Expr::Group(expr) => eval_group_expr(context, &expr, unit),
        Expr::FnCall(identifier, expressions) => {
            eval_fn_call_expr(context, identifier, expressions, unit)
        }
    }
}

fn eval_binary_expr(
    context: &mut Context,
    left_expr: &Expr,
    op: &TokenKind,
    right_expr: &Expr,
    unit: &str,
) -> Result<(Float, String), CalcError> {
    if let TokenKind::ToKeyword = op {
        // TODO: When the unit conversion function takes a Float instead of Expr,
        // move this to the match statement further down.
        if let Expr::Var(right_unit) = right_expr {
            let (_, left_unit) = eval_expr(context, left_expr, "")?;
            return convert_unit(context, left_expr, &left_unit, &right_unit); // TODO: Avoid evaluating this twice.
        }
    }

    let (left, left_unit) = eval_expr(context, left_expr, "")?;
    let (right, _) = if left_unit.len() > 0 {
        let (_, right_unit) = eval_expr(context, right_expr, "")?; // TODO: Avoid evaluating this twice.

        if right_unit.len() > 0 {
            convert_unit(context, right_expr, &right_unit, &left_unit)?
        } else {
            eval_expr(context, right_expr, unit)?
        }
    } else {
        eval_expr(context, right_expr, unit)?
    };

    let final_unit = if unit.len() == 0 {
        left_unit
    } else {
        unit.into()
    };

    Ok((
        match op {
            TokenKind::Plus => left + right,
            TokenKind::Minus => left - right,
            TokenKind::Star => left * right,
            TokenKind::Slash => left / right,
            TokenKind::Power => left.pow(right),
            _ => Float::with_val(1, 1),
        },
        final_unit,
    ))
}

fn eval_unary_expr(
    context: &mut Context,
    op: &TokenKind,
    expr: &Expr,
    unit: &str,
) -> Result<(Float, String), CalcError> {
    let (expr_value, unit) = eval_expr(context, &expr, unit)?;

    match op {
        TokenKind::Minus => Ok((-expr_value, unit)),
        TokenKind::Exclamation => Ok((
            Float::with_val(
                context.precision,
                prelude::special_funcs::factorial(expr_value),
            ),
            unit,
        )),
        _ => Err(CalcError::InvalidOperator),
    }
}

fn eval_unit_expr(
    context: &mut Context,
    identifier: &str,
    expr: &Expr,
) -> Result<(Float, String), CalcError> {
    let angle_unit = &context.angle_unit.clone();
    if (identifier == "rad" || identifier == "deg") && angle_unit != identifier {
        return convert_unit(context, expr, identifier, angle_unit);
    }

    eval_expr(context, expr, identifier)
}

pub fn convert_unit(
    context: &mut Context,
    expr: &Expr,
    from_unit: &str,
    to_unit: &str,
) -> Result<(Float, String), CalcError> {
    if let Some(Stmt::UnitDecl(_, _, unit_def)) =
        context.symbol_table.get_unit(to_unit, from_unit).cloned()
    {
        context
            .symbol_table
            .insert(Stmt::VarDecl(DECL_UNIT.into(), Box::new(expr.clone())));

        Ok((eval_expr(context, &unit_def, "")?.0, to_unit.into()))
    } else {
        Err(CalcError::InvalidUnit)
    }
}

fn eval_var_expr(
    context: &mut Context,
    identifier: &str,
    unit: &str,
) -> Result<(Float, String), CalcError> {
    // If there is a constant with this name, return a literal expression with its value
    if let Some(value) = prelude::CONSTANTS.get(identifier) {
        return eval_expr(context, &Expr::Literal((*value).to_string()), unit);
    }

    // Look for the variable in the symbol table
    let var_decl = context.symbol_table.get_var(identifier).cloned();
    match var_decl {
        Some(Stmt::VarDecl(_, expr)) => eval_expr(context, &expr, unit),
        _ => Err(CalcError::UndefinedVar(identifier.into())),
    }
}

fn eval_literal_expr(
    context: &mut Context,
    value: &str,
    unit: &str,
) -> Result<(Float, String), CalcError> {
    match Float::parse(value) {
        Ok(parsed_value) => Ok((
            Float::with_val(context.precision, parsed_value),
            unit.into(),
        )),
        Err(_) => Err(CalcError::InvalidNumberLiteral(value.into())),
    }
}

fn eval_group_expr(
    context: &mut Context,
    expr: &Expr,
    unit: &str,
) -> Result<(Float, String), CalcError> {
    eval_expr(context, expr, unit)
}

fn eval_fn_call_expr(
    context: &mut Context,
    identifier: &str,
    expressions: &[Expr],
    unit: &str,
) -> Result<(Float, String), CalcError> {
    // Prelude
    let prelude_func = match expressions.len() {
        1 => {
            let x = eval_expr(context, &expressions[0], "")?.0;
            prelude::call_unary_func(context, identifier, x, &context.angle_unit.clone())
        }
        2 => {
            let x = eval_expr(context, &expressions[0], "")?.0;
            let y = eval_expr(context, &expressions[1], "")?.0;
            prelude::call_binary_func(context, identifier, x, y, &context.angle_unit.clone())
        }
        _ => None,
    };

    if let Some(result) = prelude_func {
        return Ok((result, unit.into()));
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

            let start = eval_expr(context, &expressions[0], "")?.0.to_f64() as i128;
            let end = eval_expr(context, &expressions[1], "")?.0.to_f64() as i128;
            let mut sum = Float::with_val(context.precision, 0);

            for n in start..=end {
                let n_expr = Expr::Literal(n.to_string());

                // Update the variable "n" in the symbol table on every iteration,
                // then calculate the expression and add it to the total sum.
                context
                    .symbol_table
                    .set(Stmt::VarDecl(String::from("n"), Box::new(n_expr)));
                sum += eval_expr(context, &expressions[2], "")?.0;
            }

            return Ok((sum, unit.into()));
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

            eval_expr(context, &fn_body, unit)
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

    fn interpret_with_unit(stmt: Stmt) -> Result<Option<(Float, String)>, CalcError> {
        let mut symbol_table = SymbolTable::new();
        symbol_table
            .insert(DEG_RAD_UNIT.clone())
            .insert(RAD_DEG_UNIT.clone());

        let mut context = Context::new(&mut symbol_table, "rad", PRECISION);
        context.interpret(vec![stmt])
    }

    fn interpret(stmt: Stmt) -> Result<Option<Float>, CalcError> {
        if let Some((result, _)) = interpret_with_unit(stmt)? {
            Ok(Some(result))
        } else {
            Ok(None)
        }
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
                .unwrap()
                .0,
            0.84147098
        ));
        assert!(cmp(
            deg_context.interpret(vec![implicit]).unwrap().unwrap().0,
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
