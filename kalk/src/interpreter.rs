use crate::ast::Identifier;
use crate::ast::{Expr, Stmt};
use crate::calculus;
use crate::kalk_num::KalkNum;
use crate::lexer::TokenKind;
use crate::parser::CalcError;
use crate::parser::DECL_UNIT;
use crate::prelude;
use crate::symbol_table::SymbolTable;

pub struct Context<'a> {
    pub symbol_table: &'a mut SymbolTable,
    angle_unit: String,
    #[cfg(feature = "rug")]
    precision: u32,
    sum_n_value: Option<i128>,
    #[cfg(not(target_arch = "wasm32"))]
    timeout: Option<u128>,
    #[cfg(not(target_arch = "wasm32"))]
    start_time: std::time::SystemTime,
}

impl<'a> Context<'a> {
    pub fn new(
        symbol_table: &'a mut SymbolTable,
        angle_unit: &str,
        #[cfg(feature = "rug")] precision: u32,
        timeout: Option<u128>,
    ) -> Self {
        Context {
            angle_unit: angle_unit.into(),
            symbol_table,
            #[cfg(feature = "rug")]
            precision,
            sum_n_value: None,
            #[cfg(not(target_arch = "wasm32"))]
            timeout: timeout,
            #[cfg(not(target_arch = "wasm32"))]
            start_time: std::time::SystemTime::now(),
        }
    }

    pub fn interpret(&mut self, statements: Vec<Stmt>) -> Result<Option<KalkNum>, CalcError> {
        for (i, stmt) in statements.iter().enumerate() {
            let num = eval_stmt(self, stmt)?;

            // Insert the last value into the `ans` variable.
            self.symbol_table.set(if (&num.unit).len() > 0 {
                Stmt::VarDecl(
                    Identifier::from_full_name("ans"),
                    Box::new(Expr::Unit(
                        num.unit.clone(),
                        Box::new(crate::ast::build_literal_ast(&num)),
                    )),
                )
            } else {
                Stmt::VarDecl(
                    Identifier::from_full_name("ans"),
                    Box::new(crate::ast::build_literal_ast(&num)),
                )
            });

            if i == statements.len() - 1 {
                if let Stmt::Expr(_) = stmt {
                    return Ok(Some(num));
                }
            }
        }

        Ok(None)
    }
}

fn eval_stmt(context: &mut Context, stmt: &Stmt) -> Result<KalkNum, CalcError> {
    match stmt {
        Stmt::VarDecl(_, _) => eval_var_decl_stmt(context, stmt),
        Stmt::FnDecl(_, _, _) => eval_fn_decl_stmt(),
        Stmt::UnitDecl(_, _, _) => eval_unit_decl_stmt(),
        Stmt::Expr(expr) => eval_expr_stmt(context, &expr),
    }
}

fn eval_var_decl_stmt(context: &mut Context, stmt: &Stmt) -> Result<KalkNum, CalcError> {
    context.symbol_table.insert(stmt.clone());
    Ok(KalkNum::from(1))
}

fn eval_fn_decl_stmt() -> Result<KalkNum, CalcError> {
    Ok(KalkNum::from(1)) // Nothing needs to happen here, since the parser will already have added the FnDecl's to the symbol table.
}

fn eval_unit_decl_stmt() -> Result<KalkNum, CalcError> {
    Ok(KalkNum::from(1))
}

fn eval_expr_stmt(context: &mut Context, expr: &Expr) -> Result<KalkNum, CalcError> {
    eval_expr(context, &expr, "")
}

pub(crate) fn eval_expr(
    context: &mut Context,
    expr: &Expr,
    unit: &str,
) -> Result<KalkNum, CalcError> {
    #[cfg(not(target_arch = "wasm32"))]
    if let (Ok(elapsed), Some(timeout)) = (context.start_time.elapsed(), context.timeout) {
        if elapsed.as_millis() >= timeout {
            return Err(CalcError::TimedOut);
        }
    }

    match expr {
        Expr::Binary(left, op, right) => eval_binary_expr(context, &left, op, &right, unit),
        Expr::Unary(op, expr) => eval_unary_expr(context, op, expr, unit),
        Expr::Unit(identifier, expr) => eval_unit_expr(context, identifier, expr),
        Expr::Var(identifier) => eval_var_expr(context, identifier, unit),
        Expr::Literal(value) => eval_literal_expr(context, *value, unit),
        Expr::Group(expr) => eval_group_expr(context, &expr, unit),
        Expr::FnCall(identifier, expressions) => {
            eval_fn_call_expr(context, identifier, expressions, unit)
        }
        Expr::Piecewise(pieces) => eval_piecewise(context, pieces, unit),
    }
}

fn eval_binary_expr(
    context: &mut Context,
    left_expr: &Expr,
    op: &TokenKind,
    right_expr: &Expr,
    unit: &str,
) -> Result<KalkNum, CalcError> {
    if let TokenKind::ToKeyword = op {
        // TODO: When the unit conversion function takes a Float instead of Expr,
        // move this to the match statement further down.
        if let Expr::Var(right_unit) = right_expr {
            let left_unit = eval_expr(context, left_expr, "")?.unit;
            return convert_unit(context, left_expr, &left_unit, &right_unit.full_name);
            // TODO: Avoid evaluating this twice.
        }
    }

    let left = eval_expr(context, left_expr, "")?;
    let mut right = eval_expr(context, right_expr, "")?;
    if let Expr::Unary(TokenKind::Percent, _) = right_expr {
        right = right.mul(context, left.clone());
        if let TokenKind::Star = op {
            return Ok(right);
        }
    }

    let mut result = match op {
        TokenKind::Plus => left.add(context, right),
        TokenKind::Minus => left.sub(context, right),
        TokenKind::Star => left.mul(context, right),
        TokenKind::Slash => left.div(context, right),
        TokenKind::Percent => left.rem(context, right),
        TokenKind::Power => left.pow(context, right),
        TokenKind::Equals => left.eq(context, right),
        TokenKind::NotEquals => left.not_eq(context, right),
        TokenKind::GreaterThan => left.greater_than(context, right),
        TokenKind::LessThan => left.less_than(context, right),
        TokenKind::GreaterOrEquals => left.greater_or_equals(context, right),
        TokenKind::LessOrEquals => left.less_or_equals(context, right),
        _ => KalkNum::from(1),
    };

    if unit.len() > 0 {
        result.unit = unit.to_string();
    };

    Ok(result)
}

fn eval_unary_expr(
    context: &mut Context,
    op: &TokenKind,
    expr: &Expr,
    unit: &str,
) -> Result<KalkNum, CalcError> {
    let num = eval_expr(context, &expr, unit)?;

    match op {
        TokenKind::Minus => Ok(num.mul(context, KalkNum::from(-1f64))),
        TokenKind::Percent => Ok(num.mul(context, KalkNum::from(0.01f64))),
        TokenKind::Exclamation => Ok(prelude::special_funcs::factorial(num)),
        _ => Err(CalcError::InvalidOperator),
    }
}

fn eval_unit_expr(
    context: &mut Context,
    identifier: &str,
    expr: &Expr,
) -> Result<KalkNum, CalcError> {
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
) -> Result<KalkNum, CalcError> {
    if let Some(Stmt::UnitDecl(_, _, unit_def)) =
        context.symbol_table.get_unit(to_unit, from_unit).cloned()
    {
        context.symbol_table.insert(Stmt::VarDecl(
            Identifier::from_full_name(DECL_UNIT),
            Box::new(expr.clone()),
        ));

        let num = eval_expr(context, &unit_def, "")?;
        Ok(KalkNum::new_with_imaginary(
            num.value,
            to_unit.into(),
            num.imaginary_value,
        ))
    } else {
        Err(CalcError::InvalidUnit)
    }
}

fn eval_var_expr(
    context: &mut Context,
    identifier: &Identifier,
    unit: &str,
) -> Result<KalkNum, CalcError> {
    // If there is a constant with this name, return a literal expression with its value
    if let Some(value) = prelude::CONSTANTS.get(identifier.full_name.as_ref() as &str) {
        return eval_expr(context, &Expr::Literal(*value), unit);
    }

    if identifier.full_name == "n" {
        if let Some(value) = context.sum_n_value {
            return Ok(KalkNum::from(value));
        }
    }

    // Look for the variable in the symbol table
    let var_decl = context
        .symbol_table
        .get_var(identifier.full_name.as_ref() as &str)
        .cloned();
    match var_decl {
        Some(Stmt::VarDecl(_, expr)) => eval_expr(context, &expr, unit),
        _ => Err(CalcError::UndefinedVar(identifier.full_name.clone())),
    }
}

#[allow(unused_variables)]
fn eval_literal_expr(context: &mut Context, value: f64, unit: &str) -> Result<KalkNum, CalcError> {
    let mut num: KalkNum = value.into();
    num.unit = unit.into();

    #[cfg(feature = "rug")]
    num.value.set_prec(context.precision);

    Ok(num)
}

fn eval_group_expr(context: &mut Context, expr: &Expr, unit: &str) -> Result<KalkNum, CalcError> {
    eval_expr(context, expr, unit)
}

pub(crate) fn eval_fn_call_expr(
    context: &mut Context,
    identifier: &Identifier,
    expressions: &[Expr],
    unit: &str,
) -> Result<KalkNum, CalcError> {
    // Prelude
    let prelude_func = match expressions.len() {
        1 => {
            let x = eval_expr(context, &expressions[0], "")?;
            if identifier.prime_count > 0 {
                return calculus::derive_func(context, &identifier, x);
            } else {
                prelude::call_unary_func(
                    context,
                    &identifier.full_name,
                    x,
                    &context.angle_unit.clone(),
                )
            }
        }
        2 => {
            let x = eval_expr(context, &expressions[0], "")?;
            let y = eval_expr(context, &expressions[1], "")?;
            prelude::call_binary_func(
                context,
                &identifier.full_name,
                x,
                y,
                &context.angle_unit.clone(),
            )
        }
        _ => None,
    };

    if let Some((result, _)) = prelude_func {
        return Ok(result);
    }

    // Special functions
    match identifier.full_name.as_ref() {
        "sum" | "Σ" | "∑" | "prod" | "∏" => {
            // Make sure exactly 3 arguments were supplied.
            if expressions.len() != 3 {
                return Err(CalcError::IncorrectAmountOfArguments(
                    3,
                    "sum/prod".into(),
                    expressions.len(),
                ));
            }

            let start = eval_expr(context, &expressions[0], "")?.to_f64() as i128;
            let end = eval_expr(context, &expressions[1], "")?.to_f64() as i128;
            let sum_else_prod = match identifier.full_name.as_ref() {
                "sum" => true,
                "Σ" => true,
                "∑" => true,
                "prod" => false,
                "∏" => false,
                _ => unreachable!(),
            };
            let mut sum = if sum_else_prod {
                KalkNum::default()
            } else {
                KalkNum::from(1f64)
            };

            for n in start..=end {
                context.sum_n_value = Some(n);
                let eval = eval_expr(context, &expressions[2], "")?;
                if sum_else_prod {
                    sum.value += eval.value;
                    sum.imaginary_value += eval.imaginary_value;
                } else {
                    sum.value *= eval.value;
                    sum.imaginary_value *= eval.imaginary_value;
                }
            }

            context.sum_n_value = None;
            sum.unit = unit.into();

            return Ok(sum);
        }
        "integrate" | "integral" | "∫" => {
            // Make sure either 3 or 4 arguments were supplied.
            if expressions.len() < 3 || expressions.len() > 4 {}

            return match expressions.len() {
                3 => calculus::integrate_with_unknown_variable(
                    context,
                    &expressions[0],
                    &expressions[1],
                    &expressions[2],
                ),
                4 => calculus::integrate(
                    context,
                    &expressions[0],
                    &expressions[1],
                    &expressions[2],
                    if let Expr::Var(integration_variable) = &expressions[3] {
                        &integration_variable.full_name[1..]
                    } else {
                        return Err(CalcError::ExpectedDx);
                    },
                ),
                _ => Err(CalcError::IncorrectAmountOfArguments(
                    3,
                    "integrate".into(),
                    expressions.len(),
                )),
            };
        }
        _ => (),
    }

    // Symbol Table
    let stmt_definition = context.symbol_table.get_fn(&identifier.full_name).cloned();

    match stmt_definition {
        Some(Stmt::FnDecl(_, arguments, fn_body)) => {
            if arguments.len() != expressions.len() {
                return Err(CalcError::IncorrectAmountOfArguments(
                    arguments.len(),
                    identifier.full_name.clone(),
                    expressions.len(),
                ));
            }

            // Initialise the arguments as their own variables.
            let mut new_argument_values = Vec::new();
            for (i, argument) in arguments.iter().enumerate() {
                let argument_identifier = if argument.contains("-") {
                    let identifier_parts: Vec<&str> = argument.split('-').collect();
                    Identifier::parameter_from_name(identifier_parts[1], identifier_parts[0])
                } else {
                    Identifier::from_full_name(argument)
                };
                let var_decl = Stmt::VarDecl(
                    argument_identifier,
                    Box::new(crate::ast::build_literal_ast(&eval_expr(
                        context,
                        &expressions[i],
                        "",
                    )?)),
                );

                // Don't set these values just yet, since
                // to avoid affecting the value of arguments
                // during recursion.
                new_argument_values.push((argument, var_decl));
            }

            let mut old_argument_values = Vec::new();
            for (name, value) in new_argument_values {
                // Save the original argument values,
                // so that they can be reverted to after
                // the function call is evaluated.
                // This is necessary since recursive
                // function calls have the same argument names.
                old_argument_values.push(context.symbol_table.get_and_remove_var(name));

                // Now set the new variable value
                eval_stmt(context, &value)?;
            }

            let fn_value = eval_expr(context, &fn_body, unit);

            // Revert to original argument values
            for old_argument_value in old_argument_values {
                if let Some(old_argument_value) = old_argument_value {
                    context.symbol_table.insert(old_argument_value);
                }
            }

            fn_value
        }
        _ => Err(CalcError::UndefinedFn(identifier.full_name.clone())),
    }
}

fn eval_piecewise(
    context: &mut Context,
    pieces: &Vec<crate::ast::ConditionalPiece>,
    unit: &str,
) -> Result<KalkNum, CalcError> {
    for piece in pieces {
        if let Some(condition_is_true) = eval_expr(context, &piece.condition, unit)?.boolean_value {
            if condition_is_true {
                return Ok(eval_expr(context, &piece.expr, unit)?);
            }
        }
    }

    Err(CalcError::PiecewiseConditionsAreFalse)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::TokenKind::*;
    use crate::test_helpers::*;
    use test_case::test_case;

    lazy_static::lazy_static! {
        static ref DEG_RAD_UNIT: Stmt = unit_decl(
            "deg",
            "rad",
            binary(
                binary(
                    var(crate::parser::DECL_UNIT),
                    TokenKind::Star,
                    literal(180f64),
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
                literal(180f64),
            ),
        );
    }

    fn interpret_with_unit(stmt: Stmt) -> Result<Option<KalkNum>, CalcError> {
        let mut symbol_table = SymbolTable::new();
        symbol_table
            .insert(DEG_RAD_UNIT.clone())
            .insert(RAD_DEG_UNIT.clone());

        context(&mut symbol_table, "rad").interpret(vec![stmt])
    }

    fn interpret(stmt: Stmt) -> Result<Option<KalkNum>, CalcError> {
        if let Some(result) = interpret_with_unit(stmt)? {
            Ok(Some(result))
        } else {
            Ok(None)
        }
    }

    #[cfg(feature = "rug")]
    fn context<'a>(symbol_table: &'a mut SymbolTable, angle_unit: &str) -> Context<'a> {
        Context::new(symbol_table, angle_unit, 63, None)
    }

    #[cfg(not(feature = "rug"))]
    fn context<'a>(symbol_table: &'a mut SymbolTable, angle_unit: &str) -> Context<'a> {
        Context::new(symbol_table, angle_unit, None)
    }

    fn cmp(x: KalkNum, y: f64) -> bool {
        (x.to_f64() - y).abs() < 0.0001
    }

    #[test]
    fn test_literal() {
        let stmt = Stmt::Expr(literal(1f64));

        assert_eq!(interpret(stmt).unwrap().unwrap().to_f64(), 1f64);
    }

    #[test]
    fn test_binary() {
        let add = Stmt::Expr(binary(literal(2f64), Plus, literal(3f64)));
        let sub = Stmt::Expr(binary(literal(2f64), Minus, literal(3f64)));
        let mul = Stmt::Expr(binary(literal(2f64), Star, literal(3f64)));
        let div = Stmt::Expr(binary(literal(2f64), Slash, literal(4f64)));
        let pow = Stmt::Expr(binary(literal(2f64), Power, literal(3f64)));
        let equals = Stmt::Expr(binary(literal(2f64), Equals, literal(3f64)));
        let not_equals = Stmt::Expr(binary(literal(2f64), NotEquals, literal(3f64)));
        let greater_than = Stmt::Expr(binary(literal(2f64), GreaterThan, literal(3f64)));
        let less_than = Stmt::Expr(binary(literal(2f64), LessThan, literal(3f64)));
        let greater_or_equals = Stmt::Expr(binary(literal(2f64), GreaterOrEquals, literal(3f64)));
        let less_or_equals = Stmt::Expr(binary(literal(2f64), LessOrEquals, literal(3f64)));

        assert_eq!(interpret(add).unwrap().unwrap().to_f64(), 5f64);
        assert_eq!(interpret(sub).unwrap().unwrap().to_f64(), -1f64);
        assert_eq!(interpret(mul).unwrap().unwrap().to_f64(), 6f64);
        assert_eq!(interpret(div).unwrap().unwrap().to_f64(), 0.5f64);
        assert_eq!(interpret(pow).unwrap().unwrap().to_f64(), 8f64);

        let result = interpret(equals).unwrap().unwrap();
        assert_eq!(
            (result.to_f64(), result.boolean_value.unwrap()),
            (0f64, false)
        );

        let result = interpret(not_equals).unwrap().unwrap();
        assert_eq!(
            (result.to_f64(), result.boolean_value.unwrap()),
            (0f64, true)
        );

        let result = interpret(greater_than).unwrap().unwrap();
        assert_eq!(
            (result.to_f64(), result.boolean_value.unwrap()),
            (0f64, false)
        );

        let result = interpret(less_than).unwrap().unwrap();
        assert_eq!(
            (result.to_f64(), result.boolean_value.unwrap()),
            (0f64, true)
        );

        let result = interpret(greater_or_equals).unwrap().unwrap();
        assert_eq!(
            (result.to_f64(), result.boolean_value.unwrap()),
            (0f64, false)
        );

        let result = interpret(less_or_equals).unwrap().unwrap();
        assert_eq!(
            (result.to_f64(), result.boolean_value.unwrap()),
            (0f64, true)
        );
    }

    #[test]
    fn test_percent() {
        let stmt = Stmt::Expr(binary(
            literal(5f64),
            Percent,
            group(binary(literal(3f64), Plus, unary(Percent, literal(2f64)))),
        ));

        assert!(cmp(interpret(stmt).unwrap().unwrap(), 1.94f64));
    }

    #[test]
    fn test_unary() {
        let neg = Stmt::Expr(unary(Minus, literal(1f64)));
        let fact = Stmt::Expr(unary(Exclamation, literal(5f64)));

        assert_eq!(interpret(neg).unwrap().unwrap().to_f64(), -1f64);
        assert_eq!(interpret(fact).unwrap().unwrap().to_f64(), 120f64);
    }

    #[test]
    fn test_angle_units() {
        let rad_explicit = Stmt::Expr(fn_call("sin", vec![*unit("rad", literal(1f64))]));
        let deg_explicit = Stmt::Expr(fn_call("sin", vec![*unit("deg", literal(1f64))]));
        let implicit = Stmt::Expr(fn_call("sin", vec![*literal(1f64)]));

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
        let mut rad_context = context(&mut rad_symbol_table, "rad");
        let mut deg_context = context(&mut deg_symbol_table, "deg");

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
        symbol_table.insert(var_decl("x", literal(1f64)));

        let mut context = context(&mut symbol_table, "rad");
        assert_eq!(
            context.interpret(vec![stmt]).unwrap().unwrap().to_f64(),
            1f64
        );
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
        let stmt = var_decl("x", literal(1f64));
        let mut symbol_table = SymbolTable::new();
        context(&mut symbol_table, "rad")
            .interpret(vec![stmt])
            .unwrap();

        assert!(symbol_table.contains_var("x"));
    }

    #[test]
    fn test_fn() {
        let stmt = Stmt::Expr(fn_call("f", vec![*literal(1f64)]));

        // Prepare by inserting a variable declaration in the symbol table.
        let mut symbol_table = SymbolTable::new();
        symbol_table.insert(fn_decl(
            "f",
            vec![String::from("x")],
            binary(var("x"), TokenKind::Plus, literal(2f64)),
        ));

        let mut context = context(&mut symbol_table, "rad");
        assert_eq!(
            context.interpret(vec![stmt]).unwrap().unwrap().to_f64(),
            3f64
        );
    }

    #[test]
    fn test_undefined_fn() {
        let stmt = Stmt::Expr(fn_call("f", vec![*literal(1f64)]));

        assert_eq!(
            interpret(stmt),
            Err(CalcError::UndefinedFn(String::from("f")))
        );
    }

    #[test_case(1f64, 2f64, 9f64)]
    #[test_case(1.2f64, 2.3f64, 9f64)]
    fn test_sum_fn(start: f64, to: f64, result: f64) {
        let stmt = Stmt::Expr(fn_call(
            "sum",
            vec![
                *literal(start),
                *literal(to),
                *binary(var("n"), TokenKind::Plus, literal(3f64)),
            ],
        ));

        assert_eq!(interpret(stmt).unwrap().unwrap().to_f64(), result);
    }

    #[test]
    fn test_integrate_fn() {
        let stmt = Stmt::Expr(fn_call(
            "integrate",
            vec![
                *literal(2f64),
                *literal(4f64),
                *binary(
                    binary(var("x"), TokenKind::Power, literal(3f64)),
                    TokenKind::Star,
                    var("dx"),
                ),
            ],
        ));

        assert!((interpret(stmt).unwrap().unwrap().to_f64() - 60f64).abs() < 0.001f64);
    }
}
