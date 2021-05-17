use crate::ast::Expr;
use crate::ast::Identifier;
use crate::ast::Stmt;
use crate::interpreter;
use crate::kalk_num::KalkNum;
use crate::lexer::TokenKind;
use crate::parser::CalcError;

pub fn derive_func(
    context: &mut interpreter::Context,
    name: &Identifier,
    argument: KalkNum,
) -> Result<KalkNum, CalcError> {
    const H: f64 = 0.000001;
    let unit = &argument.unit.to_string();

    let argument_with_h = Expr::Literal(argument.clone().add(context, H.into()).to_f64());
    let argument_without_h = Expr::Literal(argument.sub(context, H.into()).to_f64());
    let new_identifier = Identifier::from_name_and_primes(&name.pure_name, name.prime_count - 1);

    let f_x_h = interpreter::eval_fn_call_expr(context, &new_identifier, &[argument_with_h], unit)?;
    let f_x =
        interpreter::eval_fn_call_expr(context, &new_identifier, &[argument_without_h], unit)?;

    Ok(f_x_h.sub(context, f_x).div(context, (2f64 * H).into()))
}

pub fn integrate(
    context: &mut interpreter::Context,
    a: &Expr,
    b: &Expr,
    expr: &Expr,
) -> Result<KalkNum, CalcError> {
    let mut integration_variable: Option<&str> = None;

    // integral(a, b, expr dx)
    if let Expr::Binary(_, TokenKind::Star, right) = expr {
        if let Expr::Var(right_name) = &**right {
            if right_name.full_name.starts_with("d") {
                // Take the value, but remove the d, so that only eg. x is left from dx
                integration_variable = Some(&right_name.full_name[1..]);
            }
        }
    }

    if integration_variable.is_none() {
        return Err(CalcError::ExpectedDx);
    }

    // "dx" is still in the expression. Set dx = 1, so that it doesn't affect the expression value.
    context.symbol_table.set(Stmt::VarDecl(
        Identifier::from_full_name(&format!("d{}", integration_variable.unwrap())),
        Box::new(Expr::Literal(1f64)),
    ));

    simpsons_rule(context, a, b, expr, integration_variable.unwrap())
}

/// Composite Simpson's 3/8 rule
fn simpsons_rule(
    context: &mut interpreter::Context,
    a_expr: &Expr,
    b_expr: &Expr,
    expr: &Expr,
    integration_variable: &str,
) -> Result<KalkNum, CalcError> {
    let mut result = KalkNum::default();

    const N: i32 = 900;
    let a = interpreter::eval_expr(context, a_expr, "")?.value.to_f64();
    let b = interpreter::eval_expr(context, b_expr, "")?.value.to_f64();
    let h = (b - a) / N as f64;
    for i in 0..=N {
        context.symbol_table.set(Stmt::VarDecl(
            Identifier::from_full_name(integration_variable),
            Box::new(Expr::Literal(a + i as f64 * h)),
        ));

        let factor = match i {
            0 | N => 1,
            _ if i % 3 == 0 => 2,
            _ => 3,
        };

        // factor * f(x_n)
        result.value += factor * interpreter::eval_expr(context, expr, "")?.value;
    }

    result.value *= (3f64 * h) / 8f64;

    Ok(result)
}
