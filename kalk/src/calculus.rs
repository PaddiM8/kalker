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

    Ok(f_x_h
        .sub(context, f_x)
        .div(context, (2f64 * H).into())
        .round_if_needed())
}

pub fn integrate_with_unknown_variable(
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

    Ok(integrate(context, a, b, expr, integration_variable.unwrap())?.round_if_needed())
}

pub fn integrate(
    context: &mut interpreter::Context,
    a: &Expr,
    b: &Expr,
    expr: &Expr,
    integration_variable: &str,
) -> Result<KalkNum, CalcError> {
    Ok(simpsons_rule(context, a, b, expr, integration_variable)?.round_if_needed())
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
    let a = interpreter::eval_expr(context, a_expr, "")?;
    let b = interpreter::eval_expr(context, b_expr, "")?;
    let h = (b.sub_without_unit(a.clone())).div_without_unit(KalkNum::from(N));
    for i in 0..=N {
        let variable_value = a
            .clone()
            .add_without_unit(KalkNum::from(i).mul_without_unit(h.clone()));
        context.symbol_table.set(Stmt::VarDecl(
            Identifier::from_full_name(integration_variable),
            if variable_value.has_imaginary() {
                Box::new(Expr::Binary(
                    Box::new(Expr::Literal(variable_value.to_f64())),
                    TokenKind::Plus,
                    Box::new(Expr::Binary(
                        Box::new(Expr::Literal(variable_value.imaginary_to_f64())),
                        TokenKind::Star,
                        Box::new(Expr::Var(Identifier::from_full_name("i"))),
                    )),
                ))
            } else {
                Box::new(Expr::Literal(variable_value.to_f64()))
            },
        ));

        let factor = KalkNum::from(match i {
            0 | N => 1,
            _ if i % 3 == 0 => 2,
            _ => 3,
        });

        // factor * f(x_n)
        let mul = factor.mul_without_unit(interpreter::eval_expr(context, expr, "")?);
        result.value += mul.value;
        result.imaginary_value += mul.imaginary_value;
    }

    Ok(result.mul_without_unit(KalkNum::new_with_imaginary(
        3f64 / 8f64 * h.value,
        &h.unit,
        3f64 / 8f64 * h.imaginary_value,
    )))
}
