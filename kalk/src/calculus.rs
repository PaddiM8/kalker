use crate::ast::Expr;
use crate::ast::Stmt;
use crate::interpreter;
use crate::kalk_num::KalkNum;
use crate::lexer::TokenKind;
use crate::parser::CalcError;

pub fn integrate(
    context: &mut interpreter::Context,
    expressions: &[Expr],
) -> Result<KalkNum, CalcError> {
    let mut integration_variable: Option<&str> = None;

    // integral(a, b, expr dx)
    if let Expr::Binary(_, TokenKind::Star, right) = &expressions[2] {
        if let Expr::Var(right_name) = &**right {
            if right_name.starts_with("d") {
                // Take the value, but remove the d, so that only eg. x is left from dx
                integration_variable = Some(&right_name[1..]);
            }
        }
    }

    if integration_variable.is_none() {
        return Err(CalcError::ExpectedDx);
    }

    // "dx" is still in the expression. Set dx = 1, so that it doesn't affect the expression value.
    context.symbol_table.set(Stmt::VarDecl(
        format!("d{}", integration_variable.unwrap()),
        Box::new(Expr::Literal(1f64)),
    ));

    simpsons_rule(
        context,
        &expressions[0],
        &expressions[1],
        &expressions[2],
        integration_variable.unwrap(),
    )
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
            integration_variable.into(),
            Box::new(Expr::Literal(a + i as f64 * h)),
        ));

        let factor = if i == 0 || i == N {
            1
        } else if i % 3 == 0 {
            2
        } else {
            3
        };

        // factor * f(x_n)
        result.value += factor * interpreter::eval_expr(context, expr, "")?.value;
    }

    result.value *= (3f64 * h) / 8f64;

    Ok(result)
}
