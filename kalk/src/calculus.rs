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
    let mut result = KalkNum::default();
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

    // delta_x/2[f(a) + 2f(x_1) + 2f(x_2) + ...2f(x_n) + f(b)]
    // where delta_x = (b - a) / n
    // and x_n = a + i * delta_x

    // f(a)
    context.symbol_table.set(Stmt::VarDecl(
        integration_variable.unwrap().into(),
        Box::new(expressions[0].clone()),
    ));

    // "dx" is still in the expression. Set dx = 1, so that it doesn't affect the expression value.
    context.symbol_table.set(Stmt::VarDecl(
        format!("d{}", integration_variable.unwrap()),
        Box::new(Expr::Literal(1f64)),
    ));

    result.value += interpreter::eval_expr(context, &expressions[2], "")?.value;

    // 2f(x_n)
    // where x_n = a + i * delta_x
    const N: i32 = 100;
    let a = interpreter::eval_expr(context, &expressions[0], "")?
        .value
        .to_f64();
    let b = interpreter::eval_expr(context, &expressions[1], "")?
        .value
        .to_f64();
    let delta_x = (b - a) / N as f64;
    for i in 1..N {
        context.symbol_table.set(Stmt::VarDecl(
            integration_variable.unwrap().into(),
            Box::new(Expr::Literal(a + i as f64 * delta_x)),
        ));

        // 2f(x_n)
        result.value += 2 * interpreter::eval_expr(context, &expressions[2], "")?.value;
    }

    // f(b)
    context.symbol_table.set(Stmt::VarDecl(
        integration_variable.unwrap().into(),
        Box::new(expressions[1].clone()),
    ));
    result.value += interpreter::eval_expr(context, &expressions[2], "")?.value;

    // Finally, delta_x/2 for all of it
    result.value *= delta_x / 2f64;

    return Ok(result);
}
