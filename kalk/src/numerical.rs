use std::ops::Add;

use crate::as_number_or_zero;
use crate::ast;
use crate::ast::Expr;
use crate::ast::Identifier;
use crate::ast::Stmt;
use crate::errors::KalkError;
use crate::float;
use crate::interpreter;
use crate::kalk_value::KalkValue;
use crate::lexer::TokenKind;
use crate::prelude::abs;
use crate::prelude::exp;
use crate::test_helpers::f64_to_float_literal;

pub fn derive_func(
    context: &mut interpreter::Context,
    name: &Identifier,
    argument: KalkValue,
) -> Result<KalkValue, KalkError> {
    let h = KalkValue::from(1e-6);

    let n: i64 = name.clone().prime_count as i64;
    let unit = argument.get_unit().cloned();
    let new_identifier = Identifier::from_name_and_primes(&name.pure_name, 0);

    // https://en.wikipedia.org/wiki/Numerical_differentiation
    // Higher derivatives
    let mut top = KalkValue::from(0);
    for k in 0..=n {
        let sign = KalkValue::from(-1).pow(context, KalkValue::from(k.add(n)))?;

        let coefficient = if k != 0 {
            let mut first_num = 1;
            let mut last_num = 1;
            (1..=k).for_each(|f| first_num = first_num * f);
            (n - k + 1..=n).for_each(|f| last_num = last_num * f);

            KalkValue::from(last_num / first_num)
        } else {
            KalkValue::from(1)
        };

        let factor = ast::build_literal_ast(
            &argument
                .clone()
                .add_without_unit(&h.clone().mul(context, KalkValue::from(k))?)?,
        );

        let f_x_kh =
            interpreter::eval_fn_call_expr(context, &new_identifier, &[factor], unit.as_ref())?;

        let to_add = sign
            .clone()
            .mul(context, coefficient.clone())?
            .mul(context, f_x_kh.clone())?;
        top = top.add(context, to_add)?;
    }

    Ok(top
        .div_without_unit(&h.clone().pow(context, KalkValue::from(n))?)?
        .round_if_needed())
}

pub fn integrate_with_unknown_variable(
    context: &mut interpreter::Context,
    a: &Expr,
    b: &Expr,
    expr: &Expr,
) -> Result<KalkValue, KalkError> {
    let mut integration_variable: Option<&str> = None;

    // integral(a, b, expr dx)
    if let Expr::Binary(_, TokenKind::Star, right) = expr {
        if let Expr::Var(right_name) = &**right {
            if right_name.full_name.starts_with('d') {
                // Take the value, but remove the d, so that only eg. x is left from dx
                integration_variable = Some(&right_name.full_name[1..]);
            }
        }
    }

    if integration_variable.is_none() {
        return Err(KalkError::ExpectedDx);
    }

    // "dx" is still in the expression. Set dx = 1, so that it doesn't affect the expression value.
    context.symbol_table.set(Stmt::VarDecl(
        Identifier::from_full_name(&format!("d{}", integration_variable.unwrap())),
        f64_to_float_literal(1f64),
    ));

    Ok(integrate(context, a, b, expr, integration_variable.unwrap())?.round_if_needed())
}

pub fn integrate(
    context: &mut interpreter::Context,
    a: &Expr,
    b: &Expr,
    expr: &Expr,
    integration_variable: &str,
) -> Result<KalkValue, KalkError> {
    let mut result = boole_rule(context, a, b, expr, integration_variable)?;
    // TODO: check whether use qthsh or not.
    if result.is_nan() {
        result = qthsh(context, a, b, expr, integration_variable)?;
    }
    Ok(result.round_if_needed())
}

// Ref. https://github.com/Robert-van-Engelen/Tanh-Sinh/blob/main/qthsh.c
// No infinate on a_expr and b_expr, I believe kalker is not support it for the moment...
fn qthsh(
    context: &mut interpreter::Context,
    a_expr: &Expr,
    b_expr: &Expr,
    expr: &Expr,
    integration_variable: &str,
) -> Result<KalkValue, KalkError> {
    const FUDGE1: i32 = 10;
    let eps = KalkValue::from(1e-9);
    let a = interpreter::eval_expr(context, a_expr, None)?;
    let b = interpreter::eval_expr(context, b_expr, None)?;

    // const double tol = FUDGE1*eps;
    let tol = KalkValue::from(FUDGE1).mul(context, eps.clone())?;

    // double c = (a+b)/2;
    let c = a
        .clone()
        .add(context, b.clone())?
        .div(context, KalkValue::from(2))?;

    // double d = (b-a)/2;
    let d = b
        .clone()
        .sub(context, a.clone())?
        .div(context, KalkValue::from(2))?;

    // double s = f(c);
    context.symbol_table.set(Stmt::VarDecl(
        Identifier::from_full_name(integration_variable),
        Box::new(crate::ast::build_literal_ast(&c)),
    ));
    let mut s = interpreter::eval_expr(context, expr, None)?;

    let mut v: KalkValue;
    let mut h = KalkValue::from(2);
    let mut k = 0;

    loop {
        // double p = 0, q, fp = 0, fm = 0, t, eh;
        let mut p: KalkValue = KalkValue::from(0);
        let mut q: KalkValue;
        let mut fp: KalkValue = KalkValue::from(0);
        let mut fm: KalkValue = KalkValue::from(0);
        let mut t: KalkValue;
        let mut eh: KalkValue;

        // h /= 2;
        // eh = exp(h);
        // t = eh;
        h = h.clone().div(context, KalkValue::from(2))?;
        eh = exp(h.clone())?;
        t = eh.clone();

        // if (k > 0)
        //  eh *= eh;
        if k > 0 {
            eh = eh.clone().mul(context, eh.clone())?;
        }
        loop {
            // double u = exp(1/t-t);      // = exp(-2*sinh(j*h)) = 1/exp(sinh(j*h))^2
            let param_u = KalkValue::from(1)
                .div(context, t.clone())?
                .sub(context, t.clone())?;
            let u = exp(param_u)?;

            // double r = 2*u/(1+u);       // = 1 - tanh(sinh(j*h))
            let param_r = u.clone().add(context, KalkValue::from(1))?;
            let r = u
                .clone()
                .mul(context, KalkValue::from(2))?
                .div(context, param_r.clone())?;

            // double w = (t+1/t)*r/(1+u); // = cosh(j*h)/cosh(sinh(j*h))^2
            // 1+u is the same as param_r
            let param_w = KalkValue::from(1)
                .div(context, t.clone())?
                .add(context, t.clone())?
                .mul(context, r.clone())?;
            let w = param_w.div(context, param_r.clone())?;

            // double x = d*r;
            let x = d.clone().mul(context, r.clone())?;

            // if too close to a then reuse previous fp
            let mut param_eval = a.clone().add(context, x.clone())?;
            if param_eval.to_f64() > a.to_f64() {
                // double y = f(a+x);
                context.symbol_table.set(Stmt::VarDecl(
                    Identifier::from_full_name(integration_variable),
                    Box::new(crate::ast::build_literal_ast(&param_eval)),
                ));
                let y = interpreter::eval_expr(context, expr, None)?;

                // if f(x) is finite, add to local sum
                if y.is_finite() {
                    fp = y;
                }
            }

            // if too close to b then reuse previous fm
            param_eval = b.clone().sub(context, x.clone())?;
            if param_eval.to_f64() < b.to_f64() {
                // double y = f(b-x);
                context.symbol_table.set(Stmt::VarDecl(
                    Identifier::from_full_name(integration_variable),
                    Box::new(crate::ast::build_literal_ast(&param_eval)),
                ));
                let y = interpreter::eval_expr(context, expr, None)?;

                // if f(x) is finite, add to local sum
                if y.is_finite() {
                    fm = y;
                }
            }

            //q = w * (fp + fm);
            let param_q = fp.clone().add(context, fm.clone())?;
            q = w.clone().mul(context, param_q.clone())?;

            //p += q;
            //t *= eh;
            p = p.clone().add(context, q.clone())?;
            t = t.clone().mul(context, eh.clone())?;

            // while (fabs(q) > eps*fabs(p))
            if abs(q)?.to_f64() <= abs(p.clone())?.mul(context, eps.clone())?.to_f64() {
                break;
            }
        }

        // v = s-p;
        // s += p;
        // ++k;
        v = s.clone().sub(context, p.clone())?;
        s = s.clone().add(context, p.clone())?;
        k += 1;

        // while (fabs(v) > tol*fabs(s) && k <= n); n = 7;
        if abs(v.clone())?.to_f64() <= abs(s.clone())?.mul(context, tol.clone())?.to_f64() && k <= 7
        {
            break;
        }
    }

    // result with estimated relative error err
    // if (err != NULL) err = fabs(v)/(FUDGE2*fabs(s)+eps);
    const FUDGE2: i32 = 1;
    let param_err = abs(s.clone())?
        .mul(context, KalkValue::from(FUDGE2))?
        .add(context, eps)?;
    let err = abs(v)?.div(context, param_err)?;
    println!("Estimate err: {}.", err);

    // return d*s*h;
    let result = d.clone().mul(context, s.clone())?.mul(context, h.clone())?;
    print!("{}", result);
    Ok(result)
}

/// Composite Boole's rule
fn boole_rule(
    context: &mut interpreter::Context,
    a_expr: &Expr,
    b_expr: &Expr,
    expr: &Expr,
    integration_variable: &str,
) -> Result<KalkValue, KalkError> {
    let mut result_real = float!(0);
    let mut result_imaginary = float!(0);
    let original_variable_value = context
        .symbol_table
        .get_and_remove_var(integration_variable);

    const N: i32 = 1200;
    let a: KalkValue = interpreter::eval_expr(context, a_expr, None)?;
    let b = interpreter::eval_expr(context, b_expr, None)?;
    let h = (b.sub_without_unit(&a))?.div_without_unit(&KalkValue::from(N))?;
    for i in 0..=N {
        let variable_value = a
            .clone()
            .add_without_unit(&KalkValue::from(i).mul_without_unit(&h.clone())?)?;
        context.symbol_table.set(Stmt::VarDecl(
            Identifier::from_full_name(integration_variable),
            Box::new(crate::ast::build_literal_ast(&variable_value)),
        ));

        let factor = KalkValue::from(match i {
            0 | N => 7,
            _ if i % 4 == 0 => 14,
            _ if (i - 2) % 4 == 0 => 12,
            _ => 32,
        } as f64);

        // factor * f(x_n)
        let (mul_real, mul_imaginary, _) = as_number_or_zero!(
            factor.mul_without_unit(&interpreter::eval_expr(context, expr, None)?)?
        );
        result_real += mul_real;
        result_imaginary += mul_imaginary;
    }

    if let Some(value) = original_variable_value {
        context.symbol_table.insert(value);
    } else {
        context
            .symbol_table
            .get_and_remove_var(integration_variable);
    }

    let result = KalkValue::Number(result_real, result_imaginary, None);
    let (h_real, h_imaginary, h_unit) = as_number_or_zero!(h);

    // Error term: (-3*(h^5)*(f^(4)(e)))/80
    // Where h = (b-a)/2, e is between a and b
    //let hpow5mul3 = b
    //   .clone()
    //   .sub(context, a.clone())?
    //   .div(context, KalkValue::from(2))?
    //   .pow(context, KalkValue::from(5))?
    //   .mul(context, KalkValue::from(3))?;
    //let param_err = hpow5mul3;

    result.mul_without_unit(&KalkValue::Number(
        4f64 / 90f64 * h_real,
        4f64 / 90f64 * h_imaginary,
        h_unit,
    ))
}

pub fn find_root(
    context: &mut interpreter::Context,
    expr: &Expr,
    var_name: &str,
) -> Result<KalkValue, KalkError> {
    const FN_NAME: &str = "tmp.";
    let f = Stmt::FnDecl(
        Identifier::from_full_name(FN_NAME),
        vec![var_name.into()],
        Box::new(expr.clone()),
    );
    context.symbol_table.set(f);
    let mut approx = KalkValue::from(1f64);
    for _ in 0..100 {
        let (new_approx, done) =
            newton_method(context, approx, &Identifier::from_full_name(FN_NAME))?;
        approx = new_approx;
        if done {
            break;
        }
    }

    // Confirm that the approximation is correct
    let (test_real, test_imaginary) = interpreter::eval_fn_call_expr(
        context,
        &Identifier::from_full_name(FN_NAME),
        &[crate::ast::build_literal_ast(&approx)],
        None,
    )?
    .values();

    context.symbol_table.get_and_remove_var(var_name);

    if test_real.is_nan() || test_real.abs() > 0.0001f64 || test_imaginary.abs() > 0.0001f64 {
        return Err(KalkError::UnableToSolveEquation);
    }

    Ok(approx)
}

fn newton_method(
    context: &mut interpreter::Context,
    initial: KalkValue,
    fn_name: &Identifier,
) -> Result<(KalkValue, bool), KalkError> {
    let f = interpreter::eval_fn_call_expr(
        context,
        fn_name,
        &[crate::ast::build_literal_ast(&initial)],
        None,
    )?;

    // If it ends up solving the equation early, abort
    const PRECISION: f64 = 0.0000001f64;
    match f {
        KalkValue::Number(x, y, _)
            if x < PRECISION && x > -PRECISION && y < PRECISION && y > -PRECISION =>
        {
            return Ok((initial, true));
        }
        _ => (),
    }

    let f_prime_name = Identifier::from_name_and_primes(&fn_name.pure_name, 1);
    let f_prime = derive_func(context, &f_prime_name, initial.clone())?;

    Ok((
        initial.sub_without_unit(&f.div_without_unit(&f_prime)?)?,
        false,
    ))
}

#[cfg(test)]
mod tests {
    use crate::ast;
    use crate::float;
    use crate::interpreter;
    use crate::kalk_value::KalkValue;
    use crate::lexer::TokenKind::*;
    use crate::numerical::Identifier;
    use crate::numerical::Stmt;
    use crate::symbol_table::SymbolTable;
    use crate::test_helpers::*;

    fn get_context(symbol_table: &mut SymbolTable) -> interpreter::Context {
        interpreter::Context::new(
            symbol_table,
            "",
            #[cfg(feature = "rug")]
            63u32,
            None,
        )
    }

    #[test]
    fn test_derive_func() {
        let mut symbol_table = SymbolTable::new();
        let mut context = get_context(&mut symbol_table);
        context.symbol_table.insert(Stmt::FnDecl(
            Identifier::from_full_name("f"),
            vec![String::from("x")],
            binary(
                f64_to_float_literal(2.5f64),
                Star,
                binary(var("x"), Power, f64_to_float_literal(3f64)),
            ),
        ));

        let call = Stmt::Expr(fn_call("f'", vec![*f64_to_float_literal(12.3456f64)]));
        assert!(cmp(
            context.interpret(vec![call]).unwrap().unwrap().to_f64(),
            1143.10379f64
        ));
    }

    #[test]
    fn test_derive_complex_func() {
        let mut symbol_table = SymbolTable::new();
        let mut context = get_context(&mut symbol_table);
        context.symbol_table.insert(Stmt::FnDecl(
            Identifier::from_full_name("f"),
            vec![String::from("x")],
            binary(
                binary(
                    f64_to_float_literal(1.5f64),
                    Star,
                    binary(var("x"), Power, f64_to_float_literal(2f64)),
                ),
                Plus,
                binary(
                    binary(var("x"), Power, f64_to_float_literal(2f64)),
                    Star,
                    var("i"),
                ),
            ),
        ));

        let call = Stmt::Expr(fn_call("f'", vec![*var("e")]));
        let result = context.interpret(vec![call]).unwrap().unwrap();
        assert!(cmp(result.to_f64(), 8.15484f64));
        assert!(cmp(result.imaginary_to_f64(), 5.43656));
    }

    #[test]
    fn test_derive_func_with_complex_argument() {
        let mut symbol_table = SymbolTable::new();
        let mut context = get_context(&mut symbol_table);
        context.symbol_table.insert(Stmt::FnDecl(
            Identifier::from_full_name("f"),
            vec![String::from("x")],
            binary(
                binary(f64_to_float_literal(3f64), Star, var("x")),
                Plus,
                binary(
                    f64_to_float_literal(0.5f64),
                    Star,
                    binary(var("x"), Power, f64_to_float_literal(3f64)),
                ),
            ),
        ));

        let result = super::derive_func(
            &mut context,
            &Identifier::from_full_name("f'"),
            KalkValue::Number(float!(2f64), float!(3f64), None),
        )
        .unwrap();
        assert!(cmp(result.to_f64(), -4.5f64) || cmp(result.to_f64(), -4.499999f64));
        assert!(cmp(result.imaginary_to_f64(), 18f64));
    }

    #[test]
    fn test_integrate_with_unknown_variable() {
        let mut symbol_table = SymbolTable::new();
        let mut context = get_context(&mut symbol_table);
        let result = super::integrate_with_unknown_variable(
            &mut context,
            &f64_to_float_literal(2f64),
            &f64_to_float_literal(4f64),
            &binary(var("x"), Star, var("dx")),
        )
        .unwrap();

        assert!(cmp(result.to_f64(), 6f64));
    }

    #[test]
    fn test_integrate() {
        let mut symbol_table = SymbolTable::new();
        let mut context = get_context(&mut symbol_table);
        let result = super::integrate(
            &mut context,
            &f64_to_float_literal(2f64),
            &f64_to_float_literal(4f64),
            &var("x"),
            "x",
        )
        .unwrap();

        assert!(cmp(result.to_f64(), 6f64));
    }

    #[test]
    fn test_integrate_complex() {
        let mut symbol_table = SymbolTable::new();
        let mut context = get_context(&mut symbol_table);
        let result = super::integrate(
            &mut context,
            &f64_to_float_literal(2f64),
            &ast::build_literal_ast(&KalkValue::Number(float!(3f64), float!(4f64), None)),
            &binary(var("x"), Star, var("i")),
            "x",
        )
        .unwrap();

        assert!(cmp(result.to_f64(), -12f64));
        assert!(cmp(result.imaginary_to_f64(), -5.5f64));
    }

    #[test]
    fn test_find_root() {
        let mut symbol_table = SymbolTable::new();
        let mut context = get_context(&mut symbol_table);
        let ast = &*binary(
            binary(var("x"), Power, f64_to_float_literal(3f64)),
            Plus,
            f64_to_float_literal(3f64),
        );
        let result = super::find_root(&mut context, ast, "x").unwrap();

        assert!(cmp(result.to_f64(), -1.4422495709));
        assert!(!result.has_imaginary());
    }
}
