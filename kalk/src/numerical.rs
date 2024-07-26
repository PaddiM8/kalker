use crate::ast;
use crate::ast::Expr;
use crate::ast::Identifier;
use crate::ast::Stmt;
use crate::errors::KalkError;
use crate::float;
use crate::interpreter;
use crate::kalk_value::KalkValue;
use crate::lexer::TokenKind;
use crate::prelude::cosh;
use crate::prelude::sinh;
use crate::prelude::tanh;
use crate::test_helpers::f64_to_float_literal;

pub fn derive_func(
    context: &mut interpreter::Context,
    name: &Identifier,
    argument: KalkValue,
) -> Result<KalkValue, KalkError> {
    const H: f64 = 0.000001;

    let unit = argument.get_unit().cloned();
    let argument_with_h = ast::build_literal_ast(&argument.clone().add_without_unit(&H.into())?);
    let argument_without_h = ast::build_literal_ast(&argument.sub_without_unit(&H.into())?);
    let new_identifier = Identifier::from_name_and_primes(&name.pure_name, name.prime_count - 1);

    let f_x_h = interpreter::eval_fn_call_expr(
        context,
        &new_identifier,
        &[argument_with_h],
        unit.as_ref(),
    )?;
    let f_x = interpreter::eval_fn_call_expr(
        context,
        &new_identifier,
        &[argument_without_h],
        unit.as_ref(),
    )?;

    Ok(f_x_h
        .sub_without_unit(&f_x)?
        .div_without_unit(&(2f64 * H).into())?
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
    Ok(qthsh(context, a, b, expr, integration_variable)?.round_if_needed())
}

/// Integrates a function from `a` to `b`.
/// Uses the Tanh-Sinh quadrature over [-1, +1]
/// and then transforms to an integral over [a, b].
pub fn qthsh(
    context: &mut interpreter::Context,
    a_expr: &Expr,
    b_expr: &Expr,
    expr: &Expr,
    integration_variable: &str,
) -> Result<KalkValue, KalkError> {
    let a = interpreter::eval_expr(context, a_expr, None)?;
    let b = interpreter::eval_expr(context, b_expr, None)?;

    let h = 2.0;
    // 2/200 = 0.01
    let step = 0.01;
    let mut k = -1.0;

    let c = b
        .clone()
        .sub(context, a.clone())?
        .div(context, KalkValue::from(2))?;
    let d: KalkValue = a
        .clone()
        .add(context, b.clone())?
        .div(context, KalkValue::from(2))?;

    let mut tanhsinhresult = KalkValue::from(0.0);

    #[cfg(feature = "rug")]
    let pi = KalkValue::Number(
        rug::Float::with_val(1024, rug::float::Constant::Pi),
        rug::Float::with_val(1024, rug::float::Special::Zero),
        None,
    );
    #[cfg(not(feature = "rug"))]
    let pi = KalkValue::from(std::f64::consts::PI);

    println!("pi: {} whatever {}", pi, KalkValue::from(0.9));

    loop {
        let kh = KalkValue::from(k * h);

        // Calculate abscissas as tanh(0.5 π sinh(kh))
        let param_abs = KalkValue::from(0.5)
            .mul(context, pi.clone())?
            .mul(context, sinh(kh.clone())?)?;
        let abscissae = tanh(param_abs.clone())?;

        // Calculate weight as
        // \frac{0.5*h*pi*cosh(kh)}{cosh^2(0.5*pi*sinh(kh))}
        let param_wei_top = KalkValue::from(0.5)
            .mul(context, KalkValue::from(h))?
            .mul(context, pi.clone())?
            .mul(context, cosh(kh.clone())?)?;
        let param_wei_bottom = cosh(param_abs.clone())?.pow(context, KalkValue::from(2))?;
        let weight = param_wei_top.div(context, param_wei_bottom)?;

        let param_out: KalkValue = c
            .clone()
            .mul(context, abscissae.clone())?
            .add(context, d.clone())?;
        context.symbol_table.set(Stmt::VarDecl(
            Identifier::from_full_name(integration_variable),
            Box::new(crate::ast::build_literal_ast(&param_out)),
        ));

        let mut out = interpreter::eval_expr(context, expr, None)?;
        if !out.to_float().is_finite() {
            out = KalkValue::from(0.0);
        }
        let param_integral = weight.clone().mul(context, out)?;
        tanhsinhresult = tanhsinhresult
            .clone()
            .add(context, param_integral.clone())?;
        //println!("tanhsinhresult: {}", tanhsinhresult);

        k += step;
        println!(
            "kh: {} k*h {} abscissae: {} original {} next_k: {}",
            float!((k - step) * h),
            (k - step) * h,
            abscissae,
            param_abs.clone(),
            k,
        );

        if k > 1.0 {
            break;
        }
    }

    c.mul(context, tanhsinhresult)
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
