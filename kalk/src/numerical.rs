use std::ops::Add;

use crate::ast;
use crate::ast::Expr;
use crate::ast::Identifier;
use crate::ast::Stmt;
use crate::errors::KalkError;
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
    //let result = boole_rule(context, a, b, expr, integration_variable)?;
    // TODO: check whether use qthsh or not.
    //if result.is_nan() {
    let qthsh_result = qthsh(context, a, b, expr, integration_variable)?.round_if_needed();
    Ok(qthsh_result)
    //} else {
    //    Ok(result)
    //}
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
            if abs(q)?.to_float() <= abs(p.clone())?.mul(context, eps.clone())?.to_float() {
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
        if abs(v.clone())?.to_float() <= abs(s.clone())?.mul(context, tol.clone())?.to_float()
            || k > 7
        {
            break;
        }
    }

    // result with estimated relative error err
    // if (err != NULL) err = fabs(v)/(FUDGE2*fabs(s)+eps);
    // const FUDGE2: i32 = 1;
    // let param_err = abs(s.clone())?
    //    .mul(context, KalkValue::from(FUDGE2))?
    //    .add(context, eps)?;
    // let err = abs(v)?.div(context, param_err)?;

    // return d*s*h;
    let result = d.clone().mul(context, s.clone())?.mul(context, h.clone())?;
    Ok(result)
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

    // Test scripts and results to compare methods: https://www.genivia.com/files/qthsh.zip
    #[test]
    fn test_integrate_data() {
        let test_data = [
            ("integral(0,1,1/sqrt(x),dx)","2"),
            ("integral(0,2,sqrt(4-(x^2)),dx)","3.141592654"),
            ("integral(0,1,log(x,e),dx)","-1"),
            ("integral(0,1,x*log(x,e),dx)","-0.25"),
            ("integral(0,1,log(x,e)/sqrt(x),dx)","-4"),
            ("integral(0,1,4/(1+(x^2)),dx)","3.141592654"),
            ("integral(0,pi/2,(sin(x)^4)*(cos(x)^2),dx)","0.09817477"),
            ("integral(0,pi,cos(x),dx)","0"),
            ("integral(0,1,cos(log(x,e)),dx)","0.5"),
            ("integral(0,2,sqrt(4*x-(x^2)),dx)","3.141592654"),
            ("integral(0,10,5*(x^2),dx)","1666.666667"),
            ("integral(0,1,x^0.125,dx)","0.888888889"),
            ("integral(1,10,1/x,dx)","2.302585093"),
            ("integral(0.5,1,log(x,e)/(1-x),dx)","-0.582240526"),
            ("integral(0,1.047197551,exp(-1/cos(x)),dx)","0.307694395"),
            //("integral(0,128,((x*(x+88)*(x-88)*(x+47)*(x-47)*(x+117)*(x-117))^2),dx)","6.55134e+27"), // 6551344776114854000000000000
            ("integral(0,1,1/(2*log(1/x,e)+100),dx)","0.009807555"),
            ("integral(0,1,2*(x^2)/(x+1)/(x-1)-x/log(x,e),dx)","0.036489974"),
            ("integral(0,1,x*log(1+x,e),dx)","0.25"),
            ("integral(0,1,(x^2)*atan(x),dx)","0.210657251"),
            ("integral(0,pi/2,exp(x)*cos(x),dx)","1.90523869"),
            ("integral(0,1,atan(sqrt((x^2)+2))/(1+(x^2))/sqrt((x^2)+2),dx)","0.514041896"),
            ("integral(0,1,log(x,e)*sqrt(x),dx)","-0.444444444"),
            ("integral(0,1,sqrt(1-(x^2)),dx)","0.785398163"),
            ("integral(0,1,sqrt(x)/sqrt(1-(x^2)),dx)","1.198140235"),
            ("integral(0,1,(log(x,e)^2),dx)","2"),
            ("integral(0,pi/2,log(cos(x),e),dx)","-1.088793045"),
            ("integral(0,1.047197551,sqrt(tan(x)),dx)","0.787779048"),
            ("integral(0,1,log((x^2),e),dx)","-2"),
            ("integral(0,pi,x*sin(x)/(1+(cos(x)^2)),dx)","2.4674011"),
            ("integral(0,1,1/(x-2)/((1-x)^0.25)/((1+x)^0.75),dx)","-0.691183689"),
            ("integral(-1,1,cos(pi*x)/sqrt(1-x),dx)","-0.690494589"),
            ("integral(0,1,(x^2)*log(x,e)/((x^2)-1)/((x^4)+1),dx)","0.180671263"),
            ("integral(0,1,1/(1-2*x+2*(x^2)),dx)","1.570796327"),
            ("integral(0,1,exp(1-1/x)/sqrt((x^3)-(x^4)),dx)","1.772453851"),
            ("integral(0,1,exp(1-1/x)*cos(1/x-1)/(x^2),dx)","0.5"),
            ("integral(0,1,x/sqrt(1-(x^2)),dx)","1"),
            ("integral(0,1,((1-x)^4)*(x^4)/(1+(x^2)),dx)","0.001264489"),
            ("integral(0,1,(x^4)*((1-x)^4),dx)","0.001587302"),
            ("integral(0,1,atan(sqrt((x^2)+1))/(((x^2)+1)^(3.0/2.0)),dx)","0.590489271"),
            ("integral(-1,1,1/(1+(x^2)+(x^4)+(x^6)),dx)","1.408623404"),
            ("integral(0,pi/4,(pi/4-x*tan(x))*tan(x),dx)","0.141798826"),
            ("integral(0,pi/2,(x^2)/(sin(x)^2),dx)","2.17758609"),
            ("integral(0,pi/2,(log(cos(x),e)^2),dx)","2.046622024"),
            ("integral(0,1,(log(x,e)^2)/((x^2)+x+1),dx)","1.768047624"),
            ("integral(0,1,log(1+(x^2),e)/(x^2),dx)","0.877649146"),
            ("integral(0,1,atan(x)/(x*sqrt(1-(x^2))),dx)","1.384458393"),
            ("integral(0,1,(x^2)/(1+(x^4))/sqrt(1-(x^4)),dx)","0.392699082"),
            ("integral(1,10,exp(-(x^2))*(log(x,e)^2),dx)","0.014465126"),
            ("integral(-1,1,1/sqrt(1-(x^2)),dx)","3.141592654"),
            ("integral(0,1,((1+x)^2)*sin(2*pi/(1+x)),dx)","-1.257734711"),
            ("integral(0,1,x*((1-x)^0.1),dx)","0.432900433"),
            ("integral(0,1,log((sin(x)^3),e)*cos(x),dx)","-2.960136087"),
            ("integral(0,1,x/(exp(x)-1),dx)","0.777504634"),
            ("integral(0,1,1/(1+exp(x)),dx)","0.379885493"),
            ("integral(0,1,exp(x),dx)","1.718281828"),
            ("integral(0,1,1/(1+x),dx)","0.693147181"),
            ("integral(-1,1,0.92*cosh(x)-cos(x),dx)","0.479428227"),
            ("integral(0,1,1/(1+(x^4)),dx)","0.866972987"),
            ("integral(-1,1,1/((x^4)+(x^2)+0.9),dx)","1.582232964"),
            ("integral(-1,1,1/((x^2)+1.005),dx)","1.564396444"),
            ("integral(0,10,25*exp(-25*x),dx)","1"),
            ("integral(0,1,sqrt(50)*exp(-50*pi*(x^2)),dx)","0.5"),
            ("integral(0,10,50/(1+2500*(x^2))/pi,dx)","0.499363381"),
            ("integral(0,1,x*sqrt(x),dx)","0.4"),
            ("integral(0,1,(x^0.5),dx)","0.666666667"),
            ("integral(0,pi,cos(cos(x)+3*sin(x)+2*cos(2*x)+3*sin(2*x)+3*cos(3*x)),dx)","0.838676343"),
            ("integral(0,10,exp(-x/5)*(2+sin(2*x)),dx)","9.108239607"),
            ("integral(0,1,(x^(-1.0/3.0))*((1-x)^5),dx)","0.417685256"),
            ("integral(0,1,sin(pi*x),dx)","0.636619772"),
            ("integral(0,1,(x^0.25),dx)","0.8"),
            ("integral(0,6.283185307,x*sin(20*x)*cos(50*x),dx)","0.05983986"),
            ("integral(0,1,log(1/x,e)*(x^4),dx)","0.04"),
            ("integral(0,pi,cos(8*sin(x)-x),dx)","0.737131824"),
            ("integral(0,6.283185307,x*cos(x)*sin(30*x),dx)","-0.20967248"),
            ("integral(0,pi/2,1/(sqrt(1-0.81*(sin(x)^2))),dx)","2.280549138"),
            ("integral(0,1,log(x,e)*log(1-x,e),dx)","0.355065933"),
            ("integral(0,pi,(sin(2*x)^2),dx)","1.570796327"),
            ("integral(0,4,(x^(5.0/6.0))*((4-x)^(1.0/6.0))/(5-x)/(6-x)/(7-x),dx)","0.284205411"),
            ("integral(0,pi/2,1/(1+(tan(x)^3)),dx)","0.785398163"),
            ("integral(0,1,atan(sqrt((x^2)+1))/sqrt((x^2)+1)/((x^2)+1),dx)","0.590489271"),
            ("integral(0,pi,log(1-4*cos(x)+4,e),dx)","4.355172181"),
            ("integral(-pi/2,pi/2,1/(1+(cos(x)^x)),dx)","1.570796327"),
            ("integral(0,1,1/sqrt(abs(x-1)),dx)","2"),
            ("integral(0,1,1/sqrt(x)*log(1/x,e),dx)","4"),
            ("integral(0,1,1/(x^(1.0/4.0))*log(1/x,e),dx)","1.777777778"),
            ("integral(0,1,sin(x)/x,dx)","0.94608307"),
            ("integral(0,pi,(sin(x)^2)*(cos(x)^4),dx)","0.196349541"),
            ("integral(0,pi/4,log(1+tan(x),10),dx)","0.118214203"),
            ("integral(0,1,sin(sin(x)),dx)","0.430606103"),
            ("integral(0,2,x*cos((x^2)+1),dx)","-0.90019763"),
            ("integral(0,pi/2,(x^2)*((x^2)-2)*sin(x),dx)","-0.47915881"),
            ("integral(0,15,(x^3)/(exp(x)-1),dx)","6.492671131"),
            ("integral(0,1,log(x,e)/(1+x),dx)","-0.822467033"),
            ("integral(0,pi/2,sqrt(1-0.5*(sin(x)^2)),dx)","1.350643881"),
            ("integral(0,1,(log(x,e)^3),dx)","-6"),
            ("integral(-1,0.5,1/(pi*sqrt(1-(x^2))),dx)","0.666666667"),
            ("integral(-1,0.5,log((1+x)/(1-x),e)/4/log(2,e),dx)","-0.405639062"),
            ("integral(-1,1,(23.0/25.0*cosh(x)-cos(x)),dx)","0.479428227"),
            ("integral(0,1,2/(2+sin(10*pi*x)),dx)","1.154700538"),
            ("integral(0,1,sin(100*pi*x)/pi/x,dx)","0.498986809"),
            ("integral(0,pi/2,1/(1+cos(x)),dx)","1"),
            ("integral(-1,1,1/(1.005+(x^2)),dx)","1.564396444"),
            ("integral(0,1,4*pi*pi*x*sin(20*pi*x)*cos(2*pi*x),dx)","-0.634665183"),
            ("integral(0,1,1/(1+((230*x-30)^2)),dx)","0.013492486"),
            ("integral(0,1,50*((sin(50*pi*x)/(50*pi*x))^2),dx)","0.498986809"),
            ("integral(0,pi,cos(cos(x)+3*sin(x)+2*cos(2*x)+3*cos(3*x)),dx)","0.291018783"),
            ("integral(0,1,(x^63),dx)","0.015625"),
            ("integral(0,1,1/(x+0.5),dx)","1.098612289"),
            ("integral(0,1,sqrt(12.25-((5*x-3)^2)),dx)","3.121768371"),
            ("integral(0,1,10/(1+((10*x-4)^2)),dx)","2.731465313"),
            ("integral(0,1,1/sqrt(x*(1-x)),dx)","3.141592654"),
            ("integral(0,1,(x^(-3.0/4.0))*((1-x)^-0.25)/(3-2*x),dx)","1.949054259"),
            ("integral(0,pi,1/(5+4*cos(x)),dx)","1.047197551"),
            ("integral(0,1,log(x,e)*sqrt(x/(1-x)),dx)","-0.606789764"),
            //("integral(1.047197551,pi/2,log(abs((tan(x)+sqrt(7))/(tan(x)-sqrt(7))),e),dx)","0.888914928"), // 0.8924943049231653
            ("integral(0,1,1/(1+25*(x^2)),dx)","0.274680153"),
            ("integral(0,pi/2,(cos(x)^3),dx)","0.666666667"),
            ("integral(0,pi/4,1/(1+sin(x)),dx)","0.585786437"),
            ("integral(0,1,1/(1-(x^4)/2),dx)","1.143667254"),
            ("integral(0,1,1/(1+100*(x^2)),dx)","0.147112767"),
            ("integral(1,2,log(x,e)/x,dx)","0.240226507"),
            ("integral(1,2,1/(exp(x)-1),dx)","0.313261688"),
            ("integral(-4,4,1/(1+(x^2)),dx)","2.651635327"),
            ("integral(0,pi/2,1/(1+(sin(x)^2)),dx)","1.110720735"),
            //("integral(0,1,sqrt(abs((x^2)-0.25)),dx)","0.464742506"), // 0.4646409979121271 
            ("integral(0,1,log(sin(pi*x),e),dx)","-0.693147181"),
            ("integral(0,1,abs(x-0.4),dx)","0.26"),
            ("integral(0,1,exp(-2*abs(x-0.4)),dx)","0.624738412"),
            //("integral(0,1,log(abs(x-0.3),e),dx)","-1.610864302"), // -1.610864302 
            //("integral(0,1,1/((x+0.01)^5),dx)","24999999.76"), // 24999999.759742472
            ("integral(0,1,1/sqrt(x+0.0001),dx)","1.980099998"),
            ("integral(0,1,1/(x+0.0001),dx)","9.210440367"),
            ("integral(0,1,1/(((230*x-30)^2)+1),dx)","0.013492486"),
            ("integral(0,1,1/(x+0.01),dx)","4.615120517"),
            ("integral(0,6.283185307,x*sin(30*x)*cos(x),dx)","-0.20967248"),
            ("integral(0,6.283185307,exp(-x)*sin(10*x),dx)","0.098825006"),
            ("integral(0,1,x*((1-x)^2)/((1+x)^3),dx)","0.034264097"),
            ("integral(0,1,(x^12)*((1-x)^12)/16/(1+(x^2)),dx)","7.38424e-10"),
            ("integral(0,1,(x^12)*((1-x)^12)/16,dx)","9.24503e-10"),
            ("integral(0,1,atan(sqrt((x^2)+2))/sqrt((x^2)+2)/((x^2)+1),dx)","0.514041896"),
            ("integral(0,1,1/(1+(x^2)+(x^4)),dx)","0.728102913"),
            ("integral(0,1,((x+0.1)^5),dx)","0.29526"),
            ("integral(0,1,1/((x^2)+0.0001),dx)","156.079666"),
            ("integral(0,1,sqrt(x*(1-x)),dx)","0.392699082"),
            ("integral(0,1,1/(9+(x^6)),dx)","0.109445561"),
            ("integral(0,1,((x+0.01)^5),dx)","0.176920025"),
            ("integral(0,1,1/(1-0.99*(x^4)),dx)","2.067156144"),
            ("integral(0,1,x*exp(-3*(x^2)),dx)","0.158368822"),
            ("integral(0,1,(x^2)*log(exp(1)/x,e),dx)","0.444444444"),
            ("integral(0,0.707106781,(4*sqrt(2)-8*(x^3)-4*sqrt(2)*(x^4)-8*(x^5))/(1-(x^8)),dx)","3.141592654"),
            ("integral(0,1,1/(3-2*x)*1/sqrt(x*(1-x)),dx)","1.813799364"),
            ("integral(0,1,cos(2*pi*x)/sqrt(1-x),dx)","0.488253406"),
            ("integral(-1,1,2*(1-(x^2))/((tan(0.5)^2)+(x^2)),dx)","6.180232912"),
            ("integral(0,pi/2,1/sqrt(1-0.5*(sin(x)^2)),dx)","1.854074677"),
            ("integral(-1,1,2*(1-(x^2))/(cos(4*atanh(x))+cosh(2)),dx)","0.711943823"),
            ("integral(0,1,acos((cos(pi*x)^2))/3/sin(pi*x)+0.5,dx)","0.995555498"),
            ("integral(0,pi,sin(x),dx)","2"),
            ("integral(0,1,sin(x)*cos(x),dx)","0.354036709"),
            ("integral(1,9,sqrt(2*x+7),dx)","32.66666667"),
            ("integral(0,5,(x^3)*exp(-x),dx)","4.409844508"),
            ("integral(0,10,sin(10*x)*exp(-x),dx)","0.099006252"),
            ("integral(0,1,1/(1+(x^64)),dx)","0.989366989"),
            ("integral(0,1,sqrt(x)/(x-1)-1/log(x,e),dx)","0.036489974"),
            ("integral(0,1,exp((x^2)*((1-x)^2)),dx)","1.034141052"),
            ("integral(0,6.283185307,log(x,e)*sin(10*x),dx)","-0.471793074"),
            ("integral(0,pi,cos(8*sin(x)-x)/pi,dx)","0.234636347"),
            ("integral(0,6.283185307,x*sin(10*x)*cos(x),dx)","-0.634665183"),
            ("integral(-1,1,23*cosh(x)/25-cos(x),dx)","0.479428227"),
            ("integral(0,1,(x^3)*sin(10*pi*x)/sqrt(1-(x^2)),dx)","-0.15091664"),
            ("integral(0,1,cos(x)*log(x,e),dx)","-0.94608307"),
            ("integral(0,6.283185307,log(1+x,e)*sin(10*x),dx)","-0.197626808"),
            //("integral(0,2,(x^(pi/4))*sin(pi/(8-4*x)),dx)","1.011239091"), //1.013246846561779
            ("integral(0,1,(x^x),dx)","0.783430511"),
            ("integral(0,1,(x^-x),dx)","1.291285997"),
            ("integral(0,pi,(4+cos(x))*log(3+cos(x),e),dx)","13.98000201"),
            ("integral(-2,4,(x^3)-3*(x^2),dx)","-12"),
            ("integral(-1,1,2*sqrt(1-(x^2)),dx)","3.141592654"),
            ("integral(0,1,(x^3)*sin(5*pi*x),dx)","0.062113904"),
            ("integral(0.05,0.333333333,1/x*sin(1/x),dx)","-0.300410827"),
            ("integral(0,1,(x^2)/((1+(x^4))*sqrt(1-(x^4))),dx)","0.392699082"),
            ("integral(0,1,sqrt(x)/sqrt(1-x),dx)","1.570796327"),
            ("integral(1,e,log(x,e)/((1+log(x,e))^2),dx)","0.359140914"),
            ("integral(0,1,atan(x)/x/((x^2)+1),dx)","0.730181058"),
            ("integral(0,6.283185307,(cos(3*x)^2)/(5-4*cos(2*x)),dx)","1.178097245"),
            ("integral(0,pi/4,x*tan(x),dx)","0.185784536"),
            ("integral(0,pi/2,(x^2)/(1-cos(x)),dx)","3.374047367"),
            ("integral(0,1,(x^4)*((1-x)^4)/(1+(x^2)),dx)","0.001264489"),
            ("integral(-100,-10,(((x^2)-x)^2)/(((x^3)-3*x+1)^2),dx)","0.102670322596975"),
            ("integral(0,pi,cos(sin(x)-x),dx)","1.382459687"),
            ("integral(-1,1,1/pi/sqrt(1-(x^2)),dx)","1"),
            ("integral(-1,0.5,1.0/4.0/log(2,e)*log((1+x)/(1-x),e),dx)","-0.405639062"),
            ("integral(0,1,((1+x)^2)*exp(-x)/(1+(x^2))^2,dx)","0.816060279"),
            ("integral(0,1,log(1-x,e)/x,dx)","-1.644934067"),
            ("integral(0,0.5,x/(1-x),dx)","0.193147181"),
            ("integral(0,pi/2,1/(1+(tan(x)^sqrt(2))),dx)","0.785398163"),
            ("integral(-1,1,sqrt(1-(x^4)),dx)","1.74803837"),
            ("integral(0,1,1/(((x-0.3)^2)+0.01)+1/(((x-0.9)^2)+0.04)-6,dx)","29.8583254"),
            ("integral(-1,1,cos((x^2))/sqrt(1-(x^2)),dx)","2.587367762"),
            ("integral(0,0.5,(exp(2*x)),dx)","0.859140914"),
            ("integral(0,1,(x^0.1),dx)","0.909090909"),
            ("integral(0,2,(x^4)*log(x+sqrt((x^2)+1),e),dx)","8.15336412"),
            ("integral(-2,2,(x^3)*exp(x),dx)","19.92085296"),
            ("integral(1,3,100/(x^2)*sin(10/x),dx)","-1.426024756"),
            ("integral(-1,1,(x^3)/sqrt(1-(x^2))*sin(3*x),dx)","0.556151443"),
            //("integral(0,5,(x^15)*exp(x-1),dx)","4.01146e+11"), // 401146603635.87866
            ("integral(-1,1,1/(x-2),dx)","-1.098612289"),
            ("integral(0,1,sin(exp(x))/sqrt(x),dx)","1.77247908"),
            ("integral(0,4,((x^2)+2*x+4)/((x^4)-7*(x^2)+2*x+17),dx)","2.501822871"),
            ("integral(0,1.047197551,(tan(x)^(1/pi)),dx)","0.853348094"),
            //("integral(0,pi,5*sin(x)+(9*x-4)*(9*x-8)*(3*x-4)*(9*x-10)*(pi-2*x)/(1+((90*x-110)^4)),dx)","9.880869774"), // 9.87728400819037
            ("integral(-4,4,-log(cos(pi/2*tanh(pi/2*sinh(x))),e)*pi/2*cosh(x)/(cosh(pi/2*sinh(x))^2),dx)","1.386294361"),
            ("integral(0,1,1/(16*((x-pi/4)^2)+1/16),dx)","2.77878442"),
            ("integral(0,pi,cos(64*sin(x)),dx)","0.290880102"),
            ("integral(0,1,exp(20*(x-1))*sin(256*x),dx)","-0.000148594"),
            ("integral(-1,1,1/(pi*sqrt(1-(x^2))),dx)","1"),
            ("integral(0,1,1.0/4.0/log(2,e)*log((1+x)/(1-x),e),dx)","0.5"),
            ("integral(-1,1,2/pi*sqrt(1-(x^2)),dx)","1"),
            ("integral(-1,1,2/pi/(1+(x^2)),dx)","1"),
            //("integral(-10,10,(x^2)/(1+4*x+3*(x^2)-4*(x^3)-2*(x^4)+2*(x^5)+(x^6)),dx)","3.141592654"), // 3.1407849081390626
            ("integral(-10,20,2/(1+(x^2))+1/(1+((x-10)^2)),dx)","8.975896816"),
            ("integral(0,50,sin(x)/log(1+x,e),dx)","1.795962557"),
            ("integral(0,1,tan(pi*x/2)*sin(4*pi*x),dx)","-1"),
            ("integral(0,pi,cos(sin(x)),dx)","2.403939431"),
            ("integral(0,6.283185307,x*pi*sin(30*x)/sqrt(4*pi*pi-(x^2)),dx)","-1.271629809"),
            ("integral(0,1,(x^2)*log(x,e)*log(x+1,e)/(x+1),dx)","-0.030262202"),
            ("integral(0,10,(abs(sin(x)/x)^2),dx)","1.518645804"),
            ("integral(0,10,3*(x^4)*log(x,e),dx)","126155.1056"),
            ("integral(0,pi,((pi-x)^2)*log(2*(sin(x/2)^2),e),dx)","-22.26946363"),
            ("integral(0,pi/4,(x^2)/(sin(x)^2),dx)","0.843511842"),
            ("integral(0,1,(16*x-16)/((x^4)-2*(x^3)+4*x-4),dx)","3.141592654"),
            ("integral(0,1,(x^4)*(1-(x^4))/(1+(x^2)),dx)","0.057142857"),
            ("integral(0,1,(2^x)*x/((2^x)-1),dx)","1.711857371"),
            ("integral(0,1,(exp(-x)-exp(-10*x)),dx)","0.532125099"),
            ("integral(-1,1,exp(x/(1+(x^2))),dx)","2.145070539"),
            ("integral(0,6.283185307,exp(sin(x)),dx)","7.954926521"),
            ("integral(0,1,2*(exp(-9*(x^2))+exp(-1024*((x-0.25)^2)))/sqrt(pi),dx)","0.39582597"),
            ("integral(0,10,50/(pi*(2500*(x^2)+1)),dx)","0.499363381"),
            ("integral(0,1,1/sqrt(abs(x)),dx)","2"),
            ("integral(0,1,2*(exp(-9*(x^2))+exp(-1024*((x-0.25)^2)))/sqrt(pi),dx)","0.39582597"),
            ("integral(0,1,1/sqrt(1-(x^2)*(sin(x)^2)),dx)","1.122801998"),
            ("integral(0,1,log(1+x,e)/x,dx)","0.822467033"),
            ("integral(0,2,(x^4)*log(x+sqrt((x^2)+1),e),dx)","8.15336412"),
            ("integral(0,pi/2,cos(x)*((x^2)+x+1),dx)","2.038197427"),
            ("integral(0,2,6*x-(x^4)-1,dx)","3.6"),
            ("integral(-2,2,exp(x)+x-2,dx)","-0.746279184"),
            ("integral(-3,3,exp(x)-4*x-4+4*log(4,e),dx)","29.30681452"),
            ("integral(1,2,exp(x)-5*x+3,dx)","0.17077427"),
            ("integral(-5,5,exp(x)-20*x+90,dx)","1048.406421"),
            ("integral(-2,2,(x^3)-x-1,dx)","-4"),
            ("integral(-3,3,(x^3)-3*(x^2)+4,dx)","-30"),
            ("integral(-2,2,(x^9)-(x^8)+(x^7)-(x^6)+(x^5)-(x^4)-(x^3)+2*(x^2)-x+0.5,dx)","-150.4825397"),
            ("integral(-5,5,2*(x^4)-9*exp(x)-22.5,dx)","939.3422096"),
            ("integral(1,10,sqrt(x)-log(x,e)-0.7,dx)","0.089333471"),
            ("integral(-10,10,7*sin(x)-exp(-x)*cos(x)-0.7,dx)","15218.32136"),
            ("integral(-5,5,exp(x)-20,dx)","-51.59357884"),
            ("integral(1,e,1/x/(1+(log(x,e)^2)),dx)","0.785398163"),
            ("integral(0,4,sqrt(1+sqrt(x)),dx)","6.075895918"),
            ("integral(0,1,1/sqrt(sin(x)),dx)","2.034805319"),
            ("integral(0,pi,cos(100*sin(x)),dx)","0.0627874"),
            //("integral(0,pi,(0.1*cos(100*sin(x))-1)*sign(x-1),dx)","-1.137541289"), // sign function not support.
            ("integral(-1,1,1/((x^6)+0.9),dx)","1.992252408"),
            ("integral(-1,1,sqrt(abs(x+0.5)),dx)","1.460447132"),
            ("integral(0,1,(sin(50*pi*x)^2),dx)","0.5"),
            ("integral(0,1,x/(exp(x)+1),dx)","0.17055735"),
            //("integral(0,1,(1/cosh(10*(x-0.2))^2)+(1/cosh(100*(x-0.4))^4)+(1/cosh(1000*(x-0.6))^6),dx)","0.210802736"), // 0.21025937517176302
            //("integral(0,1,log(abs(x-0.7),e),dx)","-1.610864302"), // -1.6093541136756688
            ("integral(0,6.283185307,exp(cos(x)),dx)","7.954926521"),
            ("integral(0,1,1/((x^(1.0/2.0))+(x^(1.0/3.0))),dx)","0.841116917"),
            ("integral(0,pi,exp(-x)*sin(50*x),dx)","0.01912807"),
            ("integral(2,7,cos(x)+5*cos(1.6*x)-2*cos(2*x)+5*cos(4.5*x)+7*cos(9*x),dx)","-4.527569625"),
            ("integral(0,pi,exp(x)*cos(x),dx)","-12.07034632"),
            ("integral(0,1,sqrt(-log(x,e)),dx)","0.886226925"),
            //("integral(-1,1,exp(-20*x),dx)","24258259.77"), // 0.8862269254527566
            ("integral(0,1,2*(x^2)/((x-1)*(x+1))-x/log(x,e),dx)","0.036489974"),
            ("integral(0,1,(x^2)*log(1/x,e),dx)","0.111111111"),
            ("integral(-1,1,23.0/50.0*(exp(x)+exp(-x))-cos(x),dx)","0.479428227"),
            ("integral(0,pi,cos(32*sin(x)),dx)","0.433788003"),
            ("integral(0,1,(abs(x-1.0/3.0)^2),dx)","0.111111111"),
            ("integral(0,1,(abs(x-pi/4)^2),dx)","0.164785445"),
            ("integral(0,1,exp(20*(x-1))*sin((2^5)*x),dx)","-0.011001836"),
            ("integral(0,6.283185307,(1-(sin(pi/12)^2))/(1-(sin(pi/12))*cos(x)),dx)","6.06909096"),
            ("integral(0,1,sqrt(x)*log(x,e),dx)","-0.444444444"),
            //("integral(0,3,(x^3)*log(abs(((x^2)-1)*((x^2)-2)),e),dx)","52.74074838"), // 52.76590865695036
            //("integral(0,1,1/sqrt(abs((x^2)+2*x-2)),dx)","1.504622762"), 1.5055901110867347
            ("integral(0,1,(x^0.2),dx)","0.833333333"),
            ("integral(0,1,sqrt(x)*((1-x)^0.3),dx)","0.474421155"),
            ("integral(0,1,(x^(5.0/2.0)),dx)","0.285714286"),
            ("integral(0,pi/2,((x^2)+x+1)*cos(x),dx)","2.038197427"),
            ("integral(0,1,1/(1-0.998*(x^2)),dx)","3.803756515"),
            ("integral(0,1,1-((((x-pi/(2*exp(1)))^2))^(1.0/3.0)),dx)","0.61692669"),
            ("integral(0,pi/2,log(2*sin(x/2),e),dx)","-0.915965594"),
            ("integral(0,1,exp(x)/(exp(x)-0.99),dx)","5.152297938"),
            ("integral(0,1,(3+(x-1)*exp(x))/((3-exp(x))^2),dx)","3.549646778"),
            ("integral(0,1,exp(x)/((3-exp(x))^2),dx)","3.049646778"),
            ("integral(1,1.5,1+(tan(x)^2),dx)","12.54401222"),
            ("integral(0,1,(2*(1-x)*sin(x)+cos(x))/sqrt(1-x),dx)","2"),
            ("integral(0,1,(1-x^(1.0/4.0))^4,dx)","0.014285714"),
            ("integral(0,1,5/(1-exp(-5))*exp(-5*x),dx)","1"),
            ("integral(0,1,100*exp(-100*x),dx)","1"),
            ("integral(0,1,1/(1-0.98*(x^2)),dx)","2.670965315"),
            ("integral(0,2,x*(x-2)*log(x/2,e),dx)","1.111111111"),
            ("integral(0,2,((x-15)^3)*log(x/2,e),dx)","6114"),
            ("integral(0,1,((x^(1.0/2.0))-(x^(-1.0/2.0)))/(1+x),dx)","-1.141592654"),
            ("integral(0,1,(x^0.5)/((1-x)^0.5),dx)","1.570796327"),
            ("integral(0,1,((x^2)+x)/(1+(x^5)),dx)","0.6606532"),
            ("integral(0,1,(1-sqrt(x))^2,dx)","0.166666667"),
            ("integral(0,1,(x^2)*(1-(x^5))^(-3.0/5.0),dx)","0.6606532"),
            ("integral(0,1,1/(1-x)-3*(x^2)/(1-(x^3)),dx)","1.098612289"),
            ("integral(0,1,((x^(1.0/2.0))-(x^(-1.0/2.0)))*x/(1-(x^2)),dx)","-0.429203673"),
            ("integral(0,1,((x^3)+(x^(7.0/2.0))-2*(x^7))/(1-x),dx)","1.386294361"),
            ("integral(0,pi,sin(x)/sqrt(1-4*cos(x)+4),dx)","1"),
            ("integral(0,pi/2,tan(x)/((cos(x)^3)+1/(cos(x)^3)),dx)","0.261799388"),
            ("integral(0,pi/2,sin(4*sin(x))*sin(2*x),dx)","0.232221499"),
            //("integral(0,pi/2,sin(4*tan(x))*sin(2*x),dx)","0.115080553"), // 0.11521694599450413
            ("integral(0,1,1/(1-x)*log(x,e),dx)","-1.644934067"),
            ("integral(0,1,log((1-x)/x,e)/(1+(x^2)),dx)","0.272198261"),
            ("integral(0,1,(x^3)/(1-x)-3*(x^11)/(1-(x^3)),dx)","1.098612289"),
            ("integral(0,3,x*exp(-16*(x^2)),dx)","0.03125"),
            ("integral(0,1,x*exp(x)/((1+x)^2),dx)","0.359140914"),
            ("integral(0,0.693147181,x/(exp(x)+2*exp(-x)-2),dx)","0.272198261"),
            ("integral(0,1,(3*exp(1-(x^(-3)))/(1-(x^3))-exp(1-1/x)/(1-x))/x,dx)","-1.098612289"),
            ("integral(0,1,x/sqrt(cosh(6)-cosh(6*x))/sinh(3*x),dx)","0.01812547"),
            ("integral(0,pi,sin(3*x)*cos(2*x)/sin(x),dx)","3.141592654"),
            ("integral(0,pi,sin(3*x)/sin(x),dx)","3.141592654"),
            ("integral(0,pi/2,sin(5*x)/sin(x),dx)","1.570796327"),
            ("integral(0,pi,cos(3*x)/(1+cos(x)/2),dx)","-0.069787332"),
            ("integral(0,pi,sin(3*x)*sin(x)/(1-4*cos(x)+4),dx)","0.09817477"),
            ("integral(0,pi,(cos(3*x)-0.5*cos(4*x))/(1-cos(x)+0.25),dx)","0.392699082"),
            ("integral(0,1,1/(1-(x^2)/2),dx)","1.24645048"),
            ("integral(0,pi/2,(cos(x)^2)*sin(4*x)/tan(x),dx)","1.570796327"),
            ("integral(0,pi/2,(cos(x)^3)*sin(4*x)/sin(x),dx)","1.570796327"),
            ("integral(0,1,1/(1+10*(x^2)),dx)","0.399876005"),
            ("integral(0,pi,(sin(x)^3)/(4+3*cos(x)),dx)","0.384393665"),
            ("integral(0,1.047197551,sqrt(tan(x))/(sin(x)+cos(x))/sin(x),dx)","1.842060081"),
            ("integral(0,pi,x*sin(x)/(1-cos(x)),dx)","4.355172181"),
            ("integral(0,pi,(x-sin(x))/(1-cos(x)),dx)","2"),
            ("integral(-pi,pi,1/(1-6*cos(x)+9),dx)","0.785398163"),
            ("integral(0,pi/2,x/((sin(x)+3*cos(x))^2),dx)","0.361377669"),
            ("integral(0,pi,x/(9*(cos(x)^2)+16*(sin(x)^2)),dx)","0.411233517"),
            ("integral(0,pi/2,(1-x/tan(x))/(sin(x)^2),dx)","0.785398163"),
            ("integral(0,1,1/(1+5*(x^2)),dx)","0.514412801"),
            ("integral(0,pi/2,(x^3)*cos(x)/(sin(x)^3),dx)","1.328486843"),
            ("integral(0,1,x*sin(10*pi*x),dx)","-0.031830989"),
            ("integral(0,pi,exp(3*cos(x))*sin(x),dx)","6.678583285"),
            ("integral(0,pi,exp(4*cos(x))*sin(4*sin(x))/sin(x),dx)","85.73380338"),
            ("integral(0,pi/2,x*exp(-4*(tan(x)^2))*(4-(cos(x)^2))/(cos(x)^4)*tan(x),dx)","0.221556731"),
            ("integral(-1,1,1/(1+(x^2)+(x^4)),dx)","1.456205826"),
            ("integral(0,1,1/sqrt(log(1/x,e)),dx)","1.772453851"),
            ("integral(0,pi/2,log(sin(x),e),dx)","-1.088793045"),
            ("integral(0,1,1/sqrt(x)*log(log(1/x,e),e),dx)","0.231863031"),
            ("integral(0,pi,log(4+2*cos(x),e),dx)","4.137345254"),
            ("integral(0,9.424777961,log(1-8*cos(x)+16,e),dx)","26.13103308"),
            ("integral(0,pi/2,log((1+sin(1)*(cos(x)^2))/(1-sin(1)*(cos(x)^2)),e),dx)","1.640659389"),
            ("integral(0,pi/2,log(2*tan(x),e),dx)","1.088793045"),
            ("integral(0,pi/2,log(9+16*(tan(x)^2),e),dx)","6.113257029"),
            ("integral(0,1,(x^2)+3*(x^3),dx)","1.083333333"),
            ("integral(0,1,(1-x)*log(x,e)/(1+x),dx)","-0.644934067"),
            ("integral(0,1,x*log(x,e)/((1+(x^2))^2),dx)","-0.173286795"),
            ("integral(0,1,1/(1-x)+x*log(x,e)/((1-x)^2),dx)","0.644934067"),
            ("integral(0,1,x*log(x,e)/sqrt(1-(x^4)),dx)","-0.272198261"),
            ("integral(0,1,x*log(x,e)/(((1-(x^3))^2)^(1/3)),dx)","-0.298679853"),
            ("integral(0,1,(log(x,e)^2)/((x^2)-x+1),dx)","2.210059529"),
            ("integral(0,1,(1-x)/(1+x)/log(x,e),dx)","-0.451582705"),
            ("integral(0,1,(x^2)/sqrt(log(1/x,e)),dx)","1.023326708"),
            ("integral(0,1,1/log(x,e)+1/(1-x),dx)","0.577215665"),
            ("integral(0,1,1/(pi*pi+(log(x,e)^2))/(1+(x^2)),dx)","0.068309886"),
            ("integral(0,1,(x*log(x,e)+1-x)/x/(log(x,e)^2)*log(1+x,e),dx)","0.241564475"),
            ("integral(0,1,x*exp(x)*log(1-x,e),dx)","-1.718281828"),
            ("integral(0,pi/2,(sin(x)^3)*log(sin(x),e)/sqrt(1+(sin(x)^2)),dx)","-0.076713205"),
            ("integral(0,pi/2,x/(1+sin(x)),dx)","0.693147181"),
            ("integral(0,pi/4,log(1+tan(x),e),dx)","0.272198261"),
            ("integral(0,pi/4,log(1/tan(x)-1,e),dx)","0.272198261"),
            ("integral(0,1, log(log(1/x,e),e)/sqrt(log(1/x,e))  ,dx)","-3.480230907"),
            ("integral(0,1,log(x,e)/(1+(x^2)),dx)","-0.915965594"),
            ("integral(0,1,log(x,e)/sqrt(1-(x^2)),dx)","-1.088793045"),
            ("integral(0,1,sqrt(1-(x^2))*log(x,e),dx)","-0.937095604"),
            ("integral(0,1,(log(x,e)^2)*(1+(x^2))/(1+(x^4)),dx)","2.055445172"),
            ("integral(0,1,(log(x,e)^3)/(1+x),dx)","-5.682196977"),
            ("integral(0,1,(log(x,e)^4)/(1+(x^2)),dx)","23.90778787"),
            ("integral(0,1,(1-x)/(1+x)*(x^2)/(1+(x^2))/log(x,e),dx)","-0.105009115"),
            ("integral(0,1,((x^3)-(x^2))/log(x,e),dx)","0.287682072"),
            ("integral(0,1,((x^4)-(x^3))*(x^4)/log(x,e),dx)","0.117783036"),
            // ("integral(0,1,((x^5)-1)/x/(log(x,e)^2)-5/log(x,e),dx)","3.047189562"), // 3.0485963795584246
            ("integral(0,1,log((1+x)/2,e)/(1-x),dx)","-0.582240526"),
            ("integral(0,1,log(1+x,e)/((3*x+3)^2),dx)","0.017047379"),
            ("integral(0,1,log(1+x,e)*(1+(x^2))/((1+x)^4),dx)","0.088395384"),
            ("integral(0,1,log((1+x)/(1-x),e)/(1+(x^2)),dx)","0.915965594"),
            ("integral(0,1,log(log(1/x,e),e)/(1+x),dx)","-0.240226507"),
            ("integral(0,1,(1-x)*exp(-x)*log(x,e),dx)","-0.632120559"),
            ("integral(0,pi/2,log(sin(x),e)*(sin(x)^2),dx)","-0.151697441"),
            ("integral(0,pi/2,sin(x)*log(cot(x/2),e),dx)","0.693147181"),
            // ("integral(0,pi,log(1+cos(x)/2,e)/cos(x),dx)","1.644934067"), // 1.6387981416049242
            ("integral(0,pi/2,log(sin(x),e)*tan(x),dx)","-0.411233517"),
            ("integral(1,11,exp(x)*sin(2*x),dx)","23841.77876"),
            ("integral(-1,1,(x^4)*(sin(pi*x)^2),dx)","0.11407779"),
            ("integral(0,pi/4,(sec(x)^3),dx)","1.147793575"),
            ("integral(0,1,-log(1-x,e)/(1-x)*(log(x,e)^2),dx)","0.541161617"),
            ("integral(0,1,1/sqrt(x)*1/sqrt(1-x),dx)","3.141592654"),
            ("integral(-1,1,(4*(x^3)-1)/(2*(x^2)+1),dx)","-1.351021718"),
            ("integral(0,0.5,2/(2*(x^2)-1),dx)","-1.24645048"),
            ("integral(0,1,atan((x+1)/3)-atan((x-1)/3),dx)","0.624418037"),
            ("integral(-1,1,cos(20*(x^2)),dx)","0.325307509"),
            ("integral(0,1,1/(1-log(x,e))/sqrt(-log(x,e)),dx)","1.343293426"),
            ("integral(0,pi/4,x/sin(x)/cos(x),dx)","0.915965594"),
            // ("integral(-pi/2,pi/2,x*(sin(x)^-1),dx)","3.663862377"), NaN on xcas and this
            ("integral(0,pi/4,log(cot(x),e),dx)","0.915965594"),
            ("integral(0,1,log(log(1/x,e),e),dx)","-0.577215665"),
            ("integral(0,1,exp(x)*cos(10*x),dx)","-0.178899603"),
            ("integral(0,1,atan(x)/x,dx)","0.915965594"),
            ("integral(0,5,sin(3*x)*cos(5*x)/x,dx)","-0.035681237"),
            ("integral(0,pi/2,1/(4+3*cos(x)),dx)","0.273167869"),
            ("integral(0,6.283185307,1/((4+3*sin(x))^2),dx)","1.357040471"),
            ("integral(0,1,(sin(5*x)^3),dx)","0.07812254"),
            ("integral(0,1,x/(1+sin(3*x)),dx)","0.303855179"),
            ("integral(0,1,cos(3*x)/(1+cos(3*x)),dx)","-3.700473316"),
            ("integral(-0.1,0.1,tan(3*x)/(tan(3*x)-1),dx)","-0.006603753"),
            ("integral(0,1,(sec(x)^2),dx)","1.557407725"),
            ("integral(0,1,1/(sec(x)+1),dx)","0.45369751"),
            ("integral(0,1,sin(3*x)*cos(3*x),dx)","0.003319143"),
            ("integral(0,1,sec(x)*tan(x),dx)","0.850815718"),
            ("integral(-1,1,(x^2)*(cos(3*pi*x/2)^2),dx)","0.310817515"),
            ("integral(0,1,x/((x^2)+9)*log((x^2)+9,e),dx)","0.118525567"),
            ("integral(5,10,1/x/log(x,e),dx)","0.35814745"),
            ("integral(10,20,1/exp(x)*(1/x-log(x,e)),dx)","-0.000104531"),
            ("integral(-2,2,1.0/5.0*(1.0/100.0*(322+3*x*(98+x*(37+x)))-24*x/(1+(x^2))),dx)","3.76"),
            ("integral(-1,1,(x^2)*(sin(3*x)^2),dx)","0.323972609"),
            ("integral(-1,1,x*sin(3*x),dx)","0.691355"),
            ("integral(-1,1,1/(1+cos(3*x)),dx)","9.400946631"),
            ("integral(-1,1,cos(3*x)/(1+cos(3*x)),dx)","-7.400946631"),
            ("integral(-1,1,(tan(x)^2),dx)","1.114815449"),
            ("integral(0,1,(log(1/x,e)^5),dx)","120"),
            ("integral(0,1,(x^8)*((1-x)^4),dx)","0.0001554"),
            ("integral(5,10,acosh(4*x),dx)","20.37430572"),
            ("integral(1,10,(4*x+3)/(6*(x^2)+3*x+8),dx)","1.392971774"),
            ("integral(0,1,cos(pi*(x^2)/2),dx)","0.7798934"),
            ("integral(0,1,sin(pi*(x^2)/2),dx)","0.438259147"),
            ("integral(0,1,1/(3+4*exp(5*x)),dx)","0.03697167"),
            ("integral(0,1,(x^4)*exp(3*x),dx)","2.628900076"),
            ("integral(0,1,x*exp(3*x)/((1+3*x)^2),dx)","0.44682047"),
            ("integral(0,1,(sinh(x)^5),dx)","0.310001629"),
            ("integral(0,1,(cosh(x)^5),dx)","2.705569517"),
            ("integral(0,1,(sinh(x)^4)*(cosh(x)^2),dx)","0.630014711"),
            ("integral(0,1,(sinh(x)^3)*(cosh(x)^3),dx)","0.915916015"),
            ("integral(0,1,sinh(x)*(cosh(x)^4),dx)","1.549738311"),
            ("integral(1,5,1/sinh(x),dx)","0.758460735"),
            ("integral(1,5,1/cosh(x),dx)","0.691551153"),
            ("integral(0,1,sinh(x)/cosh(x),dx)","0.43378083"),
            ("integral(0,1,(sinh(x)^3)/(cosh(x)^2),dx)","0.191134908"),
            ("integral(1,5,cosh(x)/sinh(x),dx)","4.145368057"),
            ("integral(1,5,(cosh(x)^2)/sinh(x),dx)","73.42532862"),
            ("integral(1,5,1/sinh(x)/cosh(x),dx)","0.272250669"),
            ("integral(0,1,sinh(2*x)/(cosh(x)^2),dx)","0.867561661"),
            ("integral(0,1,sinh(x)/(cosh(x)+sinh(x)),dx)","0.283833821"),
            ("integral(0,1,1/(cosh(x)-sinh(x)),dx)","1.718281828"),
            ("integral(1,5,1/(1-(cosh(x)^2)),dx)","-0.312944482"),
            ("integral(0,1,x*sinh(x),dx)","0.367879441"),
            ("integral(0,1,x*cosh(x),dx)","0.632120559"),
            ("integral(1,5,x/(cosh(x)^2),dx)","0.364834478"),
            ("integral(1,5,x/(1+cosh(x)),dx)","1.084047012"),
            ("integral(0,1,(sin(x)^2)*(cos(x)^2),dx)","0.148650078"),
            ("integral(0,1,sin(x)*(cos(x)^4),dx)","0.190790965"),
            ("integral(0,1,(sin(x)^4)*(cos(x)^4),dx)","0.03031619"),
            ("integral(0,1,(sin(x)^3)/(cos(x)^2),dx)","0.391118024"),
            ("integral(0,1,sin(x)/(cos(x)^4),dx)","1.780001358"),
            ("integral(1,2,cos(x)/(sin(x)^3),dx)","0.101416245"),
            ("integral(1,1.1,1/((sin(x)^4)*(cos(x)^4)),dx)","2.934722013"),
            ("integral(0,1,cos(2*x)/(cos(x)^2),dx)","0.442592275"),
            ("integral(1,2,sin(3*x)/(sin(x)^3),dx)","-0.700749489"),
            ("integral(0,1,cos(3*x)/(cos(x)^3),dx)","-0.672223174"),
            ("integral(1,2,cos(log(x,e)),dx)","0.908200178"),
            ("integral(0,1,cos(x)/((1-0.01*(sin(x)^2))^(3.0/2.0)),dx)","0.844466016"),
            ("integral(0,1,cosh(x)*cos(x),dx)","0.966710748"),
            ("integral(0,3,(x^2)*exp(-x*log(3,e)),dx)","0.964934761"),
            ("integral(0,3,x*exp(-x*log(3,e)),dx)","0.69671126"),
            ("integral(0,2,((sin(2*pi*x)+cos(pi*x))/pi/(2*x+1))*2*(-1),dx)","-0.128291943"),
            ("integral(0,2,(sin(pi*(x-0.5))-sin(2*pi*(x-0.5)))/pi/(x-0.5),dx)","-0.174295012"),
            ("integral(0,2,1+exp(-x)*sin(8*(x^(2.0/3.0))),dx)","2.01627972"),
            ("integral(0,3,3*exp(-x)*sin((x^2))+1,dx)","3.830868396"),
            ("integral(0,2,exp(-2*x)*(14*x-11*(x^2)),dx)","1.08426041"),
            ("integral(0,2,(x^10)*exp(4*(x^3)-3*(x^4)),dx)","7.258395171"),
            ("integral(0,2,2+cos(2*sqrt(x)),dx)","3.459997672"),
            ("integral(0,2,sin(pi*x)/pi/x,dx)","0.451411667"),
            ("integral(0,1,(x^4)*log(x+sqrt((x^2)+1),e),dx)","0.150948118"),
            ("integral(0,1,x*exp(x),dx)","1"),
            ("integral(0,1,atan(x),dx)","0.438824573"),
            ("integral(0,1,exp(x)*cos(x),dx)","1.378024614"),
            ("integral(-1,1,pi/2*cosh(x)*sin(exp(pi/2*sinh(x))),dx)","1.260710779"),
            ("integral(0,1,4*(x^3)*sqrt((x^4)+7),dx)","2.738105214"),
            ("integral(0,1,x*cos(x),dx)","0.381773291"),
            ("integral(0,1,(cos(x)^2),dx)","0.727324357"),
            ("integral(0,1,(tan(x)^2)*(sec(x)^4),dx)","3.091663935"),
            ("integral(0,1,(3*x+6)/((x^2)+5*x+4),dx)","1.139434283"),
            ("integral(0,1,x/((x^2)+4),dx)","0.111571776"),
            ("integral(0,1,1/((x^2)+4),dx)","0.231823805"),
            ("integral(0,1,x/(((x^2)+4)^2),dx)","0.025"),
            ("integral(0,1,tan(x),dx)","0.61562647"),
            ("integral(0,1,sec(x),dx)","1.226191171"),
            ("integral(0,1,exp(-((x^2))),dx)","0.746824133"),
            ("integral(1,2,(((2*x+3/x))^2),dx)","25.83333333"),
            ("integral(0,1,(x^(0.1))*(1.2-x)*(1-exp(20*(x-1))),dx)","0.602298071"),
            ("integral(-1,1,1/(9+(x^2)),dx)","0.21450037"),
            ("integral(-1,1,sqrt(abs(x-0.5)),dx)","1.460447132"),
            ("integral(0,1,sin(1-30*(x^2)),dx)","0.02181621"),
            ("integral(0,2,(2+cos(1+(x^(3.0/2.0)))*exp(0.5*x)/sqrt(1+sin(x)/2))*exp(0.5*x),dx)","3.70915604"),
            ("integral(-1,1,1/(1+((x+3)^2)),dx)","0.218668946"),
            ("integral(-1,1,exp(-x)/(1+(x^2)),dx)","1.795521283"),
            ("integral(-1,1,log(1+x,e)*log(1-x,e),dx)","-1.101550828"),
            ("integral(-8,23,-(x^3)/10+23*x-3.5,dx)","-1654.625"),
            ("integral(0,1,(x^25)*((1-x)^2),dx)","0.00010175"),
            ("integral(0,2,(x^10)-10*(x^8)+33*(x^6)-40*(x^4)+16*(x^2),dx)","7.388167388"),
            ("integral(0,1,exp(-x)*sin(x)/x,dx)","0.606073628"),
            ("integral(0,1,((x^2)*log(x,e))/((x^2)-1)/((x^4)+1),dx)","0.180671263"),
            ("integral(0,pi/2,asin(sqrt(2)/2*sin(x))*sin(x)/sqrt(4-2*(sin(x)^2)),dx)","0.384946473"),
            ("integral(0,1,x/(1-exp(-6*sinh(x))),dx)","0.543061029"),
            ("integral(0,1,tanh(pi/2*sinh(x)),dx)","0.60371432"),
            ("integral(-3,3,(x^3)+(x^2)+5*x+3,dx)","36"),
            ("integral(0,1,(sin(x)^3)/((sin(x)^3)+(cos(x)^3)),dx)","0.243997012"),
            ("integral(-1,1,exp(-x)*sin(x),dx)","-0.663493667"),
            ("integral(0,1,log(x+sqrt(1+(x^2)),e),dx)","0.467160025"),
            ("integral(0,1,1/sqrt(exp(2*x)+1),dx)","0.521323942"),
            ("integral(0,1,(sec(x)^8)*tan(x),dx)","17.08637016"),
            ("integral(-1,1,exp(-x)/(1+(x^4)),dx)","1.989031507"),
            ("integral(0,1,8*exp(-8*x),dx)","0.999664537"),
            ("integral(0,1,1+cos(1.95*pi*x),dx)","0.974464289"),
            ("integral(0,1,exp(-x)/sqrt(x),dx)","1.493648266"),
            ("integral(-1,1,exp(-x)*cos(x),dx)","1.933421496"),
            ("integral(-1,1,exp(-x)*(x^2),dx)","0.878884623"),
            ("integral(-1,1,100*exp(-200*(x^2)),dx)","12.53314137"),
            ("integral(0,2,(14*x-11*(x^2))*exp(-2*x),dx)","1.08426041"),
            ("integral(8,30,2000*log(140000/(140000-2100*x),e)-9.8*x,dx)","11061.33554"),
            ("integral(-0.01,0.01,1/((x^2)+10^-6),dx)","2942.255349"),
            ("integral(0,1,1+exp(-25*x)/2,dx)","1.02"),
            ("integral(0,0.5,1/(log(1/x,e)^3)/x,dx)","1.040684491"),
            ("integral(0,pi/4,exp(3*x)*sin(2*x),dx)","2.588628633"),
            ("integral(0,1,x*exp(x)/((x+1)^2),dx)","0.359140914"),
            ("integral(0,pi/2,5/(exp(pi)-2)*exp(2*x)*cos(x),dx)","1"),
            ("integral(-10,10,-((x^3)+23*x-3.5),dx)","70"),
            ("integral(-1,1,10*exp(-100*(x^2)),dx)","1.772453851"),
            ("integral(0,1,(x^3),dx)","0.25"),
            ("integral(0,1,(x^6),dx)","0.142857143"),
            ("integral(0,1,(x^10),dx)","0.090909091"),
            ("integral(0,1,sqrt((x^7)),dx)","0.222222222"),
            ("integral(0,pi/2,1/(1+(sin(x)^2)),dx)","1.110720735"),
            ("integral(0,5,(x-1)*(x-2)*(x-3)*(x-4)*(x-5)/120,dx)","-0.329861111"),
            //("integral(0,1,(x+0.01)^-5,dx)","24999999.76"), // 24999999.759742472
            ("integral(0,10,50/(pi*(1+2500*(x^2))),dx)","0.499363381"),
            ("integral(0.1,1,(sin(100*pi*x)/(pi*x)),dx)","0.009098638"),
            ("integral(0,6.283185307,x*cos(x)*sin(3*x),dx)","-2.35619449"),
            ("integral(0,1,log(x,e)*log(1-x,e),dx)","0.355065933"),
            ("integral(0,6.283185307,x*sin(30*x)/sqrt(1-(x^2)/(4*pi*pi)),dx)","-2.543259619"),
            ("integral(0,1,1/sqrt(x)*log(log(1/x,e),e),dx)","0.231863031"),
            ("integral(0,1,1/sqrt(x)/sqrt(1-x),dx)","3.141592654"),
            ("integral(0,1,1/(1-log(x,e))/sqrt(-log(x,e)),dx)","1.343293426"),
            ("integral(0,1,(x^0.95)*exp(x),dx)","1.020457365"),
            ("integral(0,1,(log(x,e)^2)*1/(1+(x^2)),dx)","1.937892293"),
            ("integral(0,1,sqrt(x)*exp(-x)/(1+x),dx)","0.256004339"),
            ("integral(0,1,log(abs(3+x),e),dx)","1.249340578"),
            ("integral(0,1,(x^3)*((1-x)^5),dx)","0.001984127"),
            ("integral(0,1,sqrt(x)/sqrt(1-x),dx)","1.570796327"),
            ("integral(0,1,(x^(-0.5))/((1-x)^0.5),dx)","3.141592654"),
            ("integral(0,1,(x^8)/sqrt(1-x),dx)","0.59907674"),
            ("integral(0,1,(x^2)/(1+(x^4))/sqrt(1-(x^4)),dx)","0.392699082"),
            ("integral(0,4,x*exp(-3*x),dx)","0.111102236"),
            ("integral(0,1,x*exp(x)/((1+x)^2),dx)","0.359140914"),
            ("integral(0,5,exp(-4*x)/sqrt(x),dx)","0.886226925"),
            ("integral(0,0.693147181,x/(exp(x)+2*exp(-x)-2),dx)","0.272198261"),
            ("integral(0,0.693147181,x/(1-exp(-x)),dx)","0.822467033"),
            ("integral(0,1,(4*exp(1-(x^-4))/(1-(x^4))-exp(1-1/x)/(1-x))/x,dx)","-1.386294361"),
            ("integral(0,1,exp(-5/x)/(x^2),dx)","0.001347589"),
            ("integral(0,pi/2,exp(-((tan(x)^2))),dx)","0.671646711"),
            ("integral(0,1,x/sqrt(cosh(8)-cosh(8*x))/sinh(4*x),dx)","0.003902617"),
            ("integral(0,pi/2,sin(7*x)/sin(x),dx)","1.570796327"),
            ("integral(0,pi/2,sin(8*x)*cos(5*x)/sin(x),dx)","1.570796327"),
            ("integral(0,pi,sin(5*x)/sin(x),dx)","3.141592654"),
            ("integral(0,pi,cos(7*x)/cos(x),dx)","-3.141592654"),
            ("integral(0,pi,cos(2*x)/(1-6*cos(x)+9),dx)","0.043633231"),
            ("integral(0,pi,sin(4*x)*sin(x)/(1-10*cos(x)+25),dx)","0.000502655"),
            ("integral(0,pi/4,(sin(x)^6)/(cos(x)^8),dx)","0.142857143"),
            ("integral(0,pi/2,sin(10*x)*(cos(x)^9)/sin(x),dx)","1.570796327"),
            ("integral(0,pi/2,sqrt(sin(x)),dx)","1.198140235"),
            ("integral(0,pi/2,1/sqrt(sin(x)),dx)","2.622057554"),
            ("integral(0,pi/2,(cos(x)^5)*cos(5*x)/(16*(sin(x)^2)+9*(cos(x)^2)),dx)","0.007975325"),
            ("integral(0,pi,(sin(x)^2)/(5+4*cos(x)),dx)","0.392699082"),
            ("integral(0,pi/4,(sin(x)^(0.5))/((cos(x)-sin(x))^(-0.5))/(cos(x)^3),dx)","0.392699082"),
            ("integral(0,pi/2,sin(x)/sqrt(1+25*(sin(x)^2)),dx)","0.274680153"),
            ("integral(0,pi,sin(x)/sqrt(1-8*cos(x)+16),dx)","0.5"),
            ("integral(0,1,sqrt((cos(2*x)-cos(2))/(cos(2*x)+1)),dx)","0.722091449"),
            ("integral(0,pi/4,1/((tan(x)^3)+(cot(x)^3))/sin(2*x),dx)","0.130899694"),
            ("integral(0,pi/2,tan(x)/((cos(x)^4)+(sec(x)^4)),dx)","0.196349541"),
            ("integral(0,pi/2,sin(4*sin(x))*sin(2*x),dx)","0.232221499"),
            ("integral(0,pi/2,sin(4*cos(x))*sin(2*x),dx)","0.232221499"),
            ("integral(0,pi/4,x*tan(x),dx)","0.185784536"),
            ("integral(0,pi/4,x*cot(x),dx)","0.730181058"),
            ("integral(0,pi/2,x/sin(x),dx)","1.831931188"),
            ("integral(0,pi/2,x*cot(x),dx)","1.088793045"),
            ("integral(0,pi/2,(pi/2-x)*tan(x),dx)","1.088793045"),
            ("integral(0,pi/4,(x^2)/(cos(x)^2),dx)","0.245281203"),
            ("integral(0,pi/4,x*(tan(x)^3),dx)","0.099613628"),
            ("integral(0,pi/4,(x^2)*tan(x)/(cos(x)^2),dx)","0.178025702"),
            ("integral(0,pi/4,(x^2)*(tan(x)^2)/(cos(x)^2),dx)","0.139207673"),
            ("integral(0,1,1/sqrt(log(1/x,e)),dx)","1.772453851"),
            ("integral(1,e,log(x,e)/((1+log(x,e))^2),dx)","0.359140914"),
            ("integral(0,1,log(x,e)*log(1-x,e),dx)","0.355065933"),
            ("integral(0,pi/4,log(tan(x),e)^4,dx)","23.90778787"),
            ("integral(0,pi/4,log(tan(x),e),dx)","-0.915965594"),
            ("integral(0,pi/4,log(1+tan(x),e),dx)","0.272198261"),
            ("integral(0,pi/4,log(sqrt(tan(x))+sqrt(cot(x)),e),dx)","0.730181058"),
            ("integral(-1,1,-log(cos(pi/2*x),e),dx)","1.386294361"),
            ("integral(0,pi/2,sin(x)/sqrt(1+9*(sin(x)^2)),dx)","0.416348591"),
            ("integral(0,pi,sin(x)/sqrt(1-6*cos(x)+9),dx)","0.666666667"),
            ("integral(0,1,sqrt((cos(2*x)-cos(2))/(cos(2*x)+1)),dx)","0.722091449"),
            ("integral(0,pi/4,sqrt(tan(x))+sqrt(cot(x)),dx)","2.221441469"),
            ("integral(0,pi/4,(sqrt(tan(x))-sqrt(cot(x)))*tan(x),dx)","-0.221441469"),
            ("integral(0,pi/4,1/((tan(x)^3)+(cot(x)^3))/sin(2*x),dx)","0.130899694"),
            ("integral(0,pi/2,sin(5*cos(x))*sin(2*x),dx)","-0.190178816"),
            ("integral(0,1,1/sqrt(1-2*x*cos(3)+(x^2)),dx)","0.694402045"),
            ("integral(0,pi/4,(cos(x)-sin(x))/(cos(x)+sin(x))*x,dx)","0.086413725"),
            ("integral(0,pi/4,(pi/4-x*tan(x))*tan(x),dx)","0.141798826"),
            ("integral(0,pi/4,(pi/4-x)*tan(x)/cos(2*x),dx)","0.185784536"),
            ("integral(0,pi/4,(pi/4-x*tan(x))/cos(2*x),dx)","0.730181058"),
            ("integral(0,pi/4,x/sin(x)/(cos(x)+sin(x)),dx)","0.643767333"),
            ("integral(0,pi/4,sin(x)*x/(sin(x)+cos(x))/(cos(x)^2),dx)","0.166626312"),
            ("integral(0,pi/2,1/x-1/tan(x),dx)","0.451582705"),
            ("integral(0,pi/2,(4*(x^2)*cos(x)+(pi-x)*x)/sin(x),dx)","6.841088464"),
            ("integral(0,pi,x*cos(x)/(1+sin(x)),dx)","-1.486276286"),
            ("integral(0,pi/2,x*sin(x)/(1-cos(x)),dx)","2.920724234"),
            ("integral(0,pi/2,x/(sin(x)+cos(x))/sin(x),dx)","1.460362117"),
            ("integral(0,pi/2,x*sin(2*x)/(9*(cos(x)^2)+16*(sin(x)^2)),dx)","0.059928749"),
            ("integral(0,pi/2,x*sin(2*x)/(1+3*(sin(x)^2))/(1+4*(sin(x)^2)),dx)","0.112547741"),
            ("integral(0,pi/4,(x^2)*tan(x)/(cos(x)^2),dx)","0.178025702"),
            ("integral(0,pi/2,(x^3)*cos(x)/(sin(x)^3),dx)","1.328486843"),
            ("integral(0,pi,exp(3*cos(x))*sin(3*sin(x))*cot(x/2),dx)","59.95898259"),
            ("integral(0,pi/4,log(cos(x)-sin(x),e),dx)","-0.730181058"),
            //("integral(0,pi/4,log(cos(x)+sin(x),e),dx)","0.185784536"), // 0.18578453580065918
            ("integral(0,pi/4,log(tan(x),e)^2,dx)","1.937892293"),
            ("integral(0,pi/2,log(cos(x),e)^2,dx)","2.046622024"),
            ("integral(0,pi/2,log(cos(x)+sin(x),e),dx)","0.371569072"),
            ("integral(0,6.283185307,x*sin(30*x)/sqrt(1-((x/2/pi)^2)),dx)","-2.543259619"),
            ("integral(-1,1,cos(sqrt(377)*x),dx)","0.055318603"),
            ("integral(-1,1,(x^2)*exp(-((x^2)))*tan(x)*acos(x),dx)","-0.321556003"),
            ("integral(-1,1,exp(-3*x)*cos(16*sqrt(3)*pi*x),dx)","-0.176358246"),
            ("integral(-1,1,exp(cos(sqrt(47*pi)*x)),dx)","2.438081482"),
            ("integral(-1,1,cosh(tanh(sinh(x))),dx)","2.278006221"),
            ("integral(-1,1,23.0/25.0*cosh(x)-cos(x),dx)","0.479428227"),
            ("integral(-1,1,exp(-2*x)*cos(16*sqrt(2)*x),dx)","-0.218673124"),
            ("integral(-1,1,x*atan((x^3)),dx)","0.355120831"),
            ("integral(-1,1,exp(x)*atan((x^3)),dx)","0.398130065"),
            ("integral(-1,1,x*sin(30*x)/sqrt(1-(x^2)/4/pi/pi),dx)","-0.012696822"),
            ("integral(-1,1,x*sin(50*x)*cos(75*x),dx)","0.033518733"),
            ("integral(-1,1,1/((x^4)+(x^2)+exp(1)),dx)","0.631299652"),
            ("integral(-1,1,tan(x)/(1+exp(x)*sin(pi*x)),dx)","-0.719818068"),
            ("integral(0,8,exp(-3*x)*cos(5*pi*x),dx)","0.011730659"),
            ("integral(-1,1,exp(x)*sin(3*x)*tanh(5*cos(30*x)),dx)","-0.017790593"),
            ("integral(-1,2,abs(x-1/sqrt(3))+abs(x-1/sqrt(2)),dx)","4.548876219"),
            ("integral(0,1,(x^(-2.0/3.0)),dx)","3"),
            ("integral(0,1,sin(10*x)/sqrt(x*(1-x)),dx)","0.535019057"),
            ("integral(-1,1,(x^3)*sin(x)/sqrt(1-(x^2)),dx)","1.021479743"),
            ("integral(0,1,(x^5)*log(1/x,e),dx)","0.027777778"),
            // ("integral(0,1,(x^(-x^((-0.3)*(-log(x,e)^(-0.7))))),dx)","0.897286844"), // 0.5799217429172405,  xcas 5.50703517360809e-01+2.63700790350981e-01*i, pow(x,pow(-x,((-3.0/10.0)*pow(-log(x),(-7.0/10.0)))))
            ("integral(0,1,(x^2)/(100*(x^(2*x))+1),dx)","0.004899121"),
            ("integral(0,1,x*(-log(x,e))/(100*(x^(2*x))+1),dx)","0.004488718"),
            ("integral(0,1,sin(x*(-log(x,e))/(100*(x^(-2*x))+1)),dx)","0.001391906"),
            ("integral(0,1,sin(100*x*(-log(x,e)))/(100*(x^(-2*x))+1),dx)","-0.000604121"), 
            // ("integral(0,1,0.01*x^(-x-1)*sin(70*x^2),dx)","0.0008853"), // 0.009762890909856766 xcas 9.76289091029919e-03 
            // ("integral(0,1,sin(70*sin(x*(-log(x,e))))/(100*(x^(x+1))),dx)","-0.001626241"), // 0.013761353877509794 xcas 1.37613538777818e-02
            ("integral(0,1,sin(((sqrt(x)*(-log(x,e)))^(1.0/10.0)))/((x^(1.0/10.0))*((-log(x,e))^(1.0/5.0))),dx)","0.974956209"),
            ("integral(0,1,sin(30*(x^(1.0/20.0))*(-log(x,e)^((1.0/10.0))/(100*(x^(1.0/10.0))*((-log(x,e))^(1.0/5.0))+1))),dx)","0.325192767"),
            ("integral(0,1,(x^2)*exp(x),dx)","0.718281828"),
            ("integral(0,1,((-log(x,e))^5)*(x^4),dx)","0.00768"),
            ("integral(0,pi/2,(sin(x)^6)*(cos(x)^6),dx)","0.007669904"),
            ("integral(0,pi/2,(log(sin(x),e)^2),dx)","2.046622024"),
            ("integral(0,1,sqrt(-log(x,e)),dx)","0.886226925"),
            ("integral(0,4,cos(x*exp(x)),dx)","0.319708546"),
            ("integral(0,1,((1-sqrt(x))^9),dx)","0.018181818"),
            ("integral(0,1,sqrt(x)*((1-x)^0.3),dx)","0.474421155"),
            ("integral(0,1,3*(x^2)/((x^6)+1),dx)","0.785398163"),
            ("integral(0,1,cos(4*x)/sqrt(x),dx)","0.461461462"),
            ("integral(0,1,(cos(x)-cos(2*x))/x,dx)","0.607570275"),
            ("integral(0,pi/2,asin(sqrt(2)/2*sin(x))*sin(x)/sqrt(4-2*(sin(x)^2)),dx)","0.384946473"),
            ("integral(-2,2,pi*cosh(x)*sin(exp(pi/2*sinh(x)))/2,dx)","1.570440064"),
            ("integral(0,1,(log(x,e)^6)*atan(abs(x*sqrt(3)/(x-2)))/(x+1),dx)","4.742841655"),
            ("integral(0,1,exp(1-1/x)*cos(1/x-1)/(x^2),dx)","0.5"),
            ("integral(0,4,(x^2)+exp(x)*sin(x),dx)","19.01719148"),
            ("integral(0,1,3*(x^2)*(sin(100*x)^3)*exp(1.0/3.0*x),dx)","-0.027493966"),
            ("integral(0,6.283185307,exp(cos(x)),dx)","7.954926521"),
            ("integral(0,6.283185307,sqrt(1-0.36*(sin(x)^2)),dx)","5.672333578"),
            ("integral(-pi,pi,(1+(tan(x/2)^2))/(1+(tan(x/2)^4)),dx)","4.442882938"),
            ("integral(-pi,pi,exp(-((tan(x/2)^2))),dx)","2.686586843"),
            ("integral(1,2,(1+x)/((x^2+2*x+5)^(1/3)),dx)","1.14658111"),
            ("integral(0,1,1/sqrt(x)*log(exp(1)/x,e),dx)","6"),
            // ("integral(0,1,x*exp((x^2))*erf(x)/sqrt(1-(x^2)),dx)","1.522787622"), erf does not support
            ("integral(0,1,(x^3)*(log(1/x,e)^4),dx)","0.0234375"),
            ("integral(0,1,1-(((x-pi/2/exp(1))^2)^(1.0/3.0)),dx)","0.61692669"),
            ("integral(0,1,1/(sqrt(x)+(x^(1.0/3.0))),dx)","0.841116917"),
            ("integral(0,1,exp(x)/sqrt(x+0.01),dx)","2.724504213"),
            ("integral(0,1,1/sqrt(x)/(1+x),dx)","1.570796327"),
            ("integral(-1,1,exp(-3*(x^2))*log(1+x+(x^2),e),dx)","0.082644457"),
            ("integral(0,1,(x^2)*log(x,e)/sqrt(1-(x^2)),dx)","-0.151697441"),
            ("integral(0,pi,sqrt(4095*(cos(x)^2)+1),dx)","128.0788373"),
            ("integral(-5,5,(x^6)-105.0/4.0*(x^4)+315/2*(x^2)-315.0/4.0,dx)","1846.428571"),
            ("integral(-3,3,(x^8)-104.0/3.0*(x^6)+658*(x^4)-2940*(x^2)+1785,dx)","4459.885714"),
            ("integral(0,pi/2,((x^2)+x+1)*cos(x),dx)","2.038197427"),
            ("integral(0,1,x^(2/3)/(x^2+(1-x)^2)^(4/3),dx)","1.1202513"),
            ("integral(1,5,log(x,e)/exp((x^2)),dx)","0.03588275"),
            ("integral(0,0.64,atan(x)/(x^(3.0/2.0)),dx)","1.561298647"),
            ("integral(0,0.64,atan(x)/sqrt(x),dx)","0.323946328"),
            ("integral(0,1,1/x/exp((-log(x,e)^2)),dx)","0.886226925"),
            ("integral(-1,2,x*abs(x),dx)","2.333333333"),
            ("integral(0,30,x/(5+x)*exp(-2*x/30),dx)","7.4028424"),
            ("integral(0.1,1,exp(x)/(x^3),dx)","59.82508398"),
            ("integral(-1,1,exp(-((cos(x)^2)))/sqrt(1-(x^2)),dx)","1.756700076"),
            ("integral(-1.5,1.5,12*x*exp(-2*x),dx)","-121.1106664"),
            //("integral(-0.01,0.01,(x^2)*(2+sin(1/x)),dx)","1.33333e-06"), // both xcas and this one cannot handle
            ("integral(0,10,sin(2*sin(2*sin(2*sin(x)))),dx)","2.3736718"),
            ("integral(-6,6,-2/sqrt(pi)*log(cos(pi*tanh(x)/2)/(cosh(x)^2),e),dx)","131.3760176"),
            ("integral(0,1,sqrt(1+1/x),dx)","2.295587149"),
            ("integral(-1,1,-log(log(2/(x+1),e),e),dx)","1.15443133"),
            ("integral(0,pi,x*sin(x)/(1+(cos(x)^2)),dx)","2.4674011"),
            ("integral(0,1,(x^2)*log(x,e)/((x^2)-1)/((x^4)+1),dx)","0.180671263"),
            ("integral(5,6,sin((x-4)*55)/(x-4.99),dx)","-0.583124928"),
            ("integral(0,100,exp(-0.01*x)*(0.01*cos(0.3*x)+0.3*sin(0.3*x)),dx)","0.943254063"),
            ("integral(0,100,(0.2*(1+x)*cos(0.2*x)-sin(0.2*x))/((1+x)^2),dx)","0.009039062"),
            ("integral(0,10,80*sin(sqrt(1+80*x))/2/sqrt(1+80*x),dx)","1.539921187"),
            ("integral(0,1,exp(-sqrt(1+x))*(0.5*cos(0.5*x)-sin(0.5*x)/2/sqrt(1+x)),dx)","0.116556371"),
            ("integral(-1,1,exp(-2/(1+x)-2/(1-x)),dx)","0.014059717"),
            ("integral(-1,1,1/((1+(x^2))^(5.0/4.0)),dx)","1.48860616"),
            ("integral(0,2,3*x^4*(x^6+(1-x^3)^2)^(-4/3),dx)","1.797772363"),
            ("integral(0,0.1,sin(21*pi*x)+sin(31*pi*x)/2,dx)","0.01075865"),
            // ("integral(-0.1,0.1,sin(100*x)/x,dx)","3.316695188"),xcas cannot handle it
            ("integral(0,1,(cos(x)-1)/x,dx)","-0.239811742"),
            ("integral(1,2,csch(x),dx)","0.499595364"),
            ("integral(1,2,sech(x),dx)","0.435990853"),
            ("integral(1,2,coth(x),dx)","1.126928011"),
            ("integral(1,2,csc(x)*cot(x),dx)","0.088644935"),
            ("integral(1,2,tanh(x),dx)","0.891221917"),
            ("integral(0,1,(log(1/x,e)^5),dx)","120"),
            ("integral(0,1,(x^(-x)),dx)","1.291285997"),
            ("integral(0,1,(x^x),dx)","0.783430511"),
            ("integral(1,2,log((x^4),e)/x,dx)","0.960906028"),
            ("integral(-4,2,(x+3)*((x-1)^2),dx)","12"),
            ("integral(0,2,(1-cos(x))/(x^2),dx)","0.897339559"),
            ("integral(0,1,log(1/x,e)/(x^0.25),dx)","1.777777778"),
            ("integral(0,1,1/(16*((x-pi/4)^2)+1.0/16.0),dx)","2.77878442"),
            ("integral(0,pi,cos(64*sin(x)),dx)","0.290880102"),
            ("integral(0,1,exp(20*(x-1))*sin(256*x),dx)","-0.000148594"),
            ("integral(-1,1,x*sin(2*exp(2*sin(2*exp(2*x)))),dx)","0.336732835"),
            ("integral(0,1,abs(sin(x)/(2*(((x^x)-(pi/2))/pi))),dx)","0.953989448"),
            ("integral(1,3,exp(2*x)*sin(3*x),dx)","108.5552812"),
            ("integral(0,5,2*x*cos(2*x)-((x-2)^2),dx)","-15.30630799"),
            ("integral(0,1,sqrt(x/(1-x*x)),dx)","1.198140235"),
            ("integral(0,1,log(x,e)*log(1-x,e),dx)","0.355065933"),
            ("integral(-1,1,1/(1+25*x*x),dx)","0.549360307"),
            ("integral(-1,1,1/(1+0.04*x*x),dx)","1.973955598"),
            ("integral(0,1,log(x,e)*cos(10*pi*x),dx)","-0.048988817"),
            // ("integral(0,1,(sech(10*(x-0.2))^2)+(sech(100*(x-0.4))^4)+(sech(1000*(x-0.6))^6)+(sech(1000*(x-0.8))^8),dx)","0.211717021"), // 0.22955306406260673
            // ("integral(-1,1,cos(10*x)*gamma(x+2)*erf(sqrt(1+x)),dx)","-0.115420769"), // erf not support
            ("integral(0,pi,x*(cos(20*x)^2),dx)","2.4674011"),
            ("integral(-1,1,sqrt(abs(x+0.5)),dx)","1.460447132"),
            ("integral(-1,1,1/(1+1000*(x^2)),dx)","0.097346549"),
            ("integral(-1,1,((1-x)^0.6)*((1+x)^-0.5),dx)","3.243968212"),
            ("integral(-1,1,((1-x)^-0.3)*((1+x)^0.2),dx)","2.31245538"),
            ("integral(0,1,log(1/x,e)*asin(x),dx)","0.263943507"),
            ("integral(1,2,1/(((x-sqrt(3))^2)+0.0001),dx)","309.0630054"),
            ("integral(0.043213918,23.14069263,sin(log(x,e)),dx)","11.54873936"),
            ("integral(0,1,(x^2)*log(x,e)/sqrt(1-(x^2)),dx)","-0.151697441"),
            ("integral(0,1,asinh(x * (1 + x / 2) / (1 + x)),dx)","0.386294361"),
            ("integral(0,1,1/(x+0.0001),dx)","9.210440367"),
            ("integral(0,1,log(1/x,e)*asin(x),dx)","0.263943507"),
            ("integral(0,pi,x*(cos(20*x)^2),dx)","2.4674011"),
            ("integral(0.012732395,0.318309886,(sin(1/x)+1+x*cos(1/x))/x,dx)","2.913298334"),
            ("integral(0,pi,(cos(x)^20)*cos(20*x),dx)","2.99606e-06"),
            ("integral(0,pi,exp(10*cos(x))*cos(10*sin(x)),dx)","3.141592654"),
            ("integral(0,16,exp(-x)/sqrt(x),dx)","1.772453824"),
            ("integral(0,1,log(1-(x^2),e)/x/(1+x),dx)","-0.480453014"),
            ("integral(0,1,exp(-(((1/x-1)^2))/2)/(x^2),dx)","1.253314137"),
            ("integral(0,1,log(1/x,e)*(x^(-0.25)),dx)","1.777777778"),
            ("integral(0,1,2*exp(-((x^2))),dx)","1.493648266"),
            ("integral(0,1,log(1+(x^2),e)/x,dx)","0.411233517"),
            ("integral(0.00001,1,exp(-((x^2)))/x,dx)","11.11462567"),
            ("integral(0.00001,1,log(1+exp(-x),e)/(x^1.5),dx)","436.0835418"),
            ("integral(0.00001,1,exp(-((x^2)))/(sin(x)^0.7),dx)","2.919559207"),
            ("integral(0,1,log(1+x+exp(x),e)/((x^0.2)+2),dx)","0.398825195"),
            ("integral(0,pi/2,asin(sqrt(2)/2*sin(x))*sin(x)/sqrt(4-2*(sin(x)^2)),dx)","0.384946473"),
            ("integral(0,1,(x^2)*atan(x),dx)","0.210657251"),
            ("integral(0,pi/2,exp(x)*cos(x),dx)","1.90523869"),
            ("integral(0,1,atan(sqrt(2+(x^2)))/(1+(x^2))/sqrt(2+(x^2)),dx)","0.514041896"),
            ("integral(0,1,sqrt(x)*log(x,e),dx)","-0.444444444"),
            ("integral(0,1,sqrt(1-(x^2)),dx)","0.785398163"),
            ("integral(0,1,sqrt(x)/sqrt(1-(x^2)),dx)","1.198140235"),
            ("integral(0,1,(log(x,e)^2),dx)","2"),
            ("integral(0,pi/2,sqrt(tan(x)),dx)","2.221441469"),
            ("integral(-1,10,log(x+1,e)/((x^2)+1),dx)","0.482392639"),
            ("integral(0,2,sqrt(x*(4-x)),dx)","3.141592654"),
            ("integral(-1,1,sqrt((1-(x^2))*(2-x)),dx)","2.203345732"),
            // ("integral(0.0078125,2.0078125,sqrt(abs(x-1)),dx)","1.331416049"), 1.333078959319974
            ("integral(0,1,(x^(-1.0/4.0))*log(1/x,e),dx)","1.777777778"),
            ("integral(0,3,0.5*(x^(-0.5))*exp(-((x^(0.5)))),dx)","0.823078794"),
            ("integral(0,3,1.5*(x^(0.5))*exp(-((x^(1.5)))),dx)","0.994462169"),
            ("integral(-1,0.5,1/pi/sqrt(1-(x^2)),dx)","0.666666667"),
            ("integral(-1,0.5,2/pi*sqrt(1-(x^2)),dx)","0.804498891"),
            ("integral(-1,1,2/pi/(1+(x^2)),dx)","1"),
            ("integral(0,1,2/pi/(1+(x^2)),dx)","0.5"),
            ("integral(-1,1,1/(x-2),dx)","-1.098612289"),
            ("integral(0,1,1.0/4.0/log(2,e)*log((1+x)/(1-x),e),dx)","0.5"),
            ("integral(0,1,log(1/x,e)/(x^0.25),dx)","1.777777778"),
            ("integral(0,pi/2,sqrt(tan(x)),dx)","2.221441469"),
            ("integral(0.00001,1,log(1+exp(-x),e)/(x^1.5),dx)","436.0835418"),
            ("integral(0,1,log(exp(x)+x+1,e)/((x^0.2)+2),dx)","0.398825195"),
            ("integral(0,4,x/(1+(x^6)*(sin(x)^2)),dx)","0.963568863"),
            ("integral(0,1,(x^-0.12345),dx)","1.140836233"),
            ("integral(0,1,cos(x)*(x^-0.12345),dx)","0.975361987"),
            ("integral(0,1,exp(-1/x-1/(1-x)),dx)","0.007029858"),
            ("integral(-1,1,2*(1-(x^2))/(cos(4*atanh(x))+cosh(2)),dx)","0.711943823"),
            ("integral(0,2,x/(1+(x^6)*(sinh(x)^2)),dx)","0.503134546"),
        ];

        let mut parser = crate::parser::Context::new();
        for i in test_data {
            #[cfg(feature = "rug")]
            let result = match crate::parser::eval(&mut parser, i.0, (2048 as isize) as u32) {
                Ok(Some(result)) => result.get_value(),
                Ok(None) => KalkValue::nan(),
                Err(_err) => KalkValue::nan(),
            };
            #[cfg(not(feature = "rug"))]
            let result = match crate::parser::eval(&mut parser, i.0) {
                Ok(Some(result)) => result.get_value(),
                Ok(None) => KalkValue::nan(),
                Err(_err) => KalkValue::nan(),
            };

            println!(
                "testing: {} {} {}",
                i.0,
                result.to_f64(),
                i.1.parse::<f64>().unwrap()
            );
            assert!(cmp(result.to_f64(), i.1.parse::<f64>().unwrap()));
        }
    }

    #[test]
    fn test_integrate_function() {
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
