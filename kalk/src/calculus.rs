use crate::ast;
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

    let argument_with_h = ast::build_literal_ast(&argument.clone().add_without_unit(H.into()));
    let argument_without_h = ast::build_literal_ast(&argument.sub_without_unit(H.into()));
    let new_identifier = Identifier::from_name_and_primes(&name.pure_name, name.prime_count - 1);

    let f_x_h = interpreter::eval_fn_call_expr(context, &new_identifier, &[argument_with_h], unit)?;
    let f_x =
        interpreter::eval_fn_call_expr(context, &new_identifier, &[argument_without_h], unit)?;

    Ok(f_x_h
        .sub_without_unit(f_x)
        .div_without_unit((2f64 * H).into())
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
            Box::new(crate::ast::build_literal_ast(&variable_value)),
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

#[cfg(test)]
mod tests {
    use crate::ast;
    use crate::calculus::Identifier;
    use crate::calculus::Stmt;
    use crate::interpreter;
    use crate::kalk_num::KalkNum;
    use crate::lexer::TokenKind::*;
    use crate::symbol_table::SymbolTable;
    use crate::test_helpers::*;

    fn get_context<'a>(symbol_table: &'a mut SymbolTable) -> interpreter::Context<'a> {
        interpreter::Context::new(
            symbol_table,
            "",
            #[cfg(feature = "rug")]
            63u32,
            None,
        )
    }

    fn cmp(x: f64, y: f64) -> bool {
        (x - y).abs() < 0.0001
    }

    #[test]
    fn test_derive_func() {
        let mut symbol_table = SymbolTable::new();
        let mut context = get_context(&mut symbol_table);
        context.symbol_table.insert(Stmt::FnDecl(
            Identifier::from_full_name("f"),
            vec![String::from("x")],
            binary(
                literal(2.5f64),
                Star,
                binary(var("x"), Power, literal(3f64)),
            ),
        ));

        let call = Stmt::Expr(fn_call("f'", vec![*literal(12.3456f64)]));
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
                    literal(1.5f64),
                    Star,
                    binary(var("x"), Power, literal(2f64)),
                ),
                Plus,
                binary(binary(var("x"), Power, literal(2f64)), Star, var("i")),
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
                binary(literal(3f64), Star, var("x")),
                Plus,
                binary(
                    literal(0.5f64),
                    Star,
                    binary(var("x"), Power, literal(3f64)),
                ),
            ),
        ));

        let result = super::derive_func(
            &mut context,
            &Identifier::from_full_name("f'"),
            KalkNum::new_with_imaginary(KalkNum::from(2f64).value, "", KalkNum::from(3f64).value),
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
            &*literal(2f64),
            &*literal(4f64),
            &*binary(var("x"), Star, var("dx")),
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
            &*literal(2f64),
            &*literal(4f64),
            &*var("x"),
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
            &*literal(2f64),
            &ast::build_literal_ast(&KalkNum::new_with_imaginary(
                KalkNum::from(3f64).value,
                "",
                KalkNum::from(4f64).value,
            )),
            &*binary(var("x"), Star, var("i")),
            "x",
        )
        .unwrap();

        assert!(cmp(result.to_f64(), -12f64));
        assert!(cmp(result.imaginary_to_f64(), -5.5f64));
    }
}
