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
    // Apply a linear change of variables:
    //
    // x = c * t + d
    //
    // where:
    //      c = 0.5 * (b - a)
    //      d = 0.5 * (a + b)

    let a = interpreter::eval_expr(context, a_expr, None)?;
    let b = interpreter::eval_expr(context, b_expr, None)?;

    let c = b
        .clone()
        .sub(context, a.clone())?
        .div(context, KalkValue::from(2))?;

    let d: KalkValue = a
        .clone()
        .add(context, b.clone())?
        .div(context, KalkValue::from(2))?;

    let mut tanhsinhresult = KalkValue::from(0.0);

    for i in 0..100 {
        let param_out: KalkValue = c
            .clone()
            .mul(context, KalkValue::from(ABSCISSAE[i]))?
            .add(context, d.clone())?;
        context.symbol_table.set(Stmt::VarDecl(
            Identifier::from_full_name(integration_variable),
            Box::new(crate::ast::build_literal_ast(&param_out)),
        ));

        let mut out = interpreter::eval_expr(context, expr, None)?;
        if !out.to_float().is_finite() {
            out = KalkValue::from(0.0);
        }
        let param_integral = KalkValue::from(WEIGHTS[i]).mul(context, out)?;
        tanhsinhresult = tanhsinhresult
            .clone()
            .add(context, param_integral.clone())?;
    }

    c.mul(context, tanhsinhresult)
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ABSCISSAE & WEIGHTS
// These are for the tanh-sinh quadrature.
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// Nodes and weights from: https://keisan.casio.com/exec/system/1285151216

/// Abscissae: the nodes for the sum evaluation.
pub const ABSCISSAE: [f64; 100] = [
    -1.0,
    -1.0,
    -1.0,
    -1.0,
    -1.0,
    -1.0,
    -1.0,
    -1.0,
    -1.0,
    -1.0,
    -1.0,
    -1.0,
    -1.0,
    -1.0,
    -1.0,
    -1.0,
    -0.999_999_999_999_999_999_999_999_92,
    -0.999_999_999_999_999_999_999_999_95,
    -0.999_999_999_999_999_999_999_999_98,
    -0.999_999_999_999_999_999_999_999_91,
    -0.999_999_999_999_999_999_999_999_95,
    -0.999_999_999_999_999_999_999_998_77,
    -0.999_999_999_999_999_999_999_103_26,
    -0.999_999_999_999_999_999_703_932_4,
    -0.999_999_999_999_999_950_531_716_2,
    -0.999_999_999_999_995_4,
    -0.999_999_999_999_755_4,
    -0.999_999_999_991_728_4,
    -0.999_999_999_814_670_3,
    -0.999_999_997_111_343_6,
    -0.999_999_967_297_845_9,
    -0.999_999_720_654_543_6,
    -0.999_998_137_805_321,
    -0.999_990_019_143_856_7,
    -0.999_955_840_978_693_4,
    -0.999_834_913_162_265,
    -0.999_467_635_803_999,
    -0.998_491_938_731_602_9,
    -0.996_186_771_585_243_7,
    -0.991_272_560_236_550_9,
    -0.981_701_565_973_876_9,
    -0.964_495_379_931_111_4,
    -0.935_708_688_740_139_8,
    -0.890_613_153_970_636_9,
    -0.824_190_972_468_412_2,
    -0.731_982_954_977_568_5,
    -0.611_228_933_047_550_6,
    -0.462_072_342_445_637_1,
    -0.288_435_163_890_398_6,
    -0.098_120_697_049_078_77,
    0.098_120_697_049_078_75,
    0.288_435_163_890_398_64,
    0.462_072_342_445_637_19,
    0.611_228_933_047_550_7,
    0.731_982_954_977_568_5,
    0.824_190_972_468_412_2,
    0.890_613_153_970_636_9,
    0.935_708_688_740_139_9,
    0.964_495_379_931_111_5,
    0.981_701_565_973_876_9,
    0.991_272_560_236_551,
    0.996_186_771_585_243_7,
    0.998_491_938_731_603,
    0.999_467_635_803_999,
    0.999_834_913_162_265,
    0.999_955_840_978_693_5,
    0.999_990_019_143_856_8,
    0.999_998_137_805_321_1,
    0.999_999_720_654_543_7,
    0.999_999_967_297_846,
    0.999_999_997_111_343_7,
    0.999_999_999_814_670_2,
    0.999_999_999_991_728_4,
    0.999_999_999_999_755_4,
    0.999_999_999_999_995_4,
    0.999_999_999_999_999_950_531_74,
    0.999_999_999_999_999_999_703_94,
    0.999_999_999_999_999_999_999_156,
    0.999_999_999_999_999_999_999_987,
    0.999_999_999_999_999_999_999_985,
    0.999_999_999_999_999_999_999_951,
    0.999_999_999_999_999_999_999_958,
    0.999_999_999_999_999_999_999_955,
    0.999_999_999_999_999_999_999_912,
    1.0,
    1.0,
    1.0,
    1.0,
    1.0,
    1.0,
    1.0,
    1.0,
    1.0,
    1.0,
    1.0,
    1.0,
    1.0,
    1.0,
    1.0,
    1.0,
];

/// Weights for the sum evaluation.
pub const WEIGHTS: [f64; 100] = [
    0.0,
    4.575_617_930_178_338e-295,
    3.316_994_462_570_346e-260,
    1.865_212_622_495_173_5e-229,
    2.479_121_672_052_092e-202,
    2.081_537_448_295_626e-178,
    2.628_202_011_356_882_4e-157,
    1.072_629_885_654_983_3e-138,
    2.779_482_305_261_637e-122,
    8.296_418_844_663_515e-108,
    4.824_667_051_120_648e-95,
    8.690_880_649_537_159e-84,
    7.300_384_623_604_137e-74,
    4.102_662_749_806_205e-65,
    2.120_927_715_027_845e-57,
    1.335_824_993_406_033_2e-50,
    1.313_406_878_320_599_1e-44,
    2.508_807_816_395_078_6e-39,
    1.129_193_867_059_432_8e-34,
    1.419_894_975_881_043_8e-30,
    5.796_754_421_896_154_4e-27,
    8.772_732_913_927_396e-24,
    5.532_447_125_135_289e-21,
    1.612_026_230_404_078_1e-18,
    2.377_241_245_744_043_6e-16,
    1.922_871_877_455_573e-14,
    9.158_786_378_811_089e-13,
    2.735_019_685_607_782_3e-11,
    5.412_036_348_700_474e-10,
    7.452_095_495_199_113e-9,
    7.455_643_353_281_056e-8,
    5.630_935_258_210_419e-7,
    3.320_892_221_887_424e-6,
    1.575_869_255_697_420_2e-5,
    6.178_940_879_197_28e-5,
    2.049_612_222_935_924_2e-4,
    5.873_088_850_232_362e-4,
    0.001_480_856_522_480_776_8,
    0.003_339_108_906_155_36,
    0.006_827_704_416_155_456,
    0.012_809_851_589_261_179,
    0.022_262_908_367_071_66,
    0.036_106_623_280_003_48,
    0.054_935_001_719_810_43,
    0.078_672_104_392_875_16,
    0.106_225_018_644_775_54,
    0.135_274_854_478_869_08,
    0.162_387_107_725_986_52,
    0.183_570_841_955_285,
    0.195_234_233_228_838_65,
    0.195_234_233_228_838_65,
    0.183_570_841_955_285,
    0.162_387_107_725_986_52,
    0.135_274_854_478_869_08,
    0.106_225_018_644_775_54,
    0.078_672_104_392_875_16,
    0.054_935_001_719_810_43,
    0.036_106_623_280_003_48,
    0.022_262_908_367_071_66,
    0.012_809_851_589_261_179,
    0.006_827_704_416_155_456,
    0.003_339_108_906_155_36,
    0.001_480_856_522_480_776_8,
    5.873_088_850_232_362e-4,
    2.049_612_222_935_924_2e-4,
    6.178_940_879_197_28e-5,
    1.575_869_255_697_420_2e-5,
    3.320_892_221_887_424e-6,
    5.630_935_258_210_419e-7,
    7.455_643_353_281_056e-8,
    7.452_095_495_199_113e-9,
    5.412_036_348_700_474e-10,
    2.735_019_685_607_782_3e-11,
    9.158_786_378_811_089e-13,
    1.922_871_877_455_573e-14,
    2.377_241_245_744_043_6e-16,
    1.612_026_230_404_078_1e-18,
    5.532_447_125_135_289e-21,
    8.772_732_913_927_396e-24,
    5.796_754_421_896_154_4e-27,
    1.419_894_975_881_043_8e-30,
    1.129_193_867_059_432_8e-34,
    2.508_807_816_395_078_6e-39,
    1.313_406_878_320_599_1e-44,
    1.335_824_993_406_033_2e-50,
    2.120_927_715_027_845e-57,
    4.102_662_749_806_205e-65,
    7.300_384_623_604_137e-74,
    8.690_880_649_537_159e-84,
    4.824_667_051_120_648e-95,
    8.296_418_844_663_515e-108,
    2.779_482_305_261_637e-122,
    1.072_629_885_654_983_3e-138,
    2.628_202_011_356_882_4e-157,
    2.081_537_448_295_626e-178,
    2.479_121_672_052_092e-202,
    1.865_212_622_495_173_5e-229,
    3.316_994_462_570_346e-260,
    4.575_617_930_178_338e-295,
    0.0,
];

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
    let a = interpreter::eval_expr(context, a_expr, None)?;
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
