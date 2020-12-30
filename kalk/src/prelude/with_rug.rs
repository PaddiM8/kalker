use super::*;
use crate::ast::Expr;
use crate::interpreter;
use rug::Float;

// Unary functions
pub struct UnaryFuncInfo(pub(super) fn(Float) -> Float, pub(super) FuncType);

pub struct BinaryFuncInfo(pub(super) fn(Float, Float) -> Float, pub(super) FuncType);

impl UnaryFuncInfo {
    fn call(&self, context: &mut interpreter::Context, x: Float, angle_unit: &str) -> Float {
        let func = self.0;
        match self.1 {
            FuncType::Trig => func(from_angle_unit(context, x, angle_unit)),
            FuncType::InverseTrig => to_angle_unit(context, func(x), angle_unit),
            FuncType::Other => func(x),
        }
    }
}

impl BinaryFuncInfo {
    fn call(
        &self,
        context: &mut interpreter::Context,
        x: Float,
        y: Float,
        angle_unit: &str,
    ) -> Float {
        let func = self.0;
        match self.1 {
            FuncType::Trig => func(
                from_angle_unit(context, x, angle_unit),
                from_angle_unit(context, y, angle_unit),
            ),
            FuncType::InverseTrig => to_angle_unit(context, func(x, y), angle_unit),
            FuncType::Other => func(x, y),
        }
    }
}

pub fn is_prelude_func(identifier: &str) -> bool {
    identifier == "sum"
        || identifier == "Σ"
        || UNARY_FUNCS.contains_key(identifier)
        || BINARY_FUNCS.contains_key(identifier)
}

pub fn call_unary_func(
    context: &mut interpreter::Context,
    name: &str,
    x: Float,
    angle_unit: &str,
) -> Option<(Float, String)> {
    if let Some((func_info, func_unit)) = UNARY_FUNCS.get(name) {
        Some((
            func_info.call(context, x, &angle_unit),
            func_unit.to_string(),
        ))
    } else {
        None
    }
}

pub fn call_binary_func(
    context: &mut interpreter::Context,
    name: &str,
    x: Float,
    y: Float,
    angle_unit: &str,
) -> Option<(Float, String)> {
    if let Some((func_info, func_unit)) = BINARY_FUNCS.get(name) {
        Some((
            func_info.call(context, x, y, angle_unit),
            func_unit.to_string(),
        ))
    } else {
        None
    }
}

fn to_angle_unit(context: &mut interpreter::Context, x: Float, angle_unit: &str) -> Float {
    match angle_unit {
        "rad" => x,
        _ => {
            interpreter::convert_unit(
                context,
                &Expr::Literal(x.to_f64_round(rug::float::Round::Nearest)),
                "rad",
                angle_unit,
            )
            .unwrap()
            .value
        }
    }
}

fn from_angle_unit(context: &mut interpreter::Context, x: Float, angle_unit: &str) -> Float {
    match angle_unit {
        "rad" => x,
        _ => {
            interpreter::convert_unit(
                context,
                &Expr::Literal(x.to_f64_round(rug::float::Round::Nearest)),
                angle_unit,
                "rad",
            )
            .unwrap()
            .value
        }
    }
}

pub mod special_funcs {
    use rug::Float;

    pub fn factorial(x: Float) -> Float {
        ((x + 1) as Float).gamma()
    }
}

pub(super) mod funcs {
    use rug::ops::Pow;
    use rug::Float;

    pub fn abs(x: Float) -> Float {
        x.abs()
    }

    pub fn acos(x: Float) -> Float {
        x.acos()
    }

    pub fn acosh(x: Float) -> Float {
        x.acosh()
    }

    pub fn acot(x: Float) -> Float {
        (1f64 / x).atan()
    }

    pub fn acoth(x: Float) -> Float {
        (1f64 / x).atanh()
    }

    pub fn acosec(x: Float) -> Float {
        (1f64 / x).asin()
    }

    pub fn acosech(x: Float) -> Float {
        (1f64 / x).asinh()
    }

    pub fn asec(x: Float) -> Float {
        (1f64 / x).acos()
    }

    pub fn asech(x: Float) -> Float {
        (1f64 / x).acosh()
    }

    pub fn asin(x: Float) -> Float {
        x.asin()
    }

    pub fn asinh(x: Float) -> Float {
        x.asinh()
    }

    pub fn atan(x: Float) -> Float {
        x.atan()
    }

    pub fn atanh(x: Float) -> Float {
        x.atanh()
    }

    pub fn cbrt(x: Float) -> Float {
        x.cbrt()
    }

    pub fn ceil(x: Float) -> Float {
        x.ceil()
    }

    pub fn cos(x: Float) -> Float {
        x.cos()
    }

    pub fn cosh(x: Float) -> Float {
        x.cos()
    }

    pub fn cosec(x: Float) -> Float {
        1f64 / x.sin()
    }

    pub fn cosech(x: Float) -> Float {
        1f64 / x.sinh()
    }

    pub fn cot(x: Float) -> Float {
        x.clone().cos() / x.sin()
    }

    pub fn coth(x: Float) -> Float {
        x.clone().cosh() / x.sinh()
    }

    pub fn exp(x: Float) -> Float {
        x.exp()
    }

    pub fn floor(x: Float) -> Float {
        x.floor()
    }

    pub fn frac(x: Float) -> Float {
        x.fract()
    }

    pub fn gamma(x: Float) -> Float {
        x.gamma()
    }

    pub fn hyp(x: Float, y: Float) -> Float {
        x.hypot(&y)
    }

    pub fn log(x: Float) -> Float {
        x.log10()
    }

    pub fn logx(x: Float, y: Float) -> Float {
        x.log10() / y.log10()
    }

    pub fn ln(x: Float) -> Float {
        x.ln()
    }

    pub fn max(x: Float, y: Float) -> Float {
        x.max(&y)
    }

    pub fn min(x: Float, y: Float) -> Float {
        x.min(&y)
    }

    pub fn round(x: Float) -> Float {
        x.round()
    }

    pub fn sec(x: Float) -> Float {
        1f64 / x.cos()
    }

    pub fn sech(x: Float) -> Float {
        1f64 / x.cosh()
    }

    pub fn sin(x: Float) -> Float {
        x.sin()
    }

    pub fn sinh(x: Float) -> Float {
        x.sinh()
    }

    pub fn sqrt(x: Float) -> Float {
        x.sqrt()
    }

    pub fn nth_root(x: Float, n: Float) -> Float {
        x.pow(Float::with_val(1, 1) / n)
    }

    pub fn tan(x: Float) -> Float {
        x.tan()
    }

    pub fn tanh(x: Float) -> Float {
        x.tanh()
    }

    pub fn trunc(x: Float) -> Float {
        x.trunc()
    }
}
