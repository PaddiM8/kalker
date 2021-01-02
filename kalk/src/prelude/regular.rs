use super::*;
use crate::ast::Expr;
use crate::interpreter;

// Unary functions
pub struct UnaryFuncInfo(pub(super) fn(f64) -> f64, pub(super) FuncType);

pub struct BinaryFuncInfo(pub(super) fn(f64, f64) -> f64, pub(super) FuncType);

impl UnaryFuncInfo {
    fn call(&self, context: &mut interpreter::Context, x: f64, angle_unit: &str) -> f64 {
        let func = self.0;
        match self.1 {
            FuncType::Trig => func(from_angle_unit(context, x, angle_unit)),
            FuncType::InverseTrig => to_angle_unit(context, func(x), angle_unit),
            FuncType::Other => func(x),
        }
    }
}

impl BinaryFuncInfo {
    fn call(&self, context: &mut interpreter::Context, x: f64, y: f64, angle_unit: &str) -> f64 {
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
    x: f64,
    angle_unit: &str,
) -> Option<(f64, String)> {
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
    x: f64,
    y: f64,
    angle_unit: &str,
) -> Option<(f64, String)> {
    if let Some((func_info, func_unit)) = BINARY_FUNCS.get(name) {
        Some((
            func_info.call(context, x, y, angle_unit),
            func_unit.to_string(),
        ))
    } else {
        None
    }
}

fn to_angle_unit(context: &mut interpreter::Context, x: f64, angle_unit: &str) -> f64 {
    match angle_unit {
        "rad" => x,
        _ => {
            interpreter::convert_unit(context, &Expr::Literal(x), "rad", angle_unit)
                .unwrap()
                .value
        }
    }
}

fn from_angle_unit(context: &mut interpreter::Context, x: f64, angle_unit: &str) -> f64 {
    match angle_unit {
        "rad" => x,
        _ => {
            interpreter::convert_unit(context, &Expr::Literal(x), angle_unit, "rad")
                .unwrap()
                .value
        }
    }
}

pub mod special_funcs {
    pub fn factorial(x: f64) -> f64 {
        super::funcs::gamma(x) - 1f64
    }
}

pub(super) mod funcs {
    pub fn abs(x: f64) -> f64 {
        x.abs()
    }

    pub fn acos(x: f64) -> f64 {
        x.acos()
    }

    pub fn acosh(x: f64) -> f64 {
        x.acosh()
    }

    pub fn acot(x: f64) -> f64 {
        (1f64 / x).atan()
    }

    pub fn acoth(x: f64) -> f64 {
        (1f64 / x).atanh()
    }

    pub fn acosec(x: f64) -> f64 {
        (1f64 / x).asin()
    }

    pub fn acosech(x: f64) -> f64 {
        (1f64 / x).asinh()
    }

    pub fn asec(x: f64) -> f64 {
        (1f64 / x).acos()
    }

    pub fn asech(x: f64) -> f64 {
        (1f64 / x).acosh()
    }

    pub fn asin(x: f64) -> f64 {
        x.asin()
    }

    pub fn asinh(x: f64) -> f64 {
        x.asinh()
    }

    pub fn atan(x: f64) -> f64 {
        x.atan()
    }

    pub fn atanh(x: f64) -> f64 {
        x.atanh()
    }

    pub fn cbrt(x: f64) -> f64 {
        x.cbrt()
    }

    pub fn ceil(x: f64) -> f64 {
        x.ceil()
    }

    pub fn cos(x: f64) -> f64 {
        x.cos()
    }

    pub fn cosh(x: f64) -> f64 {
        x.cos()
    }

    pub fn cosec(x: f64) -> f64 {
        1f64 / x.sin()
    }

    pub fn cosech(x: f64) -> f64 {
        1f64 / x.sinh()
    }

    pub fn cot(x: f64) -> f64 {
        x.clone().cos() / x.sin()
    }

    pub fn coth(x: f64) -> f64 {
        x.clone().cosh() / x.sinh()
    }

    pub fn exp(x: f64) -> f64 {
        x.exp()
    }

    pub fn floor(x: f64) -> f64 {
        x.floor()
    }

    pub fn frac(x: f64) -> f64 {
        x.fract()
    }

    // Matthias Eiholzer - https://gitlab.com/matthiaseiholzer/mathru/-/tree/master
    pub fn gamma(x: f64) -> f64 {
        let pi = 3.1415926535897932384626433832795028841971693993751058209749445923f64;
        if x == 0f64 {
            return f64::NAN;
        }

        if x < 0.5f64 {
            return pi / gamma((pi * x).sin() * (1f64 - x));
        }

        let t = x + 6.5;
        let x = 0.99999999999980993 + 676.5203681218851 / x - 1259.1392167224028 / (x + 1f64)
            + 771.32342877765313 / (x + 2f64)
            - 176.61502916214059 / (x + 3f64)
            + 12.507343278686905 / (x + 4f64)
            - 0.13857109526572012 / (x + 5f64)
            + 9.9843695780195716e-6 / (x + 6f64)
            + 1.5056327351493116e-7 / (x + 7f64);

        2f64.sqrt() * pi.sqrt() * t.powf(x - 0.5f64) * (-t).exp() * x
    }

    pub fn hyp(x: f64, y: f64) -> f64 {
        x.hypot(y)
    }

    pub fn log(x: f64) -> f64 {
        x.log10()
    }

    pub fn logx(x: f64, y: f64) -> f64 {
        x.log10() / y.log10()
    }

    pub fn ln(x: f64) -> f64 {
        x.ln()
    }

    pub fn max(x: f64, y: f64) -> f64 {
        x.max(y)
    }

    pub fn min(x: f64, y: f64) -> f64 {
        x.min(y)
    }

    pub fn round(x: f64) -> f64 {
        x.round()
    }

    pub fn sec(x: f64) -> f64 {
        1f64 / x.cos()
    }

    pub fn sech(x: f64) -> f64 {
        1f64 / x.cosh()
    }

    pub fn sin(x: f64) -> f64 {
        x.sin()
    }

    pub fn sinh(x: f64) -> f64 {
        x.sinh()
    }

    pub fn sqrt(x: f64) -> f64 {
        x.sqrt()
    }

    pub fn nth_root(x: f64, n: f64) -> f64 {
        x.powf(1f64 / n)
    }

    pub fn tan(x: f64) -> f64 {
        x.tan()
    }

    pub fn tanh(x: f64) -> f64 {
        x.tanh()
    }

    pub fn trunc(x: f64) -> f64 {
        x.trunc()
    }
}
