use crate::parser::Unit;
use FuncType::*;

pub const DEFAULT_ANGLE_UNIT: Unit = Unit::Radians;
pub const CONSTANTS: &[(&str, &str)] = &[
    ("pi", "3.14159265"),
    ("π", "3.14159265"),
    ("e", "2.71828182"),
    ("tau", "6.28318530"),
    ("τ", "6.28318530"),
    ("phi", "1.61803398"),
    ("ϕ", "1.61803398"),
];

use funcs::*;
pub const UNARY_FUNCS: phf::Map<&'static str, UnaryFuncInfo> = phf::phf_map! {
    "cos" => UnaryFuncInfo(cos, Trig),
    "cosec" => UnaryFuncInfo(cosec, Trig),
    "cosech" => UnaryFuncInfo(cosech, Trig),
    "cosh" => UnaryFuncInfo(cosh, Trig),
    "cot" => UnaryFuncInfo(cot, Trig),
    "coth" => UnaryFuncInfo(coth, Trig),
    "sec" => UnaryFuncInfo(sec, Trig),
    "sech" => UnaryFuncInfo(sech, Trig),
    "sin" => UnaryFuncInfo(sin, Trig),
    "sinh" => UnaryFuncInfo(sinh, Trig),
    "tan" => UnaryFuncInfo(tan, Trig),
    "tanh" => UnaryFuncInfo(tanh, Trig),

    "acos" => UnaryFuncInfo(acos, InverseTrig),
    "acosec" => UnaryFuncInfo(acosec, InverseTrig),
    "acosech" => UnaryFuncInfo(acosech, InverseTrig),
    "acosh" => UnaryFuncInfo(acosh, InverseTrig),
    "acot" => UnaryFuncInfo(acot, InverseTrig),
    "acoth" => UnaryFuncInfo(acoth, InverseTrig),
    "asec" => UnaryFuncInfo(asec, InverseTrig),
    "asech" => UnaryFuncInfo(asech, InverseTrig),
    "asin" => UnaryFuncInfo(asin, InverseTrig),
    "asinh" => UnaryFuncInfo(asinh, InverseTrig),
    "atan" => UnaryFuncInfo(atan, InverseTrig),
    "atanh" => UnaryFuncInfo(atanh, InverseTrig),

    "abs" => UnaryFuncInfo(abs, Other),
    "cbrt" => UnaryFuncInfo(cbrt, Other),
    "ceil" => UnaryFuncInfo(ceil, Other),
    "exp" => UnaryFuncInfo(exp, Other),
    "floor" => UnaryFuncInfo(floor, Other),
    "frac" => UnaryFuncInfo(frac, Other),
    "log" => UnaryFuncInfo(log, Other),
    "ln" => UnaryFuncInfo(ln, Other),
    "round" => UnaryFuncInfo(round, Other),
    "sqrt" => UnaryFuncInfo(sqrt, Other),
    "trunc" => UnaryFuncInfo(trunc, Other),
};
pub const BINARY_FUNCS: phf::Map<&'static str, BinaryFuncInfo> = phf::phf_map! {
    "max" => BinaryFuncInfo(max, Other),
    "min" => BinaryFuncInfo(min, Other),
};

enum FuncType {
    Trig,
    InverseTrig,
    Other,
}

// Unary functions
pub struct UnaryFuncInfo(fn(f64) -> f64, FuncType);

pub struct BinaryFuncInfo(fn(f64, f64) -> f64, FuncType);

impl UnaryFuncInfo {
    fn call(&self, x: f64, angle_unit: &Unit) -> f64 {
        let func = self.0;
        match self.1 {
            FuncType::Trig => func(from_angle_unit(x, angle_unit)),
            FuncType::InverseTrig => to_angle_unit(func(x), angle_unit),
            FuncType::Other => func(x),
        }
    }
}

impl BinaryFuncInfo {
    fn call(&self, x: f64, y: f64, angle_unit: &Unit) -> f64 {
        let func = self.0;
        match self.1 {
            FuncType::Trig => func(
                from_angle_unit(x, angle_unit),
                from_angle_unit(y, angle_unit),
            ),
            FuncType::InverseTrig => to_angle_unit(func(x, y), angle_unit),
            FuncType::Other => func(x, y),
        }
    }
}

pub fn call_unary_func(name: &str, x: f64, angle_unit: &Unit) -> Option<f64> {
    if let Some(func_info) = UNARY_FUNCS.get(name) {
        Some(func_info.call(x, &angle_unit))
    } else {
        None
    }
}

pub fn call_binary_func(name: &str, x: f64, y: f64, angle_unit: &Unit) -> Option<f64> {
    if let Some(func_info) = BINARY_FUNCS.get(name) {
        Some(func_info.call(x, y, angle_unit))
    } else {
        None
    }
}

fn to_angle_unit(x: f64, angle_unit: &Unit) -> f64 {
    match angle_unit {
        Unit::Radians => x,
        Unit::Degrees => x.to_degrees(),
    }
}

fn from_angle_unit(x: f64, angle_unit: &Unit) -> f64 {
    match angle_unit {
        Unit::Radians => x,
        Unit::Degrees => x.to_radians(),
    }
}

mod funcs {
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
        x.cos() / x.sin()
    }

    pub fn coth(x: f64) -> f64 {
        x.cosh() / x.sinh()
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

    pub fn log(x: f64) -> f64 {
        x.log(10f64)
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
