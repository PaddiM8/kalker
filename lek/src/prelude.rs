use rug::Float;
use FuncType::*;

pub const CONSTANTS: phf::Map<&'static str, &'static str> = phf::phf_map! {
    "pi" => "3.14159265",
    "π" => "3.14159265",
    "e" => "2.71828182",
    "tau" => "6.28318530",
    "τ" => "6.28318530",
    "phi" => "1.61803398",
    "ϕ" => "1.61803398",
};

use crate::parser::Unit;
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
    "hyp" => BinaryFuncInfo(hyp, Other),
    "log" => BinaryFuncInfo(logx, Other),
    "sqrt" => BinaryFuncInfo(nth_sqrt, Other),
};

enum FuncType {
    Trig,
    InverseTrig,
    Other,
}

// Unary functions
pub struct UnaryFuncInfo(fn(Float) -> Float, FuncType);

pub struct BinaryFuncInfo(fn(Float, Float) -> Float, FuncType);

impl UnaryFuncInfo {
    fn call(&self, x: Float, angle_unit: &Unit) -> Float {
        let func = self.0;
        match self.1 {
            FuncType::Trig => func(from_angle_unit(x, angle_unit)),
            FuncType::InverseTrig => to_angle_unit(func(x), angle_unit),
            FuncType::Other => func(x),
        }
    }
}

impl BinaryFuncInfo {
    fn call(&self, x: Float, y: Float, angle_unit: &Unit) -> Float {
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

pub fn call_unary_func(name: &str, x: Float, angle_unit: &Unit) -> Option<Float> {
    if let Some(func_info) = UNARY_FUNCS.get(name) {
        Some(func_info.call(x, &angle_unit))
    } else {
        None
    }
}

pub fn call_binary_func(name: &str, x: Float, y: Float, angle_unit: &Unit) -> Option<Float> {
    if let Some(func_info) = BINARY_FUNCS.get(name) {
        Some(func_info.call(x, y, angle_unit))
    } else {
        None
    }
}

fn to_angle_unit(x: Float, angle_unit: &Unit) -> Float {
    match angle_unit {
        Unit::Radians => x,
        Unit::Degrees => special_funcs::to_degrees(x),
    }
}

fn from_angle_unit(x: Float, angle_unit: &Unit) -> Float {
    match angle_unit {
        Unit::Radians => x,
        Unit::Degrees => special_funcs::to_radians(x),
    }
}

pub mod special_funcs {
    use rug::Float;

    pub fn factorial(x: Float) -> Float {
        ((x + 1) as Float).gamma()
    }

    pub fn to_degrees(x: Float) -> Float {
        Float::with_val(10, x.to_f64().to_degrees())
    }

    pub fn to_radians(x: Float) -> Float {
        Float::with_val(10, x.to_f64().to_radians())
    }
}

mod funcs {
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

    pub fn nth_sqrt(x: Float, n: Float) -> Float {
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
