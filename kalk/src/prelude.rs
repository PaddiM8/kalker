use crate::ast::Expr;
use crate::interpreter;
use funcs::*;
use lazy_static::lazy_static;
use rug::Float;
use std::collections::HashMap;
use FuncType::*;

pub const INIT: &'static str = "unit deg = (rad*180)/pi";

lazy_static! {
    pub static ref CONSTANTS: HashMap<&'static str, f64> = {
        let mut m = HashMap::new();
        m.insert("pi", 3.14159265);
        m.insert("π", 3.14159265);
        m.insert("e", 2.71828182);
        m.insert("tau", 6.28318530);
        m.insert("τ", 6.28318530);
        m.insert("phi", 1.61803398);
        m.insert("ϕ", 1.61803398);
        m
    };
    pub static ref UNARY_FUNCS: HashMap<&'static str, (UnaryFuncInfo, &'static str)> = {
        let mut m = HashMap::new();
        m.insert("cos", (UnaryFuncInfo(cos, Trig), ""));
        m.insert("cosec", (UnaryFuncInfo(cosec, Trig), ""));
        m.insert("cosech", (UnaryFuncInfo(cosech, Trig), ""));
        m.insert("cosh", (UnaryFuncInfo(cosh, Trig), ""));
        m.insert("cot", (UnaryFuncInfo(cot, Trig), ""));
        m.insert("coth", (UnaryFuncInfo(coth, Trig), ""));
        m.insert("sec", (UnaryFuncInfo(sec, Trig), ""));
        m.insert("sech", (UnaryFuncInfo(sech, Trig), ""));
        m.insert("sin", (UnaryFuncInfo(sin, Trig), ""));
        m.insert("sinh", (UnaryFuncInfo(sinh, Trig), ""));
        m.insert("tan", (UnaryFuncInfo(tan, Trig), ""));
        m.insert("tanh", (UnaryFuncInfo(tanh, Trig), ""));

        m.insert("acos", (UnaryFuncInfo(acos, InverseTrig), "rad"));
        m.insert("acosec", (UnaryFuncInfo(acosec, InverseTrig), "rad"));
        m.insert("acosech", (UnaryFuncInfo(acosech, InverseTrig), "rad"));
        m.insert("acosh", (UnaryFuncInfo(acosh, InverseTrig), "rad"));
        m.insert("acot", (UnaryFuncInfo(acot, InverseTrig), "rad"));
        m.insert("acoth", (UnaryFuncInfo(acoth, InverseTrig), "rad"));
        m.insert("asec", (UnaryFuncInfo(asec, InverseTrig), "rad"));
        m.insert("asech", (UnaryFuncInfo(asech, InverseTrig), "rad"));
        m.insert("asin", (UnaryFuncInfo(asin, InverseTrig), "rad"));
        m.insert("asinh", (UnaryFuncInfo(asinh, InverseTrig), "rad"));
        m.insert("atan", (UnaryFuncInfo(atan, InverseTrig), "rad"));
        m.insert("atanh", (UnaryFuncInfo(atanh, InverseTrig), "rad"));

        m.insert("abs", (UnaryFuncInfo(abs, Other), ""));
        m.insert("cbrt", (UnaryFuncInfo(cbrt, Other), ""));
        m.insert("ceil", (UnaryFuncInfo(ceil, Other), ""));
        m.insert("exp", (UnaryFuncInfo(exp, Other), ""));
        m.insert("floor", (UnaryFuncInfo(floor, Other), ""));
        m.insert("frac", (UnaryFuncInfo(frac, Other), ""));
        m.insert("gamma", (UnaryFuncInfo(gamma, Other), ""));
        m.insert("Γ", (UnaryFuncInfo(gamma, Other), ""));
        m.insert("log", (UnaryFuncInfo(log, Other), ""));
        m.insert("ln", (UnaryFuncInfo(ln, Other), ""));
        m.insert("round", (UnaryFuncInfo(round, Other), ""));
        m.insert("sqrt", (UnaryFuncInfo(sqrt, Other), ""));
        m.insert("√", (UnaryFuncInfo(sqrt, Other), ""));
        m.insert("trunc", (UnaryFuncInfo(trunc, Other), ""));
        m
    };
    pub static ref BINARY_FUNCS: HashMap<&'static str, (BinaryFuncInfo, &'static str)> = {
        let mut m = HashMap::new();
        m.insert("max", (BinaryFuncInfo(max, Other), ""));
        m.insert("min", (BinaryFuncInfo(min, Other), ""));
        m.insert("hyp", (BinaryFuncInfo(hyp, Other), ""));
        m.insert("log", (BinaryFuncInfo(logx, Other), ""));
        m.insert("root", (BinaryFuncInfo(nth_root, Other), ""));
        m
    };
}

enum FuncType {
    Trig,
    InverseTrig,
    Other,
}

// Unary functions
pub struct UnaryFuncInfo(fn(Float) -> Float, FuncType);

pub struct BinaryFuncInfo(fn(Float, Float) -> Float, FuncType);

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
