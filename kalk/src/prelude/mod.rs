use crate::kalk_num::KalkNum;
use lazy_static::lazy_static;
use std::collections::HashMap;
use FuncType::*;

#[cfg(feature = "rug")]
pub mod with_rug;
#[cfg(feature = "rug")]
pub use with_rug::funcs::*;
#[cfg(feature = "rug")]
pub use with_rug::*;

#[cfg(not(feature = "rug"))]
pub mod regular;
#[cfg(not(feature = "rug"))]
pub use regular::funcs::*;
#[cfg(not(feature = "rug"))]
pub use regular::*;

use crate::ast::Expr;
use crate::interpreter;
pub use funcs::*;

// `i` is added in the symbol_table module, since for some reason it didn't work here.
pub const INIT: &'static str = "unit deg = (rad*180)/pi";

lazy_static! {
    pub static ref CONSTANTS: HashMap<&'static str, f64> = {
        let mut m = HashMap::new();
        m.insert(
            "pi",
            3.1415926535897932384626433832795028841971693993751058209749445923,
        );
        m.insert(
            "π",
            3.1415926535897932384626433832795028841971693993751058209749445923,
        );
        m.insert(
            "e",
            2.7182818284590452353602874713526624977572470936999595749669676277,
        );
        m.insert(
            "tau",
            6.2831853071795864769252867665590057683943387987502116419498891846,
        );
        m.insert(
            "τ",
            6.2831853071795864769252867665590057683943387987502116419498891846,
        );
        m.insert(
            "phi",
            1.6180339887498948482045868343656381177203091798057628621354486227,
        );
        m.insert(
            "ϕ",
            1.6180339887498948482045868343656381177203091798057628621354486227,
        );
        m
    };
    pub static ref UNARY_FUNCS: HashMap<&'static str, (UnaryFuncInfo, &'static str)> = {
        let mut m = HashMap::new();
        m.insert("cos", (UnaryFuncInfo(cos, Trig), ""));
        m.insert("csc", (UnaryFuncInfo(csc, Trig), ""));
        m.insert("csch", (UnaryFuncInfo(csch, Trig), ""));
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
        m.insert("acsc", (UnaryFuncInfo(acsc, InverseTrig), "rad"));
        m.insert("acsch", (UnaryFuncInfo(acsch, InverseTrig), "rad"));
        m.insert("acosh", (UnaryFuncInfo(acosh, InverseTrig), "rad"));
        m.insert("acot", (UnaryFuncInfo(acot, InverseTrig), "rad"));
        m.insert("acoth", (UnaryFuncInfo(acoth, InverseTrig), "rad"));
        m.insert("asec", (UnaryFuncInfo(asec, InverseTrig), "rad"));
        m.insert("asech", (UnaryFuncInfo(asech, InverseTrig), "rad"));
        m.insert("asin", (UnaryFuncInfo(asin, InverseTrig), "rad"));
        m.insert("asinh", (UnaryFuncInfo(asinh, InverseTrig), "rad"));
        m.insert("atan", (UnaryFuncInfo(atan, InverseTrig), "rad"));
        m.insert("atanh", (UnaryFuncInfo(atanh, InverseTrig), "rad"));

        m.insert("arg", (UnaryFuncInfo(arg, Other), ""));
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
        m.insert("hypot", (BinaryFuncInfo(hypot, Other), ""));
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
pub struct UnaryFuncInfo(fn(KalkNum) -> KalkNum, FuncType);

pub struct BinaryFuncInfo(fn(KalkNum, KalkNum) -> KalkNum, FuncType);

impl UnaryFuncInfo {
    fn call(&self, context: &mut interpreter::Context, x: KalkNum, angle_unit: &str) -> KalkNum {
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
        x: KalkNum,
        y: KalkNum,
        angle_unit: &str,
    ) -> KalkNum {
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
        || identifier == "integrate"
        || identifier == "∫"
        || UNARY_FUNCS.contains_key(identifier)
        || BINARY_FUNCS.contains_key(identifier)
}

pub fn call_unary_func(
    context: &mut interpreter::Context,
    name: &str,
    x: KalkNum,
    angle_unit: &str,
) -> Option<(KalkNum, String)> {
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
    x: KalkNum,
    y: KalkNum,
    angle_unit: &str,
) -> Option<(KalkNum, String)> {
    if let Some((func_info, func_unit)) = BINARY_FUNCS.get(name) {
        Some((
            func_info.call(context, x, y, angle_unit),
            func_unit.to_string(),
        ))
    } else {
        None
    }
}

fn to_angle_unit(context: &mut interpreter::Context, x: KalkNum, angle_unit: &str) -> KalkNum {
    match angle_unit {
        "rad" => x,
        _ => interpreter::convert_unit(context, &Expr::Literal(x.to_f64()), "rad", angle_unit)
            .unwrap(),
    }
}

fn from_angle_unit(context: &mut interpreter::Context, x: KalkNum, angle_unit: &str) -> KalkNum {
    match angle_unit {
        "rad" => x,
        _ => interpreter::convert_unit(context, &Expr::Literal(x.to_f64()), angle_unit, "rad")
            .unwrap(),
    }
}

pub mod funcs {
    #[cfg(not(feature = "rug"))]
    pub use super::regular::funcs::*;
    #[cfg(feature = "rug")]
    pub use super::with_rug::funcs::*;
    use crate::kalk_num::KalkNum;

    pub fn abs(x: KalkNum) -> KalkNum {
        if x.has_imaginary() {
            // |z| = sqrt(a² + b²)
            let a = x.value.clone() * x.value;
            let b = x.imaginary_value.clone() * x.imaginary_value;

            sqrt(KalkNum::new(a + b, &x.unit))
        } else {
            KalkNum::new(x.value.abs(), &x.unit)
        }
    }

    pub fn acos(x: KalkNum) -> KalkNum {
        if x.has_imaginary() {
            // -i * ln(i * sqrt(1 - z²) + z)
            let root =
                sqrt(KalkNum::from(1f64).sub_without_unit(x.clone().mul_without_unit(x.clone())));
            let iroot = multiply_with_i(root.clone());
            let ln = ln(iroot.add_without_unit(x));

            // -iz = -i(a + bi) = b - ai
            KalkNum::new_with_imaginary(ln.imaginary_value, &ln.unit, -ln.value)
        } else {
            KalkNum::new(x.value.acos(), &x.unit)
        }
    }

    pub fn acosh(x: KalkNum) -> KalkNum {
        if x.has_imaginary() {
            let sqrt1 = sqrt(KalkNum::new_with_imaginary(
                x.value.clone() + 1f64,
                &x.unit,
                x.imaginary_value.clone(),
            ));
            let sqrt2 = sqrt(KalkNum::new_with_imaginary(
                x.value.clone() - 1f64,
                &x.unit,
                x.imaginary_value.clone(),
            ));

            ln(x.add_without_unit(sqrt1.mul_without_unit(sqrt2)))
        } else {
            KalkNum::new(x.value.acosh(), &x.unit)
        }
    }

    pub fn acot(x: KalkNum) -> KalkNum {
        if x.has_imaginary() {
            // atan(1/z)
            atan(KalkNum::from(1f64).div_without_unit(x))
        } else {
            KalkNum::new((1f64 / x.value).atan(), &x.unit)
        }
    }

    pub fn acoth(x: KalkNum) -> KalkNum {
        if x.has_imaginary() {
            // 1 / z
            let inv_x = KalkNum::from(1f64).div_without_unit(x);
            let ln1 = ln(KalkNum::new_with_imaginary(
                1f64 + inv_x.value.clone(),
                &inv_x.unit,
                inv_x.imaginary_value.clone(),
            ));
            let ln2 = ln(KalkNum::new_with_imaginary(
                1f64 - inv_x.value,
                &inv_x.unit,
                -inv_x.imaginary_value,
            ));

            ln1.sub_without_unit(ln2)
                .div_without_unit(KalkNum::from(2f64))
        } else {
            KalkNum::new((1f64 / x.value).atanh(), &x.unit)
        }
    }

    pub fn acsc(x: KalkNum) -> KalkNum {
        if x.has_imaginary() {
            // asin(1/z)
            asin(KalkNum::from(1f64).div_without_unit(x))
        } else {
            KalkNum::new((1f64 / x.value).asin(), &x.unit)
        }
    }

    pub fn acsch(x: KalkNum) -> KalkNum {
        if x.has_imaginary() {
            let inv_x2 =
                KalkNum::from(1f64).div_without_unit(x.clone().mul_without_unit(x.clone()));
            let sqrt = sqrt(KalkNum::new_with_imaginary(
                1f64 + inv_x2.value,
                &inv_x2.unit,
                inv_x2.imaginary_value,
            ));
            let inv_x = KalkNum::from(1f64).div_without_unit(x.clone());

            ln(sqrt.add_without_unit(inv_x))
        } else {
            KalkNum::new((1f64 / x.value).asinh(), &x.unit)
        }
    }

    pub fn asec(x: KalkNum) -> KalkNum {
        if x.has_imaginary() {
            // acos(1/z)
            acos(KalkNum::from(1f64).div_without_unit(x))
        } else {
            KalkNum::new((1f64 / x.value).acos(), &x.unit)
        }
    }

    pub fn asech(x: KalkNum) -> KalkNum {
        if x.has_imaginary() {
            // 1/z
            let inv_x = KalkNum::from(1f64).div_without_unit(x.clone());
            // sqrt(1/z - 1)
            let sqrt1 = sqrt(KalkNum::new_with_imaginary(
                inv_x.value.clone() - 1f64,
                &inv_x.unit,
                inv_x.imaginary_value.clone(),
            ));
            // sqrt(1/z + 1)
            let sqrt2 = sqrt(KalkNum::new_with_imaginary(
                inv_x.value.clone() + 1f64,
                &inv_x.unit,
                inv_x.imaginary_value.clone(),
            ));

            // ln(1/z + sqrt(1/z - 1) * sqrt(1/z + 1))
            ln(sqrt1.mul_without_unit(sqrt2).add_without_unit(inv_x))
        } else {
            KalkNum::new((1f64 / x.value).acosh(), &x.unit)
        }
    }

    pub fn asin(x: KalkNum) -> KalkNum {
        if x.has_imaginary() {
            // i * ln(sqrt(1 - z²) - iz)
            let root =
                sqrt(KalkNum::from(1f64).sub_without_unit(x.clone().mul_without_unit(x.clone())));
            let iz = multiply_with_i(x.clone());
            let ln = ln(root.sub_without_unit(iz));
            multiply_with_i(ln)
        } else {
            KalkNum::new(x.value.asin(), &x.unit)
        }
    }

    pub fn asinh(x: KalkNum) -> KalkNum {
        if x.has_imaginary() {
            let x2 = x.clone().mul_without_unit(x.clone());
            let sqrt = sqrt(KalkNum::new_with_imaginary(
                x2.value + 1f64,
                &x2.unit,
                x2.imaginary_value,
            ));

            ln(x.add_without_unit(sqrt))
        } else {
            KalkNum::new(x.value.asinh(), &x.unit)
        }
    }

    pub fn atan(x: KalkNum) -> KalkNum {
        if x.has_imaginary() {
            let iz = multiply_with_i(x);
            // 1 + iz
            let numerator = KalkNum::new_with_imaginary(
                1f64 + iz.value.clone(),
                &iz.unit,
                iz.imaginary_value.clone(),
            );
            // 1 - iz
            let denominator =
                KalkNum::new_with_imaginary(1f64 - iz.value, &iz.unit, -iz.imaginary_value);
            let ln = ln(numerator.div_without_unit(denominator));

            // -0.5iz = -0.5i(a + bi) = b/2 - ai/2
            multiply_with_i(ln).div_without_unit(KalkNum::from(-2f64))
        } else {
            KalkNum::new(x.value.atan(), &x.unit)
        }
    }

    pub fn atanh(x: KalkNum) -> KalkNum {
        if x.has_imaginary() {
            // 1/2 * log(z + 1) - 1/2 * log(-z + 1)
            let log1 = ln(KalkNum::new_with_imaginary(
                1f64 + x.value.clone(),
                &x.unit,
                x.imaginary_value.clone(),
            ));
            let log2 = ln(KalkNum::new_with_imaginary(
                1f64 - x.value,
                &x.unit,
                -x.imaginary_value,
            ));

            log1.sub_without_unit(log2)
                .div_without_unit(KalkNum::from(2f64))
        } else {
            KalkNum::new(x.value.atanh(), &x.unit)
        }
    }

    pub fn cbrt(x: KalkNum) -> KalkNum {
        KalkNum::new(x.value.cbrt(), &x.unit)
    }

    pub fn ceil(x: KalkNum) -> KalkNum {
        KalkNum::new_with_imaginary(x.value.ceil(), &x.unit, x.imaginary_value.ceil())
    }

    pub fn cos(x: KalkNum) -> KalkNum {
        KalkNum::new_with_imaginary(
            x.value.clone().cos() * x.imaginary_value.clone().cosh(),
            &x.unit,
            -x.value.sin() * x.imaginary_value.sinh(),
        )
    }

    pub fn cosh(x: KalkNum) -> KalkNum {
        KalkNum::new_with_imaginary(
            x.value.clone().cosh() * x.imaginary_value.clone().cos(),
            &x.unit,
            x.value.sinh() * x.imaginary_value.sin(),
        )
    }

    pub fn csc(x: KalkNum) -> KalkNum {
        KalkNum::from(1f64).div_without_unit(sin(x))
    }

    pub fn csch(x: KalkNum) -> KalkNum {
        KalkNum::from(1f64).div_without_unit(sinh(x))
    }

    pub fn cot(x: KalkNum) -> KalkNum {
        let a = x.value * 2f64;
        let b = x.imaginary_value * 2f64;
        KalkNum::new_with_imaginary(
            -a.clone().sin() / (a.clone().cos() - b.clone().cosh()),
            &x.unit,
            b.clone().sinh() / (a.cos() - b.cosh()),
        )
    }

    pub fn coth(x: KalkNum) -> KalkNum {
        let a = x.value * 2f64;
        let b = x.imaginary_value * 2f64;
        KalkNum::new_with_imaginary(
            -a.clone().sinh() / (b.clone().cos() - a.clone().cosh()),
            &x.unit,
            b.clone().sin() / (b.cos() - a.cosh()),
        )
    }

    pub fn exp(x: KalkNum) -> KalkNum {
        if x.has_imaginary() {
            // e^a*cos(b) + ie^a*sin(b)
            let exp_a = x.value.exp();
            let b = x.imaginary_value;
            KalkNum::new_with_imaginary(exp_a.clone() * b.clone().cos(), &x.unit, exp_a * b.sin())
        } else {
            KalkNum::new(x.value.exp(), &x.unit)
        }
    }

    pub fn floor(x: KalkNum) -> KalkNum {
        KalkNum::new_with_imaginary(x.value.floor(), &x.unit, x.imaginary_value.floor())
    }

    pub fn frac(x: KalkNum) -> KalkNum {
        KalkNum::new_with_imaginary(x.value.fract(), &x.unit, x.imaginary_value.fract())
    }

    pub fn log(x: KalkNum) -> KalkNum {
        if x.has_imaginary() {
            // ln(z) / ln(10)
            ln(x).div_without_unit(KalkNum::from(10f64.ln()))
        } else {
            KalkNum::new(x.value.log10(), &x.unit)
        }
    }

    pub fn logx(x: KalkNum, y: KalkNum) -> KalkNum {
        if x.has_imaginary() || y.has_imaginary() {
            // ln(z) / ln(n)
            ln(x).div_without_unit(ln(y))
        } else {
            KalkNum::new(x.value.log10() / y.value.log10(), &x.unit)
        }
    }

    pub fn ln(x: KalkNum) -> KalkNum {
        if x.has_imaginary() {
            let r = abs(x.clone());
            // ln|z| + i * arg z
            ln(r).add_without_unit(multiply_with_i(arg(x)))
        } else {
            KalkNum::new(x.value.ln(), &x.unit)
        }
    }

    pub fn nth_root(x: KalkNum, n: KalkNum) -> KalkNum {
        x.pow_without_unit(KalkNum::from(1f64).div_without_unit(n))
    }

    pub fn round(x: KalkNum) -> KalkNum {
        KalkNum::new_with_imaginary(x.value.round(), &x.unit, x.imaginary_value.round())
    }

    pub fn sec(x: KalkNum) -> KalkNum {
        KalkNum::from(1f64).div_without_unit(cos(x))
    }

    pub fn sech(x: KalkNum) -> KalkNum {
        KalkNum::from(1f64).div_without_unit(cosh(x))
    }

    pub fn sin(x: KalkNum) -> KalkNum {
        KalkNum::new_with_imaginary(
            x.value.clone().sin() * x.imaginary_value.clone().cosh(),
            &x.unit,
            x.value.cos() * x.imaginary_value.sinh(),
        )
    }

    pub fn sinh(x: KalkNum) -> KalkNum {
        KalkNum::new_with_imaginary(
            x.value.clone().sinh() * x.imaginary_value.clone().cos(),
            &x.unit,
            x.value.cosh() * x.imaginary_value.sin(),
        )
    }

    pub fn sqrt(x: KalkNum) -> KalkNum {
        if x.has_imaginary() {
            let abs = abs(x.clone());
            let r = abs.value;
            let a = x.value;
            let b = x.imaginary_value;

            // sqrt((|z| + a) / 2) + i * (b / |b|) * sqrt((|z| - a) / 2)
            KalkNum::new_with_imaginary(
                ((r.clone() + a.clone()) / 2f64).sqrt(),
                &abs.unit,
                (b.clone() / b.abs()) * ((r - a) / 2f64).sqrt(),
            )
        } else {
            KalkNum::new(x.value.sqrt(), &x.unit)
        }
    }

    pub fn tan(x: KalkNum) -> KalkNum {
        if x.has_imaginary() {
            let a = x.value * 2f64;
            let b = x.imaginary_value * 2f64;
            KalkNum::new_with_imaginary(
                a.clone().sin() / (a.clone().cos() + b.clone().cosh()),
                &x.unit,
                b.clone().sinh() / (a.cos() + b.cosh()),
            )
        } else {
            KalkNum::new(x.value.tan(), &x.unit)
        }
    }

    pub fn tanh(x: KalkNum) -> KalkNum {
        if x.has_imaginary() {
            let a = x.value * 2f64;
            let b = x.imaginary_value * 2f64;
            KalkNum::new_with_imaginary(
                a.clone().sinh() / (a.clone().cosh() + b.clone().cos()),
                &x.unit,
                b.clone().sin() / (a.cosh() + b.cos()),
            )
        } else {
            KalkNum::new(x.value.tanh(), &x.unit)
        }
    }

    pub fn trunc(x: KalkNum) -> KalkNum {
        KalkNum::new_with_imaginary(x.value.trunc(), &x.unit, x.imaginary_value.trunc())
    }

    fn multiply_with_i(z: KalkNum) -> KalkNum {
        // iz = i(a + bi) = -b + ai
        KalkNum::new_with_imaginary(-z.imaginary_value, &z.unit, z.value)
    }
}
