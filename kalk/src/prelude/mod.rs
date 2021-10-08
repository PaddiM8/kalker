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
            "e",
            2.7182818284590452353602874713526624977572470936999595749669676277,
        );
        m.insert(
            "tau",
            6.2831853071795864769252867665590057683943387987502116419498891846,
        );
        m.insert(
            "phi",
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
        m.insert("iverson", (UnaryFuncInfo(inverson, Other), ""));
        m.insert("exp", (UnaryFuncInfo(exp, Other), ""));
        m.insert("floor", (UnaryFuncInfo(floor, Other), ""));
        m.insert("frac", (UnaryFuncInfo(frac, Other), ""));
        m.insert("Im", (UnaryFuncInfo(im, Other), ""));
        m.insert("gamma", (UnaryFuncInfo(gamma, Other), ""));
        m.insert("Γ", (UnaryFuncInfo(gamma, Other), ""));
        m.insert("log", (UnaryFuncInfo(log, Other), ""));
        m.insert("ln", (UnaryFuncInfo(ln, Other), ""));
        m.insert("Re", (UnaryFuncInfo(re, Other), ""));
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
        m.insert("gcd", (BinaryFuncInfo(gcd, Other), ""));
        m.insert("lcm", (BinaryFuncInfo(lcm, Other), ""));
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
        || identifier == "∑"
        || identifier == "prod"
        || identifier == "∏"
        || identifier == "integrate"
        || identifier == "integral"
        || identifier == "∫"
        || UNARY_FUNCS.contains_key(identifier)
        || BINARY_FUNCS.contains_key(identifier)
}

pub fn is_constant(identifier: &str) -> bool {
    CONSTANTS.contains_key(identifier)
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
        if x.has_imaginary() || x.value > 1f64 || x.value < -1f64 {
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
        if x.has_imaginary() || x.value < 1f64 {
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
        if x.has_imaginary() || x.value <= 1f64 || x.value >= -1f64 {
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
        if x.has_imaginary() || x.value < 1f64 || x.value > -1f64 {
            // asin(1/z)
            asin(KalkNum::from(1f64).div_without_unit(x))
        } else {
            KalkNum::new((1f64 / x.value).asin(), &x.unit)
        }
    }

    pub fn acsch(x: KalkNum) -> KalkNum {
        if x.has_imaginary() || x.value == 0f64 {
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
        if x.has_imaginary() || x.value < 1f64 || x.value > -1f64 {
            // acos(1/z)
            acos(KalkNum::from(1f64).div_without_unit(x))
        } else {
            KalkNum::new((1f64 / x.value).acos(), &x.unit)
        }
    }

    pub fn asech(x: KalkNum) -> KalkNum {
        if x.has_imaginary() || x.value <= 0f64 || x.value > 1f64 {
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
        if x.has_imaginary() || x.value > 1f64 || x.value < -1f64 {
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

            // 1 - iz
            let neg = KalkNum::new_with_imaginary(
                1f64 - iz.value.clone(),
                &iz.unit,
                -iz.imaginary_value.clone(),
            );

            // 1 + iz
            let pos = KalkNum::new_with_imaginary(1f64 + iz.value, &iz.unit, iz.imaginary_value);

            // ln(1 - iz) - ln(1 + iz)
            let ln = ln(neg).sub_without_unit(ln(pos));

            multiply_with_i(ln).div_without_unit(KalkNum::from(2f64))
        } else {
            KalkNum::new(x.value.atan(), &x.unit)
        }
    }

    pub fn atanh(x: KalkNum) -> KalkNum {
        if x.has_imaginary() || x.value >= 1f64 || x.value <= -1f64 {
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

    pub fn gcd(x: KalkNum, y: KalkNum) -> KalkNum {
        // Find the norm of a Gaussian integer
        fn norm(x: KalkNum) -> KalkNum {
            KalkNum::new((x.value.clone() * x.value) + (x.imaginary_value.clone() * x.imaginary_value), &x.unit)
        }

        if x.has_imaginary() || y.has_imaginary() {
            if x.value.clone().fract() != 0f64 || y.value.clone().fract() != 0f64
            || x.imaginary_value.clone().fract() != 0f64 || y.imaginary_value.clone().fract() != 0f64 {
                // Not a Gaussian integer!
                // TODO: throw an actual error instead of returning NaN
                return KalkNum::from(f64::NAN);
            }

            // Partially derived from:
            // https://stackoverflow.com/a/52692832

            let a;
            let b;

            // Ensure a > b
            if norm(x.clone()).value < norm(y.clone()).value {
                a = y;
                b = x;
            } else {
                a = x;
                b = y;
            }

            let mut c = a.clone().div_without_unit(b.clone());
            if c.imaginary_value.clone().fract() == 0f64 {
                KalkNum::new_with_imaginary(b.value.abs(), &b.unit, b.imaginary_value)
            } else {
                c.value = c.value.round();
                c.imaginary_value = c.imaginary_value.round();
                gcd(a.sub_without_unit(b.clone().mul_without_unit(c)), b)
            }
        } else {
            if x.value < 0f64 || y.value < 0f64 {
                return gcd(KalkNum::new(x.value.abs(), &x.unit), KalkNum::new(y.value.abs(), &y.unit));
            }

            // Euclidean GCD algorithm, but with modulus
            let mut x_a = x.clone();
            let mut y_a = y.clone();
            while !y_a.value.eq(&0f64) {
                let t = y_a.value.clone();
                y_a.value = x_a.value % y_a.value;
                x_a.value = t;
            }

            // Usually we'd need to return max(x, -x), but since we've handled negative
            // values above, that is unnecessary.
            x_a
        }
    }

    pub fn im(x: KalkNum) -> KalkNum {
        KalkNum::new_with_imaginary(x.value, "", KalkNum::default().value)
    }

    pub fn inverson(x: KalkNum) -> KalkNum {
        KalkNum::from(if let Some(boolean_value) = x.boolean_value {
            if boolean_value {
                1
            } else {
                0
            }
        } else {
            1
        })
    }

    //              ⎛           ⎞
    //              ⎜    ⎜a⎜    ⎟
    // lcm(a, b) =  ⎜ ───────── ⎟ × ⎜b⎜
    //              ⎜ gcd(a, b) ⎟
    //              ⎝           ⎠
    pub fn lcm(x: KalkNum, y: KalkNum) -> KalkNum {
        let gcd = gcd(x.clone(), y.clone());
        let absx = KalkNum::new_with_imaginary(x.value.abs(), &x.unit, x.imaginary_value);
        let absy = KalkNum::new_with_imaginary(y.value.abs(), &y.unit, y.imaginary_value);
        return absx.div_without_unit(gcd).mul_without_unit(absy);
    }

    pub fn log(x: KalkNum) -> KalkNum {
        if x.has_imaginary() || x.value < 0f64 {
            // ln(z) / ln(10)
            ln(x).div_without_unit(KalkNum::from(10f64.ln()))
        } else {
            KalkNum::new(x.value.log10(), &x.unit)
        }
    }

    pub fn logx(x: KalkNum, y: KalkNum) -> KalkNum {
        if x.has_imaginary() || y.has_imaginary() || x.value < 0f64 || y.value < 0f64 {
            // ln(z) / ln(n)
            ln(x).div_without_unit(ln(y))
        } else {
            KalkNum::new(x.value.log10() / y.value.log10(), &x.unit)
        }
    }

    pub fn ln(x: KalkNum) -> KalkNum {
        if x.has_imaginary() || x.value < 0f64 {
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

    pub fn re(x: KalkNum) -> KalkNum {
        KalkNum::new_with_imaginary(KalkNum::default().value, "", x.imaginary_value)
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
        } else if x.value < 0f64 {
            KalkNum::from_imaginary(x.value.abs().sqrt())
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

#[cfg(test)]
mod tests {
    use super::funcs::*;
    use crate::prelude::KalkNum;
    use crate::test_helpers::cmp;

    #[test]
    fn test_unary_funcs() {
        let in_out = vec![
            (abs as fn(KalkNum) -> KalkNum, (3f64, 4f64), (5f64, 0f64)),
            (abs, (-3f64, 4f64), (5f64, 0f64)),
            (abs, (3f64, -4f64), (5f64, 0f64)),
            (abs, (-3f64, 0f64), (3f64, 0f64)),
        ];

        for (i, (func, input, expected_output)) in in_out.iter().enumerate() {
            let actual_output = func(KalkNum::new_with_imaginary(
                KalkNum::from(input.0).value,
                "",
                KalkNum::from(input.1).value,
            ));

            println!(
                "{} | expected: {}, {}",
                i, expected_output.0, expected_output.1
            );
            println!(
                "{} | got: {}, {}",
                i,
                actual_output.to_f64(),
                actual_output.imaginary_to_f64()
            );
            assert!(cmp(expected_output.0, actual_output.to_f64()));
            assert!(cmp(expected_output.1, actual_output.imaginary_to_f64()));
        }
    }

    #[test]
    fn test_binary_funcs() {
        let in_out = vec![
            (gcd as fn(KalkNum, KalkNum) -> KalkNum, ((12f64,  0f64), (18f64,  0f64)), ( 6f64,  0f64)),
            (gcd, ((30f64,  0f64), (18f64,  0f64)), ( 6f64,  0f64)),
            (gcd, (( 5f64,  0f64), ( 2f64,  1f64)), ( 2f64,  1f64)),
            (gcd, ((18f64,  4f64), (30f64,  0f64)), ( 4f64,  2f64)),
            (gcd, (( 3f64,  1f64), ( 1f64, -1f64)), ( 1f64, -1f64)),
            (gcd, ((12f64, -8f64), ( 6f64,  4f64)), ( 2f64,  0f64)),
            (lcm, ((12f64, -8f64), ( 6f64,  4f64)), (52f64,  0f64)),
            (lcm, (( 1f64, -2f64), ( 3f64,  1f64)), ( 5f64, -5f64)),
        ];

        for (i, (func, input, expected_output)) in in_out.iter().enumerate() {
            let actual_output = func(
                KalkNum::new_with_imaginary(KalkNum::from(input.0.0).value, "", KalkNum::from(input.0.1).value),
                KalkNum::new_with_imaginary(KalkNum::from(input.1.0).value, "", KalkNum::from(input.1.1).value),
            );

            println!(
                "{} | expected: {}, {}",
                i, expected_output.0, expected_output.1
            );
            println!(
                "{} | got: {}, {}",
                i,
                actual_output.to_f64(),
                actual_output.imaginary_to_f64()
            );
            assert!(cmp(expected_output.0, actual_output.to_f64()));
            assert!(cmp(expected_output.1, actual_output.imaginary_to_f64()));
        }
    }

    #[test]
    fn test_trig_funcs() {
        // Auto-generated using kalk/scripts/generate_funcs_test_cases.py
        let in_out = vec![
            (
                arg as fn(KalkNum) -> KalkNum,
                (0.3f64, 0f64),
                (0.0f64, 0.0f64),
            ),
            (arg, (0f64, 0.0f64), (0.0f64, 0.0f64)),
            (arg, (-0.1f64, -0.5f64), (-1.7681919f64, 0.0f64)),
            (arg, (0.6f64, -0.5f64), (-0.6947383f64, 0.0f64)),
            (arg, (2.8f64, 0f64), (0.0f64, 0.0f64)),
            (arg, (-2.6f64, 0f64), (3.1415927f64, 0.0f64)),
            (arg, (0f64, 0f64), (0.0f64, 0.0f64)),
            (ceil, (0.2f64, 0f64), (1.0f64, 0.0f64)),
            (ceil, (0f64, -0.7f64), (0.0f64, 0.0f64)),
            (ceil, (-0.8f64, -0.3f64), (0.0f64, 0.0f64)),
            (ceil, (0.5f64, -0.6f64), (1.0f64, 0.0f64)),
            (ceil, (2.2f64, 0f64), (3.0f64, 0.0f64)),
            (ceil, (-2.7f64, 0f64), (-2.0f64, 0.0f64)),
            (ceil, (0f64, 0f64), (0.0f64, 0.0f64)),
            (exp, (-0.3f64, 0f64), (0.7408182f64, 0.0f64)),
            (exp, (0f64, -1.0f64), (0.5403023f64, -0.841471f64)),
            (exp, (0.5f64, 0.8f64), (1.1486752f64, 1.1827202f64)),
            (exp, (-0.8f64, 0.5f64), (0.3943233f64, 0.2154198f64)),
            (exp, (2.9f64, 0f64), (18.1741454f64, 0.0f64)),
            (exp, (-2.0f64, 0f64), (0.1353353f64, 0.0f64)),
            (exp, (0f64, 0f64), (1.0f64, 0.0f64)),
            (floor, (-0.8f64, 0f64), (-1.0f64, 0.0f64)),
            (floor, (0f64, 0.4f64), (0.0f64, 0.0f64)),
            (floor, (0.1f64, 0.8f64), (0.0f64, 0.0f64)),
            (floor, (0.6f64, -0.4f64), (0.0f64, -1.0f64)),
            (floor, (1.8f64, 0f64), (1.0f64, 0.0f64)),
            (floor, (-1.8f64, 0f64), (-2.0f64, 0.0f64)),
            (floor, (0f64, 0f64), (0.0f64, 0.0f64)),
            (log, (0.4f64, 0f64), (-0.39794f64, 0.0f64)),
            (log, (0f64, -0.6f64), (-0.2218487f64, -0.6821882f64)),
            (log, (0.6f64, 0.1f64), (-0.2158991f64, 0.0717232f64)),
            (log, (-0.7f64, -0.8f64), (0.0265392f64, -0.9943721f64)),
            (log, (2.2f64, 0f64), (0.3424227f64, 0.0f64)),
            (log, (-2.9f64, 0f64), (0.462398f64, 1.3643764f64)),
            (log, (0f64, 0f64), (f64::NEG_INFINITY, 0.0f64)),
            (ln, (0.3f64, 0f64), (-1.2039728f64, 0.0f64)),
            (ln, (0f64, 0.0f64), (f64::NEG_INFINITY, 0.0f64)),
            (ln, (-1.0f64, 0.8f64), (0.2473481f64, 2.4668517f64)),
            (ln, (-0.2f64, 1.0f64), (0.0196104f64, 1.7681919f64)),
            (ln, (1.5f64, 0f64), (0.4054651f64, 0.0f64)),
            (ln, (-2.4f64, 0f64), (0.8754687f64, 3.1415927f64)),
            (ln, (0f64, 0f64), (f64::NEG_INFINITY, 0.0f64)),
            (sqrt, (-0.9f64, 0f64), (0.0f64, 0.9486833f64)),
            (sqrt, (0f64, 0.3f64), (0.3872983f64, 0.3872983f64)),
            (sqrt, (0.6f64, -0.2f64), (0.7850018f64, -0.1273882f64)),
            (sqrt, (0.0f64, 0.4f64), (0.4472136f64, 0.4472136f64)),
            (sqrt, (2.5f64, 0f64), (1.5811388f64, 0.0f64)),
            (sqrt, (-2.5f64, 0f64), (0.0f64, 1.5811388f64)),
            (sqrt, (0f64, 0f64), (0.0f64, 0.0f64)),
            (acos, (-0.8f64, 0f64), (2.4980915f64, 0.0f64)),
            (acos, (0f64, 0.4f64), (1.5707963f64, -0.3900353f64)),
            (acos, (-0.5f64, 0.9f64), (1.9389155f64, -0.8561366f64)),
            (acos, (0.4f64, -1.0f64), (1.2899829f64, 0.9099081f64)),
            (acos, (2.2f64, 0f64), (0.0f64, 1.4254169f64)),
            (acos, (-1.5f64, 0f64), (3.1415927f64, -0.9624237f64)),
            (acos, (0f64, 0f64), (1.5707963f64, 0.0f64)),
            (acosh, (0.6f64, 0f64), (0.0f64, 0.9272952f64)),
            (acosh, (0f64, 0.5f64), (0.4812118f64, 1.5707963f64)),
            (acosh, (1.0f64, -1.0f64), (1.0612751f64, -0.9045569f64)),
            (acosh, (0.5f64, 0.6f64), (0.6197743f64, 1.1403656f64)),
            (acosh, (2.2f64, 0f64), (1.4254169f64, 0.0f64)),
            (acosh, (-2.7f64, 0f64), (1.6501935f64, 3.1415927f64)),
            (acosh, (0f64, 0f64), (0.0f64, 1.5707963f64)),
            (acsc, (-0.9f64, 0f64), (-1.5707963f64, 0.4671453f64)),
            (acsc, (0f64, -0.9f64), (0.0f64, 0.9578004f64)),
            (acsc, (0.8f64, -0.6f64), (0.6847192f64, 0.7127085f64)),
            (acsc, (0.3f64, -0.5f64), (0.4742891f64, 1.2767722f64)),
            (acsc, (2.7f64, 0f64), (0.3794077f64, 0.0f64)),
            (acsc, (-1.7f64, 0f64), (-0.6288749f64, 0.0f64)),
            (acsc, (0f64, 0f64), (f64::NAN, f64::NAN)),
            (acsch, (-0.8f64, 0f64), (-1.047593f64, 0.0f64)),
            (acsch, (0f64, 0.4f64), (-1.5667992f64, -1.5707963f64)),
            (acsch, (-0.1f64, -0.9f64), (-0.5019389f64, 1.335583f64)),
            (acsch, (1.0f64, -0.7f64), (0.6735491f64, 0.3900529f64)),
            (acsch, (2.5f64, 0f64), (0.3900353f64, 0.0f64)),
            (acsch, (-2.4f64, 0f64), (-0.4054651f64, 0.0f64)),
            (acsch, (0f64, 0f64), (f64::NAN, f64::NAN)),
            (acot, (-1.0f64, 0f64), (-0.7853982f64, 0.0f64)),
            (acot, (0f64, -0.5f64), (1.5707963f64, 0.5493061f64)),
            (acot, (-0.4f64, -0.4f64), (-1.1376452f64, 0.3513356f64)),
            (acot, (-0.9f64, -0.7f64), (-0.7028238f64, 0.3534233f64)),
            (acot, (2.3f64, 0f64), (0.4101273f64, 0.0f64)),
            (acot, (-1.7f64, 0f64), (-0.5317241f64, 0.0f64)),
            (acot, (0f64, 0f64), (1.5707963f64, 0.0f64)),
            (acoth, (0.5f64, 0f64), (0.5493061f64, -1.5707963f64)),
            (acoth, (0f64, 0.9f64), (0.0f64, -0.8379812f64)),
            (acoth, (-0.4f64, -0.1f64), (-0.4180715f64, 1.4525683f64)),
            (acoth, (-0.9f64, -0.5f64), (-0.6744352f64, 0.7554341f64)),
            (acoth, (1.7f64, 0f64), (0.6749634f64, 0.0f64)),
            (acoth, (-2.1f64, 0f64), (-0.518046f64, 0.0f64)),
            (acoth, (0f64, 0f64), (0.0f64, 1.5707963f64)),
            (asec, (-0.1f64, 0f64), (3.1415927f64, -2.9932228f64)),
            (asec, (0f64, 0.7f64), (1.5707963f64, 1.1544774f64)),
            (asec, (0.9f64, -0.4f64), (0.6818568f64, -0.6148366f64)),
            (asec, (-0.5f64, 0.9f64), (1.9278638f64, 0.8134796f64)),
            (asec, (1.5f64, 0f64), (0.8410687f64, 0.0f64)),
            (asec, (-2.7f64, 0f64), (1.950204f64, 0.0f64)),
            (asec, (0f64, 0f64), (f64::NAN, f64::NAN)),
            (asech, (0.6f64, 0f64), (1.0986123f64, 0.0f64)),
            (asech, (0f64, -1.0f64), (0.8813736f64, 1.5707963f64)),
            (asech, (0.1f64, -0.7f64), (1.1434743f64, 1.4548078f64)),
            (asech, (-0.4f64, -0.3f64), (1.3742673f64, 2.4355905f64)),
            (asech, (2.3f64, 0f64), (0.0f64, 1.1209995f64)),
            (asech, (-2.2f64, 0f64), (0.0f64, 2.0426582f64)),
            (asech, (0f64, 0f64), (f64::NAN, f64::NAN)),
            (asin, (-0.8f64, 0f64), (-0.9272952f64, 0.0f64)),
            (asin, (0f64, 0.5f64), (0.0f64, 0.4812118f64)),
            (asin, (-0.2f64, -1.0f64), (-0.1411811f64, -0.8884618f64)),
            (asin, (-0.8f64, 0.3f64), (-0.8214537f64, 0.427304f64)),
            (asin, (1.5f64, 0f64), (1.5707963f64, -0.9624237f64)),
            (asin, (-3.0f64, 0f64), (-1.5707963f64, 1.7627472f64)),
            (asin, (0f64, 0f64), (0.0f64, 0.0f64)),
            (asinh, (1.0f64, 0f64), (0.8813736f64, 0.0f64)),
            (asinh, (0f64, 0.1f64), (0.0f64, 0.1001674f64)),
            (asinh, (0.9f64, 1.0f64), (1.0029766f64, 0.7031f64)),
            (asinh, (-0.5f64, 0.8f64), (-0.627368f64, 0.7272513f64)),
            (asinh, (1.8f64, 0f64), (1.3504407f64, 0.0f64)),
            (asinh, (-2.4f64, 0f64), (-1.6094379f64, 0.0f64)),
            (asinh, (0f64, 0f64), (0.0f64, 0.0f64)),
            (atan, (0.0f64, 0f64), (0.0f64, 0.0f64)),
            (atan, (0f64, 0.8f64), (0.0f64, 1.0986123f64)),
            (atan, (0.4f64, 0.2f64), (0.3926991f64, 0.1732868f64)),
            (atan, (0.4f64, -0.3f64), (0.4088225f64, -0.2614921f64)),
            (atan, (1.5f64, 0f64), (0.9827937f64, 0.0f64)),
            (atan, (-1.8f64, 0f64), (-1.0636978f64, 0.0f64)),
            (atan, (0f64, 0f64), (0.0f64, 0.0f64)),
            (atanh, (0.9f64, 0f64), (1.4722195f64, 0.0f64)),
            (atanh, (0f64, -0.1f64), (0.0f64, -0.0996687f64)),
            (atanh, (-1.0f64, -0.3f64), (-0.9541226f64, -0.8598431f64)),
            (atanh, (0.6f64, -0.4f64), (0.5350165f64, -0.5151884f64)),
            (atanh, (1.5f64, 0f64), (0.804719f64, -1.5707963f64)),
            (atanh, (-2.3f64, 0f64), (-0.4657791f64, 1.5707963f64)),
            (atanh, (0f64, 0f64), (0.0f64, 0.0f64)),
            (cos, (0.1f64, 0f64), (0.9950042f64, 0.0f64)),
            (cos, (0f64, -0.2f64), (1.0200668f64, 0.0f64)),
            (cos, (-0.1f64, -0.1f64), (0.9999833f64, -0.01f64)),
            (cos, (0.5f64, -0.4f64), (0.9487303f64, 0.1969252f64)),
            (cos, (2.4f64, 0f64), (-0.7373937f64, 0.0f64)),
            (cos, (-2.2f64, 0f64), (-0.5885011f64, 0.0f64)),
            (cos, (0f64, 0f64), (1.0f64, 0.0f64)),
            (cosh, (0.0f64, 0f64), (1.0f64, 0.0f64)),
            (cosh, (0f64, 0.9f64), (0.62161f64, 0.0f64)),
            (cosh, (0.8f64, -0.4f64), (1.2318592f64, -0.3458448f64)),
            (cosh, (-0.2f64, 1.0f64), (0.5511444f64, -0.1694184f64)),
            (cosh, (2.9f64, 0f64), (9.1145843f64, 0.0f64)),
            (cosh, (-2.7f64, 0f64), (7.4734686f64, 0.0f64)),
            (cosh, (0f64, 0f64), (1.0f64, 0.0f64)),
            (csc, (0.5f64, 0f64), (2.0858296f64, 0.0f64)),
            (csc, (0f64, -0.6f64), (0.0f64, 1.5707129f64)),
            (csc, (-0.9f64, -0.7f64), (-0.8268848f64, 0.3965713f64)),
            (csc, (0.3f64, -0.5f64), (0.9285645f64, 1.3871816f64)),
            (csc, (2.3f64, 0f64), (1.3410125f64, 0.0f64)),
            (csc, (-2.5f64, 0f64), (-1.6709215f64, 0.0f64)),
            (csc, (0f64, 0f64), (f64::NAN, f64::NAN)),
            (csch, (-0.8f64, 0f64), (-1.1259917f64, 0.0f64)),
            (csch, (0f64, 0.7f64), (0.0f64, -1.5522703f64)),
            (csch, (0.6f64, -0.4f64), (1.0528253f64, 0.8288386f64)),
            (csch, (-0.4f64, 0.0f64), (-2.4345571f64, 0.0f64)),
            (csch, (2.7f64, 0f64), (0.1350209f64, 0.0f64)),
            (csch, (-3.0f64, 0f64), (-0.0998216f64, 0.0f64)),
            (csch, (0f64, 0f64), (f64::NAN, f64::NAN)),
            (cot, (-0.3f64, 0f64), (-3.2327281f64, 0.0f64)),
            (cot, (0f64, 0.4f64), (0.0f64, -2.6319324f64)),
            (cot, (-0.9f64, 0.4f64), (-0.6224112f64, -0.5676115f64)),
            (cot, (-0.2f64, 0.8f64), (-0.2350987f64, -1.4341723f64)),
            (cot, (1.7f64, 0f64), (-0.1299275f64, 0.0f64)),
            (cot, (-1.8f64, 0f64), (0.2333035f64, 0.0f64)),
            (cot, (0f64, 0f64), (f64::NAN, f64::NAN)),
            (coth, (0.3f64, 0f64), (3.4327384f64, 0.0f64)),
            (coth, (0f64, 0.9f64), (0.0f64, -0.7935511f64)),
            (coth, (-0.7f64, -0.5f64), (-1.1823582f64, 0.5224593f64)),
            (coth, (-0.8f64, -0.3f64), (-1.3558181f64, 0.3222608f64)),
            (coth, (2.8f64, 0f64), (1.0074232f64, 0.0f64)),
            (coth, (-2.3f64, 0f64), (-1.0203078f64, 0.0f64)),
            (coth, (0f64, 0f64), (f64::NAN, f64::NAN)),
            (sec, (0.4f64, 0f64), (1.0857044f64, 0.0f64)),
            (sec, (0f64, -0.7f64), (0.7967055f64, 0.0f64)),
            (sec, (0.7f64, -0.2f64), (1.2472669f64, -0.2073543f64)),
            (sec, (0.0f64, 0.1f64), (0.9950207f64, 0.0f64)),
            (sec, (1.5f64, 0f64), (14.1368329f64, 0.0f64)),
            (sec, (-1.6f64, 0f64), (-34.2471356f64, 0.0f64)),
            (sec, (0f64, 0f64), (1.0f64, 0.0f64)),
            (sech, (-0.6f64, 0f64), (0.8435507f64, 0.0f64)),
            (sech, (0f64, 1.0f64), (1.8508157f64, 0.0f64)),
            (sech, (0.5f64, -0.9f64), (1.0653621f64, 0.6204037f64)),
            (sech, (-0.8f64, 0.9f64), (0.7074639f64, 0.5919997f64)),
            (sech, (2.0f64, 0f64), (0.2658022f64, 0.0f64)),
            (sech, (-2.0f64, 0f64), (0.2658022f64, 0.0f64)),
            (sech, (0f64, 0f64), (1.0f64, 0.0f64)),
            (sin, (1.0f64, 0f64), (0.841471f64, 0.0f64)),
            (sin, (0f64, -0.3f64), (0.0f64, -0.3045203f64)),
            (sin, (-0.1f64, -0.6f64), (-0.118349f64, -0.633473f64)),
            (sin, (-0.4f64, 0.4f64), (-0.4209894f64, 0.3783279f64)),
            (sin, (2.8f64, 0f64), (0.3349882f64, 0.0f64)),
            (sin, (-2.0f64, 0f64), (-0.9092974f64, 0.0f64)),
            (sin, (0f64, 0f64), (0.0f64, 0.0f64)),
            (sinh, (-0.4f64, 0f64), (-0.4107523f64, 0.0f64)),
            (sinh, (0f64, -1.0f64), (0.0f64, -0.841471f64)),
            (sinh, (-0.9f64, -0.4f64), (-0.9454845f64, -0.5580701f64)),
            (sinh, (0.0f64, 0.9f64), (0.0f64, 0.7833269f64)),
            (sinh, (2.7f64, 0f64), (7.4062631f64, 0.0f64)),
            (sinh, (-1.9f64, 0f64), (-3.2681629f64, 0.0f64)),
            (sinh, (0f64, 0f64), (0.0f64, 0.0f64)),
            (tan, (-0.6f64, 0f64), (-0.6841368f64, 0.0f64)),
            (tan, (0f64, -0.9f64), (0.0f64, -0.7162979f64)),
            (tan, (-0.1f64, 0.1f64), (-0.099328f64, 0.1006613f64)),
            (tan, (0.7f64, -0.1f64), (0.8280854f64, -0.1691851f64)),
            (tan, (2.8f64, 0f64), (-0.3555298f64, 0.0f64)),
            (tan, (-1.6f64, 0f64), (34.2325327f64, 0.0f64)),
            (tan, (0f64, 0f64), (0.0f64, 0.0f64)),
            (tanh, (-0.4f64, 0f64), (-0.379949f64, 0.0f64)),
            (tanh, (0f64, -0.3f64), (0.0f64, -0.3093362f64)),
            (tanh, (-0.8f64, 0.5f64), (-0.7619454f64, 0.2698954f64)),
            (tanh, (-0.3f64, 0.0f64), (-0.2913126f64, 0.0f64)),
            (tanh, (1.5f64, 0f64), (0.9051483f64, 0.0f64)),
            (tanh, (-2.3f64, 0f64), (-0.9800964f64, 0.0f64)),
            (tanh, (0f64, 0f64), (0.0f64, 0.0f64)),
        ];

        for (i, (func, input, expected_output)) in in_out.iter().enumerate() {
            let actual_output = func(KalkNum::new_with_imaginary(
                KalkNum::from(input.0).value,
                "",
                KalkNum::from(input.1).value,
            ));

            let expected_has_nan_or_inf = expected_output.0.is_nan()
                || expected_output.0.is_infinite()
                || expected_output.1.is_nan()
                || expected_output.1.is_infinite();
            let actual_has_nan_or_inf = actual_output.to_f64().is_nan()
                || actual_output.to_f64().is_infinite()
                || actual_output.imaginary_to_f64().is_nan()
                || actual_output.imaginary_to_f64().is_infinite();

            if expected_has_nan_or_inf || actual_has_nan_or_inf {
                assert!(true);
                continue;
            }

            println!(
                "{} | expected: {}, {}",
                i, expected_output.0, expected_output.1
            );
            println!(
                "{} | got: {}, {}",
                i,
                actual_output.to_f64(),
                actual_output.imaginary_to_f64()
            );
            assert!(cmp(expected_output.0, actual_output.to_f64()));
            assert!(cmp(expected_output.1, actual_output.imaginary_to_f64()));
        }
    }
}
