use crate::kalk_value::KalkValue;
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

        m.insert("abs", (UnaryFuncInfo(abs, Other), ""));
        m.insert("arg", (UnaryFuncInfo(arg, Other), ""));
        m.insert("average", (UnaryFuncInfo(average, Other), ""));
        m.insert("cbrt", (UnaryFuncInfo(cbrt, Other), ""));
        m.insert("ceil", (UnaryFuncInfo(ceil, Other), ""));
        m.insert("iverson", (UnaryFuncInfo(iverson, Other), ""));
        m.insert("exp", (UnaryFuncInfo(exp, Other), ""));
        m.insert("floor", (UnaryFuncInfo(floor, Other), ""));
        m.insert("frac", (UnaryFuncInfo(frac, Other), ""));
        m.insert("Im", (UnaryFuncInfo(im, Other), ""));
        m.insert("gamma", (UnaryFuncInfo(gamma, Other), ""));
        m.insert("Γ", (UnaryFuncInfo(gamma, Other), ""));
        m.insert("log", (UnaryFuncInfo(log, Other), ""));
        m.insert("ln", (UnaryFuncInfo(ln, Other), ""));
        m.insert("max", (UnaryFuncInfo(max_vec, Other), ""));
        m.insert("min", (UnaryFuncInfo(min_vec, Other), ""));
        m.insert("Re", (UnaryFuncInfo(re, Other), ""));
        m.insert("round", (UnaryFuncInfo(round, Other), ""));
        m.insert("sqrt", (UnaryFuncInfo(sqrt, Other), ""));
        m.insert("√", (UnaryFuncInfo(sqrt, Other), ""));
        m.insert("trunc", (UnaryFuncInfo(trunc, Other), ""));
        m.insert("bitcmp", (UnaryFuncInfo(bitcmp, Other), ""));
        m
    };
    pub static ref BINARY_FUNCS: HashMap<&'static str, (BinaryFuncInfo, &'static str)> = {
        let mut m = HashMap::new();
        m.insert("bitand", (BinaryFuncInfo(bitand, Other), ""));
        m.insert("bitor", (BinaryFuncInfo(bitor, Other), ""));
        m.insert("bitxor", (BinaryFuncInfo(bitxor, Other), ""));
        m.insert("bitshift", (BinaryFuncInfo(bitshift, Other), ""));
        m.insert("max", (BinaryFuncInfo(max, Other), ""));
        m.insert("min", (BinaryFuncInfo(min, Other), ""));
        m.insert("hypot", (BinaryFuncInfo(hypot, Other), ""));
        m.insert("gcd", (BinaryFuncInfo(gcd, Other), ""));
        m.insert("lcm", (BinaryFuncInfo(lcm, Other), ""));
        m.insert("log", (BinaryFuncInfo(logx, Other), ""));
        m.insert("root", (BinaryFuncInfo(nth_root, Other), ""));
        m.insert("nCr", (BinaryFuncInfo(ncr, Other), ""));
        m.insert("comb", (BinaryFuncInfo(ncr, Other), ""));
        m.insert("nPr", (BinaryFuncInfo(npr, Other), ""));
        m.insert("perm", (BinaryFuncInfo(npr, Other), ""));
        m
    };
}

enum FuncType {
    Trig,
    InverseTrig,
    Other,
}

pub struct UnaryFuncInfo(fn(KalkValue) -> KalkValue, FuncType);

pub struct BinaryFuncInfo(fn(KalkValue, KalkValue) -> KalkValue, FuncType);

impl UnaryFuncInfo {
    fn call(
        &self,
        context: &mut interpreter::Context,
        x: KalkValue,
        angle_unit: &str,
    ) -> KalkValue {
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
        x: KalkValue,
        y: KalkValue,
        angle_unit: &str,
    ) -> KalkValue {
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
    x: KalkValue,
    angle_unit: &str,
) -> Option<(KalkValue, String)> {
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
    x: KalkValue,
    y: KalkValue,
    angle_unit: &str,
) -> Option<(KalkValue, String)> {
    if let Some((func_info, func_unit)) = BINARY_FUNCS.get(name) {
        Some((
            func_info.call(context, x, y, angle_unit),
            func_unit.to_string(),
        ))
    } else {
        None
    }
}

fn to_angle_unit(context: &mut interpreter::Context, x: KalkValue, angle_unit: &str) -> KalkValue {
    match angle_unit {
        "rad" => x,
        _ => interpreter::convert_unit(context, &Expr::Literal(x.to_f64()), "rad", angle_unit)
            .unwrap(),
    }
}

fn from_angle_unit(
    context: &mut interpreter::Context,
    x: KalkValue,
    angle_unit: &str,
) -> KalkValue {
    match angle_unit {
        "rad" => x,
        _ => interpreter::convert_unit(context, &Expr::Literal(x.to_f64()), angle_unit, "rad")
            .unwrap(),
    }
}

pub mod funcs {
    #[cfg(not(feature = "rug"))]
    pub use super::regular::funcs::*;
    use super::special_funcs::factorial;
    #[cfg(feature = "rug")]
    pub use super::with_rug::funcs::*;
    use crate::{as_number_or_return, as_vector_or_return, float, kalk_value::KalkValue};

    pub fn abs(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x);
        if imaginary != 0f64 {
            // |z| = sqrt(a² + b²)
            let a = real.clone() * real;
            let b = imaginary.clone() * imaginary;

            sqrt(KalkValue::Number(a + b, float!(0), unit))
        } else {
            KalkValue::Number(real.abs(), float!(0), unit)
        }
    }

    pub fn acos(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x.clone());
        if imaginary != 0f64 || real > 1f64 || real < -1f64 {
            // -i * ln(i * sqrt(1 - z²) + z)
            let root = sqrt(
                KalkValue::from(1f64).sub_without_unit(&x.clone().mul_without_unit(&x.clone())),
            );
            let iroot = multiply_with_i(root.clone());
            let (ln_real, ln_imaginary, ln_unit) =
                as_number_or_return!(ln(iroot.add_without_unit(&x)));

            // -iz = -i(a + bi) = b - ai
            KalkValue::Number(ln_imaginary, -ln_real, ln_unit)
        } else {
            KalkValue::Number(real.acos(), float!(0), unit)
        }
    }

    pub fn acosh(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x.clone());
        if imaginary != 0f64 || real < 1f64 {
            let sqrt1 = sqrt(KalkValue::Number(
                real.clone() + 1f64,
                imaginary.clone(),
                unit.clone(),
            ));
            let sqrt2 = sqrt(KalkValue::Number(
                real.clone() - 1f64,
                imaginary.clone(),
                unit,
            ));

            ln(x.add_without_unit(&sqrt1.mul_without_unit(&sqrt2)))
        } else {
            KalkValue::Number(real.acosh(), float!(0), unit)
        }
    }

    pub fn acot(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x.clone());
        if imaginary != 0f64 {
            // atan(1/z)
            atan(KalkValue::from(1f64).div_without_unit(&x))
        } else {
            KalkValue::Number((1f64 / real).atan(), float!(0), unit)
        }
    }

    pub fn acoth(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x.clone());
        if imaginary != 0f64 || real <= 1f64 || real >= -1f64 {
            // 1 / z
            let (inv_real, inv_imaginary, inv_unit) =
                as_number_or_return!(KalkValue::from(1f64).div_without_unit(&x));
            let ln1 = ln(KalkValue::Number(
                1f64 + inv_real.clone(),
                inv_imaginary.clone(),
                inv_unit.clone(),
            ));
            let ln2 = ln(KalkValue::Number(1f64 - inv_real, -inv_imaginary, inv_unit));

            ln1.sub_without_unit(&ln2)
                .div_without_unit(&KalkValue::from(2f64))
        } else {
            KalkValue::Number((1f64 / real).atanh(), float!(0), unit)
        }
    }

    pub fn acsc(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x.clone());
        if imaginary != 0f64 || real < 1f64 || real > -1f64 {
            // asin(1/z)
            asin(KalkValue::from(1f64).div_without_unit(&x))
        } else {
            KalkValue::Number((1f64 / real).asin(), float!(0), unit)
        }
    }

    pub fn acsch(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x.clone());
        if imaginary != 0f64 || real == 0f64 {
            let (inv_x2_real, inv_x2_imaginary, inv_x2_unit) = as_number_or_return!(
                KalkValue::from(1f64).div_without_unit(&x.clone().mul_without_unit(&x.clone()))
            );
            let sqrt = sqrt(KalkValue::Number(
                1f64 + inv_x2_real,
                inv_x2_imaginary,
                inv_x2_unit,
            ));
            let inv_x = KalkValue::from(1f64).div_without_unit(&x.clone());

            ln(sqrt.add_without_unit(&inv_x))
        } else {
            KalkValue::Number((1f64 / real).asinh(), float!(0), unit)
        }
    }

    pub fn asec(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x.clone());
        if imaginary != 0f64 || real < 1f64 || real > -1f64 {
            // acos(1/z)
            acos(KalkValue::from(1f64).div_without_unit(&x))
        } else {
            KalkValue::Number((1f64 / real).acos(), float!(0), unit)
        }
    }

    pub fn asech(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x.clone());
        if imaginary != 0f64 || real <= 0f64 || real > 1f64 {
            // 1/z
            let inv = KalkValue::from(1f64).div_without_unit(&x.clone());
            let (inv_real, inv_imaginary, inv_unit) = as_number_or_return!(inv.clone());
            // sqrt(1/z - 1)
            let sqrt1 = sqrt(KalkValue::Number(
                inv_real.clone() - 1f64,
                inv_imaginary.clone(),
                inv_unit.clone(),
            ));
            // sqrt(1/z + 1)
            let sqrt2 = sqrt(KalkValue::Number(
                inv_real.clone() + 1f64,
                inv_imaginary.clone(),
                inv_unit,
            ));

            // ln(1/z + sqrt(1/z - 1) * sqrt(1/z + 1))
            ln(sqrt1.mul_without_unit(&sqrt2).add_without_unit(&inv))
        } else {
            KalkValue::Number((1f64 / real).acosh(), float!(0), unit)
        }
    }

    pub fn asin(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x.clone());
        if imaginary != 0f64 || real > 1f64 || real < -1f64 {
            // i * ln(sqrt(1 - z²) - iz)
            let root = sqrt(
                KalkValue::from(1f64).sub_without_unit(&x.clone().mul_without_unit(&x.clone())),
            );
            let iz = multiply_with_i(x.clone());
            let ln = ln(root.sub_without_unit(&iz));
            multiply_with_i(ln)
        } else {
            KalkValue::Number(real.asin(), float!(0), unit)
        }
    }

    pub fn asinh(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x.clone());
        if imaginary != 0f64 {
            let (x2_real, x2_imaginary, x2_unit) =
                as_number_or_return!(x.clone().mul_without_unit(&x.clone()));
            let sqrt = sqrt(KalkValue::Number(x2_real + 1f64, x2_imaginary, x2_unit));

            ln(x.add_without_unit(&sqrt))
        } else {
            KalkValue::Number(real.asinh(), float!(0), unit)
        }
    }

    pub fn atan(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x.clone());
        if imaginary != 0f64 {
            let (iz_real, iz_imaginary, iz_unit) = as_number_or_return!(multiply_with_i(x));

            // 1 - iz
            let neg = KalkValue::Number(
                1f64 - iz_real.clone(),
                -iz_imaginary.clone(),
                iz_unit.clone(),
            );

            // 1 + iz
            let pos = KalkValue::Number(1f64 + iz_real, iz_imaginary, iz_unit);

            // ln(1 - iz) - ln(1 + iz)
            let ln = ln(neg).sub_without_unit(&ln(pos));

            multiply_with_i(ln).div_without_unit(&KalkValue::from(2f64))
        } else {
            KalkValue::Number(real.atan(), float!(0), unit)
        }
    }

    pub fn atanh(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x);
        if imaginary != 0f64 || real >= 1f64 || real <= -1f64 {
            // 1/2 * log(z + 1) - 1/2 * log(-z + 1)
            let log1 = ln(KalkValue::Number(
                1f64 + real.clone(),
                imaginary.clone(),
                unit.clone(),
            ));
            let log2 = ln(KalkValue::Number(1f64 - real, -imaginary, unit));

            log1.sub_without_unit(&log2)
                .div_without_unit(&KalkValue::from(2f64))
        } else {
            KalkValue::Number(real.atanh(), float!(0), unit)
        }
    }

    pub fn average(x: KalkValue) -> KalkValue {
        let values = as_vector_or_return!(x);
        let count = values.len() as i64;
        let mut sum_real = float!(0);
        let mut sum_imaginary = float!(0);
        for value in values {
            let (real, imaginary, _) = as_number_or_return!(value);
            sum_real += real;
            sum_imaginary += imaginary;
        }

        KalkValue::Number(sum_real, sum_imaginary, String::new())
            .div_without_unit(&KalkValue::from(count))
    }

    pub fn cbrt(x: KalkValue) -> KalkValue {
        let (real, _, unit) = as_number_or_return!(x);

        KalkValue::Number(real.cbrt(), float!(0), unit)
    }

    pub fn ceil(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x);

        KalkValue::Number(real.ceil(), imaginary.ceil(), unit)
    }

    pub fn cos(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x);

        KalkValue::Number(
            real.clone().cos() * imaginary.clone().cosh(),
            -real.sin() * imaginary.sinh(),
            unit,
        )
    }

    pub fn cosh(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x);

        KalkValue::Number(
            real.clone().cosh() * imaginary.clone().cos(),
            real.sinh() * imaginary.sin(),
            unit,
        )
    }

    pub fn csc(x: KalkValue) -> KalkValue {
        KalkValue::from(1f64).div_without_unit(&sin(x))
    }

    pub fn csch(x: KalkValue) -> KalkValue {
        KalkValue::from(1f64).div_without_unit(&sinh(x))
    }

    pub fn cot(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x);

        let a = real * 2f64;
        let b = imaginary * 2f64;
        KalkValue::Number(
            -a.clone().sin() / (a.clone().cos() - b.clone().cosh()),
            b.clone().sinh() / (a.cos() - b.cosh()),
            unit,
        )
    }

    pub fn coth(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x);
        let a = real * 2f64;
        let b = imaginary * 2f64;
        KalkValue::Number(
            -a.clone().sinh() / (b.clone().cos() - a.clone().cosh()),
            b.clone().sin() / (b.cos() - a.cosh()),
            unit,
        )
    }

    pub fn exp(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x);

        if imaginary != 0f64 {
            // e^a*cos(b) + ie^a*sin(b)
            let exp_a = real.exp();
            let b = imaginary;
            KalkValue::Number(exp_a.clone() * b.clone().cos(), exp_a * b.sin(), unit)
        } else {
            KalkValue::Number(real.exp(), float!(0), unit)
        }
    }

    pub fn floor(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x);

        KalkValue::Number(real.floor(), imaginary.floor(), unit)
    }

    pub fn frac(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x);

        KalkValue::Number(real.fract(), imaginary.fract(), unit)
    }

    pub fn gcd(x: KalkValue, y: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x.clone());
        let (real_rhs, imaginary_rhs, _) = as_number_or_return!(y.clone());

        // Find the norm of a Gaussian integer
        fn norm(x: KalkValue) -> KalkValue {
            let (real, imaginary, unit) = as_number_or_return!(x);
            KalkValue::Number(
                (real.clone() * real) + (imaginary.clone() * imaginary),
                float!(0),
                unit,
            )
        }

        if imaginary != 0f64 || y.has_imaginary() {
            if real.clone().fract() != 0f64
                || real_rhs.clone().fract() != 0f64
                || imaginary.clone().fract() != 0f64
                || imaginary_rhs.clone().fract() != 0f64
            {
                // Not a Gaussian integer!
                // TODO: throw an actual error instead of returning NaN
                return KalkValue::from(f64::NAN);
            }

            // Partially derived from:
            // https://stackoverflow.com/a/52692832

            let a;
            let b;

            // Ensure a > b
            if norm(x.clone()).values().0 < norm(y.clone()).values().0 {
                a = y;
                b = x;
            } else {
                a = x;
                b = y;
            }

            let (b_real, b_imaginary, b_unit) = as_number_or_return!(b.clone());
            let (c_real, c_imaginary, c_unit) =
                as_number_or_return!(a.clone().div_without_unit(&b.clone()));
            if c_imaginary.clone().fract() == 0f64 {
                KalkValue::Number(b_real.abs(), b_imaginary, b_unit)
            } else {
                let rounded_c = KalkValue::Number(c_real.round(), c_imaginary.round(), c_unit);
                gcd(
                    a.sub_without_unit(&b.clone().mul_without_unit(&rounded_c)),
                    b,
                )
            }
        } else {
            if real < 0f64 || real_rhs < 0f64 {
                return gcd(
                    KalkValue::Number(real.abs(), float!(0), unit.clone()),
                    KalkValue::Number(real_rhs.abs(), float!(0), unit),
                );
            }

            // Euclidean GCD algorithm, but with modulus
            let mut x_a = real.clone();
            let mut y_a = real_rhs.clone();
            while !y_a.eq(&0f64) {
                let t = y_a.clone();
                y_a = x_a % y_a;
                x_a = t;
            }

            // Usually we'd need to return max(x, -x), but since we've handled negative
            // values above, that is unnecessary.
            KalkValue::Number(x_a, float!(0), unit)
        }
    }

    pub fn im(x: KalkValue) -> KalkValue {
        let (_, imaginary, unit) = as_number_or_return!(x);

        KalkValue::Number(imaginary, float!(0), unit)
    }

    pub fn iverson(x: KalkValue) -> KalkValue {
        KalkValue::from(if let KalkValue::Boolean(boolean_value) = x {
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
    pub fn lcm(x: KalkValue, y: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x.clone());
        let (real_rhs, imaginary_rhs, unit_rhs) = as_number_or_return!(y.clone());
        let gcd = gcd(x.clone(), y.clone());
        let absx = KalkValue::Number(real.abs(), imaginary, unit);
        let absy = KalkValue::Number(real_rhs.abs(), imaginary_rhs, unit_rhs);
        return absx.div_without_unit(&gcd).mul_without_unit(&absy);
    }

    pub fn log(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x.clone());
        if imaginary != 0f64 || real < 0f64 {
            // ln(z) / ln(10)
            ln(x).div_without_unit(&KalkValue::from(10f64.ln()))
        } else {
            KalkValue::Number(real.log10(), float!(0), unit)
        }
    }

    pub fn logx(x: KalkValue, y: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x.clone());
        let (real_rhs, _, _) = as_number_or_return!(y.clone());
        if imaginary != 0f64 || y.has_imaginary() || real < 0f64 || real_rhs < 0f64 {
            // ln(z) / ln(n)
            ln(x).div_without_unit(&ln(y))
        } else {
            KalkValue::Number(real.log10() / real_rhs.log10(), float!(0), unit)
        }
    }

    pub fn ln(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x.clone());
        if imaginary != 0f64 || real < 0f64 {
            let r = abs(x.clone());
            // ln|z| + i * arg z
            ln(r).add_without_unit(&multiply_with_i(arg(x)))
        } else {
            KalkValue::Number(real.ln(), float!(0), unit)
        }
    }

    pub fn max_vec(x: KalkValue) -> KalkValue {
        let values = as_vector_or_return!(x);
        let mut max = &values[0];
        for value in &values {
            if let KalkValue::Boolean(greater) = value.greater_than_without_unit(&max) {
                if greater {
                    max = value;
                }
            }
        }

        max.clone()
    }

    pub fn min_vec(x: KalkValue) -> KalkValue {
        let values = as_vector_or_return!(x);
        let mut min = &values[0];
        for value in &values {
            if let KalkValue::Boolean(less) = value.less_than_without_unit(&min) {
                if less {
                    min = value;
                }
            }
        }

        min.clone()
    }

    pub fn nth_root(x: KalkValue, n: KalkValue) -> KalkValue {
        x.pow_without_unit(&KalkValue::from(1f64).div_without_unit(&n))
    }

    pub fn re(x: KalkValue) -> KalkValue {
        let (real, _, unit) = as_number_or_return!(x);
        KalkValue::Number(real, float!(0), unit)
    }

    pub fn round(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x);
        KalkValue::Number(real.round(), imaginary.round(), unit)
    }

    pub fn sec(x: KalkValue) -> KalkValue {
        KalkValue::from(1f64).div_without_unit(&cos(x))
    }

    pub fn sech(x: KalkValue) -> KalkValue {
        KalkValue::from(1f64).div_without_unit(&cosh(x))
    }

    pub fn sin(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x);
        KalkValue::Number(
            real.clone().sin() * imaginary.clone().cosh(),
            real.cos() * imaginary.sinh(),
            unit,
        )
    }

    pub fn sinh(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x);
        KalkValue::Number(
            real.clone().sinh() * imaginary.clone().cos(),
            real.cosh() * imaginary.sin(),
            unit,
        )
    }

    pub fn sqrt(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x.clone());
        if imaginary != 0f64 {
            let (abs_real, _, abs_unit) = as_number_or_return!(abs(x.clone()));
            let r = abs_real;
            let a = real;
            let b = imaginary;

            // sqrt((|z| + a) / 2) + i * (b / |b|) * sqrt((|z| - a) / 2)
            KalkValue::Number(
                ((r.clone() + a.clone()) / 2f64).sqrt(),
                (b.clone() / b.abs()) * ((r - a) / 2f64).sqrt(),
                abs_unit,
            )
        } else if real < 0f64 {
            KalkValue::Number(float!(0), real.abs().sqrt(), unit)
        } else {
            KalkValue::Number(real.sqrt(), float!(0), unit)
        }
    }

    pub fn tan(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x);
        if imaginary != 0f64 {
            let a = real * 2f64;
            let b = imaginary * 2f64;
            KalkValue::Number(
                a.clone().sin() / (a.clone().cos() + b.clone().cosh()),
                b.clone().sinh() / (a.cos() + b.cosh()),
                unit,
            )
        } else {
            KalkValue::Number(real.tan(), float!(0), unit)
        }
    }

    pub fn tanh(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x);
        if imaginary != 0f64 {
            let a = real * 2f64;
            let b = imaginary * 2f64;
            KalkValue::Number(
                a.clone().sinh() / (a.clone().cosh() + b.clone().cos()),
                b.clone().sin() / (a.cosh() + b.cos()),
                unit,
            )
        } else {
            KalkValue::Number(real.tanh(), float!(0), unit)
        }
    }

    pub fn trunc(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x);
        KalkValue::Number(real.trunc(), imaginary.trunc(), unit)
    }

    pub fn ncr(x: KalkValue, y: KalkValue) -> KalkValue {
        factorial(x.clone()).div_without_unit(
            &factorial(y.clone()).mul_without_unit(&factorial(x.sub_without_unit(&y))),
        )
    }

    pub fn npr(x: KalkValue, y: KalkValue) -> KalkValue {
        factorial(x.clone()).div_without_unit(&factorial(x.sub_without_unit(&y)))
    }

    fn multiply_with_i(z: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(z);

        // iz = i(a + bi) = -b + ai
        KalkValue::Number(-imaginary, real, unit)
    }
}

#[cfg(test)]
mod tests {
    use super::funcs::*;
    use crate::float;
    use crate::prelude::KalkValue;
    use crate::test_helpers::cmp;

    #[test]
    fn test_unary_funcs() {
        let in_out = vec![
            (
                abs as fn(KalkValue) -> KalkValue,
                (3f64, 4f64),
                (5f64, 0f64),
            ),
            (abs, (-3f64, 4f64), (5f64, 0f64)),
            (abs, (3f64, -4f64), (5f64, 0f64)),
            (abs, (-3f64, 0f64), (3f64, 0f64)),
        ];

        for (i, (func, input, expected_output)) in in_out.iter().enumerate() {
            let actual_output = func(KalkValue::Number(
                float!(input.0),
                float!(input.1),
                String::new(),
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
            (
                gcd as fn(KalkValue, KalkValue) -> KalkValue,
                ((12f64, 0f64), (18f64, 0f64)),
                (6f64, 0f64),
            ),
            (gcd, ((30f64, 0f64), (18f64, 0f64)), (6f64, 0f64)),
            (gcd, ((5f64, 0f64), (2f64, 1f64)), (2f64, 1f64)),
            (gcd, ((18f64, 4f64), (30f64, 0f64)), (4f64, 2f64)),
            (gcd, ((3f64, 1f64), (1f64, -1f64)), (1f64, -1f64)),
            (gcd, ((12f64, -8f64), (6f64, 4f64)), (2f64, 0f64)),
            (lcm, ((12f64, -8f64), (6f64, 4f64)), (52f64, 0f64)),
            (lcm, ((1f64, -2f64), (3f64, 1f64)), (5f64, -5f64)),
        ];

        for (i, (func, input, expected_output)) in in_out.iter().enumerate() {
            let actual_output = func(
                KalkValue::Number(float!(input.0 .0), float!(input.0 .1), String::new()),
                KalkValue::Number(float!(input.1 .0), float!(input.1 .1), String::new()),
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
                arg as fn(KalkValue) -> KalkValue,
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
            let actual_output = func(KalkValue::Number(
                float!(input.0),
                float!(input.1),
                String::new(),
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
