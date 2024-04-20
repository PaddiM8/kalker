#![allow(clippy::redundant_clone)] // giving false flags
use crate::errors::KalkError;
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
pub const INIT: &str = "unit deg = (rad*180)/pi";

lazy_static! {
    pub static ref CONSTANTS: HashMap<&'static str, f64> = {
        let mut m = HashMap::new();
        m.insert("pi", std::f64::consts::PI);
        m.insert("e", std::f64::consts::E);
        m.insert("tau", std::f64::consts::TAU);
        m.insert("phi", 1.618_033_988_749_895);
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
        m.insert("bitcmp", (UnaryFuncInfo(bitcmp, Other), ""));
        m.insert("cbrt", (UnaryFuncInfo(cbrt, Other), ""));
        m.insert("ceil", (UnaryFuncInfo(ceil, Other), ""));
        m.insert("exp", (UnaryFuncInfo(exp, Other), ""));
        m.insert("floor", (UnaryFuncInfo(floor, Other), ""));
        m.insert("frac", (UnaryFuncInfo(frac, Other), ""));
        m.insert("gamma", (UnaryFuncInfo(gamma, Other), ""));
        m.insert("Γ", (UnaryFuncInfo(gamma, Other), ""));
        m.insert("iverson", (UnaryFuncInfo(iverson, Other), ""));
        m.insert("Im", (UnaryFuncInfo(im, Other), ""));
        m.insert("ln", (UnaryFuncInfo(ln, Other), ""));
        m.insert("length", (UnaryFuncInfo(length, Other), ""));
        m.insert("log", (UnaryFuncInfo(log, Other), ""));
        m.insert("Re", (UnaryFuncInfo(re, Other), ""));
        m.insert("round", (UnaryFuncInfo(round, Other), ""));
        m.insert("sgn", (UnaryFuncInfo(sgn, Other), ""));
        m.insert("sort", (UnaryFuncInfo(sort, Other), ""));
        m.insert("sqrt", (UnaryFuncInfo(sqrt, Other), ""));
        m.insert("√", (UnaryFuncInfo(sqrt, Other), ""));
        m.insert("transpose", (UnaryFuncInfo(transpose, Other), ""));
        m.insert("trunc", (UnaryFuncInfo(trunc, Other), ""));
        m
    };
    pub static ref BINARY_FUNCS: HashMap<&'static str, (BinaryFuncInfo, &'static str)> = {
        let mut m = HashMap::new();
        m.insert("append", (BinaryFuncInfo(append, Other), ""));
        m.insert("bitand", (BinaryFuncInfo(bitand, Other), ""));
        m.insert("bitor", (BinaryFuncInfo(bitor, Other), ""));
        m.insert("bitxor", (BinaryFuncInfo(bitxor, Other), ""));
        m.insert("bitshift", (BinaryFuncInfo(bitshift, Other), ""));
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
    pub static ref VECTOR_FUNCS: HashMap<&'static str, VectorFuncInfo> = {
        let mut m = HashMap::new();
        m.insert("average", VectorFuncInfo(average, Other));
        m.insert("diag", VectorFuncInfo(diag, Other));
        m.insert("matrix", VectorFuncInfo(matrix, Other));
        m.insert("max", VectorFuncInfo(max, Other));
        m.insert("min", VectorFuncInfo(min, Other));
        m.insert("perms", VectorFuncInfo(perms, Other));
        m.insert("permutations", VectorFuncInfo(perms, Other));
        m.insert("prod", VectorFuncInfo(prod, Other));
        m.insert("sum", VectorFuncInfo(sum, Other));
        m
    };
}

enum FuncType {
    Trig,
    InverseTrig,
    Other,
}

pub struct UnaryFuncInfo(fn(KalkValue) -> Result<KalkValue, KalkError>, FuncType);

pub struct BinaryFuncInfo(
    fn(KalkValue, KalkValue) -> Result<KalkValue, KalkError>,
    FuncType,
);

#[allow(dead_code)]
pub struct VectorFuncInfo(fn(KalkValue) -> Result<KalkValue, KalkError>, FuncType);

impl UnaryFuncInfo {
    fn call(
        &self,
        context: &mut interpreter::Context,
        x: KalkValue,
        angle_unit: &str,
    ) -> Result<KalkValue, KalkError> {
        let func = self.0;
        match self.1 {
            FuncType::Trig => func(from_angle_unit(context, x, angle_unit)),
            FuncType::InverseTrig => Ok(to_angle_unit(context, func(x)?, angle_unit)),
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
    ) -> Result<KalkValue, KalkError> {
        let func = self.0;
        match self.1 {
            FuncType::Trig => func(
                from_angle_unit(context, x, angle_unit),
                from_angle_unit(context, y, angle_unit),
            ),
            FuncType::InverseTrig => Ok(to_angle_unit(context, func(x, y)?, angle_unit)),
            FuncType::Other => func(x, y),
        }
    }
}

impl VectorFuncInfo {
    fn call(&self, x: KalkValue) -> Result<KalkValue, KalkError> {
        let func = self.0;
        func(x)
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
        || VECTOR_FUNCS.contains_key(identifier)
}

pub fn is_vector_func(identifier: &str) -> bool {
    VECTOR_FUNCS.contains_key(identifier)
}

pub fn is_constant(identifier: &str) -> bool {
    CONSTANTS.contains_key(identifier)
}

pub fn call_unary_func(
    context: &mut interpreter::Context,
    name: &str,
    x: KalkValue,
    angle_unit: &str,
) -> Option<(Result<KalkValue, KalkError>, String)> {
    if let Some((func_info, func_unit)) = UNARY_FUNCS.get(name) {
        Some((
            func_info.call(context, x, angle_unit),
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
) -> Option<(Result<KalkValue, KalkError>, String)> {
    if let Some((func_info, func_unit)) = BINARY_FUNCS.get(name) {
        Some((
            func_info.call(context, x, y, angle_unit),
            func_unit.to_string(),
        ))
    } else {
        None
    }
}

pub fn call_vector_func(name: &str, x: KalkValue) -> Option<Result<KalkValue, KalkError>> {
    VECTOR_FUNCS.get(name).map(|func_info| func_info.call(x))
}

fn to_angle_unit(context: &mut interpreter::Context, x: KalkValue, angle_unit: &str) -> KalkValue {
    match angle_unit {
        "rad" => x,
        _ => interpreter::convert_unit(
            context,
            &Expr::Literal(x.to_float()),
            Some(&String::from("rad")),
            Some(&angle_unit.to_string()),
        )
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
        _ => interpreter::convert_unit(
            context,
            &Expr::Literal(x.to_float()),
            Some(&angle_unit.to_string()),
            Some(&String::from("rad")),
        )
        .unwrap(),
    }
}

pub mod funcs {
    use std::cmp::Ordering;

    #[cfg(not(feature = "rug"))]
    pub use super::regular::funcs::*;
    use super::special_funcs::factorial;
    #[cfg(feature = "rug")]
    pub use super::with_rug::funcs::*;
    use crate::{
        as_number_or_return, as_vector_or_return, errors::KalkError, float, kalk_value::KalkValue,
    };

    pub fn abs(x: KalkValue) -> Result<KalkValue, KalkError> {
        let has_imaginary = x.has_imaginary();
        let (real, imaginary, unit) = as_number_or_return!(x);
        if has_imaginary {
            // |z| = sqrt(a² + b²)
            let a = real.clone() * real;
            let b = imaginary.clone() * imaginary;

            sqrt(KalkValue::Number(a + b, float!(0), unit))
        } else {
            Ok(KalkValue::Number(real.abs(), float!(0), unit))
        }
    }

    pub fn acos(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, unit) = as_number_or_return!(x.clone());
        if x.has_imaginary() || !(-1f64..=1f64).contains(&real) {
            // -i * ln(i * sqrt(1 - z²) + z)
            let root =
                sqrt(KalkValue::from(1f64).sub_without_unit(&x.clone().mul_without_unit(&x)?)?)?;
            let iroot = multiply_with_i(root)?;
            let ln = ln(iroot.add_without_unit(&x)?)?;
            let (ln_real, ln_imaginary, ln_unit) = as_number_or_return!(ln);
            // -iz = -i(a + bi) = b - ai
            Ok(KalkValue::Number(ln_imaginary, -ln_real, ln_unit))
        } else {
            Ok(KalkValue::Number(real.acos(), float!(0), unit))
        }
    }

    pub fn acosh(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, imaginary, unit) = as_number_or_return!(x.clone());
        if x.has_imaginary() || real < 1f64 {
            let sqrt1 = sqrt(KalkValue::Number(
                real.clone() + 1f64,
                imaginary.clone(),
                unit.clone(),
            ))?;
            let sqrt2 = sqrt(KalkValue::Number(real - 1f64, imaginary, unit))?;

            ln(x.add_without_unit(&sqrt1.mul_without_unit(&sqrt2)?)?)
        } else {
            Ok(KalkValue::Number(real.acosh(), float!(0), unit))
        }
    }

    pub fn acot(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, unit) = as_number_or_return!(x.clone());
        if x.has_imaginary() {
            // atan(1/z)
            atan(KalkValue::from(1f64).div_without_unit(&x)?)
        } else {
            Ok(KalkValue::Number((1f64 / real).atan(), float!(0), unit))
        }
    }

    pub fn acoth(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, unit) = as_number_or_return!(x.clone());
        if x.has_imaginary() || real <= 1f64 || real >= -1f64 {
            // 1 / z
            let (inv_real, inv_imaginary, inv_unit) =
                as_number_or_return!(KalkValue::from(1f64).div_without_unit(&x)?);
            let ln1 = ln(KalkValue::Number(
                1f64 + inv_real.clone(),
                inv_imaginary.clone(),
                inv_unit.clone(),
            ))?;
            let ln2 = ln(KalkValue::Number(1f64 - inv_real, -inv_imaginary, inv_unit))?;

            ln1.sub_without_unit(&ln2)?
                .div_without_unit(&KalkValue::from(2f64))
        } else {
            Ok(KalkValue::Number((1f64 / real).atanh(), float!(0), unit))
        }
    }

    pub fn acsc(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, unit) = as_number_or_return!(x.clone());
        if x.has_imaginary() || real < 1f64 || real > -1f64 {
            // asin(1/z)
            asin(KalkValue::from(1f64).div_without_unit(&x)?)
        } else {
            Ok(KalkValue::Number((1f64 / real).asin(), float!(0), unit))
        }
    }

    pub fn acsch(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, unit) = as_number_or_return!(x.clone());
        if x.has_imaginary() || real == 0f64 {
            let (inv_x2_real, inv_x2_imaginary, inv_x2_unit) = as_number_or_return!(
                KalkValue::from(1f64).div_without_unit(&x.clone().mul_without_unit(&x)?)?
            );
            let sqrt = sqrt(KalkValue::Number(
                1f64 + inv_x2_real,
                inv_x2_imaginary,
                inv_x2_unit,
            ))?;
            let inv_x = KalkValue::from(1f64).div_without_unit(&x)?;

            ln(sqrt.add_without_unit(&inv_x)?)
        } else {
            Ok(KalkValue::Number((1f64 / real).asinh(), float!(0), unit))
        }
    }

    pub fn asec(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, unit) = as_number_or_return!(x.clone());
        if x.has_imaginary() || real < 1f64 || real > -1f64 {
            // acos(1/z)
            acos(KalkValue::from(1f64).div_without_unit(&x)?)
        } else {
            Ok(KalkValue::Number((1f64 / real).acos(), float!(0), unit))
        }
    }

    pub fn asech(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, unit) = as_number_or_return!(x.clone());
        if x.has_imaginary() || real <= 0f64 || real > 1f64 {
            // 1/z
            let inv = KalkValue::from(1f64).div_without_unit(&x)?;
            let (inv_real, inv_imaginary, inv_unit) = as_number_or_return!(inv.clone());
            // sqrt(1/z - 1)
            let sqrt1 = sqrt(KalkValue::Number(
                inv_real.clone() - 1f64,
                inv_imaginary.clone(),
                inv_unit.clone(),
            ))?;
            // sqrt(1/z + 1)
            let sqrt2 = sqrt(KalkValue::Number(inv_real + 1f64, inv_imaginary, inv_unit))?;

            // ln(1/z + sqrt(1/z - 1) * sqrt(1/z + 1))
            ln(sqrt1.mul_without_unit(&sqrt2)?.add_without_unit(&inv)?)
        } else {
            Ok(KalkValue::Number((1f64 / real).acosh(), float!(0), unit))
        }
    }

    pub fn asin(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, unit) = as_number_or_return!(x.clone());
        if x.has_imaginary() || !(-1f64..=1f64).contains(&real) {
            // i * ln(sqrt(1 - z²) - iz)
            let root =
                sqrt(KalkValue::from(1f64).sub_without_unit(&x.clone().mul_without_unit(&x)?)?)?;
            let iz = multiply_with_i(x)?;
            let ln = ln(root.sub_without_unit(&iz)?)?;
            multiply_with_i(ln)
        } else {
            Ok(KalkValue::Number(real.asin(), float!(0), unit))
        }
    }

    pub fn asinh(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, unit) = as_number_or_return!(x.clone());
        if x.has_imaginary() {
            let (x2_real, x2_imaginary, x2_unit) =
                as_number_or_return!(x.clone().mul_without_unit(&x)?);
            let sqrt = sqrt(KalkValue::Number(x2_real + 1f64, x2_imaginary, x2_unit))?;

            ln(x.add_without_unit(&sqrt)?)
        } else {
            Ok(KalkValue::Number(real.asinh(), float!(0), unit))
        }
    }

    pub fn atan(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, unit) = as_number_or_return!(x.clone());
        if x.has_imaginary() {
            let mul_i = multiply_with_i(x)?;
            let (iz_real, iz_imaginary, iz_unit) = as_number_or_return!(mul_i);

            // 1 - iz
            let neg = KalkValue::Number(
                1f64 - iz_real.clone(),
                -iz_imaginary.clone(),
                iz_unit.clone(),
            );

            // 1 + iz
            let pos = KalkValue::Number(1f64 + iz_real, iz_imaginary, iz_unit);

            // ln(1 - iz) - ln(1 + iz)
            let ln = ln(neg)?.sub_without_unit(&ln(pos)?)?;

            multiply_with_i(ln)?.div_without_unit(&KalkValue::from(2f64))
        } else {
            Ok(KalkValue::Number(real.atan(), float!(0), unit))
        }
    }

    pub fn atanh(x: KalkValue) -> Result<KalkValue, KalkError> {
        let has_imaginary = x.has_imaginary();
        let (real, imaginary, unit) = as_number_or_return!(x);
        if has_imaginary || real >= 1f64 || real <= -1f64 {
            // 1/2 * log(z + 1) - 1/2 * log(-z + 1)
            let log1 = ln(KalkValue::Number(
                1f64 + real.clone(),
                imaginary.clone(),
                unit.clone(),
            ))?;
            let log2 = ln(KalkValue::Number(1f64 - real, -imaginary, unit))?;

            log1.sub_without_unit(&log2)?
                .div_without_unit(&KalkValue::from(2f64))
        } else {
            Ok(KalkValue::Number(real.atanh(), float!(0), unit))
        }
    }

    pub fn average(x: KalkValue) -> Result<KalkValue, KalkError> {
        let values = as_vector_or_return!(x);
        let count = values.len() as i64;
        let mut sum_real = float!(0);
        let mut sum_imaginary = float!(0);
        for value in values {
            let (real, imaginary, _) = as_number_or_return!(value);
            sum_real += real;
            sum_imaginary += imaginary;
        }

        KalkValue::Number(sum_real, sum_imaginary, None).div_without_unit(&KalkValue::from(count))
    }

    pub fn cbrt(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, unit) = as_number_or_return!(x);

        Ok(KalkValue::Number(real.cbrt(), float!(0), unit))
    }

    pub fn ceil(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, imaginary, unit) = as_number_or_return!(x);

        Ok(KalkValue::Number(real.ceil(), imaginary.ceil(), unit))
    }

    pub fn cos(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, imaginary, _) = as_number_or_return!(x);

        Ok(KalkValue::Number(
            real.clone().cos() * imaginary.clone().cosh(),
            -real.sin() * imaginary.sinh(),
            None,
        ))
    }

    pub fn cosh(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, imaginary, _) = as_number_or_return!(x);

        Ok(KalkValue::Number(
            real.clone().cosh() * imaginary.clone().cos(),
            real.sinh() * imaginary.sin(),
            None,
        ))
    }

    pub fn csc(x: KalkValue) -> Result<KalkValue, KalkError> {
        KalkValue::from(1f64).div_without_unit(&sin(x)?)
    }

    pub fn csch(x: KalkValue) -> Result<KalkValue, KalkError> {
        KalkValue::from(1f64).div_without_unit(&sinh(x)?)
    }

    pub fn cot(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, imaginary, _) = as_number_or_return!(x);

        let a = real * 2f64;
        let b = imaginary * 2f64;
        Ok(KalkValue::Number(
            -a.clone().sin() / (a.clone().cos() - b.clone().cosh()),
            b.clone().sinh() / (a.cos() - b.cosh()),
            None,
        ))
    }

    pub fn coth(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, imaginary, _) = as_number_or_return!(x);
        let a = real * 2f64;
        let b = imaginary * 2f64;
        Ok(KalkValue::Number(
            -a.clone().sinh() / (b.clone().cos() - a.clone().cosh()),
            b.clone().sin() / (b.cos() - a.cosh()),
            None,
        ))
    }

    pub fn diag(x: KalkValue) -> Result<KalkValue, KalkError> {
        if let KalkValue::Vector(values) = x {
            let mut result = vec![vec![KalkValue::from(0f64); values.len()]; values.len()];
            for (i, value) in values.iter().enumerate() {
                result[i][i] = value.clone();
            }

            Ok(KalkValue::Matrix(result))
        } else {
            Err(KalkError::UnexpectedType(
                x.get_type_name(),
                vec![String::from("vector")],
            ))
        }
    }

    pub fn exp(x: KalkValue) -> Result<KalkValue, KalkError> {
        let has_imaginary = x.has_imaginary();
        let (real, imaginary, unit) = as_number_or_return!(x);
        if has_imaginary {
            // e^a*cos(b) + ie^a*sin(b)
            let exp_a = real.exp();
            let b = imaginary;
            Ok(KalkValue::Number(
                exp_a.clone() * b.clone().cos(),
                exp_a * b.sin(),
                unit,
            ))
        } else {
            Ok(KalkValue::Number(real.exp(), float!(0), unit))
        }
    }

    pub fn floor(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, imaginary, unit) = as_number_or_return!(x);

        Ok(KalkValue::Number(real.floor(), imaginary.floor(), unit))
    }

    pub fn frac(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, imaginary, unit) = as_number_or_return!(x);

        Ok(KalkValue::Number(real.fract(), imaginary.fract(), unit))
    }

    pub fn gcd(x: KalkValue, y: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, imaginary, unit) = as_number_or_return!(x.clone());
        let (real_rhs, imaginary_rhs, _) = as_number_or_return!(y.clone());

        // Find the norm of a Gaussian integer
        fn norm(x: KalkValue) -> Result<KalkValue, KalkError> {
            let (real, imaginary, unit) = as_number_or_return!(x);
            Ok(KalkValue::Number(
                (real.clone() * real) + (imaginary.clone() * imaginary),
                float!(0),
                unit,
            ))
        }

        if x.has_imaginary() || y.has_imaginary() {
            if real.fract() != 0f64
                || real_rhs.fract() != 0f64
                || imaginary.fract() != 0f64
                || imaginary_rhs.fract() != 0f64
            {
                // Not a Gaussian integer!
                return Err(KalkError::Expected(String::from("a gaussian integer")));
            }

            // Partially derived from:
            // https://stackoverflow.com/a/52692832

            let a;
            let b;

            // Ensure a > b
            if norm(x.clone())?.values().0 < norm(y.clone())?.values().0 {
                a = y;
                b = x;
            } else {
                a = x;
                b = y;
            }

            let (b_real, b_imaginary, b_unit) = as_number_or_return!(b.clone());
            let (c_real, c_imaginary, c_unit) =
                as_number_or_return!(a.clone().div_without_unit(&b)?);
            if c_imaginary.clone().fract() == 0f64 {
                Ok(KalkValue::Number(b_real.abs(), b_imaginary, b_unit))
            } else {
                let rounded_c = KalkValue::Number(c_real.round(), c_imaginary.round(), c_unit);
                gcd(
                    a.sub_without_unit(&b.clone().mul_without_unit(&rounded_c)?)?,
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
            let mut x_a = real;
            let mut y_a = real_rhs;
            while !y_a.eq(&0f64) {
                let t = y_a.clone();
                y_a = x_a % y_a;
                x_a = t;
            }

            // Usually we'd need to return max(x, -x), but since we've handled negative
            // values above, that is unnecessary.
            Ok(KalkValue::Number(x_a, float!(0), unit))
        }
    }

    pub fn im(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (_, imaginary, unit) = as_number_or_return!(x);

        Ok(KalkValue::Number(imaginary, float!(0), unit))
    }

    pub fn iverson(x: KalkValue) -> Result<KalkValue, KalkError> {
        Ok(KalkValue::from(
            if let KalkValue::Boolean(boolean_value) = x {
                i32::from(boolean_value)
            } else {
                1
            },
        ))
    }

    pub fn append(x: KalkValue, y: KalkValue) -> Result<KalkValue, KalkError> {
        if let KalkValue::Vector(items) = x {
            let mut new_items = items.clone();
            new_items.push(y);

            Ok(KalkValue::Vector(new_items))
        } else {
            Err(KalkError::Expected(String::from("Vector")))
        }
    }

    //              ⎛           ⎞
    //              ⎜    ⎜a⎜    ⎟
    // lcm(a, b) =  ⎜ ───────── ⎟ × ⎜b⎜
    //              ⎜ gcd(a, b) ⎟
    //              ⎝           ⎠
    pub fn lcm(x: KalkValue, y: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, imaginary, unit) = as_number_or_return!(x.clone());
        let (real_rhs, imaginary_rhs, unit_rhs) = as_number_or_return!(y.clone());
        let gcd = gcd(x, y)?;
        let absx = KalkValue::Number(real.abs(), imaginary, unit);
        let absy = KalkValue::Number(real_rhs.abs(), imaginary_rhs, unit_rhs);
        absx.div_without_unit(&gcd)?.mul_without_unit(&absy)
    }

    pub fn log(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, unit) = as_number_or_return!(x.clone());
        if x.has_imaginary() || real < 0f64 {
            // ln(z) / ln(10)
            ln(x)?.div_without_unit(&KalkValue::from(10f64.ln()))
        } else {
            Ok(KalkValue::Number(real.log10(), float!(0), unit))
        }
    }

    pub fn logx(x: KalkValue, y: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, unit) = as_number_or_return!(x.clone());
        let (real_rhs, _, _) = as_number_or_return!(y.clone());
        if x.has_imaginary() || y.has_imaginary() || real < 0f64 || real_rhs < 0f64 {
            // ln(z) / ln(n)
            ln(x)?.div_without_unit(&ln(y)?)
        } else {
            Ok(KalkValue::Number(
                real.log10() / real_rhs.log10(),
                float!(0),
                unit,
            ))
        }
    }

    pub fn ln(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, unit) = as_number_or_return!(x.clone());
        if x.has_imaginary() || real < 0f64 {
            let r = abs(x.clone())?;
            // ln|z| + i * arg z
            ln(r)?.add_without_unit(&multiply_with_i(arg(x)?)?)
        } else {
            Ok(KalkValue::Number(real.ln(), float!(0), unit))
        }
    }

    pub fn length(x: KalkValue) -> Result<KalkValue, KalkError> {
        Ok(match x {
            KalkValue::Vector(values) => KalkValue::from(values.len() as f64),
            KalkValue::Matrix(rows) => KalkValue::from(rows.len() as f64),
            _ => KalkValue::from(0f64),
        })
    }

    pub fn matrix(x: KalkValue) -> Result<KalkValue, KalkError> {
        let rows = as_vector_or_return!(x);
        let column_width =
            if let KalkValue::Vector(first_vec) = rows.first().unwrap_or(&KalkValue::nan()) {
                first_vec.len()
            } else {
                0
            };

        let mut columns = Vec::new();
        for value in rows {
            let column = as_vector_or_return!(value);
            if column.len() != column_width {
                return Err(KalkError::InconsistentColumnWidths);
            }

            columns.push(column);
        }

        Ok(KalkValue::Matrix(columns))
    }

    pub fn max(x: KalkValue) -> Result<KalkValue, KalkError> {
        let values = as_vector_or_return!(x);
        let mut max = &values[0];
        for value in &values {
            if let KalkValue::Boolean(greater) = value.greater_than_without_unit(max)? {
                if greater {
                    max = value;
                }
            }
        }

        Ok(max.clone())
    }

    pub fn min(x: KalkValue) -> Result<KalkValue, KalkError> {
        let values = as_vector_or_return!(x);
        let mut min = &values[0];
        for value in &values {
            if let KalkValue::Boolean(less) = value.less_than_without_unit(min)? {
                if less {
                    min = value;
                }
            }
        }

        Ok(min.clone())
    }

    pub fn nth_root(x: KalkValue, n: KalkValue) -> Result<KalkValue, KalkError> {
        x.pow_without_unit(&KalkValue::from(1f64).div_without_unit(&n)?)
    }

    pub fn perms(x: KalkValue) -> Result<KalkValue, KalkError> {
        if let KalkValue::Vector(values) = sort(x)? {
            let mut result: Vec<Vec<KalkValue>> = vec![values];

            // Permutations in lexographic order: https://www.baeldung.com/cs/array-generate-all-permutations
            loop {
                let prev_values = result.last().unwrap();
                let mut i = prev_values.len() - 1;
                for _ in (1..prev_values.len()).rev() {
                    if let (KalkValue::Number(real, _, _), KalkValue::Number(real_2, _, _)) =
                        (&prev_values[i - 1], &prev_values[i])
                    {
                        if real >= real_2 {
                            i -= 1;
                        } else {
                            break;
                        }

                        // Needs to be checked inside the loop as well
                        // since the counter is of type usize, which
                        // can't be negative.
                        if i == 0 {
                            return Ok(KalkValue::Matrix(result));
                        }
                    }
                }

                if i == 0 {
                    return Ok(KalkValue::Matrix(result));
                }

                let pivot = if let KalkValue::Number(real, _, _) = &prev_values[i - 1] {
                    real
                } else {
                    return Err(KalkError::UnexpectedType(
                        prev_values[i - 1].get_type_name(),
                        vec![String::from("number")],
                    ));
                };

                let mut j = prev_values.len() - 1;
                while let KalkValue::Number(real, _, _) = &prev_values[j] {
                    if real <= pivot {
                        j -= 1;
                    } else {
                        break;
                    }
                }

                let mut new_values = prev_values.clone();
                new_values.swap(i - 1, j);

                // Reverse the part after new_values[i]
                i += 1;
                j = new_values.len();
                while i < j {
                    new_values.swap(i - 1, j - 1);
                    i += 1;
                    j -= 1;
                }

                result.push(new_values);
            }
        } else {
            Err(KalkError::UnexpectedType(
                String::from("unknown"),
                vec![String::from("vector")],
            ))
        }
    }

    pub fn prod(x: KalkValue) -> Result<KalkValue, KalkError> {
        let values = as_vector_or_return!(x);
        let mut prod = KalkValue::from(1f64);
        for value in values {
            prod = prod.mul_without_unit(&value)?;
        }

        Ok(prod)
    }

    pub fn re(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, unit) = as_number_or_return!(x);
        Ok(KalkValue::Number(real, float!(0), unit))
    }

    pub fn round(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, imaginary, unit) = as_number_or_return!(x);
        Ok(KalkValue::Number(real.round(), imaginary.round(), unit))
    }

    pub fn sec(x: KalkValue) -> Result<KalkValue, KalkError> {
        KalkValue::from(1f64).div_without_unit(&cos(x)?)
    }

    pub fn sech(x: KalkValue) -> Result<KalkValue, KalkError> {
        KalkValue::from(1f64).div_without_unit(&cosh(x)?)
    }

    pub fn sin(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, imaginary, _) = as_number_or_return!(x);
        Ok(KalkValue::Number(
            real.clone().sin() * imaginary.clone().cosh(),
            real.cos() * imaginary.sinh(),
            None,
        ))
    }

    pub fn sinh(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, imaginary, _) = as_number_or_return!(x);
        Ok(KalkValue::Number(
            real.clone().sinh() * imaginary.clone().cos(),
            real.cosh() * imaginary.sin(),
            None,
        ))
    }

    pub fn sgn(x: KalkValue) -> Result<KalkValue, KalkError> {
        if x.has_imaginary() {
            x.clone().div_without_unit(&abs(x)?)
        } else {
            let (real, _, unit) = as_number_or_return!(x);
            if real == 0f64 {
                Ok(KalkValue::Number(float!(0), float!(0), unit))
            } else {
                Ok(KalkValue::Number(real.signum(), float!(0), unit))
            }
        }
    }

    pub fn sort(x: KalkValue) -> Result<KalkValue, KalkError> {
        if let KalkValue::Vector(mut values) = x {
            values.sort_by(|a, b| {
                if let KalkValue::Boolean(true) =
                    a.eq_without_unit(b).unwrap_or_else(|_| KalkValue::nan())
                {
                    Ordering::Equal
                } else if let KalkValue::Boolean(true) = a
                    .greater_than_without_unit(b)
                    .unwrap_or_else(|_| KalkValue::nan())
                {
                    Ordering::Greater
                } else {
                    Ordering::Less
                }
            });

            Ok(KalkValue::Vector(values))
        } else {
            Err(KalkError::UnexpectedType(
                x.get_type_name(),
                vec![String::from("vector")],
            ))
        }
    }

    pub fn sqrt(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, imaginary, unit) = as_number_or_return!(x.clone());
        if x.has_imaginary() {
            let abs = abs(x)?;
            let (abs_real, _, abs_unit) = as_number_or_return!(abs);
            let r = abs_real;
            let a = real;
            let b = imaginary;

            // sqrt((|z| + a) / 2) + i * (b / |b|) * sqrt((|z| - a) / 2)
            Ok(KalkValue::Number(
                ((r.clone() + a.clone()) / 2f64).sqrt(),
                (b.clone() / b.abs()) * ((r - a) / 2f64).sqrt(),
                abs_unit,
            ))
        } else if real < 0f64 {
            Ok(KalkValue::Number(float!(0), real.abs().sqrt(), unit))
        } else {
            Ok(KalkValue::Number(real.sqrt(), float!(0), unit))
        }
    }

    pub fn sum(x: KalkValue) -> Result<KalkValue, KalkError> {
        let values = as_vector_or_return!(x);
        let mut sum = KalkValue::from(0f64);
        for value in values {
            sum = sum.add_without_unit(&value)?;
        }

        Ok(sum)
    }

    pub fn tan(x: KalkValue) -> Result<KalkValue, KalkError> {
        let has_imaginary = x.has_imaginary();
        let (real, imaginary, _) = as_number_or_return!(x);
        if has_imaginary {
            let a = real * 2f64;
            let b = imaginary * 2f64;
            Ok(KalkValue::Number(
                a.clone().sin() / (a.clone().cos() + b.clone().cosh()),
                b.clone().sinh() / (a.cos() + b.cosh()),
                None,
            ))
        } else {
            Ok(KalkValue::Number(real.tan(), float!(0), None))
        }
    }

    pub fn tanh(x: KalkValue) -> Result<KalkValue, KalkError> {
        let has_imaginary = x.has_imaginary();
        let (real, imaginary, _) = as_number_or_return!(x);
        if has_imaginary {
            let a = real * 2f64;
            let b = imaginary * 2f64;
            Ok(KalkValue::Number(
                a.clone().sinh() / (a.clone().cosh() + b.clone().cos()),
                b.clone().sin() / (a.cosh() + b.cos()),
                None,
            ))
        } else {
            Ok(KalkValue::Number(real.tanh(), float!(0), None))
        }
    }

    #[allow(clippy::needless_range_loop)]
    pub fn transpose(x: KalkValue) -> Result<KalkValue, KalkError> {
        if let KalkValue::Matrix(rows) = x {
            let original_row_count = rows.len();
            let original_column_count = rows.first().unwrap().len();
            let mut result =
                vec![vec![KalkValue::from(0f64); original_row_count]; original_column_count];
            for i in 0..original_row_count {
                for j in 0..original_column_count {
                    result[j][i] = rows[i][j].clone();
                }
            }

            Ok(KalkValue::Matrix(result))
        } else {
            Err(KalkError::UnexpectedType(
                x.get_type_name(),
                vec![String::from("matrix")],
            ))
        }
    }

    pub fn trunc(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, imaginary, unit) = as_number_or_return!(x);
        Ok(KalkValue::Number(real.trunc(), imaginary.trunc(), unit))
    }

    pub fn ncr(x: KalkValue, y: KalkValue) -> Result<KalkValue, KalkError> {
        factorial(x.clone())?.div_without_unit(
            &factorial(y.clone())?.mul_without_unit(&factorial(x.sub_without_unit(&y)?)?)?,
        )
    }

    pub fn npr(x: KalkValue, y: KalkValue) -> Result<KalkValue, KalkError> {
        factorial(x.clone())?.div_without_unit(&factorial(x.sub_without_unit(&y)?)?)
    }

    fn multiply_with_i(z: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, imaginary, unit) = as_number_or_return!(z);

        // iz = i(a + bi) = -b + ai
        Ok(KalkValue::Number(-imaginary, real, unit))
    }
}

#[cfg(test)]
mod tests {
    use super::funcs::*;
    use crate::errors::KalkError;
    use crate::float;
    use crate::prelude::KalkValue;
    use crate::test_helpers::cmp;

    fn val(x: f64) -> KalkValue {
        KalkValue::from(x)
    }

    #[test]
    fn test_unary_funcs() {
        let in_out = vec![
            (
                abs as fn(KalkValue) -> Result<KalkValue, KalkError>,
                (3f64, 4f64),
                (5f64, 0f64),
            ),
            (abs, (-3f64, 4f64), (5f64, 0f64)),
            (abs, (3f64, -4f64), (5f64, 0f64)),
            (abs, (-3f64, 0f64), (3f64, 0f64)),
        ];

        for (i, (func, input, expected_output)) in in_out.iter().enumerate() {
            let actual_output =
                func(KalkValue::Number(float!(input.0), float!(input.1), None)).unwrap();

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
                gcd as fn(KalkValue, KalkValue) -> Result<KalkValue, KalkError>,
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
                KalkValue::Number(float!(input.0 .0), float!(input.0 .1), None),
                KalkValue::Number(float!(input.1 .0), float!(input.1 .1), None),
            )
            .unwrap();

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
    fn test_perms() {
        let vecs = vec![
            (KalkValue::Vector(vec![val(1f64)]), 1),
            (KalkValue::Vector(vec![val(1f64), val(2f64)]), 2),
            (KalkValue::Vector(vec![val(1f64), val(2f64), val(3f64)]), 6),
            (KalkValue::Vector(vec![val(2f64), val(3f64), val(1f64)]), 6),
            (
                KalkValue::Vector(vec![val(2f64), val(3f64), val(1f64), val(3f64)]),
                12,
            ),
        ];
        for (vec, expected_len) in vecs {
            let permutations = if let KalkValue::Matrix(permutations) = perms(vec).unwrap() {
                permutations
            } else {
                unreachable!()
            };
            assert_eq!(permutations.len(), expected_len);

            let mut results = std::collections::HashSet::with_capacity(permutations.len());
            for permutation in permutations {
                let as_str = format!("{:?}", permutation);
                assert!(!results.contains(&as_str));
                results.insert(as_str);
            }
        }
    }

    #[test]
    fn test_transpose() {
        fn to_matrix(rows: Vec<Vec<i32>>) -> KalkValue {
            let mut new_rows = Vec::new();
            for row in rows {
                let mut new_row = Vec::new();
                for value in row {
                    new_row.push(KalkValue::from(value as f64));
                }

                new_rows.push(new_row);
            }

            KalkValue::Matrix(new_rows)
        }

        assert_eq!(
            transpose(to_matrix(vec![vec![1, 2], vec![3, 4]])).unwrap(),
            to_matrix(vec![vec![1, 3], vec![2, 4]])
        );

        assert_eq!(
            transpose(to_matrix(vec![vec![1, 2], vec![3, 4], vec![5, 6]])).unwrap(),
            to_matrix(vec![vec![1, 3, 5], vec![2, 4, 6]])
        );

        assert_eq!(
            transpose(to_matrix(vec![vec![1, 2, 3], vec![4, 5, 6], vec![7, 8, 9]])).unwrap(),
            to_matrix(vec![vec![1, 4, 7], vec![2, 5, 8], vec![3, 6, 9]])
        );
    }

    #[test]
    #[allow(clippy::approx_constant)]
    fn test_trig_funcs() {
        // Auto-generated using kalk/scripts/generate_funcs_test_cases.py
        let in_out = vec![
            (
                arg as fn(KalkValue) -> Result<KalkValue, KalkError>,
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
            let actual_output =
                func(KalkValue::Number(float!(input.0), float!(input.1), None)).unwrap();

            let expected_has_nan_or_inf = expected_output.0.is_nan()
                || expected_output.0.is_infinite()
                || expected_output.1.is_nan()
                || expected_output.1.is_infinite();
            let actual_has_nan_or_inf = actual_output.to_f64().is_nan()
                || actual_output.to_f64().is_infinite()
                || actual_output.imaginary_to_f64().is_nan()
                || actual_output.imaginary_to_f64().is_infinite();

            if expected_has_nan_or_inf || actual_has_nan_or_inf {
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
