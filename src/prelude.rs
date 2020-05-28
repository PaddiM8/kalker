use crate::{math_parser, parser::Unit};
use std::collections::HashMap;

pub const CONSTANTS: &[(&str, &str)] = &[
    ("pi", "3.14159265"),
    ("π", "3.14159265"),
    ("e", "2.71828182"),
    ("tau", "6.28318530"),
    ("τ", "6.28318530"),
    ("phi", "1.61803398"),
    ("ϕ", "1.61803398"),
];

enum FuncType {
    Trig,
    InverseTrig,
    Other,
}

struct UnaryFuncInfo {
    func: Box<fn(f64) -> f64>,
    func_type: FuncType,
}

impl UnaryFuncInfo {
    fn call(&self, x: f64, angle_unit: &Unit) -> f64 {
        let func = *self.func;
        match self.func_type {
            FuncType::Trig => func(self.from_angle_unit(x, angle_unit)),
            FuncType::InverseTrig => self.to_angle_unit(func(x), angle_unit),
            FuncType::Other => func(x),
        }
    }

    fn to_angle_unit(&self, x: f64, angle_unit: &Unit) -> f64 {
        match angle_unit {
            Unit::Radians => x,
            Unit::Degrees => x.to_degrees(),
        }
    }

    fn from_angle_unit(&self, x: f64, angle_unit: &Unit) -> f64 {
        match angle_unit {
            Unit::Radians => x,
            Unit::Degrees => x.to_radians(),
        }
    }
}

pub struct Prelude {
    pub angle_unit: Unit,
    unary: HashMap<String, UnaryFuncInfo>,
}

impl Prelude {
    pub fn new() -> Self {
        Prelude {
            unary: HashMap::new(),
            angle_unit: math_parser::DEFAULT_ANGLE_UNIT,
        }
    }

    pub fn call_unary_func(&mut self, name: &str, x: f64) -> Option<f64> {
        if let Some(func_info) = self.unary.get(name) {
            Some(func_info.call(x, &self.angle_unit))
        } else {
            let trig_func: Option<fn(f64) -> f64> = match name {
                "cos" => Some(funcs::cos),
                "cosec" => Some(funcs::cosec),
                "cosech" => Some(funcs::cosech),
                "cosh" => Some(funcs::cosh),
                "cot" => Some(funcs::cot),
                "coth" => Some(funcs::coth),
                "sec" => Some(funcs::sec),
                "sech" => Some(funcs::sech),
                "sin" => Some(funcs::sin),
                "sinh" => Some(funcs::sinh),
                "tan" => Some(funcs::tan),
                "tanh" => Some(funcs::tanh),
                _ => None,
            };

            if let Some(func) = trig_func {
                let func_info = UnaryFuncInfo {
                    func: Box::new(func),
                    func_type: FuncType::Trig,
                };
                let value = func_info.call(x, &self.angle_unit);
                self.unary.insert(name.to_string(), func_info);

                return Some(value);
            }

            let inv_trig_func: Option<fn(f64) -> f64> = match name {
                "acos" => Some(funcs::acos),
                "acosh" => Some(funcs::acosh),
                "acot" => Some(funcs::acot),
                "acoth" => Some(funcs::acoth),
                "acosec" => Some(funcs::acosec),
                "asec" => Some(funcs::asec),
                "asech" => Some(funcs::asech),
                "asin" => Some(funcs::asin),
                "asinh" => Some(funcs::asinh),
                "atan" => Some(funcs::atan),
                "atanh" => Some(funcs::atanh),
                _ => None,
            };

            if let Some(func) = inv_trig_func {
                let func_info = UnaryFuncInfo {
                    func: Box::new(func),
                    func_type: FuncType::InverseTrig,
                };
                let value = func_info.call(x, &self.angle_unit);
                self.unary.insert(name.to_string(), func_info);

                return Some(value);
            }

            let misc_func: Option<fn(f64) -> f64> = match name {
                "abs" => Some(funcs::abs),
                "cbrt" => Some(funcs::cbrt),
                "ceil" => Some(funcs::ceil),
                "exp" => Some(funcs::exp),
                "floor" => Some(funcs::floor),
                "frac" => Some(funcs::frac),
                "log" => Some(funcs::log),
                "ln" => Some(funcs::ln),
                "round" => Some(funcs::round),
                "sqrt" => Some(funcs::sqrt),
                "trunc" => Some(funcs::trunc),
                _ => None,
            };

            if let Some(func) = misc_func {
                let func_info = UnaryFuncInfo {
                    func: Box::new(func),
                    func_type: FuncType::Other,
                };
                let value = func_info.call(x, &self.angle_unit);
                self.unary.insert(name.to_string(), func_info);

                return Some(value);
            } else {
                None
            }
        }
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
        (1f64 / x).sinh()
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
