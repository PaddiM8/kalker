use lazy_static::lazy_static;
use std::collections::HashMap;
use FuncType::*;

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
