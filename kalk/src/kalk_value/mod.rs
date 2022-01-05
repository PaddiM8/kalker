#[cfg(feature = "rug")]
pub mod with_rug;
#[cfg(feature = "rug")]
use rug::Float;
#[cfg(feature = "rug")]
pub use with_rug::*;

#[cfg(not(feature = "rug"))]
pub mod regular;
#[cfg(not(feature = "rug"))]
pub use regular::*;

mod rounding;

use crate::ast::Expr;
use crate::radix;
use lazy_static::lazy_static;
use std::collections::HashMap;
use wasm_bindgen::prelude::*;

const ACCEPTABLE_COMPARISON_MARGIN: f64 = 0.00000001;

lazy_static! {
    static ref CONSTANTS: HashMap<&'static str, &'static str> = {
        let mut m = HashMap::new();
        m.insert("3.141592", "π");
        m.insert("2.718281", "e");
        m.insert("6.283185", "τ");
        m.insert("1.618033", "ϕ");
        m.insert("1.414213", "√2");
        // Radian values for common angles
        m.insert("0.523598", "π/6");
        m.insert("0.785398", "π/4");
        m.insert("1.047197", "π/3");
        m.insert("1.570796", "π/2");
        m.insert("2.094395", "2π/3");
        m.insert("2.356194", "3π/4");
        m.insert("2.617993", "5π/6");
        m.insert("3.665191", "7π/6");
        m.insert("3.926990", "5π/4");
        m.insert("4.188790", "4π/3");
        m.insert("4.712388", "3π/2");
        m.insert("5.23598", "5π/3");
        m.insert("5.497787", "7π/4");
        m.insert("5.759586", "11π/6");
        m.insert("6.283185", "2π");
        m.insert("0.866025", "√3/2");
        m
    };
}

#[macro_export]
#[cfg(not(feature = "rug"))]
macro_rules! float {
    ($x:expr) => {{
        $x.clone() as f64
    }};
}

#[macro_export]
#[cfg(feature = "rug")]
macro_rules! float {
    ($x:expr) => {{
        use rug::Float;
        Float::with_val(63, $x)
    }};
}

#[macro_export]
#[cfg(not(feature = "rug"))]
macro_rules! primitive {
    ($x:expr) => {{
        $x.clone()
    }};
}

#[macro_export]
#[cfg(feature = "rug")]
macro_rules! primitive {
    ($x:expr) => {{
        $x.to_f64()
    }};
}

#[macro_export]
macro_rules! as_number_or_return {
    ($x:expr) => {{
        if let KalkValue::Number(real, imaginary, unit) = $x {
            (
                real,
                if imaginary == -0f64 {
                    float!(0)
                } else {
                    imaginary
                },
                unit,
            )
        } else {
            return KalkValue::nan();
        }
    }};
}

#[macro_export]
macro_rules! as_vector_or_return {
    ($x:expr) => {{
        if let KalkValue::Vector(values) = $x {
            if values.len() == 0 {
                return KalkValue::nan();
            }

            values
        } else {
            return KalkValue::nan();
        }
    }};
}

#[macro_export]
macro_rules! as_number_or_zero {
    ($x:expr) => {{
        use crate::float;
        if let KalkValue::Number(real, imaginary, unit) = $x {
            (real, imaginary, unit)
        } else {
            (float!(0), float!(0), String::new())
        }
    }};
}

#[wasm_bindgen]
#[derive(Clone)]
pub struct ScientificNotation {
    pub negative: bool,
    pub value: f64,
    pub exponent: i32,
    pub imaginary: bool,
}

#[wasm_bindgen]
#[derive(PartialEq)]
pub enum ComplexNumberType {
    Real,
    Imaginary,
}

#[wasm_bindgen]
impl ScientificNotation {
    #[wasm_bindgen(js_name = toString)]
    pub fn to_string(&self) -> String {
        let sign = if self.negative { "-" } else { "" };
        let digits_and_mul = if self.value == 1f64 {
            String::new()
        } else {
            format!("{}×", format_number(self.value))
        };

        format!(
            "{}{}10^{} {}",
            sign,
            digits_and_mul,
            self.exponent - 1,
            if self.imaginary { "i" } else { "" }
        )
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum KalkValue {
    #[cfg(not(feature = "rug"))]
    Number(f64, f64, String),
    #[cfg(feature = "rug")]
    Number(Float, Float, String),
    Boolean(bool),
    Vector(Vec<KalkValue>),
}

impl KalkValue {
    pub fn nan() -> Self {
        KalkValue::Number(float!(f64::NAN), float!(0f64), String::new())
    }

    pub fn to_string(&self) -> String {
        match self {
            KalkValue::Number(real, imaginary, _) => {
                let as_str = format_number(primitive!(real));

                if self.has_imaginary() {
                    let imaginary_as_str = format_number(primitive!(imaginary).abs());
                    let sign = if imaginary < &0f64 { "-" } else { "+" };

                    format!("{} {} {}i", as_str, sign, imaginary_as_str)
                } else {
                    as_str
                }
            }
            KalkValue::Boolean(is_true) => {
                if *is_true {
                    String::from("true")
                } else {
                    String::from("false")
                }
            }
            KalkValue::Vector(values) => {
                let mut result = String::from("[");
                for value in values {
                    result.push_str(&value.to_string());
                    result.push_str(", ");
                }

                if values.len() > 0 {
                    result.pop();
                    result.pop();
                }

                result.push_str("]");

                result
            }
        }
    }

    pub fn to_string_big(&self) -> String {
        if let KalkValue::Number(real, imaginary, _) = self {
            if !self.has_imaginary() {
                return real.to_string();
            }

            let sign = if imaginary < &0f64 { "-" } else { "+" };
            format!("{} {} {}", real.to_string(), sign, imaginary.to_string())
        } else {
            self.to_string()
        }
    }

    pub fn to_string_real(&self, radix: u8) -> String {
        radix::to_radix_pretty(self.to_f64(), radix)
    }

    pub fn to_string_imaginary(&self, radix: u8, include_i: bool) -> String {
        let value = radix::to_radix_pretty(self.imaginary_to_f64(), radix);
        if include_i && value == "1" {
            String::from("i")
        } else if include_i && value == "-1" {
            String::from("-i")
        } else if include_i {
            format!("{}i", value)
        } else {
            value
        }
    }

    pub(crate) fn to_string_pretty_radix(&self, radix: u8) -> String {
        let (real, imaginary, unit) = match self {
            KalkValue::Number(real, imaginary, unit) => (real, imaginary, unit),
            _ => return self.to_string(),
        };

        let real_f64 = self.to_f64();
        let imaginary_f64 = self.imaginary_to_f64();
        if real_f64.is_nan() || imaginary_f64.is_nan() {
            return String::from("Not defined.");
        }

        if real_f64.is_infinite() {
            return format!("{}∞", if real_f64.is_sign_negative() { "-" } else { "" });
        }

        let sci_notation_real = self.to_scientific_notation(ComplexNumberType::Real);
        let mut new_real = real.clone();
        let mut new_imaginary = imaginary.clone();
        let result_str = if (-6..8).contains(&sci_notation_real.exponent) || real == &0f64 {
            self.to_string_real(radix)
        } else if sci_notation_real.exponent <= -14 {
            new_real = float!(0);
            String::from("0")
        } else {
            if radix == 10 {
                sci_notation_real.to_string().trim().to_string()
            } else {
                return String::new();
            }
        };

        let sci_notation_imaginary = self.to_scientific_notation(ComplexNumberType::Imaginary);
        let result_str_imaginary = if (-6..8).contains(&sci_notation_imaginary.exponent)
            || imaginary == &0f64
            || imaginary == &1f64
        {
            self.to_string_imaginary(radix, true)
        } else if sci_notation_imaginary.exponent <= -14 {
            new_imaginary = float!(0);
            String::from("0")
        } else {
            if radix == 10 {
                format!("{}", sci_notation_imaginary.to_string().trim())
            } else {
                return String::new();
            }
        };

        let mut output = result_str;
        if imaginary != &0f64 && new_imaginary != 0f64 && result_str_imaginary != "0" {
            // If the real value is 0, and there is an imaginary one,
            // clear the output so that the real value is not shown.
            if output == "0" {
                output = String::new();
            }

            // If there is a real value as well
            if output.len() > 0 {
                output.push_str(&format!(
                    " {} {}",
                    if imaginary < &0f64 { "-" } else { "+" },
                    result_str_imaginary.trim_start_matches("-"),
                ));
            } else {
                output.push_str(&format!("{}", result_str_imaginary));
            }
        }

        if unit != "" {
            output.push_str(&format!(" {}", unit));
        }

        let new_value = KalkValue::Number(new_real, new_imaginary, unit.clone());

        if let Some(estimate) = new_value.estimate() {
            if estimate != output && radix == 10 {
                output.push_str(&format!(" ≈ {}", estimate));
            }
        }

        output
    }

    pub fn to_string_pretty(&self) -> String {
        self.to_string_pretty_radix(10)
    }

    pub fn to_string_with_unit(&self) -> String {
        match self {
            KalkValue::Number(_, _, unit) => format!("{} {}", self.to_string(), unit),
            _ => self.to_string(),
        }
    }

    /// Get an estimate of what the number is, eg. 3.141592 => π. Does not work properly with scientific notation.
    pub fn estimate(&self) -> Option<String> {
        let rounded_real = rounding::estimate(self, ComplexNumberType::Real);
        let rounded_imaginary = rounding::estimate(self, ComplexNumberType::Imaginary);

        if let (None, None) = (&rounded_real, &rounded_imaginary) {
            return None;
        }

        let mut output = String::new();
        if let Some(value) = rounded_real {
            output.push_str(&value);
        } else if self.has_real() {
            output.push_str(&self.to_string_real(10));
        }

        let imaginary_value = if let Some(value) = rounded_imaginary {
            Some(value)
        } else if self.has_imaginary() {
            Some(self.to_string_imaginary(10, false))
        } else {
            None
        };

        if let Some(value) = imaginary_value {
            // Clear output if it's just 0.
            if output == "0" {
                output = String::new();
            }

            if value == "0" {
                // If both values ended up being estimated as zero,
                // return zero.
                if output.len() == 0 {
                    return Some(String::from("0"));
                }
            } else {
                let sign = if value.starts_with("-") { "-" } else { "+" };
                let value = match value.as_ref() {
                    "1" => String::from("i"),
                    "-1" => String::from("-i"),
                    _ => format!("{}i", value),
                };

                // If there is a real value as well
                if output.len() > 0 {
                    output.push_str(&format!(" {} {}", sign, value.trim_start_matches("-")));
                } else {
                    output.push_str(&value);
                }
            }
        }

        Some(output)
    }

    /// Basic up/down rounding from 0.00xxx or 0.999xxx or xx.000xxx, etc.
    pub fn round(&self) -> Option<KalkValue> {
        let rounded_real = rounding::round(self, ComplexNumberType::Real);
        let rounded_imaginary = rounding::round(self, ComplexNumberType::Imaginary);

        if let (None, None) = (&rounded_real, &rounded_imaginary) {
            return None;
        }

        let (original_real, original_imaginary, unit) = match self {
            KalkValue::Number(real, imaginary, unit) => (real, imaginary, unit),
            _ => return None,
        };

        Some(KalkValue::Number(
            if let Some(KalkValue::Number(real, _, _)) = rounded_real {
                real
            } else {
                original_real.clone()
            },
            if let Some(KalkValue::Number(_, imaginary, _)) = rounded_imaginary {
                imaginary
            } else {
                original_imaginary.clone()
            },
            unit.to_string(),
        ))
    }

    pub fn round_if_needed(self) -> KalkValue {
        if let Some(rounded) = self.round() {
            rounded
        } else {
            self
        }
    }

    pub fn has_real(&self) -> bool {
        if let KalkValue::Number(real, _, _) = self {
            real != &0f64
        } else {
            false
        }
    }

    pub fn has_imaginary(&self) -> bool {
        if let KalkValue::Number(_, imaginary, _) = self {
            imaginary != &0f64
        } else {
            false
        }
    }

    pub fn is_nan(&self) -> bool {
        if let KalkValue::Number(real, imaginary, _) = self {
            real.is_nan() || imaginary.is_nan()
        } else {
            false
        }
    }

    pub fn to_scientific_notation(
        &self,
        complex_number_type: ComplexNumberType,
    ) -> ScientificNotation {
        let value = match complex_number_type {
            ComplexNumberType::Real => self.to_f64(),
            ComplexNumberType::Imaginary => self.imaginary_to_f64(),
        };
        let exponent = value.clone().abs().log10().floor() as i32 + 1;

        ScientificNotation {
            negative: value < 0f64,
            value: value / (10f64.powf(exponent as f64 - 1f64) as f64),
            // I... am not sure what else to do...
            exponent,
            imaginary: complex_number_type == ComplexNumberType::Imaginary,
        }
    }

    pub fn has_unit(&self) -> bool {
        if let KalkValue::Number(_, _, unit) = self {
            unit.len() > 0
        } else {
            false
        }
    }

    pub fn get_unit(&self) -> String {
        if let KalkValue::Number(_, _, unit) = self {
            unit.to_string()
        } else {
            String::new()
        }
    }

    pub(crate) fn convert_to_unit(
        &self,
        context: &mut crate::interpreter::Context,
        to_unit: &str,
    ) -> Option<KalkValue> {
        if let KalkValue::Number(real, _, unit) = self {
            let result = crate::interpreter::convert_unit(
                context,
                &Expr::Literal(primitive!(real)),
                &unit,
                to_unit,
            );

            if let Ok(num) = result {
                Some(num)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub(crate) fn add(
        self,
        context: &mut crate::interpreter::Context,
        rhs: KalkValue,
    ) -> KalkValue {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        self.add_without_unit(&right)
    }

    pub(crate) fn sub(
        self,
        context: &mut crate::interpreter::Context,
        rhs: KalkValue,
    ) -> KalkValue {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        self.sub_without_unit(&right)
    }

    pub(crate) fn mul(
        self,
        context: &mut crate::interpreter::Context,
        rhs: KalkValue,
    ) -> KalkValue {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        self.mul_without_unit(&right)
    }

    pub(crate) fn div(
        self,
        context: &mut crate::interpreter::Context,
        rhs: KalkValue,
    ) -> KalkValue {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs.clone());
        self.div_without_unit(&right)
    }

    pub(crate) fn pow(
        self,
        context: &mut crate::interpreter::Context,
        rhs: KalkValue,
    ) -> KalkValue {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        self.pow_without_unit(&right)
    }

    pub(crate) fn rem(
        self,
        context: &mut crate::interpreter::Context,
        rhs: KalkValue,
    ) -> KalkValue {
        if let KalkValue::Number(real, _, _) = &self {
            let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
            if let KalkValue::Number(right_real, _, right_unit) = right {
                KalkValue::Number(real % right_real, float!(0f64), right_unit)
            } else {
                self
            }
        } else {
            self
        }
    }

    pub(crate) fn eq(self, context: &mut crate::interpreter::Context, rhs: KalkValue) -> KalkValue {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        self.eq_without_unit(&right)
    }

    pub(crate) fn not_eq(
        self,
        context: &mut crate::interpreter::Context,
        rhs: KalkValue,
    ) -> KalkValue {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        self.not_eq_without_unit(&right)
    }

    pub(crate) fn greater_than(
        self,
        context: &mut crate::interpreter::Context,
        rhs: KalkValue,
    ) -> KalkValue {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        self.greater_than_without_unit(&right)
    }

    pub(crate) fn less_than(
        self,
        context: &mut crate::interpreter::Context,
        rhs: KalkValue,
    ) -> KalkValue {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        self.less_than_without_unit(&right)
    }

    pub(crate) fn greater_or_equals(
        self,
        context: &mut crate::interpreter::Context,
        rhs: KalkValue,
    ) -> KalkValue {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs.clone());
        if let (KalkValue::Boolean(greater), KalkValue::Boolean(equal)) = (
            self.greater_than_without_unit(&right),
            self.eq_without_unit(&right),
        ) {
            KalkValue::Boolean(greater || equal)
        } else {
            unreachable!()
        }
    }

    pub(crate) fn less_or_equals(
        self,
        context: &mut crate::interpreter::Context,
        rhs: KalkValue,
    ) -> KalkValue {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs.clone());
        if let (KalkValue::Boolean(less), KalkValue::Boolean(equal)) = (
            self.less_than_without_unit(&right),
            self.eq_without_unit(&right),
        ) {
            KalkValue::Boolean(less || equal)
        } else {
            unreachable!()
        }
    }

    pub(crate) fn add_without_unit(self, rhs: &KalkValue) -> KalkValue {
        match (self.clone(), rhs) {
            (
                KalkValue::Number(real, imaginary, _),
                KalkValue::Number(real_rhs, imaginary_rhs, unit),
            ) => KalkValue::Number(real + real_rhs, imaginary + imaginary_rhs, unit.to_string()),
            (KalkValue::Vector(_), _) | (_, KalkValue::Vector(_)) => {
                calculate_vector(self, rhs, &KalkValue::add_without_unit)
            }
            _ => KalkValue::nan(),
        }
    }

    pub(crate) fn sub_without_unit(self, rhs: &KalkValue) -> KalkValue {
        match (self.clone(), rhs) {
            (
                KalkValue::Number(real, imaginary, _),
                KalkValue::Number(real_rhs, imaginary_rhs, unit),
            ) => KalkValue::Number(real - real_rhs, imaginary - imaginary_rhs, unit.to_string()),
            (KalkValue::Vector(_), _) | (_, KalkValue::Vector(_)) => {
                calculate_vector(self, rhs, &KalkValue::sub_without_unit)
            }
            _ => KalkValue::nan(),
        }
    }

    pub(crate) fn mul_without_unit(self, rhs: &KalkValue) -> KalkValue {
        // (a + bi)(c + di) = ac + adi + bci + bdi²
        match (self.clone(), rhs) {
            (
                KalkValue::Number(real, imaginary, _),
                KalkValue::Number(real_rhs, imaginary_rhs, unit),
            ) => KalkValue::Number(
                real.clone() * real_rhs.clone() - imaginary.clone() * imaginary_rhs.clone(),
                real * imaginary_rhs + imaginary * real_rhs,
                unit.to_string(),
            ),
            (KalkValue::Vector(_), _) | (_, KalkValue::Vector(_)) => {
                calculate_vector(self, rhs, &KalkValue::mul_without_unit)
            }
            _ => KalkValue::nan(),
        }
    }

    pub(crate) fn div_without_unit(self, rhs: &KalkValue) -> KalkValue {
        match (self.clone(), rhs.clone()) {
            (
                KalkValue::Number(real, imaginary, _),
                KalkValue::Number(real_rhs, imaginary_rhs, unit),
            ) => {
                // Avoid unecessary calculations
                if imaginary == 0f64 && imaginary_rhs == 0f64 {
                    KalkValue::Number(real / real_rhs, float!(0f64), unit)
                } else {
                    // Multiply both the numerator and denominator
                    // with the conjugate of the denominator, and divide.
                    let conjugate = rhs.get_conjugate();
                    let (numerator, numerator_imaginary) =
                        self.clone().mul_without_unit(&conjugate.clone()).values();
                    let (denominator, _) = rhs.clone().mul_without_unit(&conjugate).values();
                    KalkValue::Number(
                        numerator / denominator.clone(),
                        numerator_imaginary / denominator,
                        unit,
                    )
                }
            }
            (KalkValue::Vector(_), _) | (_, KalkValue::Vector(_)) => {
                calculate_vector(self, rhs, &KalkValue::div_without_unit)
            }
            _ => KalkValue::nan(),
        }
    }

    pub(crate) fn pow_without_unit(self, rhs: &KalkValue) -> KalkValue {
        match (self.clone(), rhs) {
            (
                KalkValue::Number(real, imaginary, _),
                KalkValue::Number(real_rhs, imaginary_rhs, unit),
            ) => {
                if imaginary != 0f64 || imaginary_rhs != &0f64 || (real < 0f64 && real_rhs < &1f64)
                {
                    let a = real.clone();
                    let b = imaginary.clone();
                    let c = real_rhs;
                    let d = imaginary_rhs;
                    let arg = crate::prelude::funcs::arg(self).values().0;
                    let raised = a.clone() * a + b.clone() * b;
                    let exp =
                        pow(raised.clone(), c.clone() / 2f64) * (-d.clone() * arg.clone()).exp();
                    let polar = c * arg + d.clone() / 2f64 * raised.ln();

                    KalkValue::Number(
                        polar.clone().cos() * exp.clone(),
                        polar.sin() * exp,
                        unit.to_string(),
                    )
                } else {
                    KalkValue::Number(pow(real, real_rhs.clone()), float!(0), unit.to_string())
                }
            }
            (KalkValue::Vector(_), _) | (_, KalkValue::Vector(_)) => {
                calculate_vector(self, rhs, &KalkValue::pow_without_unit)
            }
            _ => KalkValue::nan(),
        }
    }

    pub(crate) fn eq_without_unit(&self, rhs: &KalkValue) -> KalkValue {
        if self.has_imaginary() || rhs.has_imaginary() {
            return KalkValue::nan();
        }

        match (self, rhs) {
            (KalkValue::Number(real, _, _), KalkValue::Number(real_rhs, _, _)) => {
                KalkValue::Boolean(
                    (real.clone() - real_rhs.clone()).abs() < ACCEPTABLE_COMPARISON_MARGIN,
                )
            }
            (KalkValue::Vector(values), KalkValue::Vector(values_rhs)) => {
                let mut vecs_are_equal = true;
                for (value, value_rhs) in values.iter().zip(values_rhs) {
                    if let KalkValue::Boolean(are_equal) = value.eq_without_unit(&value_rhs) {
                        if !are_equal {
                            vecs_are_equal = false;
                        }
                    }
                }

                KalkValue::Boolean(vecs_are_equal)
            }
            _ => KalkValue::nan(),
        }
    }

    pub(crate) fn not_eq_without_unit(&self, rhs: &KalkValue) -> KalkValue {
        if self.has_imaginary() || rhs.has_imaginary() {
            return KalkValue::nan();
        }

        match (self, rhs) {
            (KalkValue::Number(real, _, _), KalkValue::Number(real_rhs, _, _)) => {
                KalkValue::Boolean(
                    (real.clone() - real_rhs.clone()).abs() > ACCEPTABLE_COMPARISON_MARGIN,
                )
            }
            _ => KalkValue::nan(),
        }
    }

    pub(crate) fn greater_than_without_unit(&self, rhs: &KalkValue) -> KalkValue {
        if self.has_imaginary() || rhs.has_imaginary() {
            return KalkValue::nan();
        }

        match (self, rhs) {
            (KalkValue::Number(real, _, _), KalkValue::Number(real_rhs, _, _)) => {
                KalkValue::Boolean(real.clone() - real_rhs.clone() > ACCEPTABLE_COMPARISON_MARGIN)
            }
            _ => KalkValue::nan(),
        }
    }

    pub(crate) fn less_than_without_unit(&self, rhs: &KalkValue) -> KalkValue {
        if self.has_imaginary() || rhs.has_imaginary() {
            return KalkValue::nan();
        }

        match (self, rhs) {
            (KalkValue::Number(real, _, _), KalkValue::Number(real_rhs, _, _)) => {
                KalkValue::Boolean(real.clone() - real_rhs.clone() < ACCEPTABLE_COMPARISON_MARGIN)
            }
            _ => KalkValue::nan(),
        }
    }

    pub fn get_conjugate(&self) -> KalkValue {
        match self {
            KalkValue::Number(real, imaginary, unit) => {
                KalkValue::Number(real.clone(), imaginary.clone() * (-1f64), unit.clone())
            }
            _ => KalkValue::nan(),
        }
    }
}

pub fn format_number(input: f64) -> String {
    let rounded = format!("{:.1$}", input, 10);
    if rounded.contains(".") {
        rounded
            .trim_end_matches('0')
            .trim_end_matches('.')
            .to_string()
    } else {
        rounded.into()
    }
}

fn calculate_vector(
    x: KalkValue,
    y: &KalkValue,
    action: &dyn Fn(KalkValue, &KalkValue) -> KalkValue,
) -> KalkValue {
    match (x, y) {
        (KalkValue::Vector(values), KalkValue::Number(_, _, _)) => {
            KalkValue::Vector(values.iter().map(|x| action(x.clone(), y)).collect())
        }
        (KalkValue::Number(_, _, _), KalkValue::Vector(values_rhs)) => {
            KalkValue::Vector(values_rhs.iter().map(|x| action(x.clone(), x)).collect())
        }
        (KalkValue::Vector(values), KalkValue::Vector(values_rhs)) => {
            if values.len() == values_rhs.len() {
                KalkValue::Vector(
                    values
                        .iter()
                        .zip(values_rhs)
                        .map(|(x, y)| x.clone().add_without_unit(&y))
                        .collect(),
                )
            } else {
                KalkValue::nan()
            }
        }
        _ => KalkValue::nan(),
    }
}

fn calculate_unit(
    context: &mut crate::interpreter::Context,
    left: &KalkValue,
    right: KalkValue,
) -> Option<KalkValue> {
    if let (KalkValue::Number(_, _, unit_left), KalkValue::Number(real_right, imaginary_right, _)) =
        (left, &right)
    {
        if left.has_unit() && right.has_unit() {
            right.convert_to_unit(context, unit_left)
        } else {
            Some(KalkValue::Number(
                real_right.clone(),
                imaginary_right.clone(),
                unit_left.to_string(),
            ))
        }
    } else {
        None
    }
}

#[cfg(not(feature = "rug"))]
fn pow(x: f64, y: f64) -> f64 {
    x.powf(y)
}

#[cfg(feature = "rug")]
fn pow(x: Float, y: Float) -> Float {
    use rug::ops::Pow;
    x.pow(y)
}

impl Into<String> for ScientificNotation {
    fn into(self) -> String {
        self.to_string()
    }
}

impl Into<String> for KalkValue {
    fn into(self) -> String {
        self.to_string()
    }
}

impl Into<f64> for KalkValue {
    fn into(self) -> f64 {
        self.to_f64()
    }
}

impl From<f64> for KalkValue {
    fn from(x: f64) -> Self {
        KalkValue::Number(float!(x), float!(0), String::new())
    }
}

impl From<f32> for KalkValue {
    fn from(x: f32) -> Self {
        KalkValue::Number(float!(x), float!(0), String::new())
    }
}

impl From<i128> for KalkValue {
    fn from(x: i128) -> Self {
        KalkValue::Number(float!(x), float!(0), String::new())
    }
}

impl From<i64> for KalkValue {
    fn from(x: i64) -> Self {
        KalkValue::Number(float!(x), float!(0), String::new())
    }
}

impl From<i32> for KalkValue {
    fn from(x: i32) -> Self {
        KalkValue::Number(float!(x), float!(0), String::new())
    }
}

#[cfg(test)]
mod tests {
    use crate::kalk_value::KalkValue;
    use crate::test_helpers::cmp;

    #[test]
    fn test_add_complex() {
        let in_out = vec![
            ((0f64, 0f64), (0f64, 0f64), (0f64, 0f64)),
            ((2f64, 0f64), (3f64, 4f64), (5f64, 4f64)),
            ((0f64, 2f64), (3f64, 4f64), (3f64, 6f64)),
            ((3f64, -2f64), (-3f64, 4f64), (0f64, 2f64)),
        ];

        for (a, b, expected_result) in in_out {
            let actual_result = KalkValue::Number(float!(a.0), float!(a.1), String::new())
                .add_without_unit(&KalkValue::Number(float!(b.0), float!(b.1), String::new()));
            assert_eq!(actual_result.to_f64(), expected_result.0);
            assert_eq!(actual_result.imaginary_to_f64(), expected_result.1);
        }
    }

    #[test]
    fn test_sub_complex() {
        let in_out = vec![
            ((0f64, 0f64), (0f64, 0f64), (0f64, 0f64)),
            ((2f64, 0f64), (3f64, 4f64), (-1f64, -4f64)),
            ((0f64, 2f64), (3f64, 4f64), (-3f64, -2f64)),
            ((3f64, -2f64), (-3f64, 4f64), (6f64, -6f64)),
        ];

        for (a, b, expected_result) in in_out {
            let actual_result = KalkValue::Number(float!(a.0), float!(a.1), String::new())
                .sub_without_unit(&KalkValue::Number(float!(b.0), float!(b.1), String::new()));
            assert_eq!(actual_result.to_f64(), expected_result.0);
            assert_eq!(actual_result.imaginary_to_f64(), expected_result.1);
        }
    }

    #[test]
    fn test_mul_complex() {
        let in_out = vec![
            ((0f64, 0f64), (0f64, 0f64), (0f64, 0f64)),
            ((2f64, 0f64), (3f64, 4f64), (6f64, 8f64)),
            ((0f64, 2f64), (3f64, 4f64), (-8f64, 6f64)),
            ((3f64, -2f64), (-3f64, 4f64), (-1f64, 18f64)),
        ];

        for (a, b, expected_result) in in_out {
            let actual_result = KalkValue::Number(float!(a.0), float!(a.1), String::new())
                .mul_without_unit(&KalkValue::Number(float!(b.0), float!(b.1), String::new()));
            assert_eq!(actual_result.to_f64(), expected_result.0);
            assert_eq!(actual_result.imaginary_to_f64(), expected_result.1);
        }
    }

    #[test]
    fn test_div_complex() {
        let in_out = vec![
            ((2f64, 0f64), (3f64, 4f64), (0.24f64, -0.32f64)),
            ((0f64, 2f64), (3f64, 4f64), (0.32f64, 0.24f64)),
            ((3f64, -2f64), (-3f64, 4f64), (-0.68f64, -0.24f64)),
        ];

        for (a, b, expected_result) in in_out {
            let actual_result = KalkValue::Number(float!(a.0), float!(a.1), String::new())
                .div_without_unit(&KalkValue::Number(float!(b.0), float!(b.1), String::new()));
            assert_eq!(actual_result.to_f64(), expected_result.0);
            assert_eq!(actual_result.imaginary_to_f64(), expected_result.1);
        }
    }

    #[test]
    fn test_pow_complex() {
        let in_out = vec![
            ((2f64, 0f64), (0f64, 3f64), (-0.4869944f64, 0.8734050f64)),
            ((2f64, 0f64), (2f64, 3f64), (-1.9479776f64, 3.4936203f64)),
            ((0f64, 2f64), (0f64, 3f64), (-0.0043748f64, 0.0078460f64)),
            ((3f64, 2f64), (0f64, 3f64), (-0.1304148f64, -0.111153f64)),
            ((3f64, 2f64), (4f64, 3f64), (28.8577819f64, -2.422530f64)),
            (
                (3f64, 0f64),
                (0f64, 1f64 / 3f64),
                (0.9336932f64, 0.3580738f64),
            ),
            (
                (3f64, 4f64),
                (0f64, 1f64 / 4f64),
                (0.7297490f64, 0.3105648f64),
            ),
        ];

        for (a, b, expected_result) in in_out {
            let actual_result = KalkValue::Number(float!(a.0), float!(a.1), String::new())
                .pow_without_unit(&KalkValue::Number(float!(b.0), float!(b.1), String::new()));
            assert!(cmp(actual_result.to_f64(), expected_result.0));
            assert!(cmp(actual_result.imaginary_to_f64(), expected_result.1));
        }
    }

    #[test]
    fn test_to_string_pretty() {
        let in_out = vec![
            (0.99999, 0.0, "0.99999 ≈ 1"),
            (-0.99999, 0.0, "-0.99999 ≈ -1"),
            (0.0, 0.99999, "0.99999i ≈ i"),
            (0.000000001, 0.0, "10^-9 ≈ 0"),
            (0.0, 0.000000001, "10^-9 i ≈ 0"),
            (0.99999, 0.999999, "0.99999 + 0.999999i ≈ 1 + i"),
            (1.0, 0.99999, "1 + 0.99999i ≈ 1 + i"),
            (-0.99999, 0.999999, "-0.99999 + 0.999999i ≈ -1 + i"),
            (0.99999, -0.999999, "0.99999 - 0.999999i ≈ 1 - i"),
            (-1.0, 0.99999, "-1 + 0.99999i ≈ -1 + i"),
            (1.0, -0.99999, "1 - 0.99999i ≈ 1 - i"),
            (-0.99999, 1.0, "-0.99999 + i ≈ -1 + i"),
            (0.99999, -1.0, "0.99999 - i ≈ 1 - i"),
            (0.000000001, 0.000000001, "10^-9 + 10^-9 i ≈ 0"),
            (1.0, 0.000000001, "1 + 10^-9 i ≈ 1"),
            (0.000000001, 1.0, "10^-9 + i ≈ i"),
            (-1.0, 0.000000001, "-1 + 10^-9 i ≈ -1"),
            (0.000000001, -1.0, "10^-9 - i ≈ -i"),
            (10e-17, 1.0, "i"),
            (1.0, 10e-17, "1"),
            (10e-16, 0.0, "0"),
            (3.00000000004, 0.0, "3"),
        ];
        for (real, imaginary, output) in in_out {
            let result = KalkValue::Number(float!(real), float!(imaginary), String::new())
                .to_string_pretty();
            assert_eq!(output, result);
        }
    }

    #[test]
    fn test_estimate() {
        let in_out = vec![
            (0.99999999, Some(String::from("1"))),
            (-0.9999999, Some(String::from("-1"))),
            (0.0000000001, Some(String::from("0"))),
            (-0.000000001, Some(String::from("0"))),
            (1.99999999, Some(String::from("2"))),
            (-1.9999999, Some(String::from("-2"))),
            (1.000000001, Some(String::from("1"))),
            (-1.000001, Some(String::from("-1"))),
            (0.5, Some(String::from("1/2"))),
            (-0.5, Some(String::from("-1/2"))),
            (0.3333333333, Some(String::from("1/3"))),
            (1.3333333333, Some(String::from("1 + 1/3"))),
            (-0.666666666, Some(String::from("-2/3"))),
            (-1.666666666, Some(String::from("-1 - 2/3"))),
            (-1.666666666, Some(String::from("-1 - 2/3"))),
            (100.33333333, Some(String::from("100 + 1/3"))),
            (-100.6666666, Some(String::from("-100 - 2/3"))),
            (0.9932611, None),
            (-0.9932611, None),
            (-0.00001, None),
            (1.9932611, None),
            (-1.9932611, None),
            (24f64, None),
            (-24f64, None),
            (1.23456f64, None),
            (-1.23456f64, None),
            (1.98, None),
            (-1.98, None),
            (9999999999f64, None),
            (-9999999999f64, None),
            (1000000001f64, None),
            (-1000000001f64, None),
            (0.53f64, None),
            (-0.53f64, None),
            (-1.51f64, None),
            (0.335f64, None),
            (-0.335f64, None),
            (0.665f64, None),
            (-0.665f64, None),
            (100f64, None),
            (-100f64, None),
            (1f64, None),
            (0.12f64, None),
            (0.1f64, None),
            (1.2f64, None),
            (1.23f64, None),
            (1.234f64, None),
            (1.2345f64, None),
            (1.23456f64, None),
            (1.234567f64, None),
            (1.2345678f64, None),
            (1.23456789f64, None),
            (-0.12f64, None),
            (-0.1f64, None),
            (-1.2f64, None),
            (-1.23f64, None),
            (-1.234f64, None),
            (-1.2345f64, None),
            (-1.23456f64, None),
            (-1.234567f64, None),
            (-1.2345678f64, None),
            (-1.23456789f64, None),
        ];

        for (input, output) in in_out {
            let result = KalkValue::from(input).estimate();
            println!("{}", input);
            assert_eq!(output, result);
        }
    }
}
