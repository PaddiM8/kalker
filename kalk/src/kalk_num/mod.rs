#[cfg(feature = "rug")]
pub mod with_rug;
#[cfg(feature = "rug")]
use rug::ops::Pow;
#[cfg(feature = "rug")]
use rug::Float;
#[cfg(feature = "rug")]
pub use with_rug::*;

#[cfg(not(feature = "rug"))]
pub mod regular;
#[cfg(not(feature = "rug"))]
pub use regular::*;

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

#[wasm_bindgen]
#[derive(Clone)]
pub struct ScientificNotation {
    pub negative: bool,
    pub(crate) digits: String,
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
        if self.digits == "" {
            return String::from("0");
        }

        let sign = if self.negative { "-" } else { "" };
        let mut digits_and_mul = if self.digits == "1" {
            String::new()
        } else {
            format!("{}×", &self.digits)
        };

        if self.digits.len() > 1 {
            digits_and_mul.insert(1usize, '.');
        }

        format!(
            "{}{}10^{} {}",
            sign,
            digits_and_mul,
            self.exponent - 1,
            if self.imaginary { "i" } else { "" }
        )
    }
}

impl KalkNum {
    pub fn has_real(&self) -> bool {
        self.value != 0f64
    }

    pub fn has_imaginary(&self) -> bool {
        self.imaginary_value != 0f64
    }

    pub fn to_scientific_notation(
        &self,
        complex_number_type: ComplexNumberType,
    ) -> ScientificNotation {
        let value_string = match complex_number_type {
            ComplexNumberType::Real => self.to_string_real(10),
            ComplexNumberType::Imaginary => self.to_string_imaginary(10, false),
        }
        .trim_start_matches("-")
        .to_string();
        let trimmed = if value_string.contains(".") {
            value_string.trim_end_matches("0")
        } else {
            &value_string
        };
        let value = match complex_number_type {
            ComplexNumberType::Real => &self.value,
            ComplexNumberType::Imaginary => &self.imaginary_value,
        };

        ScientificNotation {
            negative: value < &0f64,
            digits: trimmed
                .to_string()
                .replace(".", "")
                .trim_start_matches("0")
                .to_string(),
            // I... am not sure what else to do...
            exponent: KalkNum::new(value.clone().abs().log10() + 1f64, "").to_i32(),
            imaginary: complex_number_type == ComplexNumberType::Imaginary,
        }
    }

    pub fn to_string(&self) -> String {
        let as_str = format_number(self.to_f64());

        if self.has_imaginary() {
            let imaginary_as_str = format_number(self.imaginary_to_f64().abs());
            let sign = if self.imaginary_value < 0f64 {
                "-"
            } else {
                "+"
            };

            format!("{} {} {}i", as_str, sign, imaginary_as_str)
        } else {
            as_str
        }
    }

    pub fn to_string_big(&self) -> String {
        if !self.has_imaginary() {
            self.value.to_string()
        } else {
            let sign = if self.imaginary_value < 0f64 {
                "-"
            } else {
                "+"
            };
            format!(
                "{} {} {}",
                self.value.to_string(),
                sign,
                self.imaginary_value.to_string()
            )
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

    fn to_string_pretty_radix(&self, radix: u8) -> String {
        if let Some(boolean_value) = self.boolean_value {
            return boolean_value.to_string();
        }

        let real_f64 = self.to_f64();
        let imaginary_f64 = self.imaginary_to_f64();
        if real_f64.is_nan() || imaginary_f64.is_nan() {
            return String::from("Not defined.");
        }

        if real_f64.is_infinite() {
            return format!("{}∞", if real_f64.is_sign_negative() { "-" } else { "" });
        }

        let sci_notation_real = self.to_scientific_notation(ComplexNumberType::Real);
        let mut adjusted_num = self.clone();
        let result_str = if (-6..8).contains(&sci_notation_real.exponent) || self.value == 0f64 {
            self.to_string_real(radix)
        } else if sci_notation_real.exponent <= -14 {
            adjusted_num.value = KalkNum::from(0f64).value;
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
            || self.imaginary_value == 0f64
            || self.imaginary_value == 1f64
        {
            self.to_string_imaginary(radix, true)
        } else if sci_notation_imaginary.exponent <= -14 {
            adjusted_num.imaginary_value = KalkNum::from(0f64).value;
            String::from("0")
        } else {
            if radix == 10 {
                format!("{}", sci_notation_imaginary.to_string().trim())
            } else {
                return String::new();
            }
        };

        let mut output = result_str;
        if adjusted_num.has_imaginary() && result_str_imaginary != "0" {
            // If the real value is 0, and there is an imaginary one,
            // clear the output so that the real value is not shown.
            if output == "0" {
                output = String::new();
            }

            // If there is a real value as well
            if output.len() > 0 {
                output.push_str(&format!(
                    " {} {}",
                    if self.imaginary_value < 0f64 {
                        "-"
                    } else {
                        "+"
                    },
                    result_str_imaginary.trim_start_matches("-"),
                ));
            } else {
                output.push_str(&format!("{}", result_str_imaginary));
            }
        }

        let unit = adjusted_num.get_unit();
        if unit != "" {
            output.push_str(&format!(" {}", unit));
        }

        if let Some(estimate) = adjusted_num.estimate() {
            if estimate != output && radix == 10 {
                output.push_str(&format!(" ≈ {}", estimate));
            }
        }

        output
    }

    pub fn to_string_pretty(&self) -> String {
        if let Some(other_radix) = self.other_radix {
            let with_other_radix = self.to_string_pretty_radix(other_radix);
            if with_other_radix != "" {
                return format!("{}\n{}", self.to_string_pretty_radix(10), with_other_radix);
            }
        }

        self.to_string_pretty_radix(10)
    }

    pub fn is_too_big(&self) -> bool {
        self.value.is_infinite()
    }

    pub fn to_string_with_unit(&self) -> String {
        format!("{} {}", self.to_string(), self.unit)
    }

    pub fn has_unit(&self) -> bool {
        self.unit.len() > 0
    }

    pub(crate) fn convert_to_unit(
        &self,
        context: &mut crate::interpreter::Context,
        to_unit: &str,
    ) -> Option<KalkNum> {
        let result = crate::interpreter::convert_unit(
            context,
            &Expr::Literal(self.to_f64()),
            &self.unit,
            to_unit,
        );

        if let Ok(num) = result {
            Some(num)
        } else {
            None
        }
    }

    pub(crate) fn add(self, context: &mut crate::interpreter::Context, rhs: KalkNum) -> KalkNum {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        self.add_without_unit(right)
    }

    pub(crate) fn sub(self, context: &mut crate::interpreter::Context, rhs: KalkNum) -> KalkNum {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        self.sub_without_unit(right)
    }

    pub(crate) fn mul(self, context: &mut crate::interpreter::Context, rhs: KalkNum) -> KalkNum {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        self.mul_without_unit(right)
    }

    pub(crate) fn div(self, context: &mut crate::interpreter::Context, rhs: KalkNum) -> KalkNum {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs.clone());
        self.div_without_unit(right)
    }

    pub(crate) fn rem(self, context: &mut crate::interpreter::Context, rhs: KalkNum) -> KalkNum {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        KalkNum::new(self.value % right.value, &right.unit)
    }

    pub(crate) fn eq(self, context: &mut crate::interpreter::Context, rhs: KalkNum) -> KalkNum {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        self.eq_without_unit(&right)
    }

    pub(crate) fn not_eq(self, context: &mut crate::interpreter::Context, rhs: KalkNum) -> KalkNum {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        self.not_eq_without_unit(&right)
    }

    pub(crate) fn greater_than(
        self,
        context: &mut crate::interpreter::Context,
        rhs: KalkNum,
    ) -> KalkNum {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        self.greater_than_without_unit(&right)
    }

    pub(crate) fn less_than(
        self,
        context: &mut crate::interpreter::Context,
        rhs: KalkNum,
    ) -> KalkNum {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        self.less_than_without_unit(&right)
    }

    pub(crate) fn greater_or_equals(
        self,
        context: &mut crate::interpreter::Context,
        rhs: KalkNum,
    ) -> KalkNum {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs.clone());
        let greater = self
            .greater_than_without_unit(&right)
            .boolean_value
            .unwrap();
        let equal = self.eq_without_unit(&right).boolean_value.unwrap();
        KalkNum::from_bool(greater || equal)
    }

    pub(crate) fn less_or_equals(
        self,
        context: &mut crate::interpreter::Context,
        rhs: KalkNum,
    ) -> KalkNum {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs.clone());
        let less = self.less_than_without_unit(&right).boolean_value.unwrap();
        let equal = self.eq_without_unit(&right).boolean_value.unwrap();
        KalkNum::from_bool(less || equal)
    }

    pub(crate) fn add_without_unit(self, rhs: KalkNum) -> KalkNum {
        KalkNum::new_with_imaginary(
            self.value + rhs.value,
            &rhs.unit,
            self.imaginary_value + rhs.imaginary_value,
        )
    }

    pub(crate) fn sub_without_unit(self, rhs: KalkNum) -> KalkNum {
        KalkNum::new_with_imaginary(
            self.value - rhs.value,
            &rhs.unit,
            self.imaginary_value - rhs.imaginary_value,
        )
    }

    pub(crate) fn mul_without_unit(self, rhs: KalkNum) -> KalkNum {
        // (a + bi)(c + di) = ac + adi + bci + bdi²
        KalkNum::new_with_imaginary(
            self.value.clone() * rhs.value.clone()
                - self.imaginary_value.clone() * rhs.imaginary_value.clone(),
            &rhs.unit,
            self.value * rhs.imaginary_value + self.imaginary_value * rhs.value,
        )
    }

    pub(crate) fn div_without_unit(self, rhs: KalkNum) -> KalkNum {
        // Avoid unecessary calculations
        if self.imaginary_value == 0f64 && rhs.imaginary_value == 0f64 {
            KalkNum::new(self.value / rhs.value, &rhs.unit)
        } else {
            // Multiply both the numerator and denominator
            // with the conjugate of the denominator, and divide.
            let conjugate = rhs.get_conjugate();
            let numerator = self.clone().mul_without_unit(conjugate.clone());
            let denominator = rhs.clone().mul_without_unit(conjugate);
            KalkNum::new_with_imaginary(
                numerator.value / denominator.value.clone(),
                &rhs.unit,
                numerator.imaginary_value / denominator.value,
            )
        }
    }

    pub(crate) fn eq_without_unit(&self, rhs: &KalkNum) -> KalkNum {
        if self.has_imaginary() || rhs.has_imaginary() {
            return KalkNum::from(f64::NAN);
        }

        KalkNum::from_bool(
            (self.value.clone() - rhs.value.clone()).abs() < ACCEPTABLE_COMPARISON_MARGIN,
        )
    }

    pub(crate) fn not_eq_without_unit(&self, rhs: &KalkNum) -> KalkNum {
        if self.has_imaginary() || rhs.has_imaginary() {
            return KalkNum::from(f64::NAN);
        }

        KalkNum::from_bool(
            (self.value.clone() - rhs.value.clone()).abs() > ACCEPTABLE_COMPARISON_MARGIN,
        )
    }

    pub(crate) fn greater_than_without_unit(&self, rhs: &KalkNum) -> KalkNum {
        if self.has_imaginary() || rhs.has_imaginary() {
            return KalkNum::from(f64::NAN);
        }

        KalkNum::from_bool(self.value.clone() - rhs.value.clone() > ACCEPTABLE_COMPARISON_MARGIN)
    }

    pub(crate) fn less_than_without_unit(&self, rhs: &KalkNum) -> KalkNum {
        if self.has_imaginary() || rhs.has_imaginary() {
            return KalkNum::from(f64::NAN);
        }

        KalkNum::from_bool(self.value.clone() - rhs.value.clone() < -ACCEPTABLE_COMPARISON_MARGIN)
    }

    pub fn get_conjugate(&self) -> KalkNum {
        KalkNum::new_with_imaginary(
            self.value.clone(),
            &self.unit,
            self.imaginary_value.clone() * (-1f64),
        )
    }

    /// Get an estimate of what the number is, eg. 3.141592 => π. Does not work properly with scientific notation.
    pub fn estimate(&self) -> Option<String> {
        let rounded_real = self.estimate_one_value(ComplexNumberType::Real);
        let rounded_imaginary = self.estimate_one_value(ComplexNumberType::Imaginary);

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

    fn estimate_one_value(&self, complex_number_type: ComplexNumberType) -> Option<String> {
        let (value, value_string) = match complex_number_type {
            ComplexNumberType::Real => (&self.value, self.to_string()),
            ComplexNumberType::Imaginary => {
                (&self.imaginary_value, self.to_string_imaginary(10, true))
            }
        };

        let fract = value.clone().fract().abs();
        let integer = value.clone().trunc();

        #[cfg(feature = "rug")]
        let fract_as_string = fract.to_f64().to_string();
        #[cfg(not(feature = "rug"))]
        let fract_as_string = fract.to_string();

        // If it's an integer, there's nothing that would be done to it.
        if fract == 0f64 {
            return None;
        }

        // Eg. 0.5 to 1/2
        let as_abs_string = value_string.trim_start_matches("-").to_string();
        let sign = if *value < 0f64 { "-" } else { "" };
        if as_abs_string.starts_with("0.5") {
            if as_abs_string.len() == 3 || (as_abs_string.len() > 6 && &as_abs_string[3..5] == "00")
            {
                return Some(format!("{}1/2", sign));
            }
        }

        // Eg. 1.33333333 to 1 + 1/3
        if fract_as_string.len() >= 7 {
            let first_five_decimals = &fract_as_string[2..7];
            if first_five_decimals == "33333" || first_five_decimals == "66666" {
                let fraction = match first_five_decimals.as_ref() {
                    "33333" => "1/3",
                    "66666" => "2/3",
                    _ => "?",
                };

                if integer == 0f64 {
                    return Some(format!("{}{}", sign, fraction));
                } else {
                    let explicit_sign = if sign == "" { "+" } else { "-" };
                    return Some(format!(
                        "{} {} {}",
                        trim_zeroes(&integer.to_string()),
                        explicit_sign,
                        fraction
                    ));
                }
            }
        }

        // Match with common numbers, eg. π, 2π/3, √2
        if as_abs_string.len() >= 8 {
            if let Some(constant) = CONSTANTS.get(&as_abs_string[0..8]) {
                return Some(format!("{}{}", sign, constant.to_string()));
            }
        }

        // If the value squared (and rounded) is an integer,
        // eg. x² is an integer,
        // then it can be expressed as sqrt(x²).
        // Ignore it if the square root of the result is an integer.
        if fract != 0f64 {
            let squared = KalkNum::new(value.clone() * value, "").round_if_needed();
            if squared.value.clone().sqrt().fract() != 0f64 && squared.value.clone().fract() == 0f64
            {
                return Some(format!("√{}", squared.to_string()));
            }
        }

        // If nothing above was relevant, simply round it off a bit, eg. from 0.99999 to 1
        let rounded = match complex_number_type {
            ComplexNumberType::Real => self.round_one_value(complex_number_type)?.value,
            ComplexNumberType::Imaginary => {
                self.round_one_value(complex_number_type)?.imaginary_value
            }
        };
        let rounded_str = rounded.to_string();
        Some(trim_zeroes(if rounded_str == "-0" {
            "0"
        } else {
            &rounded_str
        }))
    }

    /// Basic up/down rounding from 0.00xxx or 0.999xxx or xx.000xxx, etc.
    pub fn round(&self) -> Option<KalkNum> {
        let rounded_real = self.round_one_value(ComplexNumberType::Real);
        let rounded_imaginary = self.round_one_value(ComplexNumberType::Imaginary);

        if let (None, None) = (&rounded_real, &rounded_imaginary) {
            return None;
        }

        Some(KalkNum::new_with_imaginary(
            if let Some(rounded) = rounded_real {
                rounded.value
            } else {
                self.value.clone()
            },
            &self.unit,
            if let Some(rounded) = rounded_imaginary {
                rounded.imaginary_value
            } else {
                self.imaginary_value.clone()
            },
        ))
    }

    pub fn round_if_needed(&self) -> KalkNum {
        if let Some(value) = self.round() {
            value
        } else {
            self.clone() // Hmm
        }
    }

    fn round_one_value(&self, complex_number_type: ComplexNumberType) -> Option<KalkNum> {
        let value = match complex_number_type {
            ComplexNumberType::Real => &self.value,
            ComplexNumberType::Imaginary => &self.imaginary_value,
        };
        let sign = if *value < 0f64 { -1f64 } else { 1f64 };
        let fract = value.clone().abs().fract();
        let integer = value.clone().abs().trunc();

        // If it's zero something, don't do the rounding as aggressively.
        let (limit_floor, limit_ceil) = if integer == 0f64 {
            (-8f64, -5f64)
        } else {
            (-4f64, -6f64)
        };

        if fract.clone().log10() < limit_floor {
            // If eg. 0.00xxx
            let mut new_num = self.clone();
            let new_value = integer * sign;
            match complex_number_type {
                ComplexNumberType::Real => new_num.value = new_value,
                ComplexNumberType::Imaginary => new_num.imaginary_value = new_value,
            }

            Some(new_num)
        } else if (1f64 - fract.clone()).log10() < limit_ceil {
            // If eg. 0.999
            // .abs() this before ceiling to make sure it rounds correctly. The sign is re-added afterwards.
            let mut new_num = self.clone();
            let new_value = value.clone().abs().ceil() * sign;
            match complex_number_type {
                ComplexNumberType::Real => new_num.value = new_value,
                ComplexNumberType::Imaginary => new_num.imaginary_value = new_value,
            }

            Some(new_num)
        } else {
            None
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

fn calculate_unit(
    context: &mut crate::interpreter::Context,
    left: &KalkNum,
    right: KalkNum,
) -> Option<KalkNum> {
    if left.has_unit() && right.has_unit() {
        right.convert_to_unit(context, &left.unit)
    } else {
        Some(KalkNum::new_with_imaginary(
            right.value,
            &left.unit,
            right.imaginary_value,
        ))
    }
}

fn trim_zeroes(input: &str) -> String {
    if input.contains(".") {
        input
            .trim_end_matches("0")
            .trim_end_matches(".")
            .to_string()
    } else {
        input.into()
    }
}

impl Into<String> for ScientificNotation {
    fn into(self) -> String {
        self.to_string()
    }
}

impl Into<String> for KalkNum {
    fn into(self) -> String {
        self.to_string()
    }
}

impl Into<f64> for KalkNum {
    fn into(self) -> f64 {
        self.to_f64()
    }
}

#[cfg(test)]
mod tests {
    use crate::kalk_num::KalkNum;
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
            let actual_result =
                KalkNum::new_with_imaginary(KalkNum::from(a.0).value, "", KalkNum::from(a.1).value)
                    .add_without_unit(KalkNum::new_with_imaginary(
                        KalkNum::from(b.0).value,
                        "",
                        KalkNum::from(b.1).value,
                    ));
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
            let actual_result =
                KalkNum::new_with_imaginary(KalkNum::from(a.0).value, "", KalkNum::from(a.1).value)
                    .sub_without_unit(KalkNum::new_with_imaginary(
                        KalkNum::from(b.0).value,
                        "",
                        KalkNum::from(b.1).value,
                    ));
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
            let actual_result =
                KalkNum::new_with_imaginary(KalkNum::from(a.0).value, "", KalkNum::from(a.1).value)
                    .mul_without_unit(KalkNum::new_with_imaginary(
                        KalkNum::from(b.0).value,
                        "",
                        KalkNum::from(b.1).value,
                    ));
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
            let actual_result =
                KalkNum::new_with_imaginary(KalkNum::from(a.0).value, "", KalkNum::from(a.1).value)
                    .div_without_unit(KalkNum::new_with_imaginary(
                        KalkNum::from(b.0).value,
                        "",
                        KalkNum::from(b.1).value,
                    ));
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
            let actual_result =
                KalkNum::new_with_imaginary(KalkNum::from(a.0).value, "", KalkNum::from(a.1).value)
                    .pow_without_unit(KalkNum::new_with_imaginary(
                        KalkNum::from(b.0).value,
                        "",
                        KalkNum::from(b.1).value,
                    ));
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
            (10e-15, 0.0, "0"),
            (3.00000000004, 0.0, "3"),
        ];
        for (real, imaginary, output) in in_out {
            let result = KalkNum::new_with_imaginary(
                KalkNum::from(real).value,
                "",
                KalkNum::from(imaginary).value,
            )
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
            let result = KalkNum::from(input).estimate();
            println!("{}", input);
            assert_eq!(output, result);
        }
    }
}
