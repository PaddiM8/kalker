#[cfg(feature = "rug")]
pub mod with_rug;

#[cfg(feature = "rug")]
use rug::{ops::Pow, Float};

#[cfg(not(feature = "rug"))]
pub mod regular;

mod rounding;

use crate::ast::Expr;
use crate::errors::KalkError;
use crate::radix;
use wasm_bindgen::prelude::*;

use self::rounding::EstimationResult;

const ACCEPTABLE_COMPARISON_MARGIN: f64 = 0.00000001;

#[cfg(feature = "rug")]
pub(crate) type KalkFloat = rug::Float;

#[cfg(not(feature = "rug"))]
pub(crate) type KalkFloat = f64;

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
        Float::with_val(1024, $x)
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
            return Err(KalkError::UnexpectedType(
                $x.get_type_name(),
                vec![String::from("number")],
            ));
        }
    }};
}

#[macro_export]
macro_rules! as_vector_or_return {
    ($x:expr) => {{
        if let KalkValue::Vector(values) = $x {
            if values.len() == 0 {
                return Err(KalkError::Expected(String::from("a non-empty vector")));
            }

            values
        } else {
            return Err(KalkError::UnexpectedType(
                $x.get_type_name(),
                vec![String::from("vector")],
            ));
        }
    }};
}

#[macro_export]
macro_rules! as_number_or_zero {
    ($x:expr) => {{
        use $crate::float;
        if let KalkValue::Number(real, imaginary, unit) = $x {
            (real, imaginary, unit)
        } else {
            (float!(0), float!(0), None)
        }
    }};
}

#[wasm_bindgen]
#[derive(Clone)]
pub struct ScientificNotation {
    pub value: f64,
    pub exponent: i32,
    pub imaginary: bool,
}

#[wasm_bindgen]
#[derive(PartialEq, Eq)]
pub enum ComplexNumberType {
    Real,
    Imaginary,
}

#[wasm_bindgen]
#[derive(Clone, Copy)]
pub enum ScientificNotationFormat {
    Normal,
    Engineering,
}

#[wasm_bindgen]
impl ScientificNotation {
    #[wasm_bindgen(js_name = toString)]
    pub fn to_js_string(&self) -> String {
        self.to_string()
    }

    pub fn to_string_format(&self, format: ScientificNotationFormat) -> String {
        match format {
            ScientificNotationFormat::Normal => self.to_string(),
            ScientificNotationFormat::Engineering => self.to_string_eng(),
        }
    }

    fn to_string_eng(&self) -> String {
        let exponent = self.exponent - 1;
        let modulo = exponent % 3;
        let value = self.value * 10_f64.powi(modulo);

        ScientificNotation {
            value,
            exponent: exponent - modulo + 1,
            imaginary: self.imaginary,
        }
        .to_string()
    }
}

impl std::fmt::Display for ScientificNotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let digits_and_mul = if self.value == 1f64 {
            String::new()
        } else {
            format!("{}×", format_number(self.value))
        };

        write!(
            f,
            "{}10^{}{}",
            digits_and_mul,
            self.exponent - 1,
            if self.imaginary { " i" } else { "" }
        )
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum KalkValue {
    #[cfg(not(feature = "rug"))]
    Number(f64, f64, Option<String>),
    #[cfg(feature = "rug")]
    Number(Float, Float, Option<String>),
    Boolean(bool),
    Vector(Vec<KalkValue>),
    Matrix(Vec<Vec<KalkValue>>),
}

impl std::fmt::Display for KalkValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KalkValue::Number(real, imaginary, _) => {
                let as_str = format_number_big(real);

                if self.has_imaginary() {
                    let imaginary_as_str = format_number_big(&imaginary.clone().abs());
                    let sign = if imaginary < &0f64 { "-" } else { "+" };

                    if &as_str == "0" {
                        write!(f, "{}", imaginary_as_str)
                    } else {
                        write!(f, "{} {} {}i", as_str, sign, imaginary_as_str)
                    }
                } else {
                    write!(f, "{}", as_str)
                }
            }
            KalkValue::Boolean(is_true) => {
                if *is_true {
                    write!(f, "true")
                } else {
                    write!(f, "false")
                }
            }
            KalkValue::Vector(values) => {
                let get_estimation: fn(&KalkValue) -> String = |x| {
                    x.estimate()
                        .unwrap_or_else(|| EstimationResult {
                            value: x.to_string(),
                            is_exact: false,
                        })
                        .value
                };

                write!(
                    f,
                    "({})",
                    values
                        .iter()
                        .map(get_estimation)
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            KalkValue::Matrix(rows) => {
                let mut value_strings = Vec::new();
                let mut longest = 0;
                for row in rows {
                    for value in row {
                        let value_str = value
                            .estimate()
                            .unwrap_or_else(|| EstimationResult {
                                value: value.to_string(),
                                is_exact: false,
                            })
                            .value;
                        longest = longest.max(value_str.len());
                        value_strings.push(format!("{},", value_str));
                    }

                    value_strings.last_mut().unwrap().pop(); // Trailing comma
                    value_strings.push(String::from("\n"));
                }

                let mut result = String::from("[");
                for value_str in value_strings {
                    if value_str == "\n" {
                        result.push_str("\n ");
                    } else {
                        result.push_str(&format!("{:width$} ", value_str, width = longest + 1));
                    }
                }

                result.pop(); // Trailing new-line
                result.pop(); // Trailing space
                result.pop(); // Trailing space
                result.pop(); // Trailing comma
                result.push(']');

                write!(f, "{}", result)
            }
        }
    }
}

impl KalkValue {
    pub fn nan() -> Self {
        KalkValue::Number(float!(f64::NAN), float!(0f64), None)
    }

    pub fn get_type_name(&self) -> String {
        match self {
            KalkValue::Number(_, _, _) => String::from("number"),
            KalkValue::Boolean(_) => String::from("boolean"),
            KalkValue::Vector(_) => String::from("vector"),
            KalkValue::Matrix(_) => String::from("matrix"),
        }
    }

    pub fn to_string_big(&self) -> String {
        fn trim_num(num_str: String) -> String {
            num_str
                .trim_end_matches('0')
                .trim_end_matches('.')
                .to_string()
        }

        if let KalkValue::Number(real, imaginary, _) = self {
            if !self.has_imaginary() {
                return trim_num(real.to_string());
            }

            let sign = if imaginary < &0f64 { "-" } else { "+" };
            format!(
                "{} {} {}i",
                spaced(&trim_num(real.to_string())),
                sign,
                spaced(&trim_num(imaginary.to_string()))
            )
        } else {
            trim_num(self.to_string())
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

    pub fn to_string_pretty_radix(&self, radix: u8, format: ScientificNotationFormat) -> String {
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
        let mut has_scientific_notation = false;
        let is_engineering_mode = matches!(format, ScientificNotationFormat::Engineering);
        let result_str = if is_engineering_mode {
            has_scientific_notation = true;

            sci_notation_real.to_string_format(ScientificNotationFormat::Engineering)
        } else if (-6..8).contains(&sci_notation_real.exponent) || real == &0f64 {
            self.to_string_real(radix)
        } else if sci_notation_real.exponent <= -14 {
            new_real = float!(0);

            String::from("0")
        } else if radix == 10 {
            has_scientific_notation = true;

            sci_notation_real.to_string_format(format)
        } else {
            self.to_string_real(radix)
        };

        let sci_notation_imaginary = self.to_scientific_notation(ComplexNumberType::Imaginary);
        let result_str_imaginary = if is_engineering_mode {
            has_scientific_notation = true;

            sci_notation_imaginary.to_string_format(ScientificNotationFormat::Engineering)
        } else if (-6..8).contains(&sci_notation_imaginary.exponent)
            || imaginary == &0f64
            || imaginary == &1f64
        {
            self.to_string_imaginary(radix, true)
        } else if sci_notation_imaginary.exponent <= -14 {
            new_imaginary = float!(0);
            String::from("0")
        } else if radix == 10 {
            has_scientific_notation = true;

            sci_notation_imaginary.to_string_format(format)
        } else {
            self.to_string_real(radix)
        };

        let mut output = result_str;
        if imaginary != &0f64 && new_imaginary != 0f64 && result_str_imaginary != "0" {
            // If the real value is 0, and there is an imaginary one,
            // clear the output so that the real value is not shown.
            if output == "0" {
                output = String::new();
            }

            // If there is a real value as well
            if !output.is_empty() {
                output.push_str(&format!(
                    " {} {}",
                    if imaginary < &0f64 { "-" } else { "+" },
                    result_str_imaginary.trim_start_matches('-'),
                ));
            } else {
                output.push_str(&result_str_imaginary);
            }
        }

        if let Some(unit) = unit {
            output.push_str(&format!(" {}", unit));
        }

        let new_value = KalkValue::Number(new_real, new_imaginary, unit.clone());

        if let Some(estimate) = new_value.estimate() {
            if estimate.value != output && radix == 10 {
                let equal_sign = if estimate.is_exact { "=" } else { "≈" };
                output.push_str(&format!(" {equal_sign} {}", estimate.value));
            }
        } else if has_scientific_notation && !is_engineering_mode {
            output.insert_str(0, &format!("{} ≈ ", self));
        }

        output
    }

    pub fn to_string_pretty(&self, format: ScientificNotationFormat) -> String {
        self.to_string_pretty_radix(10, format)
    }

    pub fn to_string_with_unit(&self) -> String {
        match self {
            KalkValue::Number(_, _, unit) => {
                format!("{} {}", self, unit.as_ref().unwrap_or(&String::new()))
            }
            _ => self.to_string(),
        }
    }

    /// Get an estimate of what the number is, eg. 3.141592 => π. Does not work properly with scientific notation.
    pub fn estimate(&self) -> Option<EstimationResult> {
        let rounded_real = rounding::estimate(self, ComplexNumberType::Real);
        let rounded_imaginary = rounding::estimate(self, ComplexNumberType::Imaginary);

        if let (None, None) = (&rounded_real, &rounded_imaginary) {
            return None;
        }

        let mut output = String::new();
        let mut real_is_exact = rounded_real.is_none();
        if let Some(result) = rounded_real {
            real_is_exact = result.is_exact;
            output.push_str(&result.value);
        } else if self.has_real() {
            output.push_str(&self.to_string_real(10));
        }

        let mut imaginary_is_exact = rounded_imaginary.is_none();
        let imaginary_value = if let Some(result) = rounded_imaginary {
            imaginary_is_exact = result.is_exact;

            Some(result.value)
        } else if self.has_imaginary() {
            Some(self.to_string_imaginary(10, false))
        } else {
            None
        };

        let is_exact = real_is_exact && imaginary_is_exact;
        if let Some(value) = imaginary_value {
            // Clear output if it's just 0.
            if output == "0" {
                output = String::new();
            }

            if value == "0" {
                // If both values ended up being estimated as zero,
                // return zero.
                if output.is_empty() {
                    return Some(EstimationResult {
                        value: String::from("0"),
                        is_exact,
                    });
                }
            } else {
                let sign = if value.starts_with('-') { "-" } else { "+" };
                let value = match value.as_ref() {
                    "1" => String::from("i"),
                    "-1" => String::from("-i"),
                    _ => format!("{}i", value),
                };

                // If there is a real value as well
                if !output.is_empty() {
                    output.push_str(&format!(" {} {}", sign, value.trim_start_matches('-')));
                } else {
                    output.push_str(&value);
                }
            }
        }

        Some(EstimationResult {
            value: output,
            is_exact,
        })
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
            unit.clone(),
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
            imaginary != &0f64 && imaginary != &-0f64
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
        let exponent = value.abs().log10().floor() as i32 + 1;

        ScientificNotation {
            value: value / (10f64.powf(exponent as f64 - 1f64)),
            // I... am not sure what else to do...
            exponent,
            imaginary: complex_number_type == ComplexNumberType::Imaginary,
        }
    }

    pub fn has_unit(&self) -> bool {
        if let KalkValue::Number(_, _, unit) = self {
            unit.is_some()
        } else {
            false
        }
    }

    pub fn get_unit(&self) -> Option<&String> {
        if let KalkValue::Number(_, _, unit) = self {
            unit.as_ref()
        } else {
            None
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
                &Expr::Literal(real.clone()),
                unit.as_ref(),
                Some(&to_unit.to_string()),
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
    ) -> Result<KalkValue, KalkError> {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        self.add_without_unit(&right)
    }

    pub(crate) fn sub(
        self,
        context: &mut crate::interpreter::Context,
        rhs: KalkValue,
    ) -> Result<KalkValue, KalkError> {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        self.sub_without_unit(&right)
    }

    pub(crate) fn mul(
        self,
        context: &mut crate::interpreter::Context,
        rhs: KalkValue,
    ) -> Result<KalkValue, KalkError> {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        self.mul_without_unit(&right)
    }

    pub(crate) fn div(
        self,
        context: &mut crate::interpreter::Context,
        rhs: KalkValue,
    ) -> Result<KalkValue, KalkError> {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        self.div_without_unit(&right)
    }

    pub(crate) fn pow(
        self,
        context: &mut crate::interpreter::Context,
        rhs: KalkValue,
    ) -> Result<KalkValue, KalkError> {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        self.pow_without_unit(&right)
    }

    pub(crate) fn rem(
        self,
        context: &mut crate::interpreter::Context,
        rhs: KalkValue,
    ) -> Result<KalkValue, KalkError> {
        Ok(if let KalkValue::Number(real, _, _) = &self {
            let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
            if let KalkValue::Number(right_real, _, right_unit) = right {
                KalkValue::Number(real % right_real, float!(0f64), right_unit)
            } else {
                self
            }
        } else {
            self
        })
    }

    pub(crate) fn eq(
        self,
        context: &mut crate::interpreter::Context,
        rhs: KalkValue,
    ) -> Result<KalkValue, KalkError> {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        self.eq_without_unit(&right)
    }

    pub(crate) fn not_eq(
        self,
        context: &mut crate::interpreter::Context,
        rhs: KalkValue,
    ) -> Result<KalkValue, KalkError> {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        self.not_eq_without_unit(&right)
    }

    pub(crate) fn greater_than(
        self,
        context: &mut crate::interpreter::Context,
        rhs: KalkValue,
    ) -> Result<KalkValue, KalkError> {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        self.greater_than_without_unit(&right)
    }

    pub(crate) fn less_than(
        self,
        context: &mut crate::interpreter::Context,
        rhs: KalkValue,
    ) -> Result<KalkValue, KalkError> {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        self.less_than_without_unit(&right)
    }

    pub(crate) fn greater_or_equals(
        self,
        context: &mut crate::interpreter::Context,
        rhs: KalkValue,
    ) -> Result<KalkValue, KalkError> {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        if let (KalkValue::Boolean(greater), KalkValue::Boolean(equal)) = (
            self.greater_than_without_unit(&right)?,
            self.eq_without_unit(&right)?,
        ) {
            Ok(KalkValue::Boolean(greater || equal))
        } else {
            unreachable!()
        }
    }

    pub(crate) fn less_or_equals(
        self,
        context: &mut crate::interpreter::Context,
        rhs: KalkValue,
    ) -> Result<KalkValue, KalkError> {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        if let (KalkValue::Boolean(less), KalkValue::Boolean(equal)) = (
            self.less_than_without_unit(&right)?,
            self.eq_without_unit(&right)?,
        ) {
            Ok(KalkValue::Boolean(less || equal))
        } else {
            unreachable!()
        }
    }

    pub(crate) fn and(self, rhs: &KalkValue) -> Result<KalkValue, KalkError> {
        match (self, rhs) {
            (KalkValue::Boolean(boolean), KalkValue::Boolean(boolean_rhs)) => {
                Ok(KalkValue::Boolean(boolean && *boolean_rhs))
            }
            (lhs, rhs) => Err(KalkError::IncompatibleTypesForOperation(
                String::from("and"),
                lhs.get_type_name(),
                rhs.get_type_name(),
            )),
        }
    }

    pub(crate) fn or(self, rhs: &KalkValue) -> Result<KalkValue, KalkError> {
        match (self, rhs) {
            (KalkValue::Boolean(boolean), KalkValue::Boolean(boolean_rhs)) => {
                Ok(KalkValue::Boolean(boolean || *boolean_rhs))
            }
            (lhs, rhs) => Err(KalkError::IncompatibleTypesForOperation(
                String::from("or"),
                lhs.get_type_name(),
                rhs.get_type_name(),
            )),
        }
    }

    pub(crate) fn add_without_unit(self, rhs: &KalkValue) -> Result<KalkValue, KalkError> {
        match (self.clone(), rhs) {
            (
                KalkValue::Number(real, imaginary, _),
                KalkValue::Number(real_rhs, imaginary_rhs, unit),
            ) => Ok(KalkValue::Number(
                real + real_rhs,
                imaginary + imaginary_rhs,
                unit.clone(),
            )),
            (KalkValue::Matrix(_), _) | (_, KalkValue::Matrix(_)) => {
                calculate_matrix(self, rhs, &KalkValue::add_without_unit)
            }
            (KalkValue::Vector(_), _) | (_, KalkValue::Vector(_)) => {
                calculate_vector(self, rhs, &KalkValue::add_without_unit)
            }
            _ => Err(KalkError::IncompatibleTypesForOperation(
                String::from("addition"),
                self.get_type_name(),
                rhs.get_type_name(),
            )),
        }
    }

    pub(crate) fn sub_without_unit(self, rhs: &KalkValue) -> Result<KalkValue, KalkError> {
        match (self.clone(), rhs) {
            (
                KalkValue::Number(real, imaginary, _),
                KalkValue::Number(real_rhs, imaginary_rhs, unit),
            ) => Ok(KalkValue::Number(
                real - real_rhs,
                imaginary - imaginary_rhs,
                unit.clone(),
            )),
            (KalkValue::Matrix(_), _) | (_, KalkValue::Matrix(_)) => {
                calculate_matrix(self, rhs, &KalkValue::sub_without_unit)
            }
            (KalkValue::Vector(_), _) | (_, KalkValue::Vector(_)) => {
                calculate_vector(self, rhs, &KalkValue::sub_without_unit)
            }
            _ => Err(KalkError::IncompatibleTypesForOperation(
                String::from("subtraction"),
                self.get_type_name(),
                rhs.get_type_name(),
            )),
        }
    }

    pub(crate) fn mul_without_unit(self, rhs: &KalkValue) -> Result<KalkValue, KalkError> {
        // Make sure matrix is always first to avoid having to match
        // different orders in the next match expression.
        let (lhs, rhs) = match (&self, rhs) {
            (KalkValue::Matrix(_), KalkValue::Matrix(_)) => (&self, rhs),
            (_, KalkValue::Matrix(_)) => (rhs, &self),
            _ => (&self, rhs),
        };

        match (lhs, rhs) {
            (
                KalkValue::Number(real, imaginary, _),
                KalkValue::Number(real_rhs, imaginary_rhs, unit),
            ) => Ok(KalkValue::Number(
                // (a + bi)(c + di) = ac + adi + bci + bdi²
                real.clone() * real_rhs - imaginary.clone() * imaginary_rhs,
                real.clone() * imaginary_rhs + imaginary * real_rhs,
                unit.clone(),
            )),
            (KalkValue::Matrix(_), KalkValue::Number(_, _, _)) => {
                calculate_matrix(lhs.clone(), rhs, &KalkValue::mul_without_unit)
            }
            (KalkValue::Matrix(rows), KalkValue::Vector(values_rhs)) => {
                if rows.first().unwrap().len() != values_rhs.len() {
                    return Err(KalkError::IncompatibleVectorsMatrixes);
                }

                let mut new_values: Vec<KalkValue> = Vec::new();
                for row in rows {
                    let mut sum = KalkValue::from(0);
                    for (x, y) in row.iter().zip(values_rhs) {
                        sum = sum
                            .clone()
                            .add_without_unit(&x.clone().mul_without_unit(y)?)?;
                    }

                    new_values.push(sum);
                }

                Ok(KalkValue::Vector(new_values))
            }
            (KalkValue::Matrix(rows), KalkValue::Matrix(rows_rhs)) => {
                let lhs_columns = rows.first().unwrap();
                if lhs_columns.len() != rows_rhs.len() {
                    return Err(KalkError::IncompatibleVectorsMatrixes);
                }

                let rhs_columns = rows_rhs.first().unwrap();
                let mut result = vec![vec![KalkValue::from(0f64); rhs_columns.len()]; rows.len()];

                // For every row in lhs
                for i in 0..rows.len() {
                    // For every column in rhs
                    for j in 0..rhs_columns.len() {
                        let mut sum = KalkValue::from(0f64);

                        // For every value in the current lhs row
                        for (k, value) in rows[i].iter().enumerate() {
                            let value_rhs = &rows_rhs[k][j];
                            sum =
                                sum.add_without_unit(&value.clone().mul_without_unit(value_rhs)?)?;
                        }

                        result[i][j] = sum;
                    }
                }

                Ok(KalkValue::Matrix(result))
            }
            (KalkValue::Vector(values), KalkValue::Number(_, _, _)) => {
                let mut multiplied_values = Vec::new();
                for value in values {
                    multiplied_values.push(value.clone().mul_without_unit(rhs)?);
                }

                Ok(KalkValue::Vector(multiplied_values))
            }
            (KalkValue::Vector(values), KalkValue::Vector(values_rhs)) => {
                if values.len() != values_rhs.len() {
                    return Err(KalkError::IncompatibleVectorsMatrixes);
                }

                let mut sum = KalkValue::from(0f64);
                for (value, value_rhs) in values.iter().zip(values_rhs) {
                    sum = sum.add_without_unit(&value.clone().mul_without_unit(value_rhs)?)?;
                }

                Ok(sum)
            }
            _ => Err(KalkError::IncompatibleTypesForOperation(
                String::from("multiplication"),
                self.get_type_name(),
                rhs.get_type_name(),
            )),
        }
    }

    pub(crate) fn div_without_unit(self, rhs: &KalkValue) -> Result<KalkValue, KalkError> {
        match (self.clone(), rhs.clone()) {
            (KalkValue::Number(real, _, _), KalkValue::Number(real_rhs, _, unit)) => {
                // Avoid unecessary calculations
                if !self.has_imaginary() && !rhs.has_imaginary() {
                    Ok(KalkValue::Number(real / real_rhs, float!(0f64), unit))
                } else {
                    // Multiply both the numerator and denominator
                    // with the conjugate of the denominator, and divide.
                    let conjugate = rhs.get_conjugate()?;
                    let (numerator, numerator_imaginary) =
                        self.mul_without_unit(&conjugate)?.values();
                    let (denominator, _) = rhs.clone().mul_without_unit(&conjugate)?.values();
                    Ok(KalkValue::Number(
                        numerator / denominator.clone(),
                        numerator_imaginary / denominator,
                        unit,
                    ))
                }
            }
            (KalkValue::Matrix(_), _) | (_, KalkValue::Matrix(_)) => {
                calculate_matrix(self, rhs, &KalkValue::div_without_unit)
            }
            (KalkValue::Vector(_), _) | (_, KalkValue::Vector(_)) => {
                calculate_vector(self, rhs, &KalkValue::div_without_unit)
            }
            _ => Err(KalkError::IncompatibleTypesForOperation(
                String::from("division"),
                self.get_type_name(),
                rhs.get_type_name(),
            )),
        }
    }

    pub(crate) fn pow_without_unit(self, rhs: &KalkValue) -> Result<KalkValue, KalkError> {
        match (self.clone(), rhs) {
            (
                KalkValue::Number(real, imaginary, _),
                KalkValue::Number(real_rhs, imaginary_rhs, unit),
            ) => {
                if self.has_imaginary()
                    || imaginary_rhs != &0f64
                    || (real < 0f64 && real_rhs.clone().abs() < 1f64)
                {
                    let a = real;
                    let b = imaginary;
                    let c = real_rhs;
                    let d = imaginary_rhs;
                    let arg = crate::prelude::funcs::arg(self)?.values().0;
                    let raised = a.clone() * a + b.clone() * b;
                    let exp =
                        pow(raised.clone(), c.clone() / 2f64) * (-d.clone() * arg.clone()).exp();
                    let polar = c * arg + d.clone() / 2f64 * raised.ln();

                    Ok(KalkValue::Number(
                        polar.clone().cos() * exp.clone(),
                        polar.sin() * exp,
                        unit.clone(),
                    ))
                } else {
                    Ok(KalkValue::Number(
                        pow(real, real_rhs.clone()),
                        float!(0),
                        unit.clone(),
                    ))
                }
            }
            (KalkValue::Matrix(rows), KalkValue::Number(real, _, _)) => {
                if real < &0f64 || real.clone().fract() > 0.000001f64 {
                    return Err(KalkError::Expected(String::from("a positive integer")));
                }

                if rhs.has_imaginary() {
                    return Err(KalkError::ExpectedReal);
                }

                if rows.len() != rows.first().unwrap().len() {
                    return Err(KalkError::Expected(String::from("a square matrix")));
                }

                if real == &0f64 {
                    return Ok(KalkValue::from(1f64));
                }

                let mut result = KalkValue::from(1f64);
                for _ in 0..primitive!(real) as i32 {
                    result = result.mul_without_unit(&self)?;
                }

                Ok(result)
            }
            (KalkValue::Number(_, _, _), KalkValue::Matrix(rows)) => {
                let mut new_rows = Vec::new();
                for row in rows {
                    new_rows.push(Vec::new());
                    for item in row {
                        new_rows
                            .last_mut()
                            .unwrap()
                            .push(self.clone().pow_without_unit(item)?);
                    }
                }

                Ok(KalkValue::Matrix(new_rows))
            }
            (KalkValue::Vector(_), _) | (_, KalkValue::Vector(_)) => {
                calculate_vector(self, rhs, &KalkValue::pow_without_unit)
            }
            _ => Err(KalkError::IncompatibleTypesForOperation(
                String::from("pow"),
                self.get_type_name(),
                rhs.get_type_name(),
            )),
        }
    }

    pub(crate) fn eq_without_unit(&self, rhs: &KalkValue) -> Result<KalkValue, KalkError> {
        match (self, rhs) {
            (
                KalkValue::Number(real, imaginary, _),
                KalkValue::Number(real_rhs, imaginary_rhs, _),
            ) => Ok(KalkValue::Boolean(
                (real.clone() - real_rhs.clone()).abs() < ACCEPTABLE_COMPARISON_MARGIN
                    && (imaginary.clone() - imaginary_rhs.clone()).abs()
                        < ACCEPTABLE_COMPARISON_MARGIN,
            )),
            (KalkValue::Boolean(boolean), KalkValue::Boolean(boolean_rhs)) => {
                Ok(KalkValue::Boolean(boolean == boolean_rhs))
            }
            (KalkValue::Matrix(rows), KalkValue::Matrix(rows_rhs)) => {
                let mut matrices_are_equal = true;
                for (row, row_rhs) in rows.iter().zip(rows_rhs) {
                    for (value, value_rhs) in row.iter().zip(row_rhs) {
                        if let KalkValue::Boolean(are_equal) = value.eq_without_unit(value_rhs)? {
                            if !are_equal {
                                matrices_are_equal = false;
                            }
                        }
                    }
                }

                Ok(KalkValue::Boolean(matrices_are_equal))
            }

            (KalkValue::Vector(values), KalkValue::Vector(values_rhs)) => {
                let mut vecs_are_equal = true;
                for (value, value_rhs) in values.iter().zip(values_rhs) {
                    if let KalkValue::Boolean(are_equal) = value.eq_without_unit(value_rhs)? {
                        if !are_equal {
                            vecs_are_equal = false;
                        }
                    }
                }

                Ok(KalkValue::Boolean(vecs_are_equal))
            }
            _ => Err(KalkError::IncompatibleTypesForOperation(
                String::from("equal"),
                self.get_type_name(),
                rhs.get_type_name(),
            )),
        }
    }

    pub(crate) fn not_eq_without_unit(&self, rhs: &KalkValue) -> Result<KalkValue, KalkError> {
        match (self, rhs) {
            (
                KalkValue::Number(real, imaginary, _),
                KalkValue::Number(real_rhs, imaginary_rhs, _),
            ) => Ok(KalkValue::Boolean(
                (real.clone() - real_rhs.clone()).abs() > ACCEPTABLE_COMPARISON_MARGIN
                    || (imaginary.clone() - imaginary_rhs.clone()).abs()
                        > ACCEPTABLE_COMPARISON_MARGIN,
            )),
            (KalkValue::Boolean(boolean), KalkValue::Boolean(boolean_rhs)) => {
                Ok(KalkValue::Boolean(boolean != boolean_rhs))
            }
            (KalkValue::Vector(_), KalkValue::Vector(_))
            | (KalkValue::Matrix(_), KalkValue::Matrix(_)) => {
                if let KalkValue::Boolean(boolean) = self.eq_without_unit(rhs)? {
                    Ok(KalkValue::Boolean(!boolean))
                } else {
                    unreachable!()
                }
            }
            _ => Err(KalkError::IncompatibleTypesForOperation(
                String::from("not equal"),
                self.get_type_name(),
                rhs.get_type_name(),
            )),
        }
    }

    pub(crate) fn greater_than_without_unit(
        &self,
        rhs: &KalkValue,
    ) -> Result<KalkValue, KalkError> {
        if self.has_imaginary() || rhs.has_imaginary() {
            return Err(KalkError::ExpectedReal);
        }

        match (self, rhs) {
            (KalkValue::Number(real, _, _), KalkValue::Number(real_rhs, _, _)) => Ok(
                KalkValue::Boolean(real.clone() - real_rhs.clone() > ACCEPTABLE_COMPARISON_MARGIN),
            ),
            _ => Err(KalkError::IncompatibleTypesForOperation(
                String::from("greater than"),
                self.get_type_name(),
                rhs.get_type_name(),
            )),
        }
    }

    pub(crate) fn less_than_without_unit(&self, rhs: &KalkValue) -> Result<KalkValue, KalkError> {
        if self.has_imaginary() || rhs.has_imaginary() {
            return Err(KalkError::ExpectedReal);
        }

        match (self, rhs) {
            (KalkValue::Number(real, _, _), KalkValue::Number(real_rhs, _, _)) => Ok(
                KalkValue::Boolean(real.clone() - real_rhs.clone() < -ACCEPTABLE_COMPARISON_MARGIN),
            ),
            _ => Err(KalkError::IncompatibleTypesForOperation(
                String::from("less than"),
                self.get_type_name(),
                rhs.get_type_name(),
            )),
        }
    }

    pub fn get_conjugate(&self) -> Result<KalkValue, KalkError> {
        match self {
            KalkValue::Number(real, imaginary, unit) => Ok(KalkValue::Number(
                real.clone(),
                imaginary.clone() * (-1f64),
                unit.clone(),
            )),
            _ => Err(KalkError::UnexpectedType(
                self.get_type_name(),
                vec![String::from("number")],
            )),
        }
    }
}

pub fn format_number(input: f64) -> String {
    let rounded = format!("{:.1$}", input, 10);
    let result = if rounded.contains('.') {
        rounded
            .trim_end_matches('0')
            .trim_end_matches('.')
            .to_string()
    } else {
        rounded
    };

    spaced(&result)
}

#[cfg(feature = "rug")]
pub fn format_number_big(input: &Float) -> String {
    if input.clone().log10() < 0f64 {
        return input.to_f64().to_string();
    }

    let input_str = input.to_string();
    let mut result = if input_str.contains('.') {
        input_str
            .trim_end_matches('0')
            .trim_end_matches('.')
            .to_string()
    } else {
        input_str
    };

    if let Some(dot_index) = result.find('.') {
        let decimal_count = result.len() - dot_index;
        if decimal_count > 10 {
            result = result[..(result.len() - decimal_count + 10)].to_string();
        }
    }

    spaced(&result)
}

#[cfg(not(feature = "rug"))]
pub fn format_number_big(input: &f64) -> String {
    format_number(*input)
}

fn calculate_vector(
    x: KalkValue,
    y: &KalkValue,
    action: &dyn Fn(KalkValue, &KalkValue) -> Result<KalkValue, KalkError>,
) -> Result<KalkValue, KalkError> {
    match (x, y) {
        (KalkValue::Vector(values), KalkValue::Number(_, _, _)) => {
            let mut new_values = Vec::new();
            for value in values {
                new_values.push(action(value.clone(), y)?);
            }

            Ok(KalkValue::Vector(new_values))
        }
        (KalkValue::Number(_, _, _), KalkValue::Vector(values_rhs)) => {
            let mut new_values = Vec::new();
            for value in values_rhs {
                new_values.push(action(y.clone(), value)?);
            }

            Ok(KalkValue::Vector(new_values))
        }
        (KalkValue::Vector(values), KalkValue::Vector(values_rhs)) => {
            if values.len() != values_rhs.len() {
                return Err(KalkError::IncompatibleVectorsMatrixes);
            }

            let mut new_values = Vec::new();
            for (value, value_rhs) in values.iter().zip(values_rhs) {
                new_values.push(action(value.clone(), value_rhs)?);
            }

            Ok(KalkValue::Vector(new_values))
        }
        (x, y) => Err(KalkError::IncompatibleTypesForOperation(
            String::from("vector operation"),
            x.get_type_name(),
            y.get_type_name(),
        )),
    }
}

fn calculate_matrix(
    x: KalkValue,
    y: &KalkValue,
    action: &dyn Fn(KalkValue, &KalkValue) -> Result<KalkValue, KalkError>,
) -> Result<KalkValue, KalkError> {
    // Make sure matrix is always first to avoid having to match
    // different orders in the next match expression.
    let (x, y) = match (&x, y) {
        (KalkValue::Matrix(_), KalkValue::Matrix(_)) => (&x, y),
        (_, KalkValue::Matrix(_)) => (y, &x),
        _ => (&x, y),
    };

    match (x, y) {
        (KalkValue::Matrix(rows), KalkValue::Number(_, _, _)) => {
            let mut new_rows = Vec::new();
            for row in rows {
                new_rows.push(Vec::new());
                for item in row {
                    new_rows.last_mut().unwrap().push(action(item.clone(), y)?);
                }
            }

            Ok(KalkValue::Matrix(new_rows))
        }
        (KalkValue::Matrix(rows), KalkValue::Vector(values_rhs)) => {
            if rows.len() != values_rhs.len() {
                return Err(KalkError::IncompatibleVectorsMatrixes);
            }

            let mut new_rows = Vec::new();
            for (i, row) in rows.iter().enumerate() {
                new_rows.push(Vec::new());
                for value in row {
                    new_rows
                        .last_mut()
                        .unwrap()
                        .push(action(value.clone(), &values_rhs[i])?)
                }
            }

            Ok(KalkValue::Matrix(new_rows))
        }
        (KalkValue::Matrix(rows), KalkValue::Matrix(rows_rhs)) => {
            if rows.len() != rows_rhs.len()
                || rows.first().unwrap().len() != rows_rhs.first().unwrap().len()
            {
                return Err(KalkError::IncompatibleVectorsMatrixes);
            }

            let mut new_rows = Vec::new();
            for (i, row) in rows.iter().enumerate() {
                new_rows.push(Vec::new());
                for (j, value) in row.iter().enumerate() {
                    new_rows
                        .last_mut()
                        .unwrap()
                        .push(action(value.clone(), &rows_rhs[i][j])?)
                }
            }

            Ok(KalkValue::Matrix(new_rows))
        }
        _ => Err(KalkError::IncompatibleTypesForOperation(
            String::from("matrix operation"),
            x.get_type_name(),
            y.get_type_name(),
        )),
    }
}

fn spaced(number_str: &str) -> String {
    let dot_pos = number_str.find('.');
    let integer_boundary = if let Some(dot_pos) = dot_pos {
        dot_pos
    } else {
        number_str.len()
    };

    if integer_boundary < 5 {
        return number_str.into();
    }

    let bytes = number_str.as_bytes();
    let mut at_decimals = dot_pos.is_some();
    let mut i = number_str.len() - 1;
    let mut c = 0;
    let mut new_str = String::new();
    while i > 0 {
        if bytes[i] as char == '.' {
            new_str.push('.');
            at_decimals = false;
            i -= 1;
            c = 0;
            continue;
        }

        if !at_decimals && c == 3 {
            new_str.push(' ');
            c = 0;
        }

        new_str.push(bytes[i] as char);
        c += 1;
        i -= 1;
    }

    if c == 3 {
        new_str.push(' ');
    }
    new_str.push(bytes[0] as char);

    new_str.chars().rev().collect::<String>()
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
            right.convert_to_unit(context, unit_left.as_ref().unwrap())
        } else {
            Some(KalkValue::Number(
                real_right.clone(),
                imaginary_right.clone(),
                unit_left.clone(),
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
    x.pow(y)
}

impl From<ScientificNotation> for String {
    fn from(val: ScientificNotation) -> Self {
        val.to_string()
    }
}

/*impl std::iter::Sum<KalkValue> for KalkValue {
    fn sum<I>(iter: I) -> KalkValue
    where
        I: std::iter::Iterator<Item = KalkValue>,
    {
        let mut sum = KalkValue::from(0f64);
        for x in iter {
            sum = sum.add_without_unit(&x);
        }

        sum
    }
}*/

impl From<KalkValue> for String {
    fn from(val: KalkValue) -> Self {
        val.to_string()
    }
}

impl From<KalkValue> for f64 {
    fn from(val: KalkValue) -> Self {
        val.to_f64()
    }
}

impl From<f64> for KalkValue {
    fn from(x: f64) -> Self {
        KalkValue::Number(float!(x), float!(0), None)
    }
}

impl From<f32> for KalkValue {
    fn from(x: f32) -> Self {
        KalkValue::Number(float!(x), float!(0), None)
    }
}

impl From<i128> for KalkValue {
    fn from(x: i128) -> Self {
        KalkValue::Number(float!(x), float!(0), None)
    }
}

impl From<i64> for KalkValue {
    fn from(x: i64) -> Self {
        KalkValue::Number(float!(x), float!(0), None)
    }
}

impl From<i32> for KalkValue {
    fn from(x: i32) -> Self {
        KalkValue::Number(float!(x), float!(0), None)
    }
}

#[cfg(test)]
mod tests {
    use crate::kalk_value::{spaced, KalkValue, ScientificNotationFormat};
    use crate::test_helpers::cmp;

    use super::ScientificNotation;

    #[test]
    fn test_spaced() {
        assert_eq!(spaced("1"), String::from("1"));
        assert_eq!(spaced("10"), String::from("10"));
        assert_eq!(spaced("100"), String::from("100"));
        assert_eq!(spaced("1000"), String::from("1000"));
        assert_eq!(spaced("10000"), String::from("10 000"));
        assert_eq!(spaced("100000"), String::from("100 000"));
        assert_eq!(spaced("1000000"), String::from("1 000 000"));
        assert_eq!(spaced("10000000"), String::from("10 000 000"));
        assert_eq!(spaced("1.12345"), String::from("1.12345"));
        assert_eq!(spaced("10.12345"), String::from("10.12345"));
        assert_eq!(spaced("100.12345"), String::from("100.12345"));
        assert_eq!(spaced("1000.12345"), String::from("1000.12345"));
        assert_eq!(spaced("10000.12345"), String::from("10 000.12345"));
        assert_eq!(spaced("100000.12345"), String::from("100 000.12345"));
        assert_eq!(spaced("1000000.12345"), String::from("1 000 000.12345"));
        assert_eq!(spaced("10000000.12345"), String::from("10 000 000.12345"));
    }

    #[test]
    fn test_add_complex() {
        let in_out = vec![
            ((0f64, 0f64), (0f64, 0f64), (0f64, 0f64)),
            ((2f64, 0f64), (3f64, 4f64), (5f64, 4f64)),
            ((0f64, 2f64), (3f64, 4f64), (3f64, 6f64)),
            ((3f64, -2f64), (-3f64, 4f64), (0f64, 2f64)),
        ];

        for (a, b, expected_result) in in_out {
            let actual_result = KalkValue::Number(float!(a.0), float!(a.1), None)
                .add_without_unit(&KalkValue::Number(float!(b.0), float!(b.1), None))
                .unwrap();
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
            let actual_result = KalkValue::Number(float!(a.0), float!(a.1), None)
                .sub_without_unit(&KalkValue::Number(float!(b.0), float!(b.1), None))
                .unwrap();
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
            let actual_result = KalkValue::Number(float!(a.0), float!(a.1), None)
                .mul_without_unit(&KalkValue::Number(float!(b.0), float!(b.1), None))
                .unwrap();
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
            let actual_result = KalkValue::Number(float!(a.0), float!(a.1), None)
                .div_without_unit(&KalkValue::Number(float!(b.0), float!(b.1), None))
                .unwrap();
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
            ((-9f64, 0f64), (0.5f64, 0f64), (0f64, 3f64)),
            ((-9f64, 0f64), (-0.5f64, 0f64), (0f64, -1f64 / 3f64)),
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
            let actual_result = KalkValue::Number(float!(a.0), float!(a.1), None)
                .pow_without_unit(&KalkValue::Number(float!(b.0), float!(b.1), None))
                .unwrap();
            assert!(cmp(actual_result.to_f64(), expected_result.0));
            assert!(cmp(actual_result.imaginary_to_f64(), expected_result.1));
        }
    }

    #[test]
    fn test_to_string_pretty() {
        let in_out = vec![
            (float!(0.99999), float!(0.0), "0.99999 ≈ 1"),
            (float!(0.00000001), float!(0.0), "0.00000001 ≈ 10^-8"),
            (float!(-0.99999), float!(0.0), "-0.99999 ≈ -1"),
            (float!(0.0), float!(0.99999), "0.99999i ≈ i"),
            (float!(0.000000001), float!(0.0), "10^-9 ≈ 0"),
            (float!(0.0), float!(0.000000001), "10^-9 i ≈ 0"),
            (
                float!(0.99999),
                float!(0.999999),
                "0.99999 + 0.999999i ≈ 1 + i",
            ),
            (float!(1.0), float!(0.99999), "1 + 0.99999i ≈ 1 + i"),
            (
                float!(-0.99999),
                float!(0.999999),
                "-0.99999 + 0.999999i ≈ -1 + i",
            ),
            (
                float!(0.99999),
                float!(-0.999999),
                "0.99999 - 0.999999i ≈ 1 - i",
            ),
            (float!(-1.0), float!(0.99999), "-1 + 0.99999i ≈ -1 + i"),
            (float!(1.0), float!(-0.99999), "1 - 0.99999i ≈ 1 - i"),
            (float!(-0.99999), float!(1.0), "-0.99999 + i ≈ -1 + i"),
            (float!(0.99999), float!(-1.0), "0.99999 - i ≈ 1 - i"),
            (
                float!(0.000000001),
                float!(0.000000001),
                "10^-9 + 10^-9 i ≈ 0",
            ),
            (float!(1.0), float!(0.000000001), "1 + 10^-9 i ≈ 1"),
            (float!(0.000000001), float!(1.0), "10^-9 + i ≈ i"),
            (float!(-1.0), float!(0.000000001), "-1 + 10^-9 i ≈ -1"),
            (float!(0.000000001), float!(-1.0), "10^-9 - i ≈ -i"),
            (float!(10e-17), float!(1.0), "i"),
            (float!(1.0), float!(10e-17), "1"),
            (float!(10e-16), float!(0.0), "0"),
            (float!(3.00000000004), float!(0.0), "3"),
        ];
        for (real, imaginary, output) in in_out {
            let result = KalkValue::Number(real, imaginary, None)
                .to_string_pretty(ScientificNotationFormat::Normal);
            assert_eq!(output, result);
        }
    }

    #[test]
    fn test_eng_mode() {
        let in_out = vec![
            (1.23, 0, "1.23×10^0"),
            (1.23, 1, "12.3×10^0"),
            (1.23, 2, "123×10^0"),
            (1.23, 3, "1.23×10^3"),
            (1.23, 4, "12.3×10^3"),
            (1.23, 5, "123×10^3"),
            (1.23, 6, "1.23×10^6"),
        ];
        for (value, exponent, output) in in_out {
            let sci = ScientificNotation {
                value,
                exponent: exponent + 1,
                imaginary: false,
            };

            assert_eq!(
                sci.to_string_format(ScientificNotationFormat::Engineering),
                output
            );
        }
    }
}
