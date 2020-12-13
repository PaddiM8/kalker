use crate::ast::Expr;
use rug::ops::Pow;
use rug::Float;
use rug::Rational;

#[derive(PartialEq, Debug, Clone)]
pub struct KalkNum {
    pub(crate) value: Float,
    pub(crate) unit: String,
    rational: Option<Rational>,
}

pub struct ScientificNotation {
    pub negative: bool,
    pub digits: String,
    pub exponent: i32,
}

impl ScientificNotation {
    pub fn to_string(&self) -> String {
        let sign = if self.negative { "-" } else { "" };
        let mut digits_and_mul = if self.digits == "1" {
            String::new()
        } else {
            format!("{}*", &self.digits)
        };

        if self.digits.len() > 1 {
            digits_and_mul.insert(1usize, '.');
        }

        format!("{}{}10^{}", sign, digits_and_mul, self.exponent - 1)
    }
}

impl KalkNum {
    pub fn new(value: Float, unit: &str) -> Self {
        Self {
            value: value.clone(),
            unit: unit.to_string(),
            rational: value.to_rational(),
        }
    }

    pub(crate) fn new_with_rational(value: Float, unit: &str, rational: Option<Rational>) -> Self {
        Self {
            value: value.clone(),
            unit: unit.to_string(),
            rational: rational,
        }
    }

    pub fn new_without_rational(value: Float, unit: &str) -> Self {
        Self {
            value: value.clone(),
            unit: unit.to_string(),
            rational: None,
        }
    }

    pub fn to_f64(&self) -> f64 {
        self.value.to_f64_round(rug::float::Round::Nearest)
    }

    pub fn to_string(&self) -> String {
        let as_str = self.to_f64().to_string();

        if as_str.contains(".") {
            as_str
                .trim_end_matches('0')
                .trim_end_matches('.')
                .to_string()
        } else {
            as_str
        }
    }

    pub fn is_too_big(&self) -> bool {
        self.value.is_infinite()
    }

    pub fn to_string_with_unit(&self) -> String {
        format!("{} {}", self.to_string(), self.unit)
    }

    pub fn get_unit(&self) -> &str {
        &self.unit
    }

    pub fn get_rational(self) -> Option<(i64, i64)> {
        let (numer, denom) = self.rational?.into_numer_denom();

        Some((numer.to_i64()?, denom.to_i64()?))
    }

    pub fn get_internal_rational(self) -> Option<Rational> {
        self.rational
    }

    pub fn has_unit(&self) -> bool {
        self.unit.len() > 0
    }

    pub fn to_scientific_notation(&self) -> ScientificNotation {
        let (neg, digits, exp_option) =
            self.value
                .to_sign_string_exp_round(10, None, rug::float::Round::Up);

        ScientificNotation {
            negative: neg,
            digits: digits
                .trim_start_matches('0')
                .trim_end_matches('0')
                .trim_end_matches('.')
                .to_string(),
            exponent: if let Some(exp) = exp_option { exp } else { 0 },
        }
    }

    pub fn convert_to_unit(
        &self,
        context: &mut crate::interpreter::Context,
        to_unit: &str,
    ) -> Option<KalkNum> {
        let result = crate::interpreter::convert_unit(
            context,
            &Expr::Literal(self.value.to_f64()),
            &self.unit,
            to_unit,
        );

        if let Ok(num) = result {
            Some(num)
        } else {
            None
        }
    }

    pub fn add(self, context: &mut crate::interpreter::Context, rhs: KalkNum) -> KalkNum {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        let mut num = KalkNum::new_without_rational(self.value + right.value, &right.unit);
        if let (Some(left_rational), Some(right_rational)) = (self.rational, right.rational) {
            num.rational = Some(left_rational + right_rational);
        };

        num
    }

    pub fn sub(self, context: &mut crate::interpreter::Context, rhs: KalkNum) -> KalkNum {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        let mut num = KalkNum::new_without_rational(self.value - right.value, &right.unit);
        if let (Some(left_rational), Some(right_rational)) = (self.rational, right.rational) {
            num.rational = Some(left_rational - right_rational);
        };

        num
    }

    pub fn mul(self, context: &mut crate::interpreter::Context, rhs: KalkNum) -> KalkNum {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        let mut num = KalkNum::new_without_rational(self.value * right.value, &right.unit);
        if let (Some(left_rational), Some(right_rational)) = (self.rational, right.rational) {
            num.rational = Some(left_rational * right_rational);
        };

        num
    }

    pub fn div(self, context: &mut crate::interpreter::Context, rhs: KalkNum) -> KalkNum {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        let mut num = KalkNum::new_without_rational(self.value / right.value, &right.unit);
        if let (Some(left_rational), Some(right_rational)) = (self.rational, right.rational) {
            num.rational = Some(left_rational / right_rational);
        };

        num
    }

    pub fn rem(self, context: &mut crate::interpreter::Context, rhs: KalkNum) -> KalkNum {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        KalkNum::new_without_rational(self.value % right.value, &right.unit)
    }

    pub fn pow(self, context: &mut crate::interpreter::Context, rhs: KalkNum) -> KalkNum {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        KalkNum::new_without_rational(self.value.pow(right.value), &right.unit)
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
        Some(KalkNum::new_with_rational(
            right.value,
            &left.unit,
            right.rational,
        ))
    }
}

impl Into<String> for ScientificNotation {
    fn into(self) -> String {
        self.to_string()
    }
}

impl Into<f64> for KalkNum {
    fn into(self) -> f64 {
        self.to_f64()
    }
}

impl Into<String> for KalkNum {
    fn into(self) -> String {
        self.to_string()
    }
}

impl From<f64> for KalkNum {
    fn from(x: f64) -> Self {
        KalkNum::new(Float::with_val(63, x), "")
    }
}

impl From<f32> for KalkNum {
    fn from(x: f32) -> Self {
        KalkNum::new(Float::with_val(63, x), "")
    }
}

impl From<i64> for KalkNum {
    fn from(x: i64) -> Self {
        KalkNum::new(Float::with_val(63, x), "")
    }
}

impl From<i32> for KalkNum {
    fn from(x: i32) -> Self {
        KalkNum::new(Float::with_val(63, x), "")
    }
}
