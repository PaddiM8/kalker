use crate::kalk_num::*;

impl Default for KalkNum {
    fn default() -> Self {
        KalkNum::new(Float::with_val(63, 0), "")
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct KalkNum {
    pub(crate) value: Float,
    pub(crate) unit: String,
    pub(crate) imaginary_value: Float,
}

impl KalkNum {
    pub fn new(value: Float, unit: &str) -> Self {
        Self {
            value,
            unit: unit.to_string(),
            imaginary_value: Float::with_val(63, 0),
        }
    }

    pub fn new_with_imaginary(value: Float, unit: &str, imaginary_value: Float) -> Self {
        Self {
            value,
            unit: unit.to_string(),
            imaginary_value,
        }
    }

    pub fn from_imaginary(value: Float) -> Self {
        Self {
            value: Float::with_val(63, 0),
            unit: String::new(),
            imaginary_value: value,
        }
    }

    pub fn to_f64(&self) -> f64 {
        self.value.to_f64_round(rug::float::Round::Nearest)
    }

    pub fn imaginary_to_f64(&self) -> f64 {
        self.imaginary_value
            .to_f64_round(rug::float::Round::Nearest)
    }

    pub fn to_i32(&self) -> i32 {
        self.value.to_i32_saturating().unwrap()
    }

    pub fn get_unit(&self) -> &str {
        &self.unit
    }

    pub(crate) fn pow(self, context: &mut crate::interpreter::Context, rhs: KalkNum) -> KalkNum {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        self.pow_without_unit(right)
    }

    pub(crate) fn pow_without_unit(self, rhs: KalkNum) -> KalkNum {
        if self.has_imaginary() || rhs.has_imaginary() {
            let a = self.value.clone();
            let b = self.imaginary_value.clone();
            let c = rhs.value;
            let d = rhs.imaginary_value;
            let arg = crate::prelude::funcs::arg(self).value;
            let raised = a.clone() * a + b.clone() * b;
            let exp = raised.clone().pow(c.clone() / 2f64) * (-d.clone() * arg.clone()).exp();
            let polar = c * arg + d / 2f64 * raised.ln();

            KalkNum::new_with_imaginary(
                polar.clone().cos() * exp.clone(),
                &rhs.unit,
                polar.sin() * exp,
            )
        } else {
            KalkNum::new(self.value.pow(rhs.value), &rhs.unit)
        }
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

impl From<i128> for KalkNum {
    fn from(x: i128) -> Self {
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
