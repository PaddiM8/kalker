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
}

impl KalkNum {
    pub fn new(value: Float, unit: &str) -> Self {
        Self {
            value,
            unit: unit.to_string(),
        }
    }

    pub fn to_f64(&self) -> f64 {
        self.value.to_f64_round(rug::float::Round::Nearest)
    }

    pub fn to_i32(&self) -> i32 {
        self.value.to_i32_saturating().unwrap()
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

    pub fn get_unit(&self) -> &str {
        &self.unit
    }

    pub(crate) fn pow(self, context: &mut crate::interpreter::Context, rhs: KalkNum) -> KalkNum {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        KalkNum::new(self.value.pow(right.value), &right.unit)
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
