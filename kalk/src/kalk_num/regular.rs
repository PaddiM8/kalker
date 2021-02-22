use crate::ast::Expr;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
#[derive(PartialEq, Debug, Clone, Default)]
pub struct KalkNum {
    pub(crate) value: f64,
    pub(crate) unit: String,
}

#[wasm_bindgen]
#[derive(Clone)]
pub struct ScientificNotation {
    pub negative: bool,
    pub(crate) digits: String,
    pub exponent: i32,
}

#[wasm_bindgen]
impl ScientificNotation {
    #[wasm_bindgen(js_name = toString)]
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

#[wasm_bindgen]
impl KalkNum {
    pub fn new(value: f64, unit: &str) -> Self {
        Self {
            value,
            unit: unit.to_string(),
        }
    }

    #[wasm_bindgen(js_name = getValue)]
    pub fn to_f64(&self) -> f64 {
        self.value
    }

    #[wasm_bindgen(js_name = toString)]
    pub fn to_string(&self) -> String {
        self.value
            .to_string()
            .trim_end_matches('0')
            .trim_end_matches('.')
    }

    #[wasm_bindgen(js_name = toStringBig)]
    pub fn to_string_big(&self) -> String {
        self.value.to_string()
    }

    #[wasm_bindgen(js_name = isTooBig)]
    pub fn is_too_big(&self) -> bool {
        self.value.is_infinite()
    }

    #[wasm_bindgen(js_name = toStringWithUnit)]
    pub fn to_string_with_unit(&self) -> String {
        format!("{} {}", self.to_string(), self.unit)
    }

    #[wasm_bindgen(js_name = getUnit)]
    pub fn get_unit(&self) -> String {
        self.unit.clone()
    }

    #[wasm_bindgen(js_name = hasUnit)]
    pub fn has_unit(&self) -> bool {
        self.unit.len() > 0
    }

    #[wasm_bindgen(js_name = toScientificNotation)]
    pub fn to_scientific_notation(&self) -> ScientificNotation {
        ScientificNotation {
            negative: self.value < 0f64,
            digits: self.value.to_string().replace(".", ""),
            exponent: self.value.log(10f64) as i32 + 1,
        }
    }

    pub(crate) fn convert_to_unit(
        &self,
        context: &mut crate::interpreter::Context,
        to_unit: &str,
    ) -> Option<KalkNum> {
        let result = crate::interpreter::convert_unit(
            context,
            &Expr::Literal(self.value),
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
        KalkNum::new(self.value + right.value, &right.unit)
    }

    pub(crate) fn sub(self, context: &mut crate::interpreter::Context, rhs: KalkNum) -> KalkNum {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        KalkNum::new(self.value - right.value, &right.unit)
    }

    pub(crate) fn mul(self, context: &mut crate::interpreter::Context, rhs: KalkNum) -> KalkNum {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        KalkNum::new(self.value * right.value, &right.unit)
    }

    pub(crate) fn div(self, context: &mut crate::interpreter::Context, rhs: KalkNum) -> KalkNum {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        KalkNum::new(self.value / right.value, &right.unit)
    }

    pub(crate) fn rem(self, context: &mut crate::interpreter::Context, rhs: KalkNum) -> KalkNum {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        KalkNum::new(self.value % right.value, &right.unit)
    }

    pub(crate) fn pow(self, context: &mut crate::interpreter::Context, rhs: KalkNum) -> KalkNum {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        KalkNum::new(self.value.powf(right.value), &right.unit)
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
        Some(KalkNum::new(right.value, &left.unit))
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
        self.value
    }
}

impl From<f64> for KalkNum {
    fn from(x: f64) -> Self {
        KalkNum::new(x, "")
    }
}

impl From<f32> for KalkNum {
    fn from(x: f32) -> Self {
        KalkNum::new(x as f64, "")
    }
}

impl From<i128> for KalkNum {
    fn from(x: i128) -> Self {
        KalkNum::new(x as f64, "")
    }
}

impl From<i64> for KalkNum {
    fn from(x: i64) -> Self {
        KalkNum::new(x as f64, "")
    }
}

impl From<i32> for KalkNum {
    fn from(x: i32) -> Self {
        KalkNum::new(x as f64, "")
    }
}
