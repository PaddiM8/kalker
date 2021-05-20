use crate::kalk_num::*;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
#[derive(PartialEq, Debug, Clone, Default)]
pub struct KalkNum {
    pub(crate) value: f64,
    pub(crate) unit: String,
    pub(crate) imaginary_value: f64,
}

#[wasm_bindgen]
impl KalkNum {
    pub fn new(value: f64, unit: &str) -> Self {
        Self {
            value,
            unit: unit.to_string(),
            imaginary_value: 0f64,
        }
    }

    pub fn new_with_imaginary(value: f64, unit: &str, imaginary_value: f64) -> Self {
        Self {
            value,
            unit: unit.to_string(),
            imaginary_value,
        }
    }

    #[wasm_bindgen(js_name = getValue)]
    pub fn to_f64(&self) -> f64 {
        self.value
    }

    pub fn to_i32(&self) -> i32 {
        self.value as i32
    }

    #[wasm_bindgen(js_name = toString)]
    pub fn to_string(&self) -> String {
        let string_value = self.value.to_string();
        if string_value.contains(".") {
            string_value
                .trim_end_matches('0')
                .trim_end_matches('.')
                .to_string()
        } else {
            string_value
        }
    }

    #[wasm_bindgen(js_name = toStringBig)]
    pub fn to_string_big_js(&self) -> String {
        self.to_string_big()
    }

    #[wasm_bindgen(js_name = isTooBig)]
    pub fn is_too_big_js(&self) -> bool {
        self.is_too_big()
    }

    #[wasm_bindgen(js_name = toStringWithUnit)]
    pub fn to_string_with_unit_js(&self) -> String {
        self.to_string_with_unit()
    }

    #[wasm_bindgen(js_name = hasUnit)]
    pub fn has_unit_js(&self) -> bool {
        self.has_unit()
    }

    #[wasm_bindgen(js_name = getUnit)]
    pub fn get_unit(&self) -> String {
        self.unit.clone()
    }

    #[wasm_bindgen(js_name = toScientificNotation)]
    pub fn to_scientific_notation_js(&self) -> ScientificNotation {
        self.to_scientific_notation()
    }

    #[wasm_bindgen(js_name = estimate)]
    pub fn estimate_js(&self) -> Option<String> {
        self.estimate()
    }

    pub(crate) fn pow(self, context: &mut crate::interpreter::Context, rhs: KalkNum) -> KalkNum {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        KalkNum::new(self.value.powf(right.value), &right.unit)
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
