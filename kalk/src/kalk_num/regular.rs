use crate::kalk_num::*;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
#[derive(PartialEq, Debug, Clone, Default)]
pub struct KalkNum {
    pub(crate) value: f64,
    pub(crate) unit: String,
    pub(crate) imaginary_value: f64,
    pub(crate) boolean_value: Option<bool>,
}

#[wasm_bindgen]
impl KalkNum {
    pub fn new(value: f64, unit: &str) -> Self {
        Self {
            value,
            unit: unit.to_string(),
            imaginary_value: 0f64,
            boolean_value: None,
        }
    }

    pub fn new_with_imaginary(value: f64, unit: &str, imaginary_value: f64) -> Self {
        Self {
            value,
            unit: unit.to_string(),
            imaginary_value: if imaginary_value == -0f64 {
                0f64
            } else {
                imaginary_value
            },
            boolean_value: None,
        }
    }

    pub fn from_imaginary(value: f64) -> Self {
        Self {
            value: 0f64,
            unit: String::new(),
            imaginary_value: value,
            boolean_value: None,
        }
    }

    pub fn from_bool(value: bool) -> Self {
        Self {
            value: 0f64,
            unit: String::new(),
            imaginary_value: 0f64,
            boolean_value: Some(value),
        }
    }

    #[wasm_bindgen(js_name = getValue)]
    pub fn to_f64(&self) -> f64 {
        self.value
    }

    #[wasm_bindgen(js_name = getImaginaryValue)]
    pub fn imaginary_to_f64(&self) -> f64 {
        self.imaginary_value
    }

    pub fn to_i32(&self) -> i32 {
        self.value as i32
    }

    #[wasm_bindgen(js_name = toString)]
    pub fn to_string_js(&self) -> String {
        self.to_string()
    }

    #[wasm_bindgen(js_name = toPrettyString)]
    pub fn to_string_pretty_js(&self) -> String {
        self.to_string_pretty()
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
    pub fn to_scientific_notation_js(
        &self,
        complex_number_type: ComplexNumberType,
    ) -> ScientificNotation {
        self.to_scientific_notation(complex_number_type)
    }

    #[wasm_bindgen(js_name = estimate)]
    pub fn estimate_js(&self) -> Option<String> {
        self.estimate()
    }

    pub(crate) fn pow(self, context: &mut crate::interpreter::Context, rhs: KalkNum) -> KalkNum {
        let right = calculate_unit(context, &self, rhs.clone()).unwrap_or(rhs);
        self.pow_without_unit(right)
    }

    pub(crate) fn pow_without_unit(self, rhs: KalkNum) -> KalkNum {
        if self.has_imaginary() || rhs.has_imaginary() || (self.value < 0f64 && rhs.value < 1f64) {
            let a = self.value.clone();
            let b = self.imaginary_value.clone();
            let c = rhs.value;
            let d = rhs.imaginary_value;
            let arg = crate::prelude::funcs::arg(self).value;
            let raised = a.clone() * a + b.clone() * b;
            let exp = raised.clone().powf(c.clone() / 2f64) * (-d.clone() * arg.clone()).exp();
            let polar = c * arg + d / 2f64 * raised.ln();

            KalkNum::new_with_imaginary(
                polar.clone().cos() * exp.clone(),
                &rhs.unit,
                polar.sin() * exp,
            )
        } else {
            KalkNum::new(self.value.powf(rhs.value), &rhs.unit)
        }
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
