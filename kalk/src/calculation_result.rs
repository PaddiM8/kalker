use wasm_bindgen::prelude::wasm_bindgen;

use crate::kalk_value::{ComplexNumberType, KalkValue, ScientificNotation};

#[wasm_bindgen]
pub struct CalculationResult {
    value: KalkValue,
    radix: u8,
}

// Wraps around KalkValue since enums don't work
// with the javascript bindings.
#[wasm_bindgen]
impl CalculationResult {
    pub(crate) fn new(value: KalkValue, radix: u8) -> Self {
        CalculationResult { value, radix }
    }

    #[allow(dead_code)]
    pub(crate) fn get_value(self) -> KalkValue {
        self.value
    }

    #[wasm_bindgen(js_name = toString)]
    pub fn to_string(&self) -> String {
        self.value.to_string()
    }

    #[wasm_bindgen(js_name = toStringBig)]
    pub fn to_string_big(&self) -> String {
        self.value.to_string_big()
    }

    #[wasm_bindgen(js_name = toPrettyString)]
    pub fn to_string_pretty(&self) -> String {
        if self.radix == 10 {
            self.value.to_string_pretty_radix(10)
        } else {
            format!(
                "{}\n{}",
                self.value.to_string_pretty_radix(10),
                self.value.to_string_pretty_radix(self.radix),
            )
        }
    }

    #[wasm_bindgen(js_name = getValue)]
    pub fn to_f64(&self) -> f64 {
        self.value.to_f64()
    }

    #[wasm_bindgen(js_name = getImaginaryValue)]
    pub fn imaginary_to_f64(&self) -> f64 {
        self.value.imaginary_to_f64()
    }

    pub(crate) fn set_radix(&mut self, radix: u8) {
        self.radix = radix;
    }

    #[wasm_bindgen(js_name = toScientificNotation)]
    pub fn to_scientific_notation_js(
        &self,
        complex_number_type: ComplexNumberType,
    ) -> ScientificNotation {
        self.value.to_scientific_notation(complex_number_type)
    }

    #[wasm_bindgen(js_name = estimate)]
    pub fn estimate_js(&self) -> Option<String> {
        self.value.estimate()
    }
}
