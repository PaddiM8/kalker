use wasm_bindgen::prelude::wasm_bindgen;

use crate::kalk_value::{ComplexNumberType, KalkValue, ScientificNotation, ScientificNotationFormat};

#[wasm_bindgen]
pub struct CalculationResult {
    value: KalkValue,
    radix: u8,
    is_approximation: bool,
    equation_variable: Option<String>,
}

// Wraps around KalkValue since enums don't work
// with the javascript bindings.
#[wasm_bindgen]
impl CalculationResult {
    pub(crate) fn new(value: KalkValue, radix: u8, is_approximation: bool, equation_variable: Option<String>) -> Self {
        CalculationResult {
            value,
            radix,
            is_approximation,
            equation_variable,
        }
    }

    #[allow(dead_code)]
    pub(crate) fn get_value(self) -> KalkValue {
        self.value
    }

    #[wasm_bindgen(js_name = toString)]
    pub fn to_js_string(&self) -> String {
        self.to_string()
    }

    #[wasm_bindgen(js_name = toStringBig)]
    pub fn to_string_big(&self) -> String {
        self.value.to_string_big()
    }

    #[wasm_bindgen(js_name = toPrettyStringWithFormat)]
    pub fn to_string_pretty_format(&self, format: ScientificNotationFormat) -> String {
        let value = if self.radix == 10 {
            self.value.to_string_pretty_radix(10, format)
        } else {
            format!(
                "{}\n{}",
                self.value.to_string_pretty_radix(10, format),
                self.value.to_string_pretty_radix(self.radix, format),
            )
        };

        let decimal_count = if let Some(dot_index) = value.chars().position(|c| c == '.') {
            let end_index = value.chars().position(|c| c == ' ' || c == 'i').unwrap_or(value.len()) - 1;

            if end_index > dot_index { end_index - dot_index } else { 0 }
        } else {
            0
        };

        let equation_variable = if let Some(name) = &self.equation_variable {
            format!("{} ", name)
        } else {
            String::new()
        };

        if self.is_approximation || decimal_count == 10 {
            format!("{}≈ {}", equation_variable, value)
        } else {
            format!("{}= {}", equation_variable, value)
        }
    }

    #[wasm_bindgen(js_name = toPrettyString)]
    pub fn to_string_pretty(&self) -> String {
        self.to_string_pretty_format(ScientificNotationFormat::Normal)
    }

    #[wasm_bindgen(js_name = getValue)]
    pub fn to_f64(&self) -> f64 {
        self.value.to_f64()
    }

    #[wasm_bindgen(js_name = getImaginaryValue)]
    pub fn imaginary_to_f64(&self) -> f64 {
        self.value.imaginary_to_f64()
    }

    #[wasm_bindgen(js_name = setRadix)]
    pub fn set_radix(&mut self, radix: u8) -> bool {
        if radix <= 1 || radix >= 50 {
            return false;
        }

        self.radix = radix;

        true
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
        self.value.estimate().map(|x| x.value)
    }
}

impl std::fmt::Display for CalculationResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}
