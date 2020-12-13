use rug::Float;

#[derive(PartialEq, Debug)]
pub struct KalkNum {
    value: Float,
    unit: String,
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
            value,
            unit: unit.to_string(),
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

    pub fn get_unit(self) -> String {
        self.unit
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
