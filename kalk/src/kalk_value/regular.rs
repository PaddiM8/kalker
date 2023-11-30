use crate::kalk_value::*;

impl KalkValue {
    pub fn to_f64(&self) -> f64 {
        if let KalkValue::Number(real, _, _) = self {
            *real
        } else {
            f64::NAN
        }
    }

    pub fn to_float(&self) -> f64 {
        self.to_f64()
    }

    pub fn imaginary_to_float(&self) -> f64 {
        self.imaginary_to_f64()
    }

    pub fn imaginary_to_f64(&self) -> f64 {
        if let KalkValue::Number(_, imaginary, _) = self {
            *imaginary
        } else {
            f64::NAN
        }
    }

    pub fn values(self) -> (f64, f64) {
        if let KalkValue::Number(real, imaginary, _) = self {
            (real, imaginary)
        } else {
            (0f64, 0f64)
        }
    }
}
