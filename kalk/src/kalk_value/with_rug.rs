use crate::kalk_value::*;

impl KalkValue {
    pub fn to_f64(&self) -> f64 {
        if let KalkValue::Number(real, _, _) = self {
            real.to_f64_round(rug::float::Round::Nearest)
        } else {
            f64::NAN
        }
    }

    pub fn to_float(&self) -> Float {
        if let KalkValue::Number(real, _, _) = self {
            real.clone()
        } else {
            crate::float!(f64::NAN)
        }
    }

    pub fn imaginary_to_f64(&self) -> f64 {
        if let KalkValue::Number(_, imaginary, _) = self {
            imaginary.to_f64_round(rug::float::Round::Nearest)
        } else {
            f64::NAN
        }
    }

    pub fn imaginary_to_float(&self) -> Float {
        if let KalkValue::Number(_, img, _) = self {
            img.clone()
        } else {
            use crate::float;
            float!(f64::NAN)
        }
    }

    pub fn values(self) -> (Float, Float) {
        if let KalkValue::Number(real, imaginary, _) = self {
            (real, imaginary)
        } else {
            (Float::with_val(63, 0), Float::with_val(63, 0))
        }
    }
}
