pub mod special_funcs {
    use crate::{as_number_or_return, errors::KalkError, float, kalk_value::KalkValue};

    pub fn factorial(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, unit) = as_number_or_return!(x);

        // Round it a bit, to prevent floating point errors.
        Ok(KalkValue::Number(
            (super::funcs::precise_gamma(real + 1f64) * 10e6f64).round() / 10e6f64,
            float!(0),
            unit,
        ))
    }
}

pub(crate) mod funcs {
    use crate::errors::KalkError;
    use crate::kalk_value::KalkValue;
    use crate::prelude::funcs::abs;
    use crate::{as_number_or_return, float};

    pub fn arg(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, imaginary, unit) = as_number_or_return!(x);

        // i(ln|x| - ln(x))
        Ok(KalkValue::Number(imaginary.atan2(real), float!(0), unit))
    }

    pub fn gamma(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, unit) = as_number_or_return!(x);

        // Round it a bit, to prevent floating point errors.
        Ok(KalkValue::Number(
            (precise_gamma(real) * 10e6f64).round() / 10e6f64,
            float!(0),
            unit,
        ))
    }

    // Matthias Eiholzer - https://gitlab.com/matthiaseiholzer/mathru/-/tree/master
    pub(super) fn precise_gamma(x: f64) -> f64 {
        let pi = 3.1415926535897932384626433832795028841971693993751058209749445923f64;
        if x == 0f64 {
            return f64::NAN;
        }

        if x < 0.5f64 {
            return pi / precise_gamma((pi * x).sin() * (1f64 - x));
        }

        let t = x + 6.5;
        let a = 0.99999999999980993 + 676.5203681218851 / x - 1259.1392167224028 / (x + 1f64)
            + 771.32342877765313 / (x + 2f64)
            - 176.61502916214059 / (x + 3f64)
            + 12.507343278686905 / (x + 4f64)
            - 0.13857109526572012 / (x + 5f64)
            + 9.9843695780195716e-6 / (x + 6f64)
            + 1.5056327351493116e-7 / (x + 7f64);

        2f64.sqrt() * pi.sqrt() * t.powf(x - 0.5f64) * (-t).exp() * a
    }

    pub fn bitcmp(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, _) = as_number_or_return!(x);

        Ok(KalkValue::from(!(real.round() as i32)))
    }

    pub fn bitand(x: KalkValue, y: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, _) = as_number_or_return!(x);
        let (real_rhs, _, _) = as_number_or_return!(y);

        Ok(KalkValue::from(
            real.round() as i32 & real_rhs.round() as i32,
        ))
    }

    pub fn bitor(x: KalkValue, y: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, _) = as_number_or_return!(x);
        let (real_rhs, _, _) = as_number_or_return!(y);

        Ok(KalkValue::from(
            real.round() as i32 | real_rhs.round() as i32,
        ))
    }

    pub fn bitxor(x: KalkValue, y: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, _) = as_number_or_return!(x);
        let (real_rhs, _, _) = as_number_or_return!(y);

        Ok(KalkValue::from(
            real.round() as i32 ^ real_rhs.round() as i32,
        ))
    }

    pub fn bitshift(x: KalkValue, y: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, _) = as_number_or_return!(x);
        let (real_rhs, _, _) = as_number_or_return!(y);
        let x = real.round() as i32;
        let y = real_rhs.round() as i32;
        if y < 0 {
            Ok(KalkValue::from(x >> y.abs()))
        } else {
            Ok(KalkValue::from(x << y))
        }
    }

    pub fn hypot(x: KalkValue, y: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, unit) = as_number_or_return!(x.clone());
        let (real_rhs, _, _) = as_number_or_return!(y.clone());
        if x.has_imaginary() || y.has_imaginary() {
            let abs_x = abs(x)?;
            let abs_y = abs(y)?;
            crate::prelude::funcs::sqrt(
                abs_x
                    .clone()
                    .mul_without_unit(&abs_x)?
                    .add_without_unit(&abs_y.clone().mul_without_unit(&abs_y)?)?,
            )
        } else {
            Ok(KalkValue::Number(real.hypot(real_rhs), float!(0), unit))
        }
    }
}
