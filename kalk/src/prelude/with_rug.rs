pub mod special_funcs {
    use crate::{as_number_or_return, errors::KalkError, float, prelude::KalkValue};

    pub fn factorial(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, unit) = as_number_or_return!(x);

        Ok(KalkValue::Number((real + 1f64).gamma(), float!(0), unit))
    }
}

pub(crate) mod funcs {
    use crate::errors::KalkError;
    use crate::kalk_value::KalkValue;
    use crate::prelude::funcs::abs;
    use crate::{as_number_or_return, float};

    pub fn arg(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, imaginary, unit) = as_number_or_return!(x);

        Ok(KalkValue::Number(imaginary.atan2(&real), float!(0), unit))
    }

    pub fn gamma(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, unit) = as_number_or_return!(x);

        Ok(KalkValue::Number(real.gamma(), float!(0), unit))
    }

    pub fn bitcmp(x: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, _) = as_number_or_return!(x);

        Ok(KalkValue::from(
            !real.to_i32_saturating().unwrap_or(i32::MAX),
        ))
    }

    pub fn bitand(x: KalkValue, y: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, _) = as_number_or_return!(x);
        let (real_rhs, _, _) = as_number_or_return!(y);

        Ok(KalkValue::from(
            real.to_i32_saturating().unwrap_or(i32::MAX)
                & real_rhs.to_i32_saturating().unwrap_or(i32::MAX),
        ))
    }

    pub fn bitor(x: KalkValue, y: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, _) = as_number_or_return!(x);
        let (real_rhs, _, _) = as_number_or_return!(y);

        Ok(KalkValue::from(
            real.to_i32_saturating().unwrap_or(i32::MAX)
                | real_rhs.to_i32_saturating().unwrap_or(i32::MAX),
        ))
    }

    pub fn bitxor(x: KalkValue, y: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, _) = as_number_or_return!(x);
        let (real_rhs, _, _) = as_number_or_return!(y);

        Ok(KalkValue::from(
            real.to_i32_saturating().unwrap_or(i32::MAX)
                ^ real_rhs.to_i32_saturating().unwrap_or(i32::MAX),
        ))
    }

    pub fn bitshift(x: KalkValue, y: KalkValue) -> Result<KalkValue, KalkError> {
        let (real, _, _) = as_number_or_return!(x);
        let (real_rhs, _, _) = as_number_or_return!(y);

        let x = real.to_i32_saturating().unwrap_or(i32::MAX) as i32;
        let y = real_rhs.to_i32_saturating().unwrap_or(i32::MAX) as i32;
        if y < 0 {
            Ok(KalkValue::from(x >> y.abs()))
        } else {
            Ok(KalkValue::from(x << y))
        }
    }

    pub fn hypot(x: KalkValue, y: KalkValue) -> Result<KalkValue, KalkError> {
        let is_complex = x.has_imaginary() || y.has_imaginary();
        let (real, imaginary, unit) = as_number_or_return!(x);
        let (real_rhs, imaginary_rhs, unit_rhs) = as_number_or_return!(y);
        if is_complex {
            let abs_x = abs(KalkValue::Number(real, imaginary, unit))?;
            let abs_y = abs(KalkValue::Number(real_rhs, imaginary_rhs, unit_rhs))?;
            crate::prelude::funcs::sqrt(
                abs_x
                    .clone()
                    .mul_without_unit(&abs_x)?
                    .add_without_unit(&abs_y.clone().mul_without_unit(&abs_y)?)?,
            )
        } else {
            Ok(KalkValue::Number(real.hypot(&real_rhs), float!(0), unit))
        }
    }
}
