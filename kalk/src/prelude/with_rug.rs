pub mod special_funcs {
    use crate::{as_number_or_return, float, prelude::KalkValue};

    pub fn factorial(x: KalkValue) -> KalkValue {
        let (real, _, unit) = as_number_or_return!(x);

        KalkValue::Number((real + 1f64).gamma(), float!(0), unit)
    }
}

pub(crate) mod funcs {
    use crate::kalk_value::KalkValue;
    use crate::prelude::funcs::abs;
    use crate::{as_number_or_return, float};

    pub fn arg(x: KalkValue) -> KalkValue {
        let (real, imaginary, unit) = as_number_or_return!(x);

        KalkValue::Number(imaginary.atan2(&real), float!(0), unit)
    }

    pub fn gamma(x: KalkValue) -> KalkValue {
        let (real, _, unit) = as_number_or_return!(x);

        KalkValue::Number(real.gamma(), float!(0), unit)
    }

    pub fn bitcmp(x: KalkValue) -> KalkValue {
        let (real, _, _) = as_number_or_return!(x);

        KalkValue::from(!real.to_i32_saturating().unwrap_or(i32::MAX))
    }

    pub fn bitand(x: KalkValue, y: KalkValue) -> KalkValue {
        let (real, _, _) = as_number_or_return!(x);
        let (real_rhs, _, _) = as_number_or_return!(y);

        KalkValue::from(
            real.to_i32_saturating().unwrap_or(i32::MAX)
                & real_rhs.to_i32_saturating().unwrap_or(i32::MAX),
        )
    }

    pub fn bitor(x: KalkValue, y: KalkValue) -> KalkValue {
        let (real, _, _) = as_number_or_return!(x);
        let (real_rhs, _, _) = as_number_or_return!(y);

        KalkValue::from(
            real.to_i32_saturating().unwrap_or(i32::MAX)
                | real_rhs.to_i32_saturating().unwrap_or(i32::MAX),
        )
    }

    pub fn bitxor(x: KalkValue, y: KalkValue) -> KalkValue {
        let (real, _, _) = as_number_or_return!(x);
        let (real_rhs, _, _) = as_number_or_return!(y);

        KalkValue::from(
            real.to_i32_saturating().unwrap_or(i32::MAX)
                ^ real_rhs.to_i32_saturating().unwrap_or(i32::MAX),
        )
    }

    pub fn bitshift(x: KalkValue, y: KalkValue) -> KalkValue {
        let (real, _, _) = as_number_or_return!(x);
        let (real_rhs, _, _) = as_number_or_return!(y);

        let x = real.to_i32_saturating().unwrap_or(i32::MAX) as i32;
        let y = real_rhs.to_i32_saturating().unwrap_or(i32::MAX) as i32;
        if y < 0 {
            KalkValue::from(x >> y.abs())
        } else {
            KalkValue::from(x << y)
        }
    }

    pub fn hypot(x: KalkValue, y: KalkValue) -> KalkValue {
        let (real, _, unit) = as_number_or_return!(x.clone());
        let (real_rhs, _, _) = as_number_or_return!(y.clone());
        if x.has_imaginary() || y.has_imaginary() {
            let abs_x = abs(x);
            let abs_y = abs(y);
            crate::prelude::funcs::sqrt(
                abs_x
                    .clone()
                    .mul_without_unit(&abs_x)
                    .add_without_unit(&abs_y.clone().mul_without_unit(&abs_y)),
            )
        } else {
            KalkValue::Number(real.hypot(&real_rhs), float!(0), unit)
        }
    }

    pub fn max(x: KalkValue, y: KalkValue) -> KalkValue {
        let (real, _, unit) = as_number_or_return!(x);
        let (real_rhs, _, _) = as_number_or_return!(y);

        KalkValue::Number(real.max(&real_rhs), float!(0), unit)
    }

    pub fn min(x: KalkValue, y: KalkValue) -> KalkValue {
        let (real, _, unit) = as_number_or_return!(x);
        let (real_rhs, _, _) = as_number_or_return!(y);

        KalkValue::Number(real.min(&real_rhs), float!(0), unit)
    }
}
