pub mod special_funcs {
    use crate::prelude::KalkNum;

    pub fn factorial(x: KalkNum) -> KalkNum {
        KalkNum::new((x.value + 1f64).gamma(), &x.unit)
    }
}

pub(crate) mod funcs {
    use crate::kalk_num::KalkNum;
    use crate::prelude::funcs::abs;

    pub fn arg(x: KalkNum) -> KalkNum {
        KalkNum::new(x.imaginary_value.atan2(&x.value), &x.unit)
    }

    pub fn gamma(x: KalkNum) -> KalkNum {
        KalkNum::new(x.value.gamma(), &x.unit)
    }

    pub fn bitcmp(x: KalkNum) -> KalkNum {
        KalkNum::from(!x.value.to_i32_saturating().unwrap_or(i32::MAX))
    }

    pub fn bitand(x: KalkNum, y: KalkNum) -> KalkNum {
        KalkNum::from(
            x.value.to_i32_saturating().unwrap_or(i32::MAX)
                & y.value.to_i32_saturating().unwrap_or(i32::MAX),
        )
    }

    pub fn bitor(x: KalkNum, y: KalkNum) -> KalkNum {
        KalkNum::from(
            x.value.to_i32_saturating().unwrap_or(i32::MAX)
                | y.value.to_i32_saturating().unwrap_or(i32::MAX),
        )
    }

    pub fn bitxor(x: KalkNum, y: KalkNum) -> KalkNum {
        KalkNum::from(
            x.value.to_i32_saturating().unwrap_or(i32::MAX)
                ^ y.value.to_i32_saturating().unwrap_or(i32::MAX),
        )
    }

    pub fn bitshift(x: KalkNum, y: KalkNum) -> KalkNum {
        let x = x.value.to_i32_saturating().unwrap_or(i32::MAX) as i32;
        let y = y.value.to_i32_saturating().unwrap_or(i32::MAX) as i32;
        if y < 0 {
            KalkNum::from(x >> y.abs())
        } else {
            KalkNum::from(x << y)
        }
    }

    pub fn hypot(x: KalkNum, y: KalkNum) -> KalkNum {
        if x.has_imaginary() || y.has_imaginary() {
            let abs_x = abs(x);
            let abs_y = abs(y);
            crate::prelude::funcs::sqrt(
                abs_x
                    .clone()
                    .mul_without_unit(abs_x)
                    .add_without_unit(abs_y.clone().mul_without_unit(abs_y)),
            )
        } else {
            KalkNum::new(x.value.hypot(&y.value), &x.unit)
        }
    }

    pub fn max(x: KalkNum, y: KalkNum) -> KalkNum {
        KalkNum::new(x.value.max(&y.value), &x.unit)
    }

    pub fn min(x: KalkNum, y: KalkNum) -> KalkNum {
        KalkNum::new(x.value.min(&y.value), &x.unit)
    }
}
