pub mod special_funcs {
    use crate::kalk_num::KalkNum;

    pub fn factorial(x: KalkNum) -> KalkNum {
        // Round it a bit, to prevent floating point errors.
        KalkNum::new(
            (super::funcs::precise_gamma(x.value + 1f64) * 10e6f64).round() / 10e6f64,
            &x.unit,
        )
    }
}

pub(crate) mod funcs {
    use crate::kalk_num::KalkNum;
    use crate::prelude::funcs::abs;

    pub fn arg(x: KalkNum) -> KalkNum {
        // i(ln|x| - ln(x))
        KalkNum::new(x.imaginary_value.atan2(x.value), &x.unit)
    }

    pub fn gamma(x: KalkNum) -> KalkNum {
        // Round it a bit, to prevent floating point errors.
        KalkNum::new(
            (precise_gamma(x.value) * 10e6f64).round() / 10e6f64,
            &x.unit,
        )
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

    pub fn bitcmp(x: KalkNum, y: KalkNum) -> KalkNum {
        KalkNum::from(!x.value.round() as i32)
    }

    pub fn bitand(x: KalkNum, y: KalkNum) -> KalkNum {
        KalkNum::from(x.value.round() as i32 & y.value.round() as i32)
    }

    pub fn bitor(x: KalkNum, y: KalkNum) -> KalkNum {
        KalkNum::from(x.value.round() as i32 | y.value.round() as i32)
    }

    pub fn bitxor(x: KalkNum, y: KalkNum) -> KalkNum {
        KalkNum::from(x.value.round() as i32 ^ y.value.round() as i32)
    }

    pub fn bitshift(x: KalkNum, y: KalkNum) -> KalkNum {
        let x = x.value.round() as i32;
        let y = y.value.round() as i32;
        if y < 0 {
            KalkNum::from((x >> y.abs()))
        } else {
            KalkNum::from((x << y))
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
            KalkNum::new(x.value.hypot(y.value), &x.unit)
        }
    }

    pub fn max(x: KalkNum, y: KalkNum) -> KalkNum {
        KalkNum::new(x.value.max(y.value), &x.unit)
    }

    pub fn min(x: KalkNum, y: KalkNum) -> KalkNum {
        KalkNum::new(x.value.min(y.value), &x.unit)
    }
}
