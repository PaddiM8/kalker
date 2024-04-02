use std::collections::HashMap;

use crate::{float, primitive};
use lazy_static::lazy_static;

use super::{ComplexNumberType, KalkValue};

lazy_static! {
    static ref CONSTANTS: HashMap<u32, (u32, &'static str)> = {
        let mut m = HashMap::new();
        m.insert(141592, (3, "π"));
        m.insert(869604, (9, "π²"));
        m.insert(318909, (0, "1/π"));
        m.insert(636619, (0, "2/π"));
        m.insert(718281, (2, "e"));
        m.insert(389056, (7, "e²"));
        m.insert(283185, (6, "τ"));
        m.insert(618033, (1, "ϕ"));
        m.insert(414213, (1, "√2"));
        m.insert(707106, (0, "1/√2"));
        m.insert(693147, (0, "ln(2)"));
        m.insert(302585, (2, "ln(10)"));
        // Radian values for common angles
        m.insert(392699, (0, "π/8"));
        m.insert(523598, (0, "π/6"));
        m.insert(785398, (0, "π/4"));
        m.insert(47197, (1, "π/3"));
        m.insert(570796, (1, "π/2"));
        m.insert(94395, (2, "2π/3"));
        m.insert(356194, (2, "3π/4"));
        m.insert(617993, (2, "5π/6"));
        m.insert(665191, (3, "7π/6"));
        m.insert(926990, (3, "5π/4"));
        m.insert(188790, (4, "4π/3"));
        m.insert(712388, (4, "3π/2"));
        m.insert(23598, (5, "5π/3"));
        m.insert(497787, (5, "7π/4"));
        m.insert(759586, (5, "11π/6"));
        m.insert(283185, (6, "2π"));
        m.insert(866025, (0, "√3/2"));
        m
    };
}

#[derive(Debug)]
pub struct EstimationResult {
    pub value: String,
    pub is_exact: bool,
}

pub(super) fn estimate(
    input: &KalkValue,
    complex_number_type: ComplexNumberType,
) -> Option<EstimationResult> {
    let (real, imaginary, _) = if let KalkValue::Number(real, imaginary, unit) = input {
        (real, imaginary, unit)
    } else {
        return None;
    };

    let value = match complex_number_type {
        ComplexNumberType::Real => real,
        ComplexNumberType::Imaginary => imaginary,
    };

    if value >= &f64::MAX {
        return None;
    }

    let value = primitive!(value);

    // If it's an integer, there's nothing that would be done to it.
    if value.fract() == 0f64 {
        return None;
    }

    if let Some(equivalent_fraction) = equivalent_fraction(value) {
        return Some(equivalent_fraction);
    }

    // Match with common numbers, eg. π, 2π/3, √2
    if let Some(equivalent_constant) = equivalent_constant(value) {
        return Some(EstimationResult {
            value: equivalent_constant,
            is_exact: false,
        });
    }

    // If the value squared (and rounded) is an integer,
    // eg. x² is an integer,
    // then it can be expressed as sqrt(x²).
    // Ignore it if the square root of the result is an integer.
    if let Some(equivalent_root) = equivalent_root(value) {
        return Some(EstimationResult {
            value: equivalent_root,
            is_exact: false,
        });
    }

    // If nothing above was relevant, simply round it off a bit, eg. from 0.99999 to 1
    let rounded = match complex_number_type {
        ComplexNumberType::Real => round(input, complex_number_type)?.values().0,
        ComplexNumberType::Imaginary => round(input, complex_number_type)?.values().1,
    };
    let rounded_str = rounded.to_string();
    let result = trim_zeroes(if rounded_str == "-0" {
        "0"
    } else {
        &rounded_str
    });

    Some(EstimationResult {
        value: result,
        is_exact: false,
    })
}

fn equivalent_fraction(value: f64) -> Option<EstimationResult> {
    fn gcd(mut a: i64, mut b: i64) -> i64 {
        while a != 0 {
            let old_a = a;
            a = b % a;
            b = old_a;
        }

        b.abs()
    }

    let original_sign = value.signum();
    let value = value.abs();
    let abs_value_str = value.to_string();

    // https://goodcalculators.com/repeating-decimal-to-fraction-conversion-calculator/
    let (mut numer, mut denom) = if let Some(repeatend_str) = find_repeatend(&abs_value_str) {
        let dot_pos = abs_value_str.find('.')?;
        let repeatend_pos = abs_value_str[dot_pos..].find(&repeatend_str)? + dot_pos;
        let non_repeating_str = &abs_value_str[..repeatend_pos].trim_end_matches('.');

        // non-repeating
        let non_repeating = non_repeating_str.parse::<f64>().unwrap_or(0f64);
        let non_repeating_dec_count = if let Some(dot_pos) = non_repeating_str.find('.') {
            non_repeating_str.len() - dot_pos - 1
        } else {
            0
        };
        let a = non_repeating.fract();

        // repeatend
        let b = match repeatend_str.parse::<i64>() {
            Ok(b) => b,
            Err(_) => return None,
        };

        let factor = 10i64.pow(non_repeating_dec_count as u32) as f64;
        let nines = (10i64.pow(repeatend_str.len() as u32) - 1) as f64;

        let a_numer = a * factor * nines;
        let b_numer = b as f64;
        let ab_denom = nines * factor;
        let integer_part_as_numer = non_repeating.trunc() * ab_denom;

        (a_numer + b_numer + integer_part_as_numer, ab_denom)
    } else {
        const PREC: f64 = 10e10f64;

        (value * PREC, PREC)
    };

    let gcd = gcd(numer as i64, denom as i64) as f64;
    numer /= gcd;
    denom /= gcd;

    numer = numer.trunc();
    denom = denom.trunc();

    if denom <= 1f64 || denom >= 100f64 || denom as i64 == 10 {
        return None;
    }

    let integer_part = (numer / denom).trunc();
    if integer_part > 1f64 {
        numer -= integer_part * denom;
        let sign = if original_sign.is_sign_positive() {
            "+"
        } else {
            "-"
        };
        let calculated_value =
            original_sign * integer_part + original_sign * (numer.abs() / denom.abs());
        let result_str = format!(
            "{} {} {}/{}",
            original_sign * integer_part,
            sign,
            numer.abs(),
            denom.abs()
        );

        Some(EstimationResult {
            value: result_str,
            is_exact: value == calculated_value,
        })
    } else {
        let calculated_value = numer * original_sign / denom;
        let result_str = format!("{}/{}", numer * original_sign, denom);

        Some(EstimationResult {
            value: result_str,
            is_exact: value == calculated_value,
        })
    }
}

fn find_repeatend(input: &str) -> Option<String> {
    if input.len() < 10 {
        return None;
    }

    for len in 1..=9 {
        for offset in 1..=len {
            let chars: Vec<char> = input[..input.len() - offset].chars().rev().collect();
            let mut chunks = chars.chunks(len);
            let mut repeats = 1;
            let mut prev: &[char] = chunks.next()?;
            for chunk in chunks {
                if chunk == prev {
                    repeats += 1;
                } else {
                    repeats = 0;
                    prev = chunk;
                }

                let required_repeats = match len {
                    1..=3 => 4,
                    _ => 2,
                };
                if repeats >= required_repeats && !prev.iter().all(|x| x == &'0') {
                    return Some(prev.iter().rev().cloned().collect::<String>());
                }
            }
        }
    }

    None
}

fn equivalent_constant(value: f64) -> Option<String> {
    if let Some((constant_trunc, constant)) = CONSTANTS.get(&((value.abs().fract() * 10e5) as u32))
    {
        let additional = value.trunc() as i32 - (*constant_trunc as f64 * value.signum()) as i32;
        let constant_sign = if value.is_sign_positive() { "" } else { "-" };

        if additional == 0 {
            Some(format!("{}{}", constant_sign, constant))
        } else {
            let additional_sign = if additional.is_positive() { "+" } else { "-" };

            Some(format!(
                "{}{} {} {}",
                constant_sign,
                constant,
                additional_sign,
                additional.abs()
            ))
        }
    } else {
        None
    }
}

fn equivalent_root(value: f64) -> Option<String> {
    if value.fract().abs() == 0f64 || value > 10e5f64 {
        return None;
    }

    let squared = KalkValue::Number(float!(value * value), float!(0), None)
        .round_if_needed()
        .values()
        .0;

    if squared.clone().sqrt().fract() != 0f64 && squared.clone().fract() == 0f64 {
        Some(format!("√{}", primitive!(squared) as i32))
    } else {
        None
    }
}

pub(super) fn round(
    input: &KalkValue,
    complex_number_type: ComplexNumberType,
) -> Option<KalkValue> {
    let (real, imaginary, _) = if let KalkValue::Number(real, imaginary, unit) = input {
        (real, imaginary, unit)
    } else {
        return None;
    };

    let value = match complex_number_type {
        ComplexNumberType::Real => real,
        ComplexNumberType::Imaginary => imaginary,
    };
    let sign = if *value < 0f64 { -1f64 } else { 1f64 };
    let fract = value.clone().abs().fract();
    let integer = value.clone().abs().trunc();

    // If it's zero something, don't do the rounding as aggressively.
    let (limit_floor, limit_ceil) = if integer == 0f64 {
        (-8f64, -5f64)
    } else {
        (-4f64, -6f64)
    };

    if fract.clone().log10() < limit_floor {
        // If eg. 0.00xxx
        let new_value = integer * sign;
        let new_num = match complex_number_type {
            ComplexNumberType::Real => {
                KalkValue::Number(new_value, imaginary.clone(), input.get_unit().cloned())
            }
            ComplexNumberType::Imaginary => {
                KalkValue::Number(real.clone(), new_value, input.get_unit().cloned())
            }
        };

        Some(new_num)
    } else if (1f64 - fract).log10() < limit_ceil {
        // If eg. 0.999
        // .abs() this before ceiling to make sure it rounds correctly. The sign is re-added afterwards.
        let new_value = value.clone().abs().ceil() * sign;
        let new_num = match complex_number_type {
            ComplexNumberType::Real => {
                KalkValue::Number(new_value, imaginary.clone(), input.get_unit().cloned())
            }
            ComplexNumberType::Imaginary => {
                KalkValue::Number(real.clone(), new_value, input.get_unit().cloned())
            }
        };

        Some(new_num)
    } else {
        None
    }
}

pub(super) fn trim_zeroes(input: &str) -> String {
    if input.contains('.') {
        input
            .trim_end_matches('0')
            .trim_end_matches('.')
            .to_string()
    } else {
        input.into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_estimate() {
        let in_out = vec![
            (0.99999999, Some(String::from("1"))),
            (-0.9999999, Some(String::from("-1"))),
            (0.0000000001, Some(String::from("0"))),
            (-0.000000001, Some(String::from("0"))),
            (1.99999999, Some(String::from("2"))),
            (-1.9999999, Some(String::from("-2"))),
            (1.000000001, Some(String::from("1"))),
            (-1.000001, Some(String::from("-1"))),
            (0.5, Some(String::from("1/2"))),
            (-0.5, Some(String::from("-1/2"))),
            (0.3333333333, Some(String::from("1/3"))),
            (1.3333333333, Some(String::from("4/3"))),
            (-0.666666666, Some(String::from("-2/3"))),
            (-1.666666666, Some(String::from("-5/3"))),
            (100.33333333, Some(String::from("100 + 1/3"))),
            (-100.6666666, Some(String::from("-100 - 2/3"))),
            (0.9932611, None),
            (-0.9932611, None),
            (-0.00001, None),
            (1.9932611, None),
            (-1.9932611, None),
            (24f64, None),
            (-24f64, None),
            (1.23456f64, None),
            (-1.23456f64, None),
            (9999999999f64, None),
            (-9999999999f64, None),
            (1000000001f64, None),
            (-1000000001f64, None),
            (0.53f64, None),
            (-0.53f64, None),
            (-1.51f64, None),
            (0.335f64, None),
            (-0.335f64, None),
            (0.665f64, None),
            (-0.665f64, None),
            (100f64, None),
            (-100f64, None),
            (1f64, None),
            (0.1f64, None),
            (1.23f64, None),
            (1.234f64, None),
            (1.2345f64, None),
            (1.23456f64, None),
            (1.234567f64, None),
            (1.2345678f64, None),
            (1.23456789f64, None),
            (-0.1f64, None),
            (-1.23f64, None),
            (-1.234f64, None),
            (-1.2345f64, None),
            (-1.23456f64, None),
            (-1.234567f64, None),
            (-1.2345678f64, None),
            (-1.23456789f64, None),
        ];

        for (input, output) in in_out {
            let result = KalkValue::from(input).estimate().map(|x| x.value);
            assert_eq!(output, result);
        }
    }

    #[test]
    fn test_equivalent_fraction() {
        assert_eq!(equivalent_fraction(0.5f64).unwrap().value, "1/2");
        assert_eq!(equivalent_fraction(-0.5f64).unwrap().value, "-1/2");
        assert_eq!(equivalent_fraction(1f64 / 3f64).unwrap().value, "1/3");
        assert_eq!(equivalent_fraction(4f64 / 3f64).unwrap().value, "4/3");
        assert_eq!(equivalent_fraction(7f64 / 3f64).unwrap().value, "2 + 1/3");
        assert_eq!(equivalent_fraction(-1f64 / 12f64).unwrap().value, "-1/12");
        assert_eq!(equivalent_fraction(-16f64 / -7f64).unwrap().value, "2 + 2/7");
        assert!(equivalent_fraction(0.123f64).is_none());
        assert!(equivalent_fraction(1f64).is_none());
        assert!(equivalent_fraction(0.01f64).is_none());
        assert!(equivalent_fraction(-0.9999999f64).is_none());
        assert!(equivalent_fraction(0.9999999f64).is_none());
        assert!(equivalent_fraction(1.9999999f64).is_none());
    }
}
