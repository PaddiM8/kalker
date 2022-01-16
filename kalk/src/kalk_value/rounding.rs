use std::{collections::HashMap};

use crate::{float, primitive};
use lazy_static::lazy_static;

use super::{ComplexNumberType, KalkValue};

lazy_static! {
    static ref CONSTANTS: HashMap<&'static str, &'static str> = {
        let mut m = HashMap::new();
        m.insert("3.141592", "π");
        m.insert("9.869604", "π²");
        m.insert("0.318309", "1/π");
        m.insert("0.636619", "2/π");
        m.insert("2.718281", "e");
        m.insert("7.389056", "e²");
        m.insert("6.283185", "τ");
        m.insert("1.618033", "ϕ");
        m.insert("1.414213", "√2");
        m.insert("0.707106", "1/√2");
        m.insert("0.693147", "ln(2)");
        m.insert("2.302585", "ln(10)");
        // Radian values for common angles
        m.insert("0.392699", "π/8");
        m.insert("0.523598", "π/6");
        m.insert("0.785398", "π/4");
        m.insert("1.047197", "π/3");
        m.insert("1.570796", "π/2");
        m.insert("2.094395", "2π/3");
        m.insert("2.356194", "3π/4");
        m.insert("2.617993", "5π/6");
        m.insert("3.665191", "7π/6");
        m.insert("3.926990", "5π/4");
        m.insert("4.188790", "4π/3");
        m.insert("4.712388", "3π/2");
        m.insert("5.23598", "5π/3");
        m.insert("5.497787", "7π/4");
        m.insert("5.759586", "11π/6");
        m.insert("6.283185", "2π");
        m.insert("0.866025", "√3/2");
        m
    };
}


pub(super) fn estimate(
    input: &KalkValue,
    complex_number_type: ComplexNumberType,
) -> Option<String> {
    let (real, imaginary, _) = if let KalkValue::Number(real, imaginary, unit) = input {
        (real, imaginary, unit)
    } else {
        return None;
    };

    let (value, value_string) = match complex_number_type {
        ComplexNumberType::Real => (real, input.to_string_real(10)),
        ComplexNumberType::Imaginary => (imaginary, input.to_string_imaginary(10, true)),
    };

    let fract = value.clone().fract().abs();
    let integer = value.clone().trunc();

    #[cfg(feature = "rug")]
    let fract_as_string = fract.to_f64().to_string();
    #[cfg(not(feature = "rug"))]
    let fract_as_string = fract.to_string();

    // If it's an integer, there's nothing that would be done to it.
    if fract == 0f64 {
        return None;
    }

    // Eg. 0.5 to 1/2
    let as_abs_string = value_string.trim_start_matches('-').to_string();
    let sign = if value < &0f64 { "-" } else { "" };
    if as_abs_string.starts_with("0.5")
        && (as_abs_string.len() == 3 || (as_abs_string.len() > 6 && &as_abs_string[3..5] == "00"))
    {
        return Some(format!("{}1/2", sign));
    }

    // Eg. 1.33333333 to 1 + 1/3
    if fract_as_string.len() >= 7 {
        let first_five_decimals = &fract_as_string[2..7];
        if first_five_decimals == "33333" || first_five_decimals == "66666" {
            let fraction = match first_five_decimals {
                "33333" => "1/3",
                "66666" => "2/3",
                _ => "?",
            };

            if integer == 0f64 {
                return Some(format!("{}{}", sign, fraction));
            } else {
                let explicit_sign = if sign.is_empty() { "+" } else { "-" };
                return Some(format!(
                    "{} {} {}",
                    trim_zeroes(&integer.to_string()),
                    explicit_sign,
                    fraction
                ));
            }
        }
    }

    // Match with common numbers, eg. π, 2π/3, √2
    if as_abs_string.len() >= 8 {
        if let Some(constant) = CONSTANTS.get(&as_abs_string[0..8]) {
            return Some(format!("{}{}", sign, constant));
        }
    }

    // If the value squared (and rounded) is an integer,
    // eg. x² is an integer,
    // then it can be expressed as sqrt(x²).
    // Ignore it if the square root of the result is an integer.
    if fract != 0f64 {
        let squared = KalkValue::Number(value.clone() * value, float!(0), String::new())
            .round_if_needed()
            .values()
            .0;
        if squared.clone().sqrt().fract() != 0f64 && squared.clone().fract() == 0f64 {
            return Some(format!("√{}", primitive!(squared) as i32));
        }
    }

    // If nothing above was relevant, simply round it off a bit, eg. from 0.99999 to 1
    let rounded = match complex_number_type {
        ComplexNumberType::Real => round(input, complex_number_type)?.values().0,
        ComplexNumberType::Imaginary => round(input, complex_number_type)?.values().1,
    };
    let rounded_str = rounded.to_string();
    Some(trim_zeroes(if rounded_str == "-0" {
        "0"
    } else {
        &rounded_str
    }))
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
                KalkValue::Number(new_value, imaginary.clone(), input.get_unit())
            }
            ComplexNumberType::Imaginary => {
                KalkValue::Number(real.clone(), new_value, input.get_unit())
            }
        };

        Some(new_num)
    } else if (1f64 - fract).log10() < limit_ceil {
        // If eg. 0.999
        // .abs() this before ceiling to make sure it rounds correctly. The sign is re-added afterwards.
        let new_value = value.clone().abs().ceil() * sign;
        let new_num = match complex_number_type {
            ComplexNumberType::Real => {
                KalkValue::Number(new_value, imaginary.clone(), input.get_unit())
            }
            ComplexNumberType::Imaginary => {
                KalkValue::Number(real.clone(), new_value, input.get_unit())
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
