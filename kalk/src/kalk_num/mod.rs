#[cfg(feature = "rug")]
pub mod with_rug;
#[cfg(feature = "rug")]
pub use with_rug::*;

#[cfg(not(feature = "rug"))]
pub mod regular;
#[cfg(not(feature = "rug"))]
pub use regular::*;

use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
    static ref CONSTANTS: HashMap<&'static str, &'static str> = {
        let mut m = HashMap::new();
        m.insert("3.141592", "π");
        m.insert("2.718281", "e");
        m.insert("6.283185", "tau");
        m.insert("6.283185", "τ");
        m.insert("1.618033", "phi");
        m.insert("1.618033", "ϕ");
        m.insert("1.414213", "√2");
        // Radian values for common angles
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

impl KalkNum {
    // Get an estimate of what the number is, eg. 3.141592 => π
    pub fn estimate(&self) -> Option<String> {
        let fract = self.value.clone().fract().abs();
        let integer = self.value.clone().trunc();

        // If it's an integer, there's nothing that would be done to it.
        if fract == 0f64 {
            return None;
        }

        // Eg. 0.5 to 1/2
        let as_abs_string = self.to_string().trim_start_matches("-").to_string();
        let sign = if self.value < 0f64 { "-" } else { "" };
        let fract_as_string = fract.to_string();
        if as_abs_string.starts_with("0.5") {
            if as_abs_string.len() == 3 || (as_abs_string.len() > 6 && &as_abs_string[3..5] == "00")
            {
                return Some(format!("{}1/2", sign));
            }
        }

        // Eg. 1.33333333 to 1 + 1/3
        if fract_as_string.len() >= 7 {
            let first_five_decimals = &fract_as_string[2..7];
            if first_five_decimals == "33333" || first_five_decimals == "66666" {
                let fraction = match first_five_decimals.as_ref() {
                    "33333" => "1/3",
                    "66666" => "2/3",
                    _ => "?",
                };

                if integer == 0f64 {
                    return Some(format!("{}{}", sign, fraction));
                } else {
                    let explicit_sign = if sign == "" { "+" } else { "-" };
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
                return Some(format!("{}{}", sign, constant.to_string()));
            }
        }

        // If nothing above was relevant, simply round it off a bit, eg. from 0.99999 to 1
        let rounded = self.round()?.to_string();
        Some(trim_zeroes(&rounded))
    }

    /// Basic up/down rounding from 0.00xxx or 0.999xxx or xx.000xxx, etc.
    pub fn round(&self) -> Option<KalkNum> {
        let sign = if self.value < 0f64 { -1f64 } else { 1f64 };
        let fract = self.value.clone().abs().fract();
        let integer = self.value.clone().abs().trunc();

        // If it's zero something, don't do the rounding as aggressively.
        let (limit_floor, limit_ceil) = if integer == 0f64 {
            (-8f64, -5f64)
        } else {
            (-4f64, -6f64)
        };

        if fract.clone().log10() < limit_floor {
            // If eg. 0.00xxx
            Some(KalkNum::new(integer * sign, &self.unit))
        } else if (1f64 - fract.clone()).log10() < limit_ceil {
            // If eg. 0.999
            // .abs() this before ceiling to make sure it rounds correctly. The sign is re-added afterwards.
            Some(KalkNum::new(
                self.value.clone().abs().ceil() * sign,
                &self.unit,
            ))
        } else {
            None
        }
    }

    pub fn round_if_needed(&self) -> KalkNum {
        if let Some(value) = self.round() {
            value
        } else {
            self.clone() // Hmm
        }
    }
}

fn trim_zeroes(input: &str) -> String {
    if input.contains(".") {
        input
            .trim_end_matches("0")
            .trim_end_matches(".")
            .to_string()
    } else {
        input.into()
    }
}

#[cfg(test)]
mod tests {
    use crate::kalk_num::KalkNum;

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
            (1.3333333333, Some(String::from("1 + 1/3"))),
            (-0.666666666, Some(String::from("-2/3"))),
            (-1.666666666, Some(String::from("-1 - 2/3"))),
            (-1.666666666, Some(String::from("-1 - 2/3"))),
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
            (1.98, None),
            (-1.98, None),
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
            (0.12f64, None),
            (0.1f64, None),
            (1.2f64, None),
            (1.23f64, None),
            (1.234f64, None),
            (1.2345f64, None),
            (1.23456f64, None),
            (1.234567f64, None),
            (1.2345678f64, None),
            (1.23456789f64, None),
            (-0.12f64, None),
            (-0.1f64, None),
            (-1.2f64, None),
            (-1.23f64, None),
            (-1.234f64, None),
            (-1.2345f64, None),
            (-1.23456f64, None),
            (-1.234567f64, None),
            (-1.2345678f64, None),
            (-1.23456789f64, None),
        ];

        for (input, output) in in_out {
            let result = KalkNum::from(input).estimate();
            println!("{}", input);
            assert_eq!(output, result);
        }
    }
}
