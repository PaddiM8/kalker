use crate::{float, kalk_value::KalkFloat};

pub fn parse_float_radix(value: &str, radix: u8) -> Option<KalkFloat> {
    if radix == 10 {
        #[cfg(feature = "rug")]
        let parsed = rug::Float::parse(value).map(|valid| crate::float!(valid));

        #[cfg(not(feature = "rug"))]
        let parsed = value.parse::<f64>();

        return if let Ok(result) = parsed {
            Some(result)
        } else {
            None
        };
    }

    let mut sum = float!(0f64);
    let length = value.find('_').unwrap_or(value.len());
    let mut i = (value.find('.').unwrap_or(length) as i32) - 1;
    for c in value.chars() {
        if c == '_' {
            break;
        }

        if c == '.' {
            continue;
        }

        let digit = c.to_digit(radix as u32)? as f64;
        sum += digit * (radix as f64).powi(i);
        i -= 1;
    }

    Some(sum)
}

const DIGITS: &str = "0123456789abcdefghijklmnopqrstuvwxyz";
pub fn int_to_radix(value: i64, radix: u8) -> String {
    let mut num = value.abs();
    let mut result_str = String::new();
    while num > 0 {
        let digit_index = (num % radix as i64) as usize;
        result_str.insert(0, DIGITS.as_bytes()[digit_index] as char);
        num /= radix as i64;
    }

    if result_str.is_empty() {
        return String::from("0");
    }

    let sign = if value.is_positive() { "" } else { "-" };
    format!("{}{}", sign, result_str)
}

pub fn float_to_radix(value: f64, radix: u8) -> String {
    let mut result = int_to_radix(value.floor() as i64, radix);
    let fract = value.fract();
    if fract != 0f64 {
        result.push('.');
        let precision = 10;
        let fract_digits = (fract * (radix as i64).pow(precision) as f64) as i64;
        result.push_str(int_to_radix(fract_digits, radix).trim_end_matches('0'))
    }

    result
}

pub fn to_radix_pretty(value: f64, radix: u8) -> String {
    if radix == 10 {
        crate::kalk_value::format_number(value)
    } else {
        format!(
            "{}{}",
            float_to_radix(value, radix),
            crate::text_utils::normal_to_subscript(radix.to_string().chars())
        )
    }
}
