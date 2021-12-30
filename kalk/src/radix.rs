pub fn parse_float_radix(value: String, radix: u8) -> Option<f64> {
    if radix == 10 {
        return if let Ok(result) = value.parse::<f64>() {
            Some(result)
        } else {
            None
        };
    }

    let mut sum = 0f64;
    let length = value.find('_').unwrap_or(value.len());
    let mut i = (value.find('.').unwrap_or(length) - 1) as i32;
    for c in value.chars() {
        if c == '_' {
            break;
        }

        if c == '.' {
            continue;
        }

        let digit = c.to_digit(radix as u32)? as f64;
        sum += digit * (radix as f64).powi(i as i32);
        i -= 1;
    }

    return Some(sum);
}
