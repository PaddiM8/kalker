pub fn is_superscript(c: &char) -> bool {
    matches!(
        c,
        '⁰' | '¹'
            | '²'
            | '³'
            | '⁴'
            | '⁵'
            | '⁶'
            | '⁷'
            | '⁸'
            | '⁹'
            | '⁺'
            | '⁻'
            | '⁼'
            | '⁽'
            | '⁾'
            | 'ᵀ'
    )
}

pub fn is_subscript(c: &char) -> bool {
    matches!(
        c,
        '₀' | '₁'
            | '₂'
            | '₃'
            | '₄'
            | '₅'
            | '₆'
            | '₇'
            | '₈'
            | '₉'
            | '₊'
            | '₋'
            | '₌'
            | '₍'
            | '₎'
            | 'ₖ'
            | 'ₗ'
            | 'ₘ'
            | 'ₙ'
            | 'ₓ'
    )
}

pub fn parse_subscript(chars: impl Iterator<Item = char>) -> Option<u8> {
    subscript_to_normal(chars).parse::<u8>().ok()
}

pub fn subscript_to_normal(chars: impl Iterator<Item = char>) -> String {
    let mut regular = String::new();
    for c in chars {
        regular.push(match c {
            '₀' => '0',
            '₁' => '1',
            '₂' => '2',
            '₃' => '3',
            '₄' => '4',
            '₅' => '5',
            '₆' => '6',
            '₇' => '7',
            '₈' => '8',
            '₉' => '9',
            '₊' => '+',
            '₋' => '-',
            '₌' => '=',
            '₍' => '(',
            '₎' => ')',
            'ₖ' => 'k',
            'ₗ' => 'l',
            'ₘ' => 'm',
            'ₙ' => 'n',
            'ᵀ' => 'T',
            _ => c,
        });
    }

    regular.trim().to_string()
}

pub fn normal_to_subscript(chars: impl Iterator<Item = char>) -> String {
    let mut subscript = String::new();
    for c in chars {
        subscript.push(match c {
            '0' => '₀',
            '1' => '₁',
            '2' => '₂',
            '3' => '₃',
            '4' => '₄',
            '5' => '₅',
            '6' => '₆',
            '7' => '₇',
            '8' => '₈',
            '9' => '₉',
            '+' => '₊',
            '-' => '₋',
            '=' => '₌',
            '(' => '₍',
            ')' => '₎',
            'k' => 'ₖ',
            'l' => 'ₗ',
            'm' => 'ₘ',
            'n' => 'ₙ',
            'x' => 'ₓ',
            _ => c,
        });
    }

    subscript
}

pub fn superscript_to_normal(chars: impl Iterator<Item = char>) -> String {
    chars
        .map(|c| match c {
            '⁰' => '0',
            '¹' => '1',
            '²' => '2',
            '³' => '3',
            '⁴' => '4',
            '⁵' => '5',
            '⁶' => '6',
            '⁷' => '7',
            '⁸' => '8',
            '⁹' => '9',
            '⁺' => '+',
            '⁻' => '-',
            '⁼' => '=',
            '⁽' => '(',
            '⁾' => ')',
            other => other,
        })
        .collect()
}

pub fn normal_to_superscript(chars: impl Iterator<Item = char>) -> String {
    chars
        .map(|c| match c {
            '0' => '⁰',
            '1' => '¹',
            '2' => '²',
            '3' => '³',
            '4' => '⁴',
            '5' => '⁵',
            '6' => '⁶',
            '7' => '⁷',
            '8' => '⁸',
            '9' => '⁹',
            '+' => '⁺',
            '-' => '⁻',
            '=' => '⁼',
            '(' => '⁽',
            ')' => '⁾',
            other => other,
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::{normal_to_superscript, superscript_to_normal};

    #[test]
    fn superscript_conversion_round_trips_signed_exponents() {
        let superscript = normal_to_superscript("-123".chars());
        assert_eq!("⁻¹²³", superscript);
        assert_eq!("-123", superscript_to_normal(superscript.chars()));
    }
}
