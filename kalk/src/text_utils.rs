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
    if let Ok(result) = subscript_to_normal(chars).parse::<u8>() {
        Some(result)
    } else {
        None
    }
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

    return regular.trim().to_string();
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
