pub type Matcher = fn(char: u8, &[u8]) -> bool;

pub const VALUE_TYPE_MATCHER: Matcher = |char, slice| {
    match (char, slice) {
        // idents
        (char, _) if char.is_ascii_alphanumeric() || char.is_ascii_whitespace() => true,

        // tuples
        (b'(', ..) | (b')', [b'(', ..]) | (b',', [b'(', ..]) => true,
        // arrays and slices
        (b'[', ..) | (b']', ..) => true,
        _ => false,
    }
};

pub const NUMBER_MATCHER: Matcher = |char, _| match char {
    b'0'..=b'9' => true,
    _ => false,
};

pub const NOT_NEWLINE_MATCHER: Matcher = |char, _| (char != b'\n').into();
pub const NOT_QUOTE_MATCHER: Matcher = |char, _| (char != b'"').into();

pub const IDENTIFIER_MATCHER: Matcher = |char, slice| match (char, slice) {
    (b'a'..=b'z' | b'A'..=b'Z' | b'_', _) => true,
    (b'0'..=b'9', [_, ..]) => true,
    _ => false,
};
