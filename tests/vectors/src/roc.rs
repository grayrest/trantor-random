//! Roc literal formatting.

pub fn from_hex(hex: &str) -> Vec<u8> {
    assert!(hex.len() % 2 == 0, "odd hex length");
    (0..hex.len()).step_by(2).map(|i| u8::from_str_radix(&hex[i..i + 2], 16).expect("hex digit")).collect()
}

pub fn u64(value: u64) -> String {
    format!("0x{value:x}")
}

pub fn i64(value: i64) -> String {
    if value == i64::MIN { "I64.lowest".to_string() } else { value.to_string() }
}

pub fn f64(value: f64) -> String {
    format!("F64.from_bits({})", u64(value.to_bits()))
}

pub fn bool(value: bool) -> String {
    if value { "True" } else { "False" }.to_string()
}

pub fn str(value: &str) -> String {
    format!("{value:?}")
}

/// `[a, b, c]`, wrapped every `per_line` items with the given indent.
pub fn list(items: impl IntoIterator<Item = String>, per_line: usize, indent: &str) -> String {
    let items: Vec<String> = items.into_iter().collect();
    if items.len() <= per_line {
        return format!("[{}]", items.join(", "));
    }
    let lines: Vec<String> = items.chunks(per_line).map(|chunk| format!("{indent}\t{},", chunk.join(", "))).collect();
    format!("[\n{}\n{indent}]", lines.join("\n"))
}

pub fn u64s(values: &[u64], indent: &str) -> String {
    list(values.iter().map(|&v| u64(v)), 4, indent)
}

pub fn bytes(values: &[u8], indent: &str) -> String {
    list(values.iter().map(|&v| format!("0x{v:02x}")), 16, indent)
}

/// A seed's 32 bytes as four little-endian words.
pub fn seed(bytes: &[u8; 32]) -> String {
    let w: Vec<u64> = bytes.chunks(8).map(|c| u64::from_le_bytes(c.try_into().unwrap())).collect();
    format!("{{ w0: {}, w1: {}, w2: {}, w3: {} }}", u64(w[0]), u64(w[1]), u64(w[2]), u64(w[3]))
}

pub fn words(seed: &[u64; 4]) -> String {
    format!("{{ w0: {}, w1: {}, w2: {}, w3: {} }}", u64(seed[0]), u64(seed[1]), u64(seed[2]), u64(seed[3]))
}
