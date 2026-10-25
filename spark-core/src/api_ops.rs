//! Shared, pure PICO-8 API operations used by *both* binding layers
//! (`spark-desktop`'s mlua bindings and `spark-pico`'s C-FFI bindings).
//!
//! Background: each target wires the PICO-8 API to its Lua runtime separately
//! (mlua on desktop, hand-written C FFI on the Pico). The *engine* logic lives
//! in [`crate::console`] / [`crate::audio`] and is already shared, but a few
//! operations have tricky, PICO-8-specific numeric semantics that used to be
//! duplicated inline in both binding layers — and that duplication is exactly
//! what drifted in practice (e.g. `tonum` parsing hex on one target but not the
//! other). Centralizing them here means there is one tested implementation and
//! the two targets cannot diverge.
//!
//! Everything here is `no_std`, pure, and uses the same 16.16 fixed-point bit
//! pattern helpers as the rest of the engine ([`crate::fixed`]).

use crate::fixed::{from_bits, to_bits};
use core::f64::consts::PI;

/// One full turn in radians (PICO-8 measures angles in turns, 0..1).
const TAU: f64 = 2.0 * PI;

// ---------------------------------------------------------------------------
// Bitwise ops — operate on the full 32-bit 16.16 fixed-point pattern.
// ---------------------------------------------------------------------------

/// `band(a, b)` — bitwise AND of the fixed-point bit patterns.
pub fn band(a: f64, b: f64) -> f64 {
    from_bits(to_bits(a) & to_bits(b))
}

/// `bor(a, b)` — bitwise OR.
pub fn bor(a: f64, b: f64) -> f64 {
    from_bits(to_bits(a) | to_bits(b))
}

/// `bxor(a, b)` — bitwise XOR.
pub fn bxor(a: f64, b: f64) -> f64 {
    from_bits(to_bits(a) ^ to_bits(b))
}

/// `bnot(a)` — bitwise NOT.
pub fn bnot(a: f64) -> f64 {
    from_bits(!to_bits(a))
}

/// `shl(a, n)` — logical left shift. PICO-8 (z8lua): n >= 32 yields 0.
pub fn shl(a: f64, n: u32) -> f64 {
    let bits = if n >= 32 { 0 } else { to_bits(a) << n };
    from_bits(bits)
}

/// `shr(a, n)` — arithmetic right shift (sign-preserving). The shift count
/// saturates at 31 so the sign bit fills for n >= 32 (not a modulo-32 wrap).
pub fn shr(a: f64, n: u32) -> f64 {
    let signed = to_bits(a) as i32;
    from_bits((signed >> n.min(31)) as u32)
}

/// `lshr(a, n)` — logical right shift (zero-fill). PICO-8: n >= 32 yields 0.
pub fn lshr(a: f64, n: u32) -> f64 {
    let bits = if n >= 32 { 0 } else { to_bits(a) >> n };
    from_bits(bits)
}

/// `rotl(a, n)` — bitwise rotate left (count taken mod 32).
pub fn rotl(a: f64, n: u32) -> f64 {
    from_bits(to_bits(a).rotate_left(n & 31))
}

/// `rotr(a, n)` — bitwise rotate right (count taken mod 32).
pub fn rotr(a: f64, n: u32) -> f64 {
    from_bits(to_bits(a).rotate_right(n & 31))
}

// ---------------------------------------------------------------------------
// Trig — PICO-8 angles are turns in 0..1, and sin is negated vs std math.
// ---------------------------------------------------------------------------

/// `sin(t)` — PICO-8 sine. `t` is in turns; the result is negated relative to
/// standard math (PICO-8 screen-space convention).
pub fn sin(t: f64) -> f64 {
    -libm::sin(t * TAU)
}

/// `cos(t)` — PICO-8 cosine. `t` is in turns.
pub fn cos(t: f64) -> f64 {
    libm::cos(t * TAU)
}

/// `atan2(dx, dy)` — PICO-8 atan2. Takes (dx, dy) with inverted y (screen
/// coords); returns an angle as a turn in `[0, 1)`.
pub fn atan2(dx: f64, dy: f64) -> f64 {
    let a = libm::atan2(-dy, dx) / TAU;
    ((a % 1.0) + 1.0) % 1.0
}

/// `sgn(x)` — PICO-8 sign. Note `sgn(0) == 1`.
pub fn sgn(x: f64) -> f64 {
    if x < 0.0 {
        -1.0
    } else {
        1.0
    }
}

// ---------------------------------------------------------------------------
// Number parsing — `tonum` accepts hex (0x, with fraction) and binary (0b).
// ---------------------------------------------------------------------------

/// Parse a string into a number the way PICO-8's `tonum` does.
///
/// Supports plain decimals (`"3.5"`, `"-12"`), hexadecimal with an optional
/// fractional part (`"0x6c"` -> 108, `"0x1.8"` -> 1.5), and binary
/// (`"0b1010"` -> 10, with an optional fractional part). Leading/trailing
/// whitespace is ignored. Returns `None` if the whole string isn't a number.
pub fn parse_pico8_number(s: &str) -> Option<f64> {
    let t = s.trim();
    if t.is_empty() {
        return None;
    }

    // Optional leading sign.
    let (neg, body) = match t.strip_prefix('-') {
        Some(rest) => (true, rest),
        None => (false, t.strip_prefix('+').unwrap_or(t)),
    };

    // Determine radix from a 0x / 0b prefix (case-insensitive). The prefix is
    // pure ASCII, so byte-level lowercasing is safe.
    let bytes = body.as_bytes();
    let parsed = if bytes.len() >= 2 && bytes[0] == b'0' && (bytes[1] == b'x' || bytes[1] == b'X') {
        parse_radix_fixed(&body[2..], 16)
    } else if bytes.len() >= 2 && bytes[0] == b'0' && (bytes[1] == b'b' || bytes[1] == b'B') {
        parse_radix_fixed(&body[2..], 2)
    } else {
        body.parse::<f64>().ok()
    }?;

    Some(if neg { -parsed } else { parsed })
}

/// Parse a string with PICO-8 `tonum(s, flags)` semantics (the inverse of
/// `tostr`'s hex flags), matching z8lua `pico8_tonum`:
///   - `flags & 0x1`: read the string as raw hex digits (non-hex chars become
///     `0`), yielding the 32-bit value; if `0x2` is also set, shift right 16.
///   - `flags & 0x2` (without `0x1`): parse as a decimal integer, then `>> 16`.
///   - otherwise: ordinary PICO-8 number parse (handles `0x`/`0b`/decimal).
pub fn parse_pico8_number_with_flags(s: &str, flags: u32) -> Option<f64> {
    if flags & 0x1 != 0 {
        // Replace any non-hex-digit with '0', then read as base-16.
        let cleaned: alloc::string::String = s
            .chars()
            .map(|c| if c.is_ascii_hexdigit() { c } else { '0' })
            .collect();
        let bits = u32::from_str_radix(&cleaned, 16).unwrap_or(0);
        let v = if flags & 0x2 != 0 { bits >> 16 } else { bits };
        Some(v as f64)
    } else if flags & 0x2 != 0 {
        // Decimal integer, then arithmetic shift of the bit pattern.
        let bits = s.trim().parse::<i64>().ok()? as u32;
        Some((bits >> 16) as f64)
    } else {
        parse_pico8_number(s)
    }
}

/// Parse an unsigned `radix`-base number with an optional fractional part
/// after a `.`, e.g. `"6c"` (base 16) or `"1010.1"` (base 2).
fn parse_radix_fixed(s: &str, radix: u32) -> Option<f64> {
    if s.is_empty() {
        return None;
    }
    let (int_part, frac_part) = match s.split_once('.') {
        Some((i, f)) => (i, f),
        None => (s, ""),
    };
    if int_part.is_empty() && frac_part.is_empty() {
        return None;
    }

    let mut value = 0.0f64;
    for ch in int_part.chars() {
        let digit = ch.to_digit(radix)? as f64;
        value = value * radix as f64 + digit;
    }

    let mut scale = 1.0f64;
    for ch in frac_part.chars() {
        let digit = ch.to_digit(radix)? as f64;
        scale /= radix as f64;
        value += digit * scale;
    }

    Some(value)
}

/// Format a number for `tostr(x, flags)`'s hex forms, using the 16.16
/// fixed-point bit pattern (matching PICO-8 / z8lua `pico8_tostr`):
///   flag 0x3 -> "0xWWWWFFFF" (packed 32-bit hex)
///   flag 0x2 -> raw 32-bit integer bit pattern as decimal
///   flag 0x1 -> "0xWWWW.FFFF" (whole.fraction hex)
pub fn format_tostr_hex(x: f64, flags: u32) -> alloc::string::String {
    use alloc::format;
    let b = to_bits(x);
    if flags & 0x3 == 0x3 {
        format!("0x{:04x}{:04x}", (b >> 16) & 0xffff, b & 0xffff)
    } else if flags & 0x2 == 0x2 {
        format!("{b}")
    } else {
        format!("0x{:04x}.{:04x}", (b >> 16) & 0xffff, b & 0xffff)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bitwise_matches_pico8() {
        assert_eq!(band(12.0, 10.0), 8.0);
        assert_eq!(bor(12.0, 10.0), 14.0);
        assert_eq!(bxor(12.0, 10.0), 6.0);
        assert!((bnot(0.0) - (-1.0 / 65536.0)).abs() < 1e-12);
    }

    #[test]
    fn tonum_flags_match_pico8() {
        // No flags: ordinary parse (handles 0x / decimal).
        assert_eq!(parse_pico8_number_with_flags("42", 0), Some(42.0));
        // 0x1: read the string as raw hex digits -> 32-bit value.
        // "ff" -> 0xff = 255.
        assert_eq!(parse_pico8_number_with_flags("ff", 0x1), Some(255.0));
        // Non-hex chars become 0: "1g" -> "10" hex = 16.
        assert_eq!(parse_pico8_number_with_flags("1g", 0x1), Some(16.0));
        // 0x1 | 0x2: hex value shifted right 16. "10000" hex = 0x10000 -> 1.
        assert_eq!(parse_pico8_number_with_flags("10000", 0x3), Some(1.0));
        // 0x2 alone: decimal integer then >>16. 65536 -> 1.
        assert_eq!(parse_pico8_number_with_flags("65536", 0x2), Some(1.0));
    }

    #[test]
    fn shifts_match_pico8() {
        assert_eq!(shl(1.0, 4), 16.0);
        assert_eq!(shr(16.0, 4), 1.0);
        // shl operates on the bit pattern: shl(0.125,3) == 1.
        assert!((shl(0.125, 3) - 1.0).abs() < 1e-12);
        // shl >= 32 yields 0.
        assert_eq!(shl(123.0, 32), 0.0);
        assert_eq!(shl(123.0, 40), 0.0);
        // arithmetic shr preserves sign and saturates the count at 31.
        assert!((shr(-1.0, 16) - (-1.0 / 65536.0)).abs() < 1e-12);
        assert_eq!(shr(-1.0, 40), shr(-1.0, 31));
        // logical lshr zero-fills; >= 32 yields 0.
        assert!((lshr(-1.0, 16) - 0.9999847412109375).abs() < 1e-9);
        assert_eq!(lshr(-1.0, 32), 0.0);
    }

    #[test]
    fn rotates_match_pico8() {
        assert_eq!(rotl(1.0, 1), 2.0);
        assert_eq!(rotl(255.0, 8), -256.0);
        assert!((rotr(2.0, 1) - 1.0).abs() < 1e-12);
        // count is mod 32.
        assert_eq!(rotl(1.0, 33), rotl(1.0, 1));
    }

    #[test]
    fn trig_pico8_conventions() {
        assert!((cos(0.0) - 1.0).abs() < 1e-9);
        assert!((sin(0.0)).abs() < 1e-9);
        // quarter turn: PICO-8 sin is negated.
        assert!((sin(0.25) - (-1.0)).abs() < 1e-9);
        assert!((cos(0.5) - (-1.0)).abs() < 1e-9);
    }

    #[test]
    fn atan2_returns_turns() {
        assert!(atan2(1.0, 0.0).abs() < 1e-9); // +x -> 0
        let a = atan2(-1.0, -1.0);
        assert!((0.0..1.0).contains(&a), "atan2 must be in [0,1), got {a}");
    }

    #[test]
    fn sgn_zero_is_one() {
        assert_eq!(sgn(0.0), 1.0);
        assert_eq!(sgn(-3.0), -1.0);
        assert_eq!(sgn(3.0), 1.0);
    }

    #[test]
    fn tonum_decimal_hex_binary() {
        assert_eq!(parse_pico8_number("42"), Some(42.0));
        assert_eq!(parse_pico8_number("-12"), Some(-12.0));
        assert_eq!(parse_pico8_number("  3.5 "), Some(3.5));
        assert_eq!(parse_pico8_number("0x6c"), Some(108.0));
        assert_eq!(parse_pico8_number("0xFF"), Some(255.0));
        assert_eq!(parse_pico8_number("-0x10"), Some(-16.0));
        assert_eq!(parse_pico8_number("0x1.8"), Some(1.5));
        assert_eq!(parse_pico8_number("0x.8"), Some(0.5));
        assert_eq!(parse_pico8_number("0b1010"), Some(10.0));
        assert_eq!(parse_pico8_number("0b1.1"), Some(1.5));
        assert_eq!(parse_pico8_number(""), None);
        assert_eq!(parse_pico8_number("nope"), None);
        assert_eq!(parse_pico8_number("0xg"), None);
        assert_eq!(parse_pico8_number("0b2"), None);
    }
}
