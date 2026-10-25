//! PICO-8 16.16 signed fixed-point helpers.
//!
//! PICO-8 stores every number as a signed 32-bit integer scaled by 65536
//! (16 integer bits, 16 fractional bits). Spark runs on IEEE floats, but a
//! few APIs are only correct when they operate on PICO-8's exact bit pattern:
//! the bitwise ops act on all 32 bits (including the fractional 16), and
//! `peek4`/`poke4` store that raw bit pattern little-endian.
//!
//! These helpers convert between a float and the 16.16 bit pattern using the
//! same truncating cast chain as z8lua's `fix32` (`int32_t(int64_t(d*65536))`),
//! so results are bit-identical to real PICO-8.

/// Convert a number to its PICO-8 16.16 fixed-point bit pattern.
#[inline]
pub fn to_bits(x: f64) -> u32 {
    ((x * 65536.0) as i64 as i32) as u32
}

/// Convert a 16.16 fixed-point bit pattern back to a float.
#[inline]
pub fn from_bits(bits: u32) -> f64 {
    (bits as i32) as f64 / 65536.0
}
