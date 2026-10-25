//! RGB565 output helpers shared by LCD back-ends.
//!
//! Small pure functions so the only device-specific code in a firmware
//! display driver is the bus glue (SPI / PIO / DMA). Everything here is
//! host-tested; see the tests at the bottom.

use crate::console::{PICO8_PALETTE, SCREEN_H, SCREEN_SIZE, SCREEN_W};

/// Pack 8-bit RGB into RGB565 (R in bits 15..11, G in 10..5, B in 4..0).
#[inline]
pub const fn rgb888_to_rgb565(r: u8, g: u8, b: u8) -> u16 {
    ((r as u16 >> 3) << 11) | ((g as u16 >> 2) << 5) | (b as u16 >> 3)
}

/// The 16 PICO-8 colors as RGB565, derived from [`PICO8_PALETTE`].
pub const PICO8_RGB565: [u16; 16] = {
    let mut t = [0u16; 16];
    let mut i = 0;
    while i < 16 {
        let c = PICO8_PALETTE[i];
        t[i] = rgb888_to_rgb565(c[0], c[1], c[2]);
        i += 1;
    }
    t
};

/// Nearest-neighbour index table: `lut[i] = i * src_len / N` for `i in 0..N`.
///
/// Used to stretch a 128-pixel axis onto a `N`-pixel panel axis (e.g.
/// 128 -> 240: 112 source pixels appear twice, 16 once). Monotonic, starts at
/// 0 and ends at `src_len - 1`.
pub const fn nearest_lut<const N: usize>(src_len: usize) -> [u8; N] {
    let mut lut = [0u8; N];
    let mut i = 0;
    while i < N {
        lut[i] = (i * src_len / N) as u8;
        i += 1;
    }
    lut
}

/// Resolve the 16 draw indices through `screen_pal` (as the desktop renderer
/// does in `screen_to_rgba_buf`) into big-endian RGB565 byte pairs, ready to
/// stream to a panel that takes the high byte first.
#[inline]
pub fn palette_rgb565_be(screen_pal: &[u8; 16]) -> [[u8; 2]; 16] {
    let mut out = [[0u8; 2]; 16];
    for (i, o) in out.iter_mut().enumerate() {
        let mapped = (screen_pal[i] & 0x0F) as usize;
        *o = PICO8_RGB565[mapped].to_be_bytes();
    }
    out
}

/// Convert one **source row** `sy` of the 128x128 screen into `out`, stretched
/// horizontally through `col_lut` (one destination pixel per LUT entry, two
/// bytes each, big-endian). `out.len()` must be `>= 2 * col_lut.len()`.
#[inline]
pub fn row_to_rgb565_be(
    screen: &[u8; SCREEN_SIZE],
    pal_be: &[[u8; 2]; 16],
    sy: usize,
    col_lut: &[u8],
    out: &mut [u8],
) {
    debug_assert!(sy < SCREEN_H);
    let row = &screen[sy * SCREEN_W..(sy + 1) * SCREEN_W];
    for (dst, &sx) in out.chunks_exact_mut(2).zip(col_lut.iter()) {
        let px = (row[sx as usize] & 0x0F) as usize;
        dst.copy_from_slice(&pal_be[px]);
    }
}

/// Convert one **source column** `sx` of the 128x128 screen into `out`,
/// stretched vertically through `row_lut`. This is the shape a portrait-
/// addressed panel wants when the image is shown rotated (each panel row is
/// one image column).
#[inline]
pub fn column_to_rgb565_be(
    screen: &[u8; SCREEN_SIZE],
    pal_be: &[[u8; 2]; 16],
    sx: usize,
    row_lut: &[u8],
    out: &mut [u8],
) {
    debug_assert!(sx < SCREEN_W);
    for (dst, &sy) in out.chunks_exact_mut(2).zip(row_lut.iter()) {
        let px = (screen[sy as usize * SCREEN_W + sx] & 0x0F) as usize;
        dst.copy_from_slice(&pal_be[px]);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rgb565_packs_red_correctly() {
        // PICO-8 color 8 is (255, 0, 77): R=31<<11, G=0, B=77>>3=9 -> 0xF809.
        assert_eq!(rgb888_to_rgb565(255, 0, 77), 0xF809);
        assert_eq!(PICO8_RGB565[8], 0xF809);
        assert_eq!(PICO8_RGB565[0], 0);
    }

    #[test]
    fn rgb565_white_is_near_max() {
        let (r, g, b) = (
            PICO8_PALETTE[7][0],
            PICO8_PALETTE[7][1],
            PICO8_PALETTE[7][2],
        );
        assert_eq!(PICO8_RGB565[7], rgb888_to_rgb565(r, g, b));
    }

    #[test]
    fn nearest_lut_128_to_240_is_monotonic_and_covers_range() {
        let lut: [u8; 240] = nearest_lut::<240>(128);
        assert_eq!(lut[0], 0);
        assert_eq!(lut[239], 127);
        for w in lut.windows(2) {
            assert!(w[1] == w[0] || w[1] == w[0] + 1, "{:?}", w);
        }
        // Every source pixel is used at least once and at most twice.
        let mut counts = [0u8; 128];
        for &s in &lut {
            counts[s as usize] += 1;
        }
        assert!(counts.iter().all(|&c| c == 1 || c == 2));
        assert_eq!(counts.iter().filter(|&&c| c == 2).count(), 112);
    }

    #[test]
    fn nearest_lut_identity_when_sizes_match() {
        let lut: [u8; 128] = nearest_lut::<128>(128);
        for (i, &v) in lut.iter().enumerate() {
            assert_eq!(v as usize, i);
        }
    }

    #[test]
    fn palette_be_applies_screen_pal_and_byte_order() {
        let mut pal = [0u8; 16];
        for (i, p) in pal.iter_mut().enumerate() {
            *p = i as u8;
        }
        let be = palette_rgb565_be(&pal);
        assert_eq!(be[8], [0xF8, 0x09]);
        // Remap index 1 -> color 8 (as pal(1, 8, 1) would); high bits ignored.
        pal[1] = 0x88;
        let be = palette_rgb565_be(&pal);
        assert_eq!(be[1], [0xF8, 0x09]);
    }

    #[test]
    fn row_and_column_conversion_agree_on_a_diagonal() {
        let mut screen = [0u8; SCREEN_SIZE];
        for i in 0..SCREEN_W {
            screen[i * SCREEN_W + i] = 8; // red diagonal
        }
        let mut pal = [0u8; 16];
        for (i, p) in pal.iter_mut().enumerate() {
            *p = i as u8;
        }
        let be = palette_rgb565_be(&pal);
        let lut: [u8; 128] = nearest_lut::<128>(128);
        let mut row = [0u8; 256];
        let mut col = [0u8; 256];
        row_to_rgb565_be(&screen, &be, 5, &lut, &mut row);
        column_to_rgb565_be(&screen, &be, 5, &lut, &mut col);
        assert_eq!(row, col);
        assert_eq!(&row[10..12], &[0xF8, 0x09]);
        assert_eq!(&row[12..14], &[0, 0]);
    }

    #[test]
    fn row_conversion_stretches_through_lut() {
        let mut screen = [0u8; SCREEN_SIZE];
        screen[127] = 7; // last pixel of row 0 is white
        screen[126] = 8; // the one before it is red
        let pal: [u8; 16] = core::array::from_fn(|i| i as u8);
        let be = palette_rgb565_be(&pal);
        let lut: [u8; 240] = nearest_lut::<240>(128);
        let mut out = [0u8; 480];
        row_to_rgb565_be(&screen, &be, 0, &lut, &mut out);
        let white = PICO8_RGB565[7].to_be_bytes();
        let red = PICO8_RGB565[8].to_be_bytes();
        // lut[239] = 127, lut[237..=238] = 126 (source 126 spans two panel px).
        assert_eq!(&out[478..480], &white);
        assert_eq!(&out[476..478], &red);
        assert_eq!(&out[474..476], &red);
        assert_eq!(&out[472..474], &[0, 0]);
    }
}
