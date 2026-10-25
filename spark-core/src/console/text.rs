//! `print` and the P8SCII control-code decoder.

use super::*;
use crate::font;

// ---------------------------------------------------------------------------
// Text / print
// ---------------------------------------------------------------------------

impl Console {
    /// Print a string using the built-in pixel font.
    ///
    /// If `x` / `y` are `None` the current cursor position is used.
    /// After printing the cursor advances to the start of the *next* line.
    /// Returns the x-pixel position directly after the last character drawn.
    pub fn print(&mut self, text: &str, x: Option<i32>, y: Option<i32>, col: Option<u8>) -> i32 {
        let mut c = self.resolve_color(col);

        // Clamp the starting position to the representable coordinate range so
        // cursor advances below can't overflow i32 on extreme `print` inputs.
        let start_x = clamp_coord(x.unwrap_or(self.cursor_x));
        let start_y = clamp_coord(y.unwrap_or(self.cursor_y));

        let mut cur_x = start_x;
        let mut cur_y = start_y;
        // "home" position for `\^g` (reset) / `\^h` (set home); starts at origin.
        let mut home_x = start_x;
        let mut home_y = start_y;
        // Background color set by `\#{c}` (P8SCII). `None` = transparent.
        let mut bg: Option<u8> = None;

        // Iterate over bytes so P8SCII control codes (1-6) and their parameter
        // bytes are handled exactly. `chars` over a `&str` would also work for
        // ASCII, but P8SCII carts pass raw bytes; `bytes()` keeps positions
        // aligned with the control-code parameter reads.
        let bytes = text.as_bytes();
        let mut i = 0;
        while i < bytes.len() {
            let b = bytes[i];
            match b {
                b'\n' => {
                    cur_x = start_x;
                    cur_y = cur_y.saturating_add(font::CHAR_H);
                    i += 1;
                }
                // \*{n}: repeat the next character n times.
                1 => {
                    let n = bytes.get(i + 1).map(|&p| p8scii_param(p)).unwrap_or(0);
                    let ch = bytes.get(i + 2).copied().unwrap_or(0);
                    for _ in 0..n {
                        self.draw_glyph(ch, cur_x, cur_y, c, bg);
                        cur_x = cur_x.saturating_add(font::CHAR_W);
                    }
                    i += 3;
                }
                // \#{c}: set a solid background color for subsequent glyphs.
                2 => {
                    bg = bytes.get(i + 1).map(|&p| p8scii_param(p) & 0x0F);
                    i += 2;
                }
                // \-{n}: move cursor horizontally by (n-16) pixels.
                3 => {
                    let n = bytes.get(i + 1).map(|&p| p8scii_param(p)).unwrap_or(0);
                    cur_x = cur_x.saturating_add(n as i32 - 16);
                    i += 2;
                }
                // \|{n}: move cursor vertically by (n-16) pixels.
                4 => {
                    let n = bytes.get(i + 1).map(|&p| p8scii_param(p)).unwrap_or(0);
                    cur_y = cur_y.saturating_add(n as i32 - 16);
                    i += 2;
                }
                // \+{x}{y}: move cursor by (x-16, y-16) pixels.
                5 => {
                    let nx = bytes.get(i + 1).map(|&p| p8scii_param(p)).unwrap_or(0);
                    let ny = bytes.get(i + 2).map(|&p| p8scii_param(p)).unwrap_or(0);
                    cur_x = cur_x.saturating_add(nx as i32 - 16);
                    cur_y = cur_y.saturating_add(ny as i32 - 16);
                    i += 3;
                }
                // \^{cmd}{params}: P8SCII special-command family. Each command
                // consumes a known number of parameter bytes (z8lua/fake-08
                // printHelper.cpp); we implement the common cursor/color ones
                // and consume the rest with the correct byte count so they
                // never leak as stray glyphs.
                6 => {
                    let cmd = bytes.get(i + 1).copied().unwrap_or(0);
                    // Bytes consumed AFTER the command char.
                    let params = p8scii_command_param_count(cmd);
                    match cmd {
                        // \^c{col}: set the foreground color.
                        b'c' => {
                            let col = bytes.get(i + 2).map(|&p| p8scii_param(p)).unwrap_or(0);
                            c = self.draw_pal[(col & 0x0F) as usize] & 0x0F;
                        }
                        // \^g: reset cursor to home.
                        b'g' => {
                            cur_x = home_x;
                            cur_y = home_y;
                        }
                        // \^h: set home to the current cursor.
                        b'h' => {
                            home_x = cur_x;
                            home_y = cur_y;
                        }
                        // \^j{x}{y}: absolute cursor position (param * 4 px).
                        b'j' => {
                            let jx = bytes.get(i + 2).map(|&p| p8scii_param(p)).unwrap_or(0);
                            let jy = bytes.get(i + 3).map(|&p| p8scii_param(p)).unwrap_or(0);
                            cur_x = jx as i32 * 4;
                            cur_y = jy as i32 * 4;
                        }
                        // \^:{16 hex} / \^;{16 hex}: define+draw a custom 8x8
                        // glyph from 8 bytes (16 hex digits, 1 byte per row).
                        b':' | b';' => {
                            let mut rows = [0u8; 8];
                            for (r, row) in rows.iter_mut().enumerate() {
                                let hi = bytes.get(i + 2 + r * 2).copied().unwrap_or(b'0');
                                let lo = bytes.get(i + 3 + r * 2).copied().unwrap_or(b'0');
                                *row = (hex_digit(hi) << 4) | hex_digit(lo);
                            }
                            self.draw_custom_glyph(&rows, cur_x, cur_y, c, bg);
                            cur_x = cur_x.saturating_add(8);
                        }
                        // \^.{8 bytes} / \^,{8 bytes}: define+draw a custom 8x8
                        // glyph from 8 raw bytes (1 byte per row).
                        b'.' | b',' => {
                            let mut rows = [0u8; 8];
                            for (r, row) in rows.iter_mut().enumerate() {
                                *row = bytes.get(i + 2 + r).copied().unwrap_or(0);
                            }
                            self.draw_custom_glyph(&rows, cur_x, cur_y, c, bg);
                            cur_x = cur_x.saturating_add(8);
                        }
                        // Other commands (style toggles, delays): consumed via
                        // `params`, no rendering effect yet.
                        _ => {}
                    }
                    i += 2 + params;
                }
                // Printable ASCII glyph.
                32..=127 => {
                    self.draw_glyph(b, cur_x, cur_y, c, bg);
                    cur_x = cur_x.saturating_add(font::CHAR_W);
                    i += 1;
                }
                // Other bytes (incl. P8SCII glyphs >=128) advance like a space
                // for now; the built-in font only covers 32-127.
                _ => {
                    cur_x = cur_x.saturating_add(font::CHAR_W);
                    i += 1;
                }
            }
        }

        // Advance cursor to the next line after the printed text.
        self.cursor_x = start_x;
        self.cursor_y = cur_y + font::CHAR_H;

        cur_x
    }

    /// Draw a single font glyph (byte 32-127) at `(gx, gy)` in color `c`,
    /// applying the camera offset. Bytes outside the range draw nothing
    /// (but a `bg` cell is still filled, matching PICO-8).
    ///
    /// When `bg` is `Some(color)` the whole glyph cell (CHAR_W x CHAR_H) is
    /// filled with that color first, so foreground pixels sit on a solid
    /// background (PICO-8 `\#` mode).
    fn draw_glyph(&mut self, byte: u8, gx: i32, gy: i32, c: u8, bg: Option<u8>) {
        // Fill the cell background first if requested.
        if let Some(bgc) = bg {
            for row in 0..font::CHAR_H {
                for col in 0..font::CHAR_W {
                    self.raw_pset(
                        px_at(gx, col, self.camera_x),
                        px_at(gy, row, self.camera_y),
                        bgc,
                    );
                }
            }
        }

        let code = byte as u32;
        if !(32..=127).contains(&code) {
            return;
        }
        let glyph = &font::FONT[(code - 32) as usize];
        for row in 0..5i32 {
            let bits = glyph[row as usize];
            for col_bit in 0..4i32 {
                if bits & (0x08 >> col_bit) != 0 {
                    let px = px_at(gx, col_bit, self.camera_x);
                    let py = px_at(gy, row, self.camera_y);
                    self.raw_pset(px, py, c);
                }
            }
        }
    }

    /// Draw a custom 8x8 glyph (PICO-8 `\^:` / `\^.`) from 8 row bytes. Each
    /// byte is one row; bit `n` (LSB first) is the pixel at column `n`, matching
    /// z8lua's `BITMASK(col) & rowByte`. Honors `bg` like [`Self::draw_glyph`].
    fn draw_custom_glyph(&mut self, rows: &[u8; 8], gx: i32, gy: i32, c: u8, bg: Option<u8>) {
        for row in 0..8i32 {
            let bits = rows[row as usize];
            for col in 0..8i32 {
                let on = bits & (1 << col) != 0;
                let px = px_at(gx, col, self.camera_x);
                let py = px_at(gy, row, self.camera_y);
                if on {
                    self.raw_pset(px, py, c);
                } else if let Some(bgc) = bg {
                    self.raw_pset(px, py, bgc);
                }
            }
        }
    }
}

/// Compute a glyph pixel coordinate `base + offset - camera` with saturating
/// arithmetic, so extreme `print` positions can't overflow `i32`. `raw_pset`
/// then bounds-checks against the screen, so a saturated value just clips.
fn px_at(base: i32, offset: i32, camera: i32) -> i32 {
    base.saturating_add(offset).saturating_sub(camera)
}

/// Decode a P8SCII parameter byte the way PICO-8 does (`p0CharToNum`):
/// `'0'..'9'` -> 0..9, `'a'..'z'` -> 10..35, anything else -> 0.
fn p8scii_param(b: u8) -> u8 {
    match b {
        b'0'..=b'9' => b - b'0',
        b'a'..=b'z' => b - b'a' + 10,
        _ => 0,
    }
}

/// Number of parameter bytes a `\^{cmd}` P8SCII command consumes after the
/// command byte itself (z8lua/fake-08 printHelper.cpp). Anything not listed
/// here takes no parameter (style toggles, home/reset, digit delays).
/// Decode a single ASCII hex digit (`0-9a-fA-F`) to its 0-15 value; other
/// bytes decode to 0. Used for `\^:` custom-glyph hex rows.
fn hex_digit(b: u8) -> u8 {
    match b {
        b'0'..=b'9' => b - b'0',
        b'a'..=b'f' => b - b'a' + 10,
        b'A'..=b'F' => b - b'A' + 10,
        _ => 0,
    }
}

fn p8scii_command_param_count(cmd: u8) -> usize {
    match cmd {
        // one parameter byte (incl. `\^-{mode}` turn-off-mode)
        b'd' | b'c' | b'r' | b's' | b'x' | b'y' | b'-' => 1,
        // two parameter bytes (absolute jump)
        b'j' => 2,
        // custom 8x8 font glyph: hex `:`/`;` take 16, binary `.`/`,` take 8
        b':' | b';' => 16,
        b'.' | b',' => 8,
        // everything else (g h w t = p i b # + * / and digits) takes none
        _ => 0,
    }
}
