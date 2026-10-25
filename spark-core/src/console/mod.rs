//! PICO-8 console: screen/sprite/map memory, draw state and the
//! rendering, text, input and memory-mapped I/O implementations.
//!
//! Split by concern; every item keeps its `spark_core::console::*` path:
//! - `draw`   — pixel/shape/sprite/map/tline rendering
//! - `text`   — `print` and the P8SCII decoder
//! - `memory` — `peek`/`poke`/`memcpy`, cartdata, cart ROM snapshot
//! - `input`  — buttons and `btnp` auto-repeat

mod draw;
mod input;
mod memory;
#[cfg(test)]
mod tests;
mod text;

pub const SCREEN_W: usize = 128;
pub const SCREEN_H: usize = 128;
pub const SCREEN_SIZE: usize = SCREEN_W * SCREEN_H;
pub const SHEET_W: usize = 128;
pub const SHEET_H: usize = 128;
pub const SPRITE_SHEET_SIZE: usize = SHEET_W * SHEET_H;
pub const SPRITE_W: usize = 8;
pub const SPRITE_H: usize = 8;
pub const SPRITES_PER_ROW: usize = 16; // 128 / 8
pub const MAP_W: usize = 128;
pub const MAP_H: usize = 64;
pub const MAP_SIZE: usize = MAP_W * MAP_H;

/// The PICO-8 16-color palette as RGBA bytes.
pub const PICO8_PALETTE: [[u8; 4]; 16] = [
    [0, 0, 0, 255],       // 0 black
    [29, 43, 83, 255],    // 1 dark_blue
    [126, 37, 83, 255],   // 2 dark_purple
    [0, 135, 81, 255],    // 3 dark_green
    [171, 82, 54, 255],   // 4 brown
    [95, 87, 79, 255],    // 5 dark_grey
    [194, 195, 199, 255], // 6 light_grey
    [255, 241, 232, 255], // 7 white
    [255, 0, 77, 255],    // 8 red
    [255, 163, 0, 255],   // 9 orange
    [255, 236, 39, 255],  // 10 yellow
    [0, 228, 54, 255],    // 11 green
    [41, 173, 255, 255],  // 12 blue
    [131, 118, 156, 255], // 13 indigo/lavender
    [255, 119, 168, 255], // 14 pink
    [255, 204, 170, 255], // 15 peach
];

/// Size of the general-purpose / user data memory region (0x4300-0x5DFF).
/// The 256 bytes above it (0x5E00-0x5EFF) are the persistent cartdata slots,
/// which live in `persistent_data` rather than here.
pub const USER_DATA_SIZE: usize = 0x5E00 - 0x4300;

// PICO-8 memory map region boundaries (see the `Memory-mapped I/O` section).
pub const MEM_SPRITES_END: u16 = 0x2000;
pub const MEM_SHARED_START: u16 = 0x1000;
pub const MEM_MAP_START: u16 = 0x2000;
pub const MEM_FLAGS_START: u16 = 0x3000;
pub const MEM_USER_START: u16 = 0x4300;
pub const MEM_CARTDATA_START: u16 = 0x5E00;
pub const MEM_DRAW_STATE_START: u16 = 0x5F00;
pub const MEM_HW_STATE_START: u16 = 0x5F40;
pub const MEM_SCREEN_START: u16 = 0x6000;
pub const MEM_SCREEN_END: u16 = 0x8000;

pub struct Console {
    /// Screen buffer: 128*128 bytes, each byte is a color index 0-15.
    pub screen: [u8; SCREEN_SIZE],
    /// Sprite sheet: 128*128 bytes (pixel color indices).
    ///
    /// Sheet rows 64-127 (memory 0x1000-0x1FFF) are the same bytes as map
    /// rows 32-63 in PICO-8. Spark stores both arrays unpacked, so every
    /// mutation path through `Console` (`poke`, `sset`, `mset`, `memcpy`,
    /// `memset`, `reload`) writes both copies; reads use the natural array.
    pub sprites: [u8; SPRITE_SHEET_SIZE],
    /// Tile map: 128*64 bytes (sprite indices 0-255). Rows 32-63 alias the
    /// lower sprite sheet (see `sprites`).
    pub map: [u8; MAP_SIZE],
    /// Sprite flags: 256 bytes (one per sprite, 8 flag bits each).
    pub flags: [u8; 256],

    // -- drawing state --
    pub draw_color: u8,
    pub camera_x: i32,
    pub camera_y: i32,
    /// Clip rectangle, stored *pre-clamped* to the screen: the invariant
    /// `0 <= clip_x <= clip_x + clip_w <= SCREEN_W` (and likewise for y) is
    /// maintained by every setter (`clip_rect`, `poke`, `cls`, reset), so the
    /// per-pixel `in_clip` test is four plain compares.
    pub clip_x: i32,
    pub clip_y: i32,
    pub clip_w: i32,
    pub clip_h: i32,
    pub cursor_x: i32,
    pub cursor_y: i32,

    /// `tline` map wrap registers (`0x5F38`-`0x5F3B`): width/height mask (0 =
    /// whole 256-tile range) and x/y tile offset.
    pub tline_map_w: u8,
    pub tline_map_h: u8,
    pub tline_map_x: u8,
    pub tline_map_y: u8,

    /// Last `line()` endpoint, for the stateful polyline form `line(x,y)` which
    /// continues from the previous line's end. `None` until a full line is
    /// drawn or after a `line()` reset.
    pub line_end: Option<(i32, i32)>,

    /// Draw palette: when code requests color `c`, the pixel stored is `draw_pal[c]`.
    pub draw_pal: [u8; 16],
    /// Transparency flags (used by sprite / map drawing only).
    pub transparent: [bool; 16],
    /// Screen (display) palette: when displaying color `c`, show `screen_pal[c]`.
    pub screen_pal: [u8; 16],

    /// Fill pattern: 4x4 bit pattern (16 bits). Each bit corresponds to a pixel
    /// in the 4x4 grid. If a bit is set, the pixel uses the alternate behavior
    /// (transparent or color 0) during fill operations.
    pub fill_pattern: u16,
    /// When true, pixels matched by the fill pattern are skipped (transparent).
    /// When false, pixels matched by the fill pattern are drawn with color 0.
    pub fill_pattern_transparency: bool,

    /// Persistent data: 64 number slots (indices 0-63) that persist during
    /// the session. Matches PICO-8 `dget`/`dset` behavior before `cartdata()`.
    pub persistent_data: [f64; 64],

    // -- input --
    /// Current-frame button state (indices 0-11).
    /// 0-5: player 1 (left, right, up, down, O, X)
    /// 6-11: player 2 (left, right, up, down, O, X)
    pub btn_state: [bool; 12],
    /// Previous-frame button state.
    pub btn_prev: [bool; 12],
    /// Consecutive frames each button has been held (0 when released). Drives
    /// PICO-8's `btnp` keyboard-style auto-repeat (re-fire at 15 frames, then
    /// every 4 frames).
    pub btn_frames_held: [u16; 12],
    /// `btnp` repeat delay register (`0x5F5C`): frames before the first repeat.
    /// 0 means the default (15); 255 disables auto-repeat entirely.
    pub btnp_repeat_delay: u8,
    /// `btnp` repeat interval register (`0x5F5D`): frames between repeats after
    /// the delay. 0 means the default (4).
    pub btnp_repeat_interval: u8,

    // -- memory-mapped user data --
    /// General-purpose / user data region (0x4300-0x5DFF in the PICO-8 memory map).
    pub user_data: [u8; USER_DATA_SIZE],

    // -- cart ROM snapshot --
    /// Snapshot of original cartridge data (sprites + flags + map).
    /// Used by `cstore` (RAM -> ROM) and `reload` (ROM -> RAM).
    /// Layout mirrors the PICO-8 memory map: 0x0000-0x42FF (0x4300 bytes).
    pub cart_rom: [u8; 0x4300],

    // -- mouse state (set by platform layer, read by stat()) --
    pub mouse_x: i32,
    pub mouse_y: i32,
    pub mouse_btn: u8,

    // -- keyboard state (set by platform layer, read by stat()) --
    pub last_key_char: Option<char>,
    pub key_pressed: bool,
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

impl Console {
    /// Resolve an optional color argument through the draw palette to a final
    /// low-nibble screen color. `None` uses the current `draw_color`. This is
    /// the single-color path used by sprite/line plotting.
    fn resolve_color(&mut self, col: Option<u8>) -> u8 {
        // PICO-8: passing a color to any drawing primitive persistently sets the
        // pen color (reference `color(col)` stores the full byte). A later draw
        // with no color arg then reuses it.
        if let Some(byte) = col {
            self.draw_color = byte;
        }
        let c = self.draw_color & 0x0F;
        self.draw_pal[c as usize] & 0x0F
    }

    /// Resolve the full two-color `fillp` pair for a shape fill.
    ///
    /// Returns `(primary, secondary)` palette-mapped colors. PICO-8's draw
    /// color byte carries the primary in the low nibble and the secondary
    /// (drawn where a `fillp` bit is set) in the high nibble; `color(0x21)`
    /// fills with color 1 and patterns with color 2. `None` uses `draw_color`.
    fn resolve_fill_colors(&mut self, col: Option<u8>) -> (u8, u8) {
        // Like `resolve_color`, a color arg sets the persistent pen byte (both
        // nibbles: primary + secondary), so subsequent draws reuse it.
        if let Some(byte) = col {
            self.draw_color = byte;
        }
        let byte = self.draw_color;
        let c0 = self.draw_pal[(byte & 0x0F) as usize] & 0x0F;
        let c1 = self.draw_pal[((byte >> 4) & 0x0F) as usize] & 0x0F;
        (c0, c1)
    }
}

// ---------------------------------------------------------------------------
// Core
// ---------------------------------------------------------------------------

impl Console {
    /// Identity palette: `[0, 1, 2, ..., 15]`.
    const IDENTITY_PAL: [u8; 16] = {
        let mut pal = [0u8; 16];
        let mut i = 0u8;
        while i < 16 {
            pal[i as usize] = i;
            i += 1;
        }
        pal
    };

    /// Create a new Console with all buffers zeroed and defaults applied.
    ///
    /// `const` so the firmware can place the (large) console in static
    /// storage instead of building it on the stack at boot.
    pub const fn new() -> Self {
        Console {
            screen: [0u8; SCREEN_SIZE],
            sprites: [0u8; SPRITE_SHEET_SIZE],
            map: [0u8; MAP_SIZE],
            flags: [0u8; 256],

            draw_color: 6, // light grey as default draw color
            camera_x: 0,
            camera_y: 0,
            clip_x: 0,
            clip_y: 0,
            clip_w: SCREEN_W as i32,
            clip_h: SCREEN_H as i32,
            cursor_x: 0,
            cursor_y: 0,

            tline_map_w: 0,
            tline_map_h: 0,
            tline_map_x: 0,
            tline_map_y: 0,

            line_end: None,

            draw_pal: Self::IDENTITY_PAL,
            transparent: Self::DEFAULT_TRANSPARENT,
            screen_pal: Self::IDENTITY_PAL,

            fill_pattern: 0,
            fill_pattern_transparency: false,

            persistent_data: [0.0f64; 64],

            btn_state: [false; 12],
            btn_prev: [false; 12],
            btn_frames_held: [0; 12],
            btnp_repeat_delay: 0,
            btnp_repeat_interval: 0,

            user_data: [0u8; USER_DATA_SIZE],

            cart_rom: [0u8; 0x4300],

            mouse_x: 0,
            mouse_y: 0,
            mouse_btn: 0,

            last_key_char: None,
            key_pressed: false,
        }
    }
}

impl Default for Console {
    fn default() -> Self {
        Self::new()
    }
}

impl Console {
    /// Reset all transient drawing state to defaults.
    pub fn reset_draw_state(&mut self) {
        self.draw_color = 6;
        self.camera_x = 0;
        self.camera_y = 0;
        self.set_clip_edges(0, 0, SCREEN_W as i32, SCREEN_H as i32);
        self.cursor_x = 0;
        self.cursor_y = 0;
        self.fill_pattern = 0;
        self.fill_pattern_transparency = false;
        self.line_end = None;
        self.pal_reset();
        self.palt_reset();
    }

    /// Convert the 128x128 screen buffer to RGBA into a caller-provided buffer.
    /// `out` must be at least `SCREEN_SIZE * 4` bytes long.
    pub fn screen_to_rgba_buf(&self, out: &mut [u8]) {
        for (&idx, rgba) in self.screen.iter().zip(out.chunks_exact_mut(4)) {
            let mapped = self.screen_pal[(idx & 0x0F) as usize] & 0x0F;
            let c = PICO8_PALETTE[mapped as usize];
            rgba.copy_from_slice(&c);
        }
    }
}

// ---------------------------------------------------------------------------
// Vec-returning methods (require alloc, gated behind `std` feature)
// ---------------------------------------------------------------------------

#[cfg(feature = "std")]
impl Console {
    /// Convert the 128x128 screen buffer to RGBA (applying the screen palette).
    pub fn screen_to_rgba(&self) -> alloc::vec::Vec<u8> {
        let mut rgba = alloc::vec![0u8; SCREEN_SIZE * 4];
        self.screen_to_rgba_buf(&mut rgba);
        rgba
    }

    /// Convert the 128x128 sprite sheet to RGBA (no palette remapping).
    pub fn sprite_sheet_to_rgba(&self) -> alloc::vec::Vec<u8> {
        let mut rgba = alloc::vec![0u8; SPRITE_SHEET_SIZE * 4];
        for (&idx, chunk) in self.sprites.iter().zip(rgba.chunks_exact_mut(4)) {
            chunk.copy_from_slice(&PICO8_PALETTE[(idx & 0x0F) as usize]);
        }
        rgba
    }
}

/// Clamp a coordinate to PICO-8's representable 16.16 integer range. Real cart
/// coordinates never exceed this; clamping bounds drawing loops and arithmetic.
#[inline]
fn clamp_coord(v: i32) -> i32 {
    v.clamp(-32768, 32767)
}

// ---------------------------------------------------------------------------
// State management
// ---------------------------------------------------------------------------

impl Console {
    /// Set the camera offset.  Drawing subtracts these values from
    /// world coordinates to get screen coordinates. Returns the *previous*
    /// camera offset `(x, y)` (PICO-8 `camera()` returns the prior value).
    pub fn camera(&mut self, x: i32, y: i32) -> (i32, i32) {
        let prev = (self.camera_x, self.camera_y);
        // Clamp to the representable coordinate range. A real cart's camera is
        // a 16.16 value within +/-32768; clamping keeps `coord - camera`
        // subtractions across the drawing primitives from overflowing i32.
        self.camera_x = clamp_coord(x);
        self.camera_y = clamp_coord(y);
        prev
    }

    /// Set the clipping rectangle (screen coordinates).
    /// Set the clip rectangle, returning the *previous* one as `(x, y, w, h)`
    /// (PICO-8 `clip()` returns the prior rect). If `clip_previous` is true, the
    /// new rectangle is intersected with the current one (PICO-8 0.2.x 5th arg).
    pub fn clip_rect(
        &mut self,
        x: i32,
        y: i32,
        w: i32,
        h: i32,
        clip_previous: bool,
    ) -> (i32, i32, i32, i32) {
        let prev = (self.clip_x, self.clip_y, self.clip_w, self.clip_h);
        // PICO-8/fake-08 clamp the clip *edges* (x and x+w) to the screen, then
        // store begin/width — NOT the origin and width independently. A negative
        // origin or an oversized width therefore shrinks the visible region
        // instead of shifting the far edge past the screen.
        let mut xb = x.clamp(0, SCREEN_W as i32);
        let mut yb = y.clamp(0, SCREEN_H as i32);
        let mut xe = x.saturating_add(w).clamp(0, SCREEN_W as i32);
        let mut ye = y.saturating_add(h).clamp(0, SCREEN_H as i32);
        if clip_previous {
            // Intersect with the existing clip rectangle.
            xb = xb.max(prev.0);
            yb = yb.max(prev.1);
            xe = xe.min(prev.0 + prev.2);
            ye = ye.min(prev.1 + prev.3);
        }
        self.set_clip_edges(xb, yb, xe, ye);
        prev
    }

    /// Map colour `c0` to `c1` in the given palette, returning the *previous*
    /// mapping for `c0` (PICO-8 `pal(c0,c1,p)` returns the prior value).
    /// `p == 0` -> draw palette, `p == 1` -> screen (display) palette.
    pub fn pal(&mut self, c0: u8, c1: u8, p: u8) -> u8 {
        let i = (c0 & 0x0F) as usize;
        let v = c1 & 0x0F;
        if p == 1 {
            let prev = self.screen_pal[i];
            self.screen_pal[i] = v;
            prev
        } else {
            let prev = self.draw_pal[i];
            self.draw_pal[i] = v;
            prev
        }
    }

    /// Reset a single palette to identity (PICO-8 `pal(p)` single-number form).
    /// `p == 0` -> draw palette (and transparency), `p == 1` -> screen palette.
    pub fn pal_reset_one(&mut self, p: u8) {
        if p == 1 {
            self.screen_pal = Self::IDENTITY_PAL;
        } else {
            self.draw_pal = Self::IDENTITY_PAL;
            self.palt_reset();
        }
    }

    /// Reset both palettes to identity. PICO-8's `pal()` (no args) also resets
    /// transparency to the default (only color 0 transparent) — the reference
    /// `pal()` calls `palt()` — so a cart that did `palt(c,true)` then `pal()`
    /// gets transparency cleared too.
    pub fn pal_reset(&mut self) {
        self.draw_pal = Self::IDENTITY_PAL;
        self.screen_pal = Self::IDENTITY_PAL;
        self.palt_reset();
    }

    /// Set whether colour `c` is transparent (for sprite drawing).
    pub fn palt(&mut self, c: u8, t: bool) {
        let i = (c & 0x0F) as usize;
        self.transparent[i] = t;
    }

    /// Default transparency flags: only color 0 is transparent.
    const DEFAULT_TRANSPARENT: [bool; 16] = [
        true, false, false, false, false, false, false, false, false, false, false, false, false,
        false, false, false,
    ];

    /// Reset transparency so that only colour 0 is transparent.
    pub fn palt_reset(&mut self) {
        self.transparent = Self::DEFAULT_TRANSPARENT;
    }

    /// Set the default draw colour, returning the *previous* draw color byte
    /// (PICO-8 `color()` returns the prior pen color).
    pub fn color(&mut self, c: u8) -> u8 {
        // Keep the full byte: the high nibble is the secondary (`fillp`) color.
        let prev = self.draw_color;
        self.draw_color = c;
        prev
    }

    /// Set the text cursor position, returning the *previous* position `(x, y)`
    /// (PICO-8 `cursor()` returns the prior cursor).
    pub fn cursor(&mut self, x: i32, y: i32) -> (i32, i32) {
        let prev = (self.cursor_x, self.cursor_y);
        self.cursor_x = x;
        self.cursor_y = y;
        prev
    }
}

// ---------------------------------------------------------------------------
// Fill pattern
// ---------------------------------------------------------------------------

impl Console {
    /// Set the 4x4 fill pattern used by subsequent drawing operations.
    ///
    /// `p` is interpreted as a 16-bit pattern.  The lower 16 bits form the
    /// 4x4 grid.  If bit 0x1.0000 (the fractional part in PICO-8's fixed
    /// point representation) is set, transparency mode is enabled.
    ///
    /// Calling `fillp(0)` or `fillp()` with no args clears the pattern.
    ///
    /// Returns the *previous* fill pattern as a PICO-8 number (the 16-bit
    /// pattern in the integer part, transparency encoded in the 0x0.8 bit),
    /// matching PICO-8 `fillp()` which returns the prior pattern.
    pub fn fillp(&mut self, p: f64) -> f64 {
        let prev = self.fill_pattern as f64
            + if self.fill_pattern_transparency {
                0.5
            } else {
                0.0
            };
        // PICO-8 encodes the transparency flag in the 0x0.8 bit of the
        // fixed-point number, i.e. when the value has a fractional part >= 0.5.
        // The integer part is the 16-bit pattern.
        let int_part = p as i32;
        self.fill_pattern = (int_part & 0xFFFF) as u16;
        // Transparency if fractional part >= 0.5
        let frac = (p - (int_part as f64)).abs();
        self.fill_pattern_transparency = frac >= 0.5;
        prev
    }
}
