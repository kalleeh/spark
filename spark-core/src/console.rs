use crate::font;

pub const SCREEN_W: usize = 128;
pub const SCREEN_H: usize = 128;
pub const SCREEN_SIZE: usize = SCREEN_W * SCREEN_H;
pub const SPRITE_SHEET_SIZE: usize = 128 * 128;
pub const SPRITE_W: usize = 8;
pub const SPRITE_H: usize = 8;
pub const SPRITES_PER_ROW: usize = 16; // 128 / 8
pub const MAP_W: usize = 128;
pub const MAP_H: usize = 64;
pub const MAP_SIZE: usize = MAP_W * MAP_H;

/// The PICO-8 16-color palette as RGBA bytes.
pub const PICO8_PALETTE: [[u8; 4]; 16] = [
    [0, 0, 0, 255],       // 0 black
    [29, 43, 83, 255],     // 1 dark_blue
    [126, 37, 83, 255],    // 2 dark_purple
    [0, 135, 81, 255],     // 3 dark_green
    [171, 82, 54, 255],    // 4 brown
    [95, 87, 79, 255],     // 5 dark_grey
    [194, 195, 199, 255],  // 6 light_grey
    [255, 241, 232, 255],  // 7 white
    [255, 0, 77, 255],     // 8 red
    [255, 163, 0, 255],    // 9 orange
    [255, 236, 39, 255],   // 10 yellow
    [0, 228, 54, 255],     // 11 green
    [41, 173, 255, 255],   // 12 blue
    [131, 118, 156, 255],  // 13 indigo/lavender
    [255, 119, 168, 255],  // 14 pink
    [255, 204, 170, 255],  // 15 peach
];

/// Size of the general-purpose / user data memory region (0x4300-0x5FFF).
pub const USER_DATA_SIZE: usize = 7424;

pub struct Console {
    /// Screen buffer: 128*128 bytes, each byte is a color index 0-15.
    pub screen: [u8; SCREEN_SIZE],
    /// Sprite sheet: 128*128 bytes (pixel color indices).
    pub sprites: [u8; SPRITE_SHEET_SIZE],
    /// Tile map: 128*64 bytes (sprite indices 0-255).
    pub map: [u8; MAP_SIZE],
    /// Sprite flags: 256 bytes (one per sprite, 8 flag bits each).
    pub flags: [u8; 256],

    // -- drawing state --
    pub draw_color: u8,
    pub camera_x: i32,
    pub camera_y: i32,
    pub clip_x: i32,
    pub clip_y: i32,
    pub clip_w: i32,
    pub clip_h: i32,
    pub cursor_x: i32,
    pub cursor_y: i32,

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

    // -- memory-mapped user data --
    /// General-purpose / user data region (0x4300-0x5FFF in the PICO-8 memory map).
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
    /// Resolve an optional color argument through the draw palette.
    /// If `col` is `None` the current `draw_color` is used instead.
    fn resolve_color(&self, col: Option<u8>) -> u8 {
        let c = col.unwrap_or(self.draw_color) & 0x0F;
        self.draw_pal[c as usize] & 0x0F
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
    pub fn new() -> Self {
        let mut transparent = [false; 16];
        transparent[0] = true; // color 0 is transparent by default

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

            draw_pal: Self::IDENTITY_PAL,
            transparent,
            screen_pal: Self::IDENTITY_PAL,

            fill_pattern: 0,
            fill_pattern_transparency: false,

            persistent_data: [0.0f64; 64],

            btn_state: [false; 12],
            btn_prev: [false; 12],

            user_data: [0u8; USER_DATA_SIZE],

            cart_rom: [0u8; 0x4300],

            mouse_x: 0,
            mouse_y: 0,
            mouse_btn: 0,

            last_key_char: None,
            key_pressed: false,
        }
    }

    /// Reset all transient drawing state to defaults.
    pub fn reset_draw_state(&mut self) {
        self.draw_color = 6;
        self.camera_x = 0;
        self.camera_y = 0;
        self.clip_x = 0;
        self.clip_y = 0;
        self.clip_w = SCREEN_W as i32;
        self.clip_h = SCREEN_H as i32;
        self.cursor_x = 0;
        self.cursor_y = 0;
        self.fill_pattern = 0;
        self.fill_pattern_transparency = false;
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

// ---------------------------------------------------------------------------
// Drawing primitives
// ---------------------------------------------------------------------------

impl Console {
    /// Set a pixel directly at screen coordinates (no camera offset) but
    /// *with* clipping.  `col` should already be palette-mapped.
    ///
    /// Applies the fill pattern if one is set: each bit of the 4x4 pattern
    /// determines whether a pixel is drawn normally (bit=0) or handled by the
    /// pattern (bit=1).  When the bit is 1, the pixel is either skipped
    /// (if `fill_pattern_transparency` is true) or drawn with color 0.
    pub fn raw_pset(&mut self, x: i32, y: i32, col: u8) {
        // Combined bounds check: clip region intersected with screen bounds.
        // The effective visible region is [max(clip_x, 0), min(clip_x+clip_w, SCREEN_W))
        // and likewise for y. Using u32 comparison trick: if x < left, the
        // subtraction wraps to a huge u32, which fails the < width test.
        let left = self.clip_x.max(0);
        let top = self.clip_y.max(0);
        let right = (self.clip_x + self.clip_w).min(SCREEN_W as i32);
        let bottom = (self.clip_y + self.clip_h).min(SCREEN_H as i32);
        if x < left || x >= right || y < top || y >= bottom {
            return;
        }

        // Apply fill pattern
        if self.fill_pattern != 0 {
            // x and y are guaranteed non-negative here, so (x & 3) == x % 4.
            let bit = ((x & 3) + (y & 3) * 4) as u16;
            if self.fill_pattern & (1 << bit) != 0 {
                if self.fill_pattern_transparency {
                    return; // skip pixel entirely
                } else {
                    self.screen[y as usize * SCREEN_W + x as usize] = 0;
                    return;
                }
            }
        }
        self.screen[y as usize * SCREEN_W + x as usize] = col & 0x0F;
    }

    /// Set a pixel at world coordinates (applies camera offset, draw palette,
    /// and clipping).
    pub fn pset(&mut self, x: i32, y: i32, col: Option<u8>) {
        let c = self.resolve_color(col);
        let sx = x - self.camera_x;
        let sy = y - self.camera_y;
        self.raw_pset(sx, sy, c);
    }

    /// Read the color index at the given world coordinates (applies camera).
    pub fn pget(&self, x: i32, y: i32) -> u8 {
        let sx = x - self.camera_x;
        let sy = y - self.camera_y;
        if sx < 0 || sy < 0 || sx >= SCREEN_W as i32 || sy >= SCREEN_H as i32 {
            return 0;
        }
        self.screen[sy as usize * SCREEN_W + sx as usize]
    }

    /// Clear the entire screen to `col` (default 0 / black).
    pub fn cls(&mut self, col: Option<u8>) {
        self.screen.fill(col.unwrap_or(0) & 0x0F);
    }

    /// Draw a line using Bresenham's algorithm.
    pub fn line(&mut self, x0: i32, y0: i32, x1: i32, y1: i32, col: Option<u8>) {
        let c = self.resolve_color(col);
        let mut cx = x0;
        let mut cy = y0;
        let dx = (x1 - x0).abs();
        let dy = -(y1 - y0).abs();
        let sx: i32 = if x0 < x1 { 1 } else { -1 };
        let sy: i32 = if y0 < y1 { 1 } else { -1 };
        let mut err = dx + dy;
        loop {
            // plot
            let px = cx - self.camera_x;
            let py = cy - self.camera_y;
            self.raw_pset(px, py, c);
            if cx == x1 && cy == y1 {
                break;
            }
            let e2 = 2 * err;
            if e2 >= dy {
                err += dy;
                cx += sx;
            }
            if e2 <= dx {
                err += dx;
                cy += sy;
            }
        }
    }

    /// Draw a rectangle outline.
    pub fn rect(&mut self, x0: i32, y0: i32, x1: i32, y1: i32, col: Option<u8>) {
        let c = self.resolve_color(col);
        let lx = x0.min(x1);
        let rx = x0.max(x1);
        let ty = y0.min(y1);
        let by = y0.max(y1);
        for x in lx..=rx {
            let px = x - self.camera_x;
            self.raw_pset(px, ty - self.camera_y, c);
            self.raw_pset(px, by - self.camera_y, c);
        }
        for y in (ty + 1)..by {
            let py = y - self.camera_y;
            self.raw_pset(lx - self.camera_x, py, c);
            self.raw_pset(rx - self.camera_x, py, c);
        }
    }

    /// Draw a filled rectangle.
    pub fn rectfill(&mut self, x0: i32, y0: i32, x1: i32, y1: i32, col: Option<u8>) {
        let c = self.resolve_color(col);
        let lx = x0.min(x1) - self.camera_x;
        let rx = x0.max(x1) - self.camera_x;
        let ty = y0.min(y1) - self.camera_y;
        let by = y0.max(y1) - self.camera_y;

        // Clip to screen and clip rect.
        let x_min = lx.max(self.clip_x).max(0);
        let x_max = rx.min(self.clip_x + self.clip_w - 1).min(SCREEN_W as i32 - 1);
        let y_min = ty.max(self.clip_y).max(0);
        let y_max = by.min(self.clip_y + self.clip_h - 1).min(SCREEN_H as i32 - 1);

        if x_min > x_max || y_min > y_max {
            return;
        }

        // Fast path: no fill pattern -- write directly to screen buffer.
        if self.fill_pattern == 0 {
            let col_masked = c & 0x0F;
            for y in y_min..=y_max {
                let row_start = y as usize * SCREEN_W + x_min as usize;
                let row_end = y as usize * SCREEN_W + x_max as usize + 1;
                self.screen[row_start..row_end].fill(col_masked);
            }
        } else {
            for y in y_min..=y_max {
                for x in x_min..=x_max {
                    self.raw_pset(x, y, c);
                }
            }
        }
    }

    /// Draw a circle outline using the midpoint circle algorithm.
    pub fn circ(&mut self, cx: i32, cy: i32, r: i32, col: Option<u8>) {
        if r < 0 {
            return;
        }
        let c = self.resolve_color(col);
        let mut x = r;
        let mut y: i32 = 0;
        let mut d = 1 - r;

        while x >= y {
            self.raw_pset(cx + x - self.camera_x, cy + y - self.camera_y, c);
            self.raw_pset(cx + y - self.camera_x, cy + x - self.camera_y, c);
            self.raw_pset(cx - y - self.camera_x, cy + x - self.camera_y, c);
            self.raw_pset(cx - x - self.camera_x, cy + y - self.camera_y, c);
            self.raw_pset(cx - x - self.camera_x, cy - y - self.camera_y, c);
            self.raw_pset(cx - y - self.camera_x, cy - x - self.camera_y, c);
            self.raw_pset(cx + y - self.camera_x, cy - x - self.camera_y, c);
            self.raw_pset(cx + x - self.camera_x, cy - y - self.camera_y, c);
            y += 1;
            if d <= 0 {
                d += 2 * y + 1;
            } else {
                x -= 1;
                d += 2 * (y - x) + 1;
            }
        }
    }

    /// Draw a filled circle.
    pub fn circfill(&mut self, cx: i32, cy: i32, r: i32, col: Option<u8>) {
        if r < 0 {
            return;
        }
        let c = self.resolve_color(col);

        // Helper: draw a horizontal span (screen coords already camera-adjusted).
        let draw_hline = |s: &mut Self, lx: i32, rx: i32, sy: i32| {
            for xx in lx..=rx {
                s.raw_pset(xx, sy, c);
            }
        };

        let mut x = r;
        let mut y: i32 = 0;
        let mut d = 1 - r;

        while x >= y {
            draw_hline(
                self,
                cx - x - self.camera_x,
                cx + x - self.camera_x,
                cy + y - self.camera_y,
            );
            draw_hline(
                self,
                cx - x - self.camera_x,
                cx + x - self.camera_x,
                cy - y - self.camera_y,
            );
            draw_hline(
                self,
                cx - y - self.camera_x,
                cx + y - self.camera_x,
                cy + x - self.camera_y,
            );
            draw_hline(
                self,
                cx - y - self.camera_x,
                cx + y - self.camera_x,
                cy - x - self.camera_y,
            );
            y += 1;
            if d <= 0 {
                d += 2 * y + 1;
            } else {
                x -= 1;
                d += 2 * (y - x) + 1;
            }
        }
    }

    /// Draw an ellipse outline within the bounding box `(x0,y0)` to `(x1,y1)`.
    ///
    /// Computes per-scanline x-extents using the ellipse equation and draws
    /// only the boundary pixels. Respects camera offset, clipping, draw
    /// palette, and fill pattern.
    pub fn oval(&mut self, x0: i32, y0: i32, x1: i32, y1: i32, col: Option<u8>) {
        let c = self.resolve_color(col);
        let lx = x0.min(x1);
        let rx = x0.max(x1);
        let ty = y0.min(y1);
        let by = y0.max(y1);

        if lx == rx && ty == by {
            self.raw_pset(lx - self.camera_x, ty - self.camera_y, c);
            return;
        }
        if lx == rx {
            for y in ty..=by {
                self.raw_pset(lx - self.camera_x, y - self.camera_y, c);
            }
            return;
        }
        if ty == by {
            for x in lx..=rx {
                self.raw_pset(x - self.camera_x, ty - self.camera_y, c);
            }
            return;
        }

        // Ellipse equation in doubled coordinates to handle half-integer
        // centers correctly:
        //   h^2 * (2x - cx2)^2 + w^2 * (2y - cy2)^2 <= w^2 * h^2
        // where w = rx - lx, h = by - ty, cx2 = lx + rx, cy2 = ty + by.
        let w = (rx - lx) as i64;
        let h = (by - ty) as i64;
        let cx2 = (lx + rx) as i64;
        let cy2 = (ty + by) as i64;
        let w2 = w * w;
        let h2 = h * h;

        let mut prev_xl: i32 = i32::MAX;
        let mut prev_xr: i32 = i32::MIN;

        for y in ty..=by {
            let dy = 2 * (y as i64) - cy2;
            let rhs = w2 * (h2 - dy * dy);
            if rhs < 0 {
                continue;
            }
            // Find max ddx >= 0 such that h2 * ddx^2 <= rhs
            let quot = (rhs / h2) as u64;
            let mut ddx = isqrt(quot) as i64;
            while h2 * (ddx + 1) * (ddx + 1) <= rhs {
                ddx += 1;
            }
            let xl = (((cx2 - ddx) / 2) as i32).max(lx);
            let xr = (((cx2 + ddx) / 2) as i32).min(rx);
            let sy = y - self.camera_y;

            // Draw left and right boundary pixels
            self.raw_pset(xl - self.camera_x, sy, c);
            if xl != xr {
                self.raw_pset(xr - self.camera_x, sy, c);
            }

            // Connect to the previous scanline's extent so the outline
            // is continuous at the top and bottom arcs.
            if prev_xl != i32::MAX {
                let fl = xl.min(prev_xl);
                let fr = xl.max(prev_xl);
                for xx in fl..=fr {
                    self.raw_pset(xx - self.camera_x, sy, c);
                }
                let fl2 = xr.min(prev_xr);
                let fr2 = xr.max(prev_xr);
                for xx in fl2..=fr2 {
                    self.raw_pset(xx - self.camera_x, sy, c);
                }
            }
            prev_xl = xl;
            prev_xr = xr;
        }
    }

    /// Draw a filled ellipse within the bounding box `(x0,y0)` to `(x1,y1)`.
    ///
    /// Computes per-scanline x-extents using the ellipse equation and fills
    /// each horizontal span. Respects camera offset, clipping, draw palette,
    /// and fill pattern.
    pub fn ovalfill(&mut self, x0: i32, y0: i32, x1: i32, y1: i32, col: Option<u8>) {
        let c = self.resolve_color(col);
        let lx = x0.min(x1);
        let rx = x0.max(x1);
        let ty = y0.min(y1);
        let by = y0.max(y1);

        if lx == rx && ty == by {
            self.raw_pset(lx - self.camera_x, ty - self.camera_y, c);
            return;
        }
        if lx == rx {
            for y in ty..=by {
                self.raw_pset(lx - self.camera_x, y - self.camera_y, c);
            }
            return;
        }
        if ty == by {
            let py = ty - self.camera_y;
            for x in lx..=rx {
                self.raw_pset(x - self.camera_x, py, c);
            }
            return;
        }

        let w = (rx - lx) as i64;
        let h = (by - ty) as i64;
        let cx2 = (lx + rx) as i64;
        let cy2 = (ty + by) as i64;
        let w2 = w * w;
        let h2 = h * h;

        for y in ty..=by {
            let dy = 2 * (y as i64) - cy2;
            let rhs = w2 * (h2 - dy * dy);
            if rhs < 0 {
                continue;
            }
            let quot = (rhs / h2) as u64;
            let mut ddx = isqrt(quot) as i64;
            while h2 * (ddx + 1) * (ddx + 1) <= rhs {
                ddx += 1;
            }
            let xl = (((cx2 - ddx) / 2) as i32).max(lx);
            let xr = (((cx2 + ddx) / 2) as i32).min(rx);
            let sy = y - self.camera_y;
            for xx in xl..=xr {
                self.raw_pset(xx - self.camera_x, sy, c);
            }
        }
    }
}

/// Integer square root (floor).
fn isqrt(n: u64) -> u64 {
    if n == 0 {
        return 0;
    }
    let mut x = libm::sqrt(n as f64) as u64;
    // Adjust for floating-point imprecision
    loop {
        let x1 = (x + n / x) / 2;
        if x1 >= x {
            break;
        }
        x = x1;
    }
    while (x + 1) * (x + 1) <= n {
        x += 1;
    }
    while x * x > n {
        if x == 0 {
            break;
        }
        x -= 1;
    }
    x
}

// ---------------------------------------------------------------------------
// Sprite functions
// ---------------------------------------------------------------------------

impl Console {
    /// Get a pixel color from the sprite sheet.
    pub fn sget(&self, x: i32, y: i32) -> u8 {
        if x < 0 || y < 0 || x >= 128 || y >= 128 {
            return 0;
        }
        self.sprites[y as usize * 128 + x as usize]
    }

    /// Set a pixel color on the sprite sheet.
    pub fn sset(&mut self, x: i32, y: i32, col: u8) {
        if x < 0 || y < 0 || x >= 128 || y >= 128 {
            return;
        }
        self.sprites[y as usize * 128 + x as usize] = col & 0x0F;
    }

    /// Draw sprite `n` at world position `(x, y)`.
    ///
    /// `w` and `h` are in *sprite units* (each unit = 8 px).
    /// `flip_x` / `flip_y` mirror the sprite.
    pub fn spr(
        &mut self,
        n: i32,
        x: i32,
        y: i32,
        w: f64,
        h: f64,
        flip_x: bool,
        flip_y: bool,
    ) {
        if n < 0 || n > 255 {
            return;
        }

        let sprite_col = (n as usize) % SPRITES_PER_ROW;
        let sprite_row = (n as usize) / SPRITES_PER_ROW;
        let src_x = (sprite_col * SPRITE_W) as i32;
        let src_y = (sprite_row * SPRITE_H) as i32;

        let pw = (w * SPRITE_W as f64) as i32; // pixel width to draw
        let ph = (h * SPRITE_H as f64) as i32;

        self.sspr(src_x, src_y, pw, ph, x, y, pw, ph, flip_x, flip_y);
    }

    /// Draw a rectangle from the sprite sheet onto the screen with optional
    /// scaling and flipping.  Respects transparency, draw palette, camera,
    /// and clipping.
    pub fn sspr(
        &mut self,
        sx: i32,
        sy: i32,
        sw: i32,
        sh: i32,
        dx: i32,
        dy: i32,
        dw: i32,
        dh: i32,
        flip_x: bool,
        flip_y: bool,
    ) {
        if sw <= 0 || sh <= 0 || dw <= 0 || dh <= 0 {
            return;
        }

        // Pre-compute the camera-adjusted destination origin once.
        let dx_cam = dx - self.camera_x;
        let dy_cam = dy - self.camera_y;

        for py in 0..dh {
            // Integer source-y: py * sh / dh using integer math.
            let mut src_py = (py * sh / dh).min(sh - 1);
            if flip_y {
                src_py = sh - 1 - src_py;
            }
            let sheet_y = sy + src_py;
            let screen_y = dy_cam + py;

            for px in 0..dw {
                // Integer source-x: px * sw / dw using integer math.
                let mut src_px = (px * sw / dw).min(sw - 1);
                if flip_x {
                    src_px = sw - 1 - src_px;
                }

                let sheet_x = sx + src_px;
                let raw_col = self.sget(sheet_x, sheet_y);

                // Transparency check (on the raw, un-remapped colour).
                if self.transparent[(raw_col & 0x0F) as usize] {
                    continue;
                }

                let mapped_col = self.draw_pal[(raw_col & 0x0F) as usize] & 0x0F;
                self.raw_pset(dx_cam + px, screen_y, mapped_col);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Map functions
// ---------------------------------------------------------------------------

impl Console {
    /// Get the tile index at map position `(x, y)`.
    pub fn mget(&self, x: i32, y: i32) -> u8 {
        if x < 0 || y < 0 || x >= MAP_W as i32 || y >= MAP_H as i32 {
            return 0;
        }
        self.map[y as usize * MAP_W + x as usize]
    }

    /// Set the tile index at map position `(x, y)`.
    pub fn mset(&mut self, x: i32, y: i32, v: u8) {
        if x < 0 || y < 0 || x >= MAP_W as i32 || y >= MAP_H as i32 {
            return;
        }
        self.map[y as usize * MAP_W + x as usize] = v;
    }

    /// Draw a region of the map to the screen.
    ///
    /// `cel_x, cel_y` — top-left cell of the map region.
    /// `sx, sy` — screen-pixel position to draw at (world coords).
    /// `cel_w, cel_h` — size of the region in cells.
    /// `layer` — if non-zero, only draw tiles whose sprite flags bitwise-AND
    ///            with `layer` is non-zero.
    pub fn map_draw(
        &mut self,
        cel_x: i32,
        cel_y: i32,
        sx: i32,
        sy: i32,
        cel_w: i32,
        cel_h: i32,
        layer: u8,
    ) {
        for cy in 0..cel_h {
            for cx in 0..cel_w {
                let tile = self.mget(cel_x + cx, cel_y + cy);
                // Layer filtering.
                if layer != 0 {
                    let f = self.flags[tile as usize];
                    if f & layer == 0 {
                        continue;
                    }
                }
                let draw_x = sx + cx * SPRITE_W as i32;
                let draw_y = sy + cy * SPRITE_H as i32;
                self.spr(tile as i32, draw_x, draw_y, 1.0, 1.0, false, false);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Sprite flags
// ---------------------------------------------------------------------------

impl Console {
    /// Get sprite flags for sprite `n`.
    /// If `f` is `Some(bit)`, return just that bit (0 or 1).
    /// If `f` is `None`, return the full flag byte.
    pub fn fget(&self, n: usize, f: Option<u8>) -> u8 {
        if n >= 256 {
            return 0;
        }
        let val = self.flags[n];
        match f {
            Some(bit) => {
                if bit > 7 {
                    return 0;
                }
                (val >> bit) & 1
            }
            None => val,
        }
    }

    /// Set sprite flags for sprite `n`.
    /// If `f` is `Some(bit)`, set/clear that single bit (v != 0 sets it).
    /// If `f` is `None`, overwrite the full byte with `v`.
    pub fn fset(&mut self, n: usize, f: Option<u8>, v: u8) {
        if n >= 256 {
            return;
        }
        match f {
            Some(bit) => {
                if bit > 7 {
                    return;
                }
                if v != 0 {
                    self.flags[n] |= 1 << bit;
                } else {
                    self.flags[n] &= !(1 << bit);
                }
            }
            None => {
                self.flags[n] = v;
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Text / print
// ---------------------------------------------------------------------------

impl Console {
    /// Print a string using the built-in pixel font.
    ///
    /// If `x` / `y` are `None` the current cursor position is used.
    /// After printing the cursor advances to the start of the *next* line.
    /// Returns the x-pixel position directly after the last character drawn.
    pub fn print(
        &mut self,
        text: &str,
        x: Option<i32>,
        y: Option<i32>,
        col: Option<u8>,
    ) -> i32 {
        let c = self.resolve_color(col);

        let start_x = x.unwrap_or(self.cursor_x);
        let start_y = y.unwrap_or(self.cursor_y);

        let mut cur_x = start_x;
        let mut cur_y = start_y;

        for ch in text.chars() {
            if ch == '\n' {
                cur_x = start_x;
                cur_y += font::CHAR_H;
                continue;
            }

            let code = ch as u32;
            if code >= 32 && code <= 127 {
                let glyph = &font::FONT[(code - 32) as usize];
                for row in 0..5i32 {
                    let bits = glyph[row as usize];
                    for col_bit in 0..4i32 {
                        if bits & (0x08 >> col_bit) != 0 {
                            let px = cur_x + col_bit - self.camera_x;
                            let py = cur_y + row - self.camera_y;
                            self.raw_pset(px, py, c);
                        }
                    }
                }
            }
            cur_x += font::CHAR_W;
        }

        // Advance cursor to the next line after the printed text.
        self.cursor_x = start_x;
        self.cursor_y = cur_y + font::CHAR_H;

        cur_x
    }
}

// ---------------------------------------------------------------------------
// State management
// ---------------------------------------------------------------------------

impl Console {
    /// Set the camera offset.  Drawing subtracts these values from
    /// world coordinates to get screen coordinates.
    pub fn camera(&mut self, x: i32, y: i32) {
        self.camera_x = x;
        self.camera_y = y;
    }

    /// Set the clipping rectangle (screen coordinates).
    pub fn clip_rect(&mut self, x: i32, y: i32, w: i32, h: i32) {
        self.clip_x = x;
        self.clip_y = y;
        self.clip_w = w;
        self.clip_h = h;
    }

    /// Map colour `c0` to `c1` in the given palette.
    /// `p == 0` -> draw palette, `p == 1` -> screen (display) palette.
    pub fn pal(&mut self, c0: u8, c1: u8, p: u8) {
        let i = (c0 & 0x0F) as usize;
        let v = c1 & 0x0F;
        if p == 1 {
            self.screen_pal[i] = v;
        } else {
            self.draw_pal[i] = v;
        }
    }

    /// Reset both palettes to identity.
    pub fn pal_reset(&mut self) {
        self.draw_pal = Self::IDENTITY_PAL;
        self.screen_pal = Self::IDENTITY_PAL;
    }

    /// Set whether colour `c` is transparent (for sprite drawing).
    pub fn palt(&mut self, c: u8, t: bool) {
        let i = (c & 0x0F) as usize;
        self.transparent[i] = t;
    }

    /// Default transparency flags: only color 0 is transparent.
    const DEFAULT_TRANSPARENT: [bool; 16] = [
        true, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false,
    ];

    /// Reset transparency so that only colour 0 is transparent.
    pub fn palt_reset(&mut self) {
        self.transparent = Self::DEFAULT_TRANSPARENT;
    }

    /// Set the default draw colour.
    pub fn color(&mut self, c: u8) {
        self.draw_color = c & 0x0F;
    }

    /// Set the text cursor position.
    pub fn cursor(&mut self, x: i32, y: i32) {
        self.cursor_x = x;
        self.cursor_y = y;
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
    pub fn fillp(&mut self, p: f64) {
        // PICO-8 encodes the transparency flag in the 0x0.8 bit of the
        // fixed-point number, i.e. when the value has a fractional part >= 0.5.
        // The integer part is the 16-bit pattern.
        let int_part = p as i32;
        self.fill_pattern = (int_part & 0xFFFF) as u16;
        // Transparency if fractional part >= 0.5
        let frac = (p - (int_part as f64)).abs();
        self.fill_pattern_transparency = frac >= 0.3;
    }
}

// ---------------------------------------------------------------------------
// Textured line (tline)
// ---------------------------------------------------------------------------

impl Console {
    /// Draw a textured line from `(x0,y0)` to `(x1,y1)`, sampling the
    /// sprite sheet along a texture-coordinate path.
    ///
    /// `(mx, my)` is the starting map-space coordinate (in tiles).
    /// `(mdx, mdy)` is the step per pixel in map-space.
    ///
    /// For each pixel along the line, the sprite sheet is sampled at the
    /// position corresponding to the map coordinate, and the color is drawn
    /// to the screen.  Transparent colors (color 0 by default) are skipped.
    pub fn tline(
        &mut self,
        x0: i32,
        y0: i32,
        x1: i32,
        y1: i32,
        mx: f64,
        my: f64,
        mdx: f64,
        mdy: f64,
    ) {
        // Count the number of pixels in the line using Chebyshev distance
        let dx = (x1 - x0).abs();
        let dy = (y1 - y0).abs();
        let steps = dx.max(dy);

        if steps == 0 {
            // Single-pixel line: sample and draw one pixel
            let sx = (mx * SPRITE_W as f64) as i32;
            let sy = (my * SPRITE_H as f64) as i32;
            let raw_col = self.sget(sx.rem_euclid(128), sy.rem_euclid(128));
            if !self.transparent[(raw_col & 0x0F) as usize] {
                let mapped_col = self.draw_pal[(raw_col & 0x0F) as usize] & 0x0F;
                let screen_x = x0 - self.camera_x;
                let screen_y = y0 - self.camera_y;
                self.raw_pset(screen_x, screen_y, mapped_col);
            }
            return;
        }

        let x_step = (x1 - x0) as f64 / steps as f64;
        let y_step = (y1 - y0) as f64 / steps as f64;

        let mut px = x0 as f64;
        let mut py = y0 as f64;
        let mut tex_x = mx;
        let mut tex_y = my;

        for _ in 0..=steps {
            // Convert map coordinates to sprite-sheet pixel coordinates
            let sx = (tex_x * SPRITE_W as f64) as i32;
            let sy = (tex_y * SPRITE_H as f64) as i32;

            // Sample sprite sheet (wrapping)
            let raw_col = self.sget(sx.rem_euclid(128), sy.rem_euclid(128));

            // Skip transparent colors
            if !self.transparent[(raw_col & 0x0F) as usize] {
                let mapped_col = self.draw_pal[(raw_col & 0x0F) as usize] & 0x0F;
                let screen_x = (px as i32) - self.camera_x;
                let screen_y = (py as i32) - self.camera_y;
                self.raw_pset(screen_x, screen_y, mapped_col);
            }

            px += x_step;
            py += y_step;
            tex_x += mdx;
            tex_y += mdy;
        }
    }
}

// ---------------------------------------------------------------------------
// Persistent data (dget / dset)
// ---------------------------------------------------------------------------

impl Console {
    /// Get a persistent data value. Index must be 0-63.
    /// Returns 0.0 for out-of-bounds indices.
    pub fn dget(&self, index: i32) -> f64 {
        if index < 0 || index > 63 {
            return 0.0;
        }
        self.persistent_data[index as usize]
    }

    /// Set a persistent data value. Index must be 0-63.
    /// Out-of-bounds indices are silently ignored.
    pub fn dset(&mut self, index: i32, value: f64) {
        if index < 0 || index > 63 {
            return;
        }
        self.persistent_data[index as usize] = value;
    }
}

// ---------------------------------------------------------------------------
// Input
// ---------------------------------------------------------------------------

impl Console {
    /// Is button `i` currently held for the given `player`?
    /// `i` is the button index (0-5: left, right, up, down, O, X).
    /// `player` selects the player (0 = player 1, 1 = player 2).
    /// Out-of-bounds combinations return `false`.
    pub fn btn(&self, i: u8, player: u8) -> bool {
        let idx = i as usize + (player as usize) * 6;
        if idx >= 12 {
            return false;
        }
        self.btn_state[idx]
    }

    /// Was button `i` *just* pressed this frame for the given `player`?
    /// `i` is the button index (0-5: left, right, up, down, O, X).
    /// `player` selects the player (0 = player 1, 1 = player 2).
    /// Out-of-bounds combinations return `false`.
    pub fn btnp(&self, i: u8, player: u8) -> bool {
        let idx = i as usize + (player as usize) * 6;
        if idx >= 12 {
            return false;
        }
        self.btn_state[idx] && !self.btn_prev[idx]
    }

    /// Called once per frame before running the game's `_update`.
    /// Saves the current state as *previous* and installs `new_state`.
    /// Indices 0-5: player 1 (left, right, up, down, O, X)
    /// Indices 6-11: player 2 (left, right, up, down, O, X)
    pub fn update_input(&mut self, new_state: [bool; 12]) {
        self.btn_prev = self.btn_state;
        self.btn_state = new_state;
    }
}

// ---------------------------------------------------------------------------
// Memory-mapped I/O (PICO-8 memory map)
// ---------------------------------------------------------------------------
//
// PICO-8 memory map (32 KB):
//   0x0000-0x0FFF  Sprite sheet     (4096 bytes, 2 pixels per byte packed)
//   0x1000-0x10FF  Sprite flags     (256 bytes, direct map)
//   0x2000-0x2FFF  Map data         (4096 bytes, lower half of 128x64 map)
//   0x3000-0x30FF  Draw state       (palette, camera, clip, etc.)
//   0x4300-0x5FFF  General purpose  (7424 bytes, user data)
//   0x6000-0x7FFF  Screen buffer    (8192 bytes, 2 pixels per byte packed)
//
// Sprite/screen packing: high nibble = left pixel, low nibble = right pixel.

impl Console {
    /// Read a single byte from the PICO-8 memory map.
    ///
    /// Addresses outside mapped regions return 0.
    pub fn peek(&self, addr: u16) -> u8 {
        match addr {
            // Sprite sheet: 0x0000-0x0FFF (4096 bytes, 2 pixels per byte)
            0x0000..=0x0FFF => {
                let offset = (addr - 0x0000) as usize;
                let pixel_idx = offset * 2;
                if pixel_idx + 1 < SPRITE_SHEET_SIZE {
                    let left = self.sprites[pixel_idx] & 0x0F;
                    let right = self.sprites[pixel_idx + 1] & 0x0F;
                    (left) | (right << 4)
                } else {
                    0
                }
            }
            // Sprite flags: 0x1000-0x10FF (256 bytes)
            0x1000..=0x10FF => {
                let offset = (addr - 0x1000) as usize;
                self.flags[offset]
            }
            // Map data: 0x2000-0x2FFF (4096 bytes)
            0x2000..=0x2FFF => {
                let offset = (addr - 0x2000) as usize;
                if offset < MAP_SIZE {
                    self.map[offset]
                } else {
                    0
                }
            }
            // Draw state: 0x3000-0x30FF
            0x3000..=0x30FF => {
                let offset = (addr - 0x3000) as usize;
                self.peek_draw_state(offset)
            }
            // General purpose / user data: 0x4300-0x5FFF
            0x4300..=0x5FFF => {
                let offset = (addr - 0x4300) as usize;
                self.user_data[offset]
            }
            // Screen buffer: 0x6000-0x7FFF (8192 bytes, 2 pixels per byte)
            0x6000..=0x7FFF => {
                let offset = (addr - 0x6000) as usize;
                let pixel_idx = offset * 2;
                if pixel_idx + 1 < SCREEN_SIZE {
                    let left = self.screen[pixel_idx] & 0x0F;
                    let right = self.screen[pixel_idx + 1] & 0x0F;
                    (left) | (right << 4)
                } else {
                    0
                }
            }
            _ => 0,
        }
    }

    /// Write a single byte to the PICO-8 memory map.
    ///
    /// Addresses outside mapped regions are silently ignored.
    pub fn poke(&mut self, addr: u16, val: u8) {
        match addr {
            // Sprite sheet: 0x0000-0x0FFF
            0x0000..=0x0FFF => {
                let offset = (addr - 0x0000) as usize;
                let pixel_idx = offset * 2;
                if pixel_idx + 1 < SPRITE_SHEET_SIZE {
                    self.sprites[pixel_idx] = val & 0x0F;
                    self.sprites[pixel_idx + 1] = (val >> 4) & 0x0F;
                }
            }
            // Sprite flags: 0x1000-0x10FF
            0x1000..=0x10FF => {
                let offset = (addr - 0x1000) as usize;
                self.flags[offset] = val;
            }
            // Map data: 0x2000-0x2FFF
            0x2000..=0x2FFF => {
                let offset = (addr - 0x2000) as usize;
                if offset < MAP_SIZE {
                    self.map[offset] = val;
                }
            }
            // Draw state: 0x3000-0x30FF
            0x3000..=0x30FF => {
                let offset = (addr - 0x3000) as usize;
                self.poke_draw_state(offset, val);
            }
            // General purpose / user data: 0x4300-0x5FFF
            0x4300..=0x5FFF => {
                let offset = (addr - 0x4300) as usize;
                self.user_data[offset] = val;
            }
            // Screen buffer: 0x6000-0x7FFF
            0x6000..=0x7FFF => {
                let offset = (addr - 0x6000) as usize;
                let pixel_idx = offset * 2;
                if pixel_idx + 1 < SCREEN_SIZE {
                    self.screen[pixel_idx] = val & 0x0F;
                    self.screen[pixel_idx + 1] = (val >> 4) & 0x0F;
                }
            }
            _ => {}
        }
    }

    /// Copy `len` bytes within the PICO-8 memory map.
    ///
    /// Reads from `src..src+len` and writes to `dest..dest+len`.
    /// Reads all source bytes first to handle overlapping regions correctly.
    pub fn memcpy(&mut self, dest: u16, src: u16, len: u16) {
        if len == 0 {
            return;
        }
        // Snapshot source bytes first so overlapping src/dest ranges are safe.
        let mut buf = alloc::vec::Vec::with_capacity(len as usize);
        for i in 0..len {
            buf.push(self.peek(src.wrapping_add(i)));
        }
        for (i, &b) in buf.iter().enumerate() {
            self.poke(dest.wrapping_add(i as u16), b);
        }
    }

    /// Fill `len` bytes in the PICO-8 memory map starting at `dest` with `val`.
    pub fn memset(&mut self, dest: u16, val: u8, len: u16) {
        for i in 0..len {
            let addr = dest.wrapping_add(i);
            self.poke(addr, val);
        }
    }

    /// Read a 16-bit value (little-endian) from two consecutive bytes.
    pub fn peek2(&self, addr: u16) -> u16 {
        let lo = self.peek(addr) as u16;
        let hi = self.peek(addr.wrapping_add(1)) as u16;
        lo | (hi << 8)
    }

    /// Read a 32-bit value (little-endian) from four consecutive bytes.
    pub fn peek4(&self, addr: u16) -> u32 {
        let b0 = self.peek(addr) as u32;
        let b1 = self.peek(addr.wrapping_add(1)) as u32;
        let b2 = self.peek(addr.wrapping_add(2)) as u32;
        let b3 = self.peek(addr.wrapping_add(3)) as u32;
        b0 | (b1 << 8) | (b2 << 16) | (b3 << 24)
    }

    /// Write a 16-bit value (little-endian) to two consecutive bytes.
    pub fn poke2(&mut self, addr: u16, val: u16) {
        self.poke(addr, (val & 0xFF) as u8);
        self.poke(addr.wrapping_add(1), ((val >> 8) & 0xFF) as u8);
    }

    /// Write a 32-bit value (little-endian) to four consecutive bytes.
    pub fn poke4(&mut self, addr: u16, val: u32) {
        self.poke(addr, (val & 0xFF) as u8);
        self.poke(addr.wrapping_add(1), ((val >> 8) & 0xFF) as u8);
        self.poke(addr.wrapping_add(2), ((val >> 16) & 0xFF) as u8);
        self.poke(addr.wrapping_add(3), ((val >> 24) & 0xFF) as u8);
    }

    /// Copy `len` bytes from live RAM to the cart ROM snapshot.
    ///
    /// `dest_addr` is the offset within `cart_rom` (0x0000-0x42FF).
    /// `src_addr` is the address in the PICO-8 memory map to read from.
    pub fn cstore(&mut self, dest_addr: u16, src_addr: u16, len: u16) {
        for i in 0..len {
            let src = src_addr.wrapping_add(i);
            let dst = dest_addr.wrapping_add(i) as usize;
            if dst < self.cart_rom.len() {
                self.cart_rom[dst] = self.peek(src);
            }
        }
    }

    /// Copy `len` bytes from the cart ROM snapshot back into live RAM.
    ///
    /// `dest_addr` is the address in the PICO-8 memory map to write to.
    /// `src_addr` is the offset within `cart_rom` (0x0000-0x42FF).
    pub fn reload(&mut self, dest_addr: u16, src_addr: u16, len: u16) {
        for i in 0..len {
            let src = src_addr.wrapping_add(i) as usize;
            let dst = dest_addr.wrapping_add(i);
            if src < self.cart_rom.len() {
                self.poke(dst, self.cart_rom[src]);
            }
        }
    }

    /// Read from the draw-state region (0x3000-0x30FF).
    fn peek_draw_state(&self, offset: usize) -> u8 {
        match offset {
            // 0x00-0x0F: draw palette (16 bytes)
            0x00..=0x0F => self.draw_pal[offset],
            // 0x10-0x1F: screen palette (16 bytes)
            0x10..=0x1F => self.screen_pal[offset - 0x10],
            // 0x20: clip_x low byte
            0x20 => (self.clip_x & 0xFF) as u8,
            // 0x21: clip_y low byte
            0x21 => (self.clip_y & 0xFF) as u8,
            // 0x22: clip_w low byte
            0x22 => (self.clip_w & 0xFF) as u8,
            // 0x23: clip_h low byte
            0x23 => (self.clip_h & 0xFF) as u8,
            // 0x24: draw_color
            0x24 => self.draw_color,
            // 0x28: camera_x low byte
            0x28 => (self.camera_x & 0xFF) as u8,
            // 0x29: camera_x high byte
            0x29 => ((self.camera_x >> 8) & 0xFF) as u8,
            // 0x2A: camera_y low byte
            0x2A => (self.camera_y & 0xFF) as u8,
            // 0x2B: camera_y high byte
            0x2B => ((self.camera_y >> 8) & 0xFF) as u8,
            _ => 0,
        }
    }

    /// Write to the draw-state region (0x3000-0x30FF).
    fn poke_draw_state(&mut self, offset: usize, val: u8) {
        match offset {
            // 0x00-0x0F: draw palette
            0x00..=0x0F => self.draw_pal[offset] = val & 0x0F,
            // 0x10-0x1F: screen palette
            0x10..=0x1F => self.screen_pal[offset - 0x10] = val & 0x0F,
            // 0x20-0x23: clip rect
            0x20 => self.clip_x = val as i32,
            0x21 => self.clip_y = val as i32,
            0x22 => self.clip_w = val as i32,
            0x23 => self.clip_h = val as i32,
            // 0x24: draw_color
            0x24 => self.draw_color = val & 0x0F,
            // 0x28-0x2B: camera
            0x28 => self.camera_x = (self.camera_x & !0xFF) | val as i32,
            0x29 => self.camera_x = (self.camera_x & 0xFF) | ((val as i32) << 8),
            0x2A => self.camera_y = (self.camera_y & !0xFF) | val as i32,
            0x2B => self.camera_y = (self.camera_y & 0xFF) | ((val as i32) << 8),
            _ => {}
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_peek_poke_sprite_sheet() {
        let mut con = Console::new();
        // Set two adjacent pixels in the sprite sheet
        con.sprites[0] = 3;  // left pixel of byte 0
        con.sprites[1] = 10; // right pixel of byte 0
        // PICO-8 packing: low nibble = left pixel, high nibble = right pixel
        let val = con.peek(0x0000);
        assert_eq!(val & 0x0F, 3, "Low nibble should be left pixel (3)");
        assert_eq!((val >> 4) & 0x0F, 10, "High nibble should be right pixel (10)");

        // Poke a packed byte and verify the sprite array is updated
        con.poke(0x0001, 0x75); // left=5, right=7
        assert_eq!(con.sprites[2], 5, "Poke should set left pixel to low nibble");
        assert_eq!(con.sprites[3], 7, "Poke should set right pixel to high nibble");

        // Round-trip
        con.poke(0x0000, 0xAB);
        assert_eq!(con.peek(0x0000), 0xAB);
    }

    #[test]
    fn test_peek_poke_sprite_flags() {
        let mut con = Console::new();
        con.flags[0] = 0xFF;
        assert_eq!(con.peek(0x1000), 0xFF);
        con.flags[255] = 0x42;
        assert_eq!(con.peek(0x10FF), 0x42);

        con.poke(0x1000, 0x12);
        assert_eq!(con.flags[0], 0x12);
        con.poke(0x10FF, 0x34);
        assert_eq!(con.flags[255], 0x34);
    }

    #[test]
    fn test_peek_poke_map_data() {
        let mut con = Console::new();
        con.map[0] = 42;
        assert_eq!(con.peek(0x2000), 42);
        con.map[4095] = 99;
        assert_eq!(con.peek(0x2FFF), 99);

        con.poke(0x2000, 55);
        assert_eq!(con.map[0], 55);
        con.poke(0x2FFF, 77);
        assert_eq!(con.map[4095], 77);
    }

    #[test]
    fn test_peek_poke_user_data() {
        let mut con = Console::new();
        // Initially all zeros
        assert_eq!(con.peek(0x4300), 0);
        assert_eq!(con.peek(0x5FFF), 0);

        con.poke(0x4300, 0xAA);
        assert_eq!(con.peek(0x4300), 0xAA);
        assert_eq!(con.user_data[0], 0xAA);

        con.poke(0x5FFF, 0xBB);
        assert_eq!(con.peek(0x5FFF), 0xBB);
        assert_eq!(con.user_data[USER_DATA_SIZE - 1], 0xBB);
    }

    #[test]
    fn test_peek_poke_screen_buffer() {
        let mut con = Console::new();
        con.screen[0] = 7;
        con.screen[1] = 12;
        let val = con.peek(0x6000);
        assert_eq!(val & 0x0F, 7);
        assert_eq!((val >> 4) & 0x0F, 12);

        con.poke(0x6000, 0x95); // left=5, right=9
        assert_eq!(con.screen[0], 5);
        assert_eq!(con.screen[1], 9);
    }

    #[test]
    fn test_peek_out_of_bounds() {
        let con = Console::new();
        // Unmapped regions should return 0
        assert_eq!(con.peek(0x1100), 0);
        assert_eq!(con.peek(0x1FFF), 0);
        assert_eq!(con.peek(0x3100), 0);
        assert_eq!(con.peek(0x4200), 0);
    }

    #[test]
    fn test_poke_out_of_bounds_is_noop() {
        let mut con = Console::new();
        // These should not panic
        con.poke(0x1100, 0xFF);
        con.poke(0x1FFF, 0xFF);
        con.poke(0x3100, 0xFF);
        con.poke(0x4200, 0xFF);
    }

    #[test]
    fn test_memcpy_within_same_region() {
        let mut con = Console::new();
        // Write some data into user data
        con.poke(0x4300, 0x11);
        con.poke(0x4301, 0x22);
        con.poke(0x4302, 0x33);

        // Copy 3 bytes within user data
        con.memcpy(0x4310, 0x4300, 3);

        assert_eq!(con.peek(0x4310), 0x11);
        assert_eq!(con.peek(0x4311), 0x22);
        assert_eq!(con.peek(0x4312), 0x33);
    }

    #[test]
    fn test_memcpy_across_regions() {
        let mut con = Console::new();
        // Set some sprite flags
        con.flags[0] = 0xAA;
        con.flags[1] = 0xBB;

        // Copy sprite flags (0x1000) into user data (0x4300)
        con.memcpy(0x4300, 0x1000, 2);

        assert_eq!(con.peek(0x4300), 0xAA);
        assert_eq!(con.peek(0x4301), 0xBB);
    }

    #[test]
    fn test_memset() {
        let mut con = Console::new();
        con.memset(0x4300, 0xFF, 10);
        for i in 0..10 {
            assert_eq!(con.peek(0x4300 + i), 0xFF);
        }
        // Byte after should be untouched
        assert_eq!(con.peek(0x430A), 0);
    }

    #[test]
    fn test_memcpy_zero_length() {
        let mut con = Console::new();
        con.poke(0x4300, 0x42);
        con.memcpy(0x4301, 0x4300, 0);
        assert_eq!(con.peek(0x4301), 0, "Zero-length memcpy should be a no-op");
    }

    #[test]
    fn test_memset_zero_length() {
        let mut con = Console::new();
        con.poke(0x4300, 0x42);
        con.memset(0x4300, 0xFF, 0);
        assert_eq!(con.peek(0x4300), 0x42, "Zero-length memset should be a no-op");
    }

    #[test]
    fn test_sprite_pixel_packing_round_trip() {
        let mut con = Console::new();
        // Set pixels via the sprite array (1 byte per pixel)
        for i in 0..16u8 {
            con.sprites[i as usize * 2] = i;           // left pixel
            con.sprites[i as usize * 2 + 1] = 15 - i;  // right pixel
        }

        // Read back via peek (packed format)
        for i in 0..16u16 {
            let packed = con.peek(i);
            let left = packed & 0x0F;
            let right = (packed >> 4) & 0x0F;
            assert_eq!(left, i as u8, "Left pixel mismatch at byte {}", i);
            assert_eq!(right, 15 - i as u8, "Right pixel mismatch at byte {}", i);
        }

        // Write via poke and read back from sprite array
        for i in 0..16u16 {
            let packed = (i as u8) | ((15 - i as u8) << 4);
            con.poke(i, packed);
        }
        for i in 0..16u16 {
            assert_eq!(con.sprites[i as usize * 2], i as u8);
            assert_eq!(con.sprites[i as usize * 2 + 1], 15 - i as u8);
        }
    }

    #[test]
    fn test_peek_poke_draw_state() {
        let mut con = Console::new();

        // Draw palette
        con.draw_pal[5] = 10;
        assert_eq!(con.peek(0x3005), 10);
        con.poke(0x3005, 7);
        assert_eq!(con.draw_pal[5], 7);

        // Screen palette
        con.screen_pal[3] = 14;
        assert_eq!(con.peek(0x3013), 14);
        con.poke(0x3013, 2);
        assert_eq!(con.screen_pal[3], 2);

        // Camera
        con.camera_x = 0;
        con.poke(0x3028, 50);
        assert_eq!(con.camera_x, 50);

        // Draw color
        con.poke(0x3024, 8);
        assert_eq!(con.draw_color, 8);
        assert_eq!(con.peek(0x3024), 8);
    }

    // -----------------------------------------------------------------------
    // fillp tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_fillp_pattern_bits_affect_drawing() {
        let mut con = Console::new();
        // Set a checkerboard-like pattern: bit 0 set means pixel (0,0) is
        // affected by the pattern.
        con.fillp(0b0000_0000_0000_0001 as f64); // only bit 0 set
        // Draw a pixel at (0,0) — bit 0 maps to (x%4=0, y%4=0)
        // Pattern is active (no transparency) so the pixel should be drawn
        // with color 0 instead of the requested color.
        con.raw_pset(0, 0, 7);
        assert_eq!(con.screen[0], 0, "Pixel at (0,0) should be color 0 due to fill pattern");

        // Pixel at (1,0) is not affected by the pattern (bit 1 is 0)
        con.raw_pset(1, 0, 7);
        assert_eq!(con.screen[1], 7, "Pixel at (1,0) should be color 7 (pattern bit not set)");
    }

    #[test]
    fn test_fillp_transparency_mode() {
        let mut con = Console::new();
        // Set pattern with transparency: bit 0 set, transparency on
        // Using 0.5 fractional part to enable transparency
        con.fillp(1.0 + 0.5); // pattern = 1 (bit 0), transparency = true
        // Pre-fill screen with color 5
        con.screen[0] = 5;
        // Draw at (0,0) — pattern bit set + transparency means pixel is skipped
        con.raw_pset(0, 0, 7);
        assert_eq!(con.screen[0], 5, "Pixel at (0,0) should be unchanged due to transparency");
    }

    #[test]
    fn test_fillp_zero_clears_pattern() {
        let mut con = Console::new();
        // Set a pattern
        con.fillp(0xFFFF as f64);
        assert_eq!(con.fill_pattern, 0xFFFF);
        // Clear it
        con.fillp(0.0);
        assert_eq!(con.fill_pattern, 0, "fillp(0) should clear the pattern");
        assert_eq!(con.fill_pattern_transparency, false, "fillp(0) should clear transparency");

        // Verify drawing works normally again
        con.raw_pset(0, 0, 7);
        assert_eq!(con.screen[0], 7, "Drawing should work normally after fillp(0)");
    }

    #[test]
    fn test_reset_draw_state_clears_fill_pattern() {
        let mut con = Console::new();
        con.fillp(0xAAAA as f64 + 0.5);
        assert_ne!(con.fill_pattern, 0);
        assert!(con.fill_pattern_transparency);

        con.reset_draw_state();
        assert_eq!(con.fill_pattern, 0, "reset_draw_state should clear fill pattern");
        assert_eq!(con.fill_pattern_transparency, false, "reset_draw_state should clear fill transparency");
    }

    // -----------------------------------------------------------------------
    // tline tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_tline_basic_draws_pixels() {
        let mut con = Console::new();
        // Set up a simple sprite sheet: fill the top-left 8x8 area with color 5
        for y in 0..8 {
            for x in 0..8 {
                con.sprites[y * 128 + x] = 5;
            }
        }
        // Color 0 is transparent by default, but color 5 is not.
        // Draw a horizontal textured line from (0,0) to (7,0)
        // mapping to tile (0,0) with step (1/8, 0) per pixel
        con.tline(0, 0, 7, 0, 0.0, 0.0, 1.0 / 8.0, 0.0);

        // Verify that pixels were drawn
        for x in 0..8 {
            assert_eq!(
                con.screen[x], 5,
                "Pixel at ({},0) should be color 5 from sprite sheet",
                x
            );
        }
    }

    #[test]
    fn test_tline_single_pixel() {
        let mut con = Console::new();
        // Set sprite pixel at (0,0) to color 3
        con.sprites[0] = 3;
        // Draw a single-pixel line
        con.tline(10, 10, 10, 10, 0.0, 0.0, 0.0, 0.0);
        assert_eq!(con.screen[10 * 128 + 10], 3, "Single-pixel tline should draw the sampled color");
    }

    // -----------------------------------------------------------------------
    // dget / dset tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_dget_default_is_zero() {
        let con = Console::new();
        for i in 0..64 {
            assert_eq!(con.dget(i), 0.0, "dget({}) should default to 0.0", i);
        }
    }

    #[test]
    fn test_dset_and_dget() {
        let mut con = Console::new();
        con.dset(0, 42.5);
        con.dset(63, -100.0);
        assert_eq!(con.dget(0), 42.5);
        assert_eq!(con.dget(63), -100.0);
        // Other indices remain 0
        assert_eq!(con.dget(1), 0.0);
        assert_eq!(con.dget(32), 0.0);
    }

    #[test]
    fn test_dget_out_of_bounds_returns_zero() {
        let con = Console::new();
        assert_eq!(con.dget(-1), 0.0, "dget(-1) should return 0.0");
        assert_eq!(con.dget(64), 0.0, "dget(64) should return 0.0");
        assert_eq!(con.dget(100), 0.0, "dget(100) should return 0.0");
    }

    #[test]
    fn test_dset_out_of_bounds_is_noop() {
        let mut con = Console::new();
        con.dset(-1, 999.0);  // should not panic
        con.dset(64, 999.0);  // should not panic
        // Verify no data was corrupted
        for i in 0..64 {
            assert_eq!(con.dget(i), 0.0);
        }
    }

    // -----------------------------------------------------------------------
    // btn / btnp with player parameter tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_btn_player0_returns_player1_state() {
        let mut con = Console::new();
        con.btn_state[0] = true; // player 1, button 0 (left)
        assert!(con.btn(0, 0), "btn(0, 0) should return player 1 left button state");
        assert!(!con.btn(1, 0), "btn(1, 0) should be false when not pressed");
    }

    #[test]
    fn test_btn_player1_returns_player2_state() {
        let mut con = Console::new();
        con.btn_state[6] = true; // player 2, button 0 (left) => index 6
        assert!(con.btn(0, 1), "btn(0, 1) should return player 2 left button (index 6)");
        assert!(!con.btn(0, 0), "btn(0, 0) should be false for player 1");
    }

    #[test]
    fn test_btn_player_isolation() {
        let mut con = Console::new();
        // Set player 1 button 3 (down)
        con.btn_state[3] = true;
        // Set player 2 button 3 (down) => index 9
        con.btn_state[9] = true;
        assert!(con.btn(3, 0), "player 1 down should be pressed");
        assert!(con.btn(3, 1), "player 2 down should be pressed");
        // Player 1 button 4 is not pressed
        assert!(!con.btn(4, 0));
        // Player 2 button 4 is not pressed
        assert!(!con.btn(4, 1));
    }

    #[test]
    fn test_btnp_player0_vs_player1() {
        let mut con = Console::new();
        // Previous frame: nothing pressed
        // Current frame: player 1 button 4 (O) and player 2 button 4 (O)
        con.btn_state[4] = true;   // player 1, button 4
        con.btn_state[10] = true;  // player 2, button 4 (index 6+4=10)
        assert!(con.btnp(4, 0), "btnp(4, 0) should detect player 1 just-pressed");
        assert!(con.btnp(4, 1), "btnp(4, 1) should detect player 2 just-pressed");

        // If player 1 button was held from previous frame, btnp should be false
        con.btn_prev[4] = true;
        assert!(!con.btnp(4, 0), "btnp(4, 0) should be false when held from previous frame");
        // Player 2 was not held previously, so still just-pressed
        assert!(con.btnp(4, 1), "btnp(4, 1) should still be true for player 2");
    }

    #[test]
    fn test_btn_out_of_bounds_player_returns_false() {
        let mut con = Console::new();
        // Set all buttons to true
        for i in 0..12 {
            con.btn_state[i] = true;
        }
        // Player 2+ should return false (player * 6 + i >= 12)
        assert!(!con.btn(0, 2), "btn(0, 2) should return false for out-of-bounds player");
        assert!(!con.btn(0, 255), "btn(0, 255) should return false for out-of-bounds player");
        assert!(!con.btn(5, 2), "btn(5, 2) should return false for out-of-bounds player");
    }

    #[test]
    fn test_btnp_out_of_bounds_player_returns_false() {
        let mut con = Console::new();
        for i in 0..12 {
            con.btn_state[i] = true;
        }
        assert!(!con.btnp(0, 2), "btnp(0, 2) should return false for out-of-bounds player");
        assert!(!con.btnp(3, 3), "btnp(3, 3) should return false for out-of-bounds player");
    }

    #[test]
    fn test_btn_default_player0_behavior() {
        let mut con = Console::new();
        con.btn_state[2] = true; // player 1, button 2 (up)
        // Calling with player=0 should behave the same as the old single-arg behavior
        assert!(con.btn(2, 0), "btn(2, 0) should return true for player 1 up");
        assert!(!con.btn(2, 1), "btn(2, 1) should return false for player 2 up");
    }

    // -----------------------------------------------------------------------
    // oval / ovalfill tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_oval_draws_outline() {
        let mut con = Console::new();
        // Draw an ellipse in bounding box (10,10) to (20,16)
        // Center = (15, 13), semi-axes = (5, 3)
        con.oval(10, 10, 20, 16, Some(7));
        // Top center and bottom center of the ellipse should be drawn
        assert_eq!(con.screen[10 * SCREEN_W + 15], 7, "Top center of oval should be drawn");
        assert_eq!(con.screen[16 * SCREEN_W + 15], 7, "Bottom center of oval should be drawn");
        // Left and right extremes at the center row
        assert_eq!(con.screen[13 * SCREEN_W + 10], 7, "Left extreme of oval should be drawn");
        assert_eq!(con.screen[13 * SCREEN_W + 20], 7, "Right extreme of oval should be drawn");
        // The center of the ellipse should NOT be drawn (it's just an outline)
        assert_ne!(con.screen[13 * SCREEN_W + 15], 7, "Center of oval should not be drawn (outline only)");
    }

    #[test]
    fn test_ovalfill_fills_interior() {
        let mut con = Console::new();
        // Draw a filled ellipse in bounding box (10,10) to (20,16)
        con.ovalfill(10, 10, 20, 16, Some(5));
        // Center should be filled
        assert_eq!(con.screen[13 * SCREEN_W + 15], 5, "Center of filled oval should be drawn");
        // Top center should be filled
        assert_eq!(con.screen[10 * SCREEN_W + 15], 5, "Top center of filled oval should be drawn");
        // Bottom center should be filled
        assert_eq!(con.screen[16 * SCREEN_W + 15], 5, "Bottom center of filled oval should be drawn");
        // A pixel clearly outside the ellipse should not be filled
        assert_ne!(con.screen[10 * SCREEN_W + 10], 5, "Corner of bounding box should not be filled");
    }

    #[test]
    fn test_oval_single_pixel() {
        let mut con = Console::new();
        // Bounding box of size 0x0 => single pixel
        con.oval(50, 50, 50, 50, Some(8));
        assert_eq!(con.screen[50 * SCREEN_W + 50], 8, "Degenerate oval should draw single pixel");
    }

    #[test]
    fn test_ovalfill_single_pixel() {
        let mut con = Console::new();
        con.ovalfill(50, 50, 50, 50, Some(8));
        assert_eq!(con.screen[50 * SCREEN_W + 50], 8, "Degenerate ovalfill should draw single pixel");
    }

    // -----------------------------------------------------------------------
    // peek2 / peek4 tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_peek2_reads_little_endian() {
        let mut con = Console::new();
        // Write two individual bytes to user data
        con.poke(0x4300, 0x34); // low byte
        con.poke(0x4301, 0x12); // high byte
        let val = con.peek2(0x4300);
        assert_eq!(val, 0x1234, "peek2 should read little-endian u16");
    }

    #[test]
    fn test_peek4_reads_little_endian() {
        let mut con = Console::new();
        con.poke(0x4300, 0x78);
        con.poke(0x4301, 0x56);
        con.poke(0x4302, 0x34);
        con.poke(0x4303, 0x12);
        let val = con.peek4(0x4300);
        assert_eq!(val, 0x12345678, "peek4 should read little-endian u32");
    }

    // -----------------------------------------------------------------------
    // poke2 / poke4 tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_poke2_writes_little_endian() {
        let mut con = Console::new();
        con.poke2(0x4300, 0xABCD);
        assert_eq!(con.peek(0x4300), 0xCD, "poke2 low byte");
        assert_eq!(con.peek(0x4301), 0xAB, "poke2 high byte");
    }

    #[test]
    fn test_poke4_writes_little_endian() {
        let mut con = Console::new();
        con.poke4(0x4300, 0xDEADBEEF);
        assert_eq!(con.peek(0x4300), 0xEF, "poke4 byte 0");
        assert_eq!(con.peek(0x4301), 0xBE, "poke4 byte 1");
        assert_eq!(con.peek(0x4302), 0xAD, "poke4 byte 2");
        assert_eq!(con.peek(0x4303), 0xDE, "poke4 byte 3");
    }

    #[test]
    fn test_poke2_peek2_round_trip() {
        let mut con = Console::new();
        con.poke2(0x4300, 0xFFEE);
        assert_eq!(con.peek2(0x4300), 0xFFEE, "poke2/peek2 round trip");
    }

    #[test]
    fn test_poke4_peek4_round_trip() {
        let mut con = Console::new();
        con.poke4(0x4300, 0xCAFEBABE);
        assert_eq!(con.peek4(0x4300), 0xCAFEBABE, "poke4/peek4 round trip");
    }

    // -----------------------------------------------------------------------
    // cursor tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_cursor_sets_position() {
        let mut con = Console::new();
        assert_eq!(con.cursor_x, 0);
        assert_eq!(con.cursor_y, 0);
        con.cursor(42, 99);
        assert_eq!(con.cursor_x, 42, "cursor should set cursor_x");
        assert_eq!(con.cursor_y, 99, "cursor should set cursor_y");
    }

    #[test]
    fn test_cursor_negative_values() {
        let mut con = Console::new();
        con.cursor(-10, -20);
        assert_eq!(con.cursor_x, -10);
        assert_eq!(con.cursor_y, -20);
    }

    // -----------------------------------------------------------------------
    // cstore / reload tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_cstore_and_reload() {
        let mut con = Console::new();
        // Write some data to sprite flags (live memory at 0x1000)
        con.poke(0x1000, 0xAA);
        con.poke(0x1001, 0xBB);
        con.poke(0x1002, 0xCC);

        // cstore: copy from live RAM (0x1000) to cart ROM (offset 0x1000)
        con.cstore(0x1000, 0x1000, 3);

        // Verify cart_rom was written
        assert_eq!(con.cart_rom[0x1000], 0xAA);
        assert_eq!(con.cart_rom[0x1001], 0xBB);
        assert_eq!(con.cart_rom[0x1002], 0xCC);

        // Now modify live memory
        con.poke(0x1000, 0x00);
        con.poke(0x1001, 0x00);
        con.poke(0x1002, 0x00);
        assert_eq!(con.peek(0x1000), 0x00, "Live memory should be zeroed");

        // reload: copy from cart ROM (offset 0x1000) back to live RAM (0x1000)
        con.reload(0x1000, 0x1000, 3);

        // Verify live memory was restored
        assert_eq!(con.peek(0x1000), 0xAA, "reload should restore byte 0");
        assert_eq!(con.peek(0x1001), 0xBB, "reload should restore byte 1");
        assert_eq!(con.peek(0x1002), 0xCC, "reload should restore byte 2");
    }

    #[test]
    fn test_cstore_reload_different_addresses() {
        let mut con = Console::new();
        // Write to user data at 0x4300
        con.poke(0x4300, 0x42);

        // cstore from live 0x4300 to ROM offset 0x0000
        con.cstore(0x0000, 0x4300, 1);
        assert_eq!(con.cart_rom[0x0000], 0x42);

        // Clear live memory
        con.poke(0x4300, 0x00);

        // reload from ROM offset 0x0000 to live 0x4300
        con.reload(0x4300, 0x0000, 1);
        assert_eq!(con.peek(0x4300), 0x42, "reload with different src/dest should work");
    }

    #[test]
    fn test_cstore_out_of_bounds_rom_is_safe() {
        let mut con = Console::new();
        // Try to write beyond cart_rom size; should not panic
        con.poke(0x4300, 0xFF);
        con.cstore(0x42FF, 0x4300, 5); // starts at last valid, goes past
        // Only the first byte should have been written
        assert_eq!(con.cart_rom[0x42FF], 0xFF);
    }

    #[test]
    fn test_reload_out_of_bounds_rom_is_safe() {
        let mut con = Console::new();
        con.cart_rom[0x42FF] = 0xEE;
        // Try to read beyond cart_rom size; should not panic
        con.reload(0x4300, 0x42FF, 5);
        assert_eq!(con.peek(0x4300), 0xEE, "First byte within ROM should be copied");
    }
}
