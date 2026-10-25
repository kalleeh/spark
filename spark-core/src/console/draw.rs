//! Rendering: pixels, shapes, sprites, map and textured lines.

use super::*;

// ---------------------------------------------------------------------------
// Drawing primitives
// ---------------------------------------------------------------------------

impl Console {
    /// Set a pixel directly at screen coordinates (no camera offset) but
    /// *with* clipping.  `col` should already be palette-mapped.
    ///
    /// This is the SINGLE-COLOR path (sprites, lines, sprite blits). For shape
    /// fills that honor the two-color `fillp` form, see [`Self::pset_fill`].
    /// When a fill pattern is set, a patterned pixel here is skipped if
    /// `fill_pattern_transparency` else drawn as color 0 (the legacy behavior,
    /// equivalent to a fill whose secondary color is 0).
    #[inline]
    pub fn raw_pset(&mut self, x: i32, y: i32, col: u8) {
        // Single-color form: a patterned pixel draws secondary color 0.
        self.pset_fill(x, y, col, 0);
    }

    /// Set a pixel honoring the full two-color `fillp` form: where the pattern
    /// bit is set, draw the secondary color `c1` (or skip if transparency is
    /// on); elsewhere draw the primary `c0`. Both must be pre-palette-mapped
    /// (see [`Self::resolve_fill_colors`]). Used by the shape primitives.
    #[inline]
    pub fn pset_fill(&mut self, x: i32, y: i32, c0: u8, c1: u8) {
        if !self.in_clip(x, y) {
            return;
        }
        let color = match self.fill_bit(x, y) {
            FillBit::Primary => c0,
            FillBit::Skip => return,
            FillBit::Secondary => c1,
        };
        self.screen[y as usize * SCREEN_W + x as usize] = color & 0x0F;
    }

    /// Clip + screen-bounds test for a screen-space pixel. The clip rect is
    /// stored pre-clamped to the screen (see `clip_x`), so this is four
    /// compares with no clamping.
    #[inline]
    fn in_clip(&self, x: i32, y: i32) -> bool {
        x >= self.clip_x
            && x < self.clip_x + self.clip_w
            && y >= self.clip_y
            && y < self.clip_y + self.clip_h
    }

    /// Store the clip rectangle from its four edges, normalizing to the
    /// pre-clamped invariant: edges clamped to `[0, 128]`, end >= begin.
    pub(super) fn set_clip_edges(&mut self, xb: i32, yb: i32, xe: i32, ye: i32) {
        let xb = xb.clamp(0, SCREEN_W as i32);
        let yb = yb.clamp(0, SCREEN_H as i32);
        let xe = xe.clamp(xb, SCREEN_W as i32);
        let ye = ye.clamp(yb, SCREEN_H as i32);
        self.clip_x = xb;
        self.clip_y = yb;
        self.clip_w = xe - xb;
        self.clip_h = ye - yb;
    }

    /// Classify a pixel against the active fill pattern.
    ///
    /// PICO-8 reads the 4x4 pattern MSB-first: bit 15 = top-left, bit 0 =
    /// bottom-right (`bitPlace = 15 - ((x&3) + 4*(y&3))`).
    #[inline]
    fn fill_bit(&self, x: i32, y: i32) -> FillBit {
        if self.fill_pattern == 0 {
            return FillBit::Primary;
        }
        // x, y are non-negative here, so (x & 3) == x % 4.
        let bit = (15 - ((x & 3) + (y & 3) * 4)) as u16;
        if self.fill_pattern & (1 << bit) == 0 {
            FillBit::Primary
        } else if self.fill_pattern_transparency {
            FillBit::Skip
        } else {
            FillBit::Secondary
        }
    }

    /// Draw a horizontal span `[xa, xb]` (either order) on screen row `y`
    /// with the two-color fill form. Clips once against the clip rect, then
    /// fills the row slice directly when no fill pattern is active.
    ///
    /// This is the single span filler shared by `rect`, `rectfill`,
    /// `circfill` and `ovalfill`.
    fn hline(&mut self, xa: i32, xb: i32, y: i32, c0: u8, c1: u8) {
        if y < self.clip_y || y >= self.clip_y + self.clip_h {
            return;
        }
        let x_min = xa.min(xb).max(self.clip_x);
        let x_max = xa.max(xb).min(self.clip_x + self.clip_w - 1);
        if x_min > x_max {
            return;
        }
        let row = y as usize * SCREEN_W;
        if self.fill_pattern == 0 {
            self.screen[row + x_min as usize..=row + x_max as usize].fill(c0 & 0x0F);
            return;
        }
        // Patterned: the pattern row is fixed for this y, so resolve the four
        // column bits once and cycle through them.
        let row_bits = (self.fill_pattern >> (12 - (y & 3) * 4)) & 0xF;
        for x in x_min..=x_max {
            // bit 3 of the nibble = column 0 (MSB-first).
            let set = row_bits & (8 >> (x & 3)) != 0;
            let color = if !set {
                c0
            } else if self.fill_pattern_transparency {
                continue;
            } else {
                c1
            };
            self.screen[row + x as usize] = color & 0x0F;
        }
    }

    /// Set a pixel at world coordinates (applies camera offset, draw palette,
    /// and clipping).
    pub fn pset(&mut self, x: i32, y: i32, col: Option<u8>) {
        let c = self.resolve_color(col);
        let sx = clamp_coord(x).saturating_sub(self.camera_x);
        let sy = clamp_coord(y).saturating_sub(self.camera_y);
        self.raw_pset(sx, sy, c);
    }

    /// Read the color index at the given world coordinates (applies camera).
    pub fn pget(&self, x: i32, y: i32) -> u8 {
        let sx = clamp_coord(x).saturating_sub(self.camera_x);
        let sy = clamp_coord(y).saturating_sub(self.camera_y);
        if sx < 0 || sy < 0 || sx >= SCREEN_W as i32 || sy >= SCREEN_H as i32 {
            return 0;
        }
        self.screen[sy as usize * SCREEN_W + sx as usize]
    }

    /// Clear the entire screen to `col` (default 0 / black).
    ///
    /// Like PICO-8, this also homes the text cursor to (0,0) and resets the
    /// clip rectangle to the full screen (`cls` ignores the current clip).
    pub fn cls(&mut self, col: Option<u8>) {
        self.screen.fill(col.unwrap_or(0) & 0x0F);
        self.cursor_x = 0;
        self.cursor_y = 0;
        self.set_clip_edges(0, 0, SCREEN_W as i32, SCREEN_H as i32);
    }

    /// Draw a line using Bresenham's algorithm.
    pub fn line(&mut self, x0: i32, y0: i32, x1: i32, y1: i32, col: Option<u8>) {
        let c = self.resolve_color(col);
        // Clamp endpoints to the representable coordinate range so the Bresenham
        // deltas and error term can't overflow and the loop is bounded (a real
        // cart never exceeds PICO-8's 16.16 coordinate range).
        let x0 = clamp_coord(x0);
        let y0 = clamp_coord(y0);
        let x1 = clamp_coord(x1);
        let y1 = clamp_coord(y1);
        let mut cx = x0;
        let mut cy = y0;
        let dx = (x1 - x0).abs();
        let dy = -(y1 - y0).abs();
        let sx: i32 = if x0 < x1 { 1 } else { -1 };
        let sy: i32 = if y0 < y1 { 1 } else { -1 };
        let mut err = dx + dy;
        loop {
            // plot
            let px = cx.saturating_sub(self.camera_x);
            let py = cy.saturating_sub(self.camera_y);
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
        // Record the endpoint for the stateful `line(x,y)` polyline form.
        self.line_end = Some((x1, y1));
    }

    /// Continue a polyline: draw from the last `line()` endpoint to `(x1, y1)`
    /// (PICO-8's `line(x1, y1)` 2-arg form). If there is no valid prior endpoint
    /// (the line state was reset or never set), PICO-8 draws nothing — only the
    /// pen color (if given) is updated, matching reference `line(x1,y1[,col])`.
    pub fn line_to(&mut self, x1: i32, y1: i32, col: Option<u8>) {
        match self.line_end {
            Some((x0, y0)) => self.line(x0, y0, x1, y1, col),
            None => {
                if let Some(byte) = col {
                    self.draw_color = byte;
                }
            }
        }
    }

    /// Reset the polyline state (PICO-8's 0-arg `line()`).
    pub fn line_reset(&mut self) {
        self.line_end = None;
    }

    /// Draw a rectangle outline.
    pub fn rect(&mut self, x0: i32, y0: i32, x1: i32, y1: i32, col: Option<u8>) {
        // PICO-8 draws rect edges as fill-pattern h/v lines (two-color form).
        let (c0, c1) = self.resolve_fill_colors(col);
        let lx = clamp_coord(x0.min(x1)) - self.camera_x;
        let rx = clamp_coord(x0.max(x1)) - self.camera_x;
        let ty = clamp_coord(y0.min(y1)) - self.camera_y;
        let by = clamp_coord(y0.max(y1)) - self.camera_y;

        // Horizontal edges: `hline` clips each to the clip rect.
        self.hline(lx, rx, ty, c0, c1);
        if by != ty {
            self.hline(lx, rx, by, c0, c1);
        }
        // Vertical edges: clip the y span once, then plot only visible rows.
        let y_min = (ty + 1).max(self.clip_y);
        let y_max = (by - 1).min(self.clip_y + self.clip_h - 1);
        for y in y_min..=y_max {
            self.pset_fill(lx, y, c0, c1);
            if rx != lx {
                self.pset_fill(rx, y, c0, c1);
            }
        }
    }

    /// Draw a filled rectangle.
    pub fn rectfill(&mut self, x0: i32, y0: i32, x1: i32, y1: i32, col: Option<u8>) {
        let (c0, c1) = self.resolve_fill_colors(col);
        let lx = clamp_coord(x0.min(x1)) - self.camera_x;
        let rx = clamp_coord(x0.max(x1)) - self.camera_x;
        let ty = clamp_coord(y0.min(y1)) - self.camera_y;
        let by = clamp_coord(y0.max(y1)) - self.camera_y;

        // Clip the row range once; `hline` clips each span horizontally.
        let y_min = ty.max(self.clip_y);
        let y_max = by.min(self.clip_y + self.clip_h - 1);
        for y in y_min..=y_max {
            self.hline(lx, rx, y, c0, c1);
        }
    }

    /// Draw a circle outline using the midpoint circle algorithm.
    pub fn circ(&mut self, cx: i32, cy: i32, r: i32, col: Option<u8>) {
        if r < 0 {
            return;
        }
        // Clamp center + radius so the midpoint-algorithm arithmetic and the
        // `cx +/- x - camera` plot coordinates stay within i32 (a radius beyond
        // the screen can't render distinguishably anyway).
        let cx = clamp_coord(cx);
        let cy = clamp_coord(cy);
        let r = r.min(0x8000);
        let c = self.resolve_color(col);
        // Midpoint circle algorithm matching PICO-8 (z8lua/fake-08
        // `Graphics::circ`) exactly: decision var `d = 1 - x`, branch on
        // `d < 0` (NOT `<= 0`). The `< 0` vs `<= 0` distinction changes which
        // pixels are plotted at certain radii — getting it wrong bulges the
        // outline by a pixel (e.g. r=1 would be an 8px blob, not a 4px diamond).
        let mut x = r;
        let mut y: i32 = 0;
        let mut d = 1 - x;

        while y <= x {
            self.raw_pset(cx + x - self.camera_x, cy + y - self.camera_y, c);
            self.raw_pset(cx + y - self.camera_x, cy + x - self.camera_y, c);
            self.raw_pset(cx - y - self.camera_x, cy + x - self.camera_y, c);
            self.raw_pset(cx - x - self.camera_x, cy + y - self.camera_y, c);
            self.raw_pset(cx - x - self.camera_x, cy - y - self.camera_y, c);
            self.raw_pset(cx - y - self.camera_x, cy - x - self.camera_y, c);
            self.raw_pset(cx + y - self.camera_x, cy - x - self.camera_y, c);
            self.raw_pset(cx + x - self.camera_x, cy - y - self.camera_y, c);
            y += 1;
            if d < 0 {
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
        // See `circ`: clamp center + radius to keep the arithmetic in range.
        let cx = clamp_coord(cx);
        let cy = clamp_coord(cy);
        let r = r.min(0x8000);
        let (c0, c1) = self.resolve_fill_colors(col);

        // Spans go through the shared `hline`, which orders the endpoints (the
        // algorithm passes `ox-x`/`ox+x` with `x` negative, so left>right) and
        // clips once so a huge radius can't iterate off-screen pixels.

        // Match PICO-8's exact filled-circle algorithm (z8lua/fake-08
        // `Graphics::circfill`): special-case r==0 / r==1, then an `err`-based
        // span fill (`x=-r, y=0, err=2-2r`). This differs pixel-for-pixel from
        // a midpoint+hline approach at many radii, so use the reference's form.
        let ox = cx - self.camera_x;
        let oy = cy - self.camera_y;
        if r == 0 {
            self.pset_fill(ox, oy, c0, c1);
            return;
        }
        if r == 1 {
            self.pset_fill(ox, oy - 1, c0, c1);
            self.hline(ox - 1, ox + 1, oy, c0, c1);
            self.pset_fill(ox, oy + 1, c0, c1);
            return;
        }
        let mut x = -r;
        let mut y = 0;
        let mut err = 2 - 2 * r;
        loop {
            self.hline(ox - x, ox + x, oy + y, c0, c1);
            self.hline(ox - x, ox + x, oy - y, c0, c1);
            let rr = err;
            if rr > x {
                x += 1;
                err += x * 2 + 1;
            }
            if rr <= y {
                y += 1;
                err += y * 2 + 1;
            }
            if x >= 0 {
                break;
            }
        }
    }

    /// Draw an ellipse outline within the bounding box `(x0,y0)` to `(x1,y1)`.
    ///
    /// Draws the outline using PICO-8's exact two-phase Bresenham ellipse
    /// (z8lua/fake-08 `Graphics::oval`): integer radii `xr=(x1-x0)/2`,
    /// `yr=(y1-y0)/2` about center `(x0+xr, y0+yr)`, traced as two arcs. This
    /// matches PICO-8 pixel-for-pixel (our old scanline-equation version
    /// over-filled the outline by several pixels at every size).
    pub fn oval(&mut self, x0: i32, y0: i32, x1: i32, y1: i32, col: Option<u8>) {
        let c = self.resolve_color(col);
        // Apply camera, then clamp to the representable range so the squared
        // terms (`xr*xr` etc.) stay within i64 and loops are bounded.
        let x0 = clamp_coord(x0).saturating_sub(self.camera_x);
        let y0 = clamp_coord(y0).saturating_sub(self.camera_y);
        let x1 = clamp_coord(x1).saturating_sub(self.camera_x);
        let y1 = clamp_coord(y1).saturating_sub(self.camera_y);
        self.oval_trace(x0, y0, x1, y1, |s, xc, yc, wx, wy| {
            // The four symmetric outline points; collapse duplicates when an
            // offset is zero so each pixel is plotted once.
            s.raw_pset(xc + wx, yc - wy, c);
            if wx != 0 {
                s.raw_pset(xc - wx, yc - wy, c);
            }
            if wy != 0 {
                s.raw_pset(xc + wx, yc + wy, c);
                if wx != 0 {
                    s.raw_pset(xc - wx, yc + wy, c);
                }
            }
        });
    }

    /// Shared ellipse tracer used by `oval` (outline pixels) and `ovalfill`
    /// (spans between the boundary x's per row). PICO-8's two-phase Bresenham
    /// (z8lua/fake-08 `Graphics::oval`): integer radii `xr=(x1-x0)/2`,
    /// `yr=(y1-y0)/2` about center `(x0+xr, y0+yr)`, traced as two arcs.
    ///
    /// `emit(self, xc, yc, wx, wy)` is called once per boundary step meaning
    /// "the points `(xc +/- wx, yc +/- wy)` lie on the outline". Degenerate
    /// boxes (a point or a 1-D line) are emitted as zero-offset points along
    /// the whole extent so both consumers reproduce them exactly.
    /// Coordinates are already camera-adjusted and clamped by the caller.
    fn oval_trace<F: FnMut(&mut Self, i32, i32, i32, i32)>(
        &mut self,
        x0: i32,
        y0: i32,
        x1: i32,
        y1: i32,
        mut emit: F,
    ) {
        let lx = x0.min(x1);
        let rx = x0.max(x1);
        let ty = y0.min(y1);
        let by = y0.max(y1);

        let xr = (rx - lx) / 2;
        let yr = (by - ty) / 2;
        let xc = lx + xr;
        let yc = ty + yr;

        // Degenerate boxes: a point or a 1-D line.
        if xr == 0 && yr == 0 {
            emit(self, xc, yc, 0, 0);
            return;
        }
        if xr == 0 {
            for y in ty..=by {
                emit(self, xc, y, 0, 0);
            }
            return;
        }
        if yr == 0 {
            for x in lx..=rx {
                emit(self, x, yc, 0, 0);
            }
            return;
        }

        // i64 for the squared terms (xr,yr <= ~32768 -> products ~9e12).
        let asq = (xr as i64) * (xr as i64);
        let bsq = (yr as i64) * (yr as i64);

        // Arc 1: top/bottom region (steep), stepping x outward.
        emit(self, xc, yc, 0, yr);
        let mut wx = 0i64;
        let mut wy = yr as i64;
        let mut xa = 0i64;
        let mut ya = asq * 2 * yr as i64;
        let mut thresh = asq / 4 - asq * yr as i64;
        loop {
            thresh += xa + bsq;
            if thresh >= 0 {
                ya -= asq * 2;
                thresh -= ya;
                wy -= 1;
            }
            xa += bsq * 2;
            wx += 1;
            if xa >= ya {
                break;
            }
            emit(self, xc, yc, wx as i32, wy as i32);
        }

        // Arc 2: left/right region (shallow), stepping y outward.
        emit(self, xc, yc, xr, 0);
        let mut wx = xr as i64;
        let mut wy = 0i64;
        let mut xa = bsq * 2 * xr as i64;
        let mut ya = 0i64;
        let mut thresh = bsq / 4 - bsq * xr as i64;
        loop {
            thresh += ya + asq;
            if thresh >= 0 {
                xa -= bsq * 2;
                thresh -= xa;
                wx -= 1;
            }
            ya += asq * 2;
            wy += 1;
            if ya > xa {
                break;
            }
            if ya == 0 && xa == 0 {
                break;
            }
            emit(self, xc, yc, wx as i32, wy as i32);
        }
    }

    /// Draw a filled ellipse within the bounding box `(x0,y0)` to `(x1,y1)`.
    ///
    /// Reuses the `oval_trace` Bresenham tracer, filling the horizontal span
    /// between the two boundary x's on every emitted row (z8lua/fake-08
    /// `Graphics::ovalfill`). Respects camera, clipping, draw palette and
    /// the two-color fill pattern.
    pub fn ovalfill(&mut self, x0: i32, y0: i32, x1: i32, y1: i32, col: Option<u8>) {
        let (c0, c1) = self.resolve_fill_colors(col);
        let x0 = clamp_coord(x0).saturating_sub(self.camera_x);
        let y0 = clamp_coord(y0).saturating_sub(self.camera_y);
        let x1 = clamp_coord(x1).saturating_sub(self.camera_x);
        let y1 = clamp_coord(y1).saturating_sub(self.camera_y);

        // The reference also draws the full center column (the steep arc's
        // initial vertical extent) before filling the arc rows.
        let xr = (x0.max(x1) - x0.min(x1)) / 2;
        let yr = (y0.max(y1) - y0.min(y1)) / 2;
        if xr != 0 && yr != 0 {
            let xc = x0.min(x1) + xr;
            let yc = y0.min(y1) + yr;
            let y_min = (yc - yr).max(self.clip_y);
            let y_max = (yc + yr).min(self.clip_y + self.clip_h - 1);
            for y in y_min..=y_max {
                self.pset_fill(xc, y, c0, c1);
            }
        }

        self.oval_trace(x0, y0, x1, y1, |s, xc, yc, wx, wy| {
            s.hline(xc - wx, xc + wx, yc - wy, c0, c1);
            if wy != 0 {
                s.hline(xc - wx, xc + wx, yc + wy, c0, c1);
            }
        });
    }
}

/// How the active fill pattern treats one pixel.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum FillBit {
    /// Pattern bit clear: draw the primary color.
    Primary,
    /// Pattern bit set with transparency on: leave the pixel alone.
    Skip,
    /// Pattern bit set, opaque: draw the secondary color.
    Secondary,
}

// ---------------------------------------------------------------------------
// Sprite functions
// ---------------------------------------------------------------------------

impl Console {
    /// Get a pixel color from the sprite sheet.
    #[inline]
    pub fn sget(&self, x: i32, y: i32) -> u8 {
        if x < 0 || y < 0 || x >= SHEET_W as i32 || y >= SHEET_H as i32 {
            return 0;
        }
        self.sprites[y as usize * SHEET_W + x as usize]
    }

    /// Set a pixel color on the sprite sheet. Pixels in sheet rows 64-127
    /// also update the aliased map cell (rows 32-63).
    pub fn sset(&mut self, x: i32, y: i32, col: u8) {
        if x < 0 || y < 0 || x >= SHEET_W as i32 || y >= SHEET_H as i32 {
            return;
        }
        let idx = y as usize * SHEET_W + x as usize;
        self.sprites[idx] = col & 0x0F;
        if idx >= 2 * MEM_SHARED_START as usize {
            self.sync_map_byte_from_sprites(idx / 2);
        }
    }

    /// Draw sprite `n` at world position `(x, y)`.
    ///
    /// `w` and `h` are in *sprite units* (each unit = 8 px).
    /// `flip_x` / `flip_y` mirror the sprite.
    #[allow(clippy::too_many_arguments)]
    pub fn spr(&mut self, n: i32, x: i32, y: i32, w: f64, h: f64, flip_x: bool, flip_y: bool) {
        if !(0..=255).contains(&n) {
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
    #[allow(clippy::too_many_arguments)]
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
        // Source extents beyond the 16.16 integer range can't select any
        // extra sheet pixels (everything past the sheet reads 0); clamping
        // keeps `(sw << 16)` inside u32 for the fixed-point stepping below.
        let sw = sw.min(0x7FFF);
        let sh = sh.min(0x7FFF);

        // Pre-compute the camera-adjusted destination origin once (saturating
        // so extreme dx/dy/camera can't overflow i32).
        let dx_cam = dx.saturating_sub(self.camera_x);
        let dy_cam = dy.saturating_sub(self.camera_y);

        // Only iterate destination pixels inside the clip rect. `dx_cam + px`
        // visible requires px in [clip_x - dx_cam, clip_x + clip_w - dx_cam);
        // intersect with [0, dw). This bounds the loop to the clip size
        // regardless of dest size and removes the per-pixel clip test.
        let py_lo = self.clip_y.saturating_sub(dy_cam).max(0);
        let py_hi = dh.min((self.clip_y + self.clip_h).saturating_sub(dy_cam));
        let px_lo = self.clip_x.saturating_sub(dx_cam).max(0);
        let px_hi = dw.min((self.clip_x + self.clip_w).saturating_sub(dx_cam));
        if py_lo >= py_hi || px_lo >= px_hi {
            return;
        }

        // PICO-8 steps the source in 16.16 fixed point: the per-destination-
        // pixel increment is `(sw << 16) / dw`, truncated ONCE, and the source
        // index is `(i * step) >> 16`. This differs from a per-pixel
        // `i * sw / dw` integer division at non-integer scales (e.g. 8->12),
        // selecting different source columns/rows — match the reference.
        // `i * step < sw << 16 <= 2^31` for all i < dw, so a u32 accumulator
        // that adds `step` per pixel replaces the per-pixel i64 multiply.
        let step_x = ((sw as u32) << 16) / dw as u32;
        let step_y = ((sh as u32) << 16) / dh as u32;

        // Hoist the lookup tables out of the loop (16 bytes each).
        let transparent = self.transparent;
        let pal = self.draw_pal;

        // Sprites ignore the fill pattern (PICO-8 applies `fillp` only to the
        // shape primitives), so this writes straight to the screen buffer.
        let mut acc_y = py_lo as u32 * step_y;
        for py in py_lo..py_hi {
            let mut src_py = (acc_y >> 16) as i32;
            acc_y += step_y;
            if flip_y {
                src_py = sh - 1 - src_py;
            }
            let sheet_y = sy.saturating_add(src_py);
            let row_ok = (0..SHEET_H as i32).contains(&sheet_y);
            let sheet_row = if row_ok {
                sheet_y as usize * SHEET_W
            } else {
                0
            };
            let screen_row = (dy_cam + py) as usize * SCREEN_W;

            let mut acc_x = px_lo as u32 * step_x;
            for px in px_lo..px_hi {
                let mut src_px = (acc_x >> 16) as i32;
                acc_x += step_x;
                if flip_x {
                    src_px = sw - 1 - src_px;
                }
                let sheet_x = sx.saturating_add(src_px);
                // Off-sheet source pixels read as color 0 (like `sget`).
                let raw_col = if row_ok && (0..SHEET_W as i32).contains(&sheet_x) {
                    (self.sprites[sheet_row + sheet_x as usize] & 0x0F) as usize
                } else {
                    0
                };
                // Transparency check (on the raw, un-remapped colour).
                if transparent[raw_col] {
                    continue;
                }
                self.screen[screen_row + (dx_cam + px) as usize] = pal[raw_col] & 0x0F;
            }
        }
    }

    /// Blit one unscaled 8x8 sprite `tile` with its top-left at screen-space
    /// `(x0, y0)` (camera already applied). The x/y spans are clipped once
    /// against the clip rect, then each row is a linear walk over the sheet.
    /// Pixel semantics match `sspr` at 1:1 scale: transparency is tested on
    /// the raw color, the draw palette is applied, no fill pattern.
    fn blit_tile(&mut self, tile: u8, x0: i32, y0: i32) {
        let sx0 = x0.max(self.clip_x);
        let sx1 = (x0 + SPRITE_W as i32).min(self.clip_x + self.clip_w);
        let sy0 = y0.max(self.clip_y);
        let sy1 = (y0 + SPRITE_H as i32).min(self.clip_y + self.clip_h);
        if sx0 >= sx1 || sy0 >= sy1 {
            return;
        }
        let sheet_x = (tile as usize % SPRITES_PER_ROW) * SPRITE_W + (sx0 - x0) as usize;
        let sheet_y = (tile as usize / SPRITES_PER_ROW) * SPRITE_H + (sy0 - y0) as usize;
        let width = (sx1 - sx0) as usize;
        let transparent = self.transparent;
        let pal = self.draw_pal;
        for (row, y) in (sy0..sy1).enumerate() {
            let src = (sheet_y + row) * SHEET_W + sheet_x;
            let dst = y as usize * SCREEN_W + sx0 as usize;
            for i in 0..width {
                let raw = (self.sprites[src + i] & 0x0F) as usize;
                if !transparent[raw] {
                    self.screen[dst + i] = pal[raw] & 0x0F;
                }
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

    /// Set the tile index at map position `(x, y)`. Cells in rows 32-63 also
    /// update the aliased sprite-sheet pixels (sheet rows 64-127).
    pub fn mset(&mut self, x: i32, y: i32, v: u8) {
        if x < 0 || y < 0 || x >= MAP_W as i32 || y >= MAP_H as i32 {
            return;
        }
        let idx = y as usize * MAP_W + x as usize;
        self.map[idx] = v;
        if idx >= MEM_SHARED_START as usize {
            self.sync_sprites_from_map_byte(idx);
        }
    }

    /// Draw a region of the map to the screen.
    ///
    /// `cel_x, cel_y` — top-left cell of the map region.
    /// `sx, sy` — screen-pixel position to draw at (world coords).
    /// `cel_w, cel_h` — size of the region in cells.
    /// `layer` — if non-zero, only draw tiles whose sprite flags bitwise-AND
    ///            with `layer` is non-zero.
    #[allow(clippy::too_many_arguments)]
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
        if cel_w <= 0 || cel_h <= 0 {
            return;
        }
        // Screen-space origin of cell (0,0) of the region.
        let ox = sx.saturating_sub(self.camera_x) as i64;
        let oy = sy.saturating_sub(self.camera_y) as i64;

        // Cull to the cells that intersect the clip rect: cell `c` covers
        // screen `[o + 8c, o + 8c + 8)`, so the first visible cell is
        // `floor((clip_begin - o) / 8)` and the end (exclusive) is
        // `ceil((clip_end - o) / 8)`. Also drop cells outside the 128x64 map
        // (`mget` would read 0 there). i64 keeps the extreme-offset math safe.
        let cell_range = |o: i64, clip_b: i64, clip_e: i64, count: i64, cel0: i64, dim: i64| {
            let lo = (clip_b - o).div_euclid(8).max(0).max(-cel0);
            let hi = (clip_e - o + 7).div_euclid(8).min(count).min(dim - cel0);
            (lo, hi)
        };
        let (cx_lo, cx_hi) = cell_range(
            ox,
            self.clip_x as i64,
            (self.clip_x + self.clip_w) as i64,
            cel_w as i64,
            cel_x as i64,
            MAP_W as i64,
        );
        let (cy_lo, cy_hi) = cell_range(
            oy,
            self.clip_y as i64,
            (self.clip_y + self.clip_h) as i64,
            cel_h as i64,
            cel_y as i64,
            MAP_H as i64,
        );
        if cx_lo >= cx_hi || cy_lo >= cy_hi {
            return;
        }

        for cy in cy_lo..cy_hi {
            let map_row = (cel_y as i64 + cy) as usize * MAP_W;
            let draw_y = (oy + cy * SPRITE_H as i64) as i32;
            for cx in cx_lo..cx_hi {
                let tile = self.map[map_row + (cel_x as i64 + cx) as usize];
                // Tile 0 is the "empty" cell and is never drawn (matches PICO-8:
                // `map()` skips cells whose sprite index is 0).
                if tile == 0 {
                    continue;
                }
                // Layer filtering.
                if layer != 0 && self.flags[tile as usize] & layer == 0 {
                    continue;
                }
                let draw_x = (ox + cx * SPRITE_W as i64) as i32;
                self.blit_tile(tile, draw_x, draw_y);
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
    #[allow(clippy::too_many_arguments)]
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
        // Clamp endpoints to the representable range so deltas / the step loop
        // can't overflow or run unbounded on extreme inputs. Work in screen
        // space (camera applied) so the clip test is direct.
        let x0 = clamp_coord(x0).saturating_sub(self.camera_x);
        let y0 = clamp_coord(y0).saturating_sub(self.camera_y);
        let x1 = clamp_coord(x1).saturating_sub(self.camera_x);
        let y1 = clamp_coord(y1).saturating_sub(self.camera_y);

        // Texture coordinates and their per-pixel step as PICO-8 16.16
        // fixed-point bit patterns (wrapping adds, like the reference).
        let mut tx = crate::fixed::to_bits(mx);
        let mut ty = crate::fixed::to_bits(my);
        let tdx = crate::fixed::to_bits(mdx);
        let tdy = crate::fixed::to_bits(mdy);
        // Map wrap masks from the 0x5F38/0x5F39 registers: 0 = whole 256-tile
        // range (`0x00FF_FFFF`), else `(w << 16) - 1`.
        let xmask = tline_mask(self.tline_map_w);
        let ymask = tline_mask(self.tline_map_h);
        let xoff = self.tline_map_x as i32;
        let yoff = self.tline_map_y as i32;

        // Walk one pixel per step along the major axis; the minor axis is a
        // 16.16 accumulator (`minor0 + i * (dminor << 16) / steps`, floored).
        let dx = x1 - x0;
        let dy = y1 - y0;
        let steps = dx.abs().max(dy.abs());
        let major_is_x = dx.abs() >= dy.abs();
        let (major_step, minor0, dminor) = if major_is_x {
            (dx.signum(), y0, dy)
        } else {
            (dy.signum(), x0, dx)
        };
        // `steps == 0` means a single pixel; `minor_step` is unused then.
        // i64: screen-space coords can reach +/-65535, so `<< 16` needs 33 bits.
        let minor_step: i64 = if steps == 0 {
            0
        } else {
            ((dminor as i64) << 16) / steps as i64
        };
        let mut major = if major_is_x { x0 } else { y0 };
        let mut minor_acc: i64 = (minor0 as i64) << 16;

        let transparent = self.transparent;
        let pal = self.draw_pal;

        for _ in 0..=steps {
            let minor = (minor_acc >> 16) as i32;
            let (px, py) = if major_is_x {
                (major, minor)
            } else {
                (minor, major)
            };
            if self.in_clip(px, py) {
                // Integer part (masked) indexes the map, the fraction's top 3
                // bits pick the pixel within the 8x8 tile (`(m << 3) & 7`).
                let cell_x = xoff.wrapping_add(((tx & xmask) >> 16) as i32);
                let cell_y = yoff.wrapping_add(((ty & ymask) >> 16) as i32);
                let tile = self.mget(cell_x, cell_y) as usize;
                if tile != 0 {
                    let sheet_x = (tile % SPRITES_PER_ROW) * SPRITE_W + ((tx >> 13) & 7) as usize;
                    let sheet_y = (tile / SPRITES_PER_ROW) * SPRITE_H + ((ty >> 13) & 7) as usize;
                    let raw = (self.sprites[sheet_y * SHEET_W + sheet_x] & 0x0F) as usize;
                    if !transparent[raw] {
                        self.screen[py as usize * SCREEN_W + px as usize] = pal[raw] & 0x0F;
                    }
                }
            }
            // Advance texture (wrapping within the masked part) and position.
            tx = (tx & !xmask) | (tx.wrapping_add(tdx) & xmask);
            ty = (ty & !ymask) | (ty.wrapping_add(tdy) & ymask);
            major += major_step;
            minor_acc = minor_acc.wrapping_add(minor_step);
        }
    }
}

/// `tline` wrap mask for a map-dimension register: 0 selects the whole
/// 256-tile range (`0x00FF_FFFF`), otherwise `(dim << 16) - 1` so the integer
/// part wraps at `dim` tiles while the fraction is preserved.
#[inline]
fn tline_mask(dim: u8) -> u32 {
    if dim == 0 {
        0x00FF_FFFF
    } else {
        ((dim as u32) << 16) - 1
    }
}
