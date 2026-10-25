//! Memory-mapped I/O: `peek`/`poke`/`memcpy`/`memset`, the shared
//! upper-sheet / lower-map aliasing, cartdata and the cart ROM snapshot.

use super::*;

// ---------------------------------------------------------------------------
// Shared sheet/map region
// ---------------------------------------------------------------------------

impl Console {
    /// Re-pack sprite pixel pair `[2*addr, 2*addr+1]` into the aliased map
    /// byte (`addr` is a memory address in `0x1000..0x2000`, which is also
    /// the index into `map` for rows 32-63).
    #[inline]
    pub(super) fn sync_map_byte_from_sprites(&mut self, addr: usize) {
        let px = addr * 2;
        self.map[addr] = (self.sprites[px] & 0x0F) | (self.sprites[px + 1] << 4);
    }

    /// Unpack the aliased map byte at `addr` (`0x1000..0x2000`) into its two
    /// sprite pixels.
    #[inline]
    pub(super) fn sync_sprites_from_map_byte(&mut self, addr: usize) {
        let v = self.map[addr];
        let px = addr * 2;
        self.sprites[px] = v & 0x0F;
        self.sprites[px + 1] = v >> 4;
    }

    /// Re-pack every aliased map byte in the address range `lo..hi`
    /// (clamped to the shared region `0x1000..0x2000`) from the sprite sheet.
    pub(super) fn sync_map_range_from_sprites(&mut self, lo: usize, hi: usize) {
        let lo = lo.max(MEM_SHARED_START as usize);
        let hi = hi.min(MEM_SPRITES_END as usize);
        for addr in lo..hi {
            self.sync_map_byte_from_sprites(addr);
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
        if !(0..=63).contains(&index) {
            return 0.0;
        }
        self.persistent_data[index as usize]
    }

    /// Set a persistent data value. Index must be 0-63.
    /// Out-of-bounds indices are silently ignored.
    pub fn dset(&mut self, index: i32, value: f64) {
        if !(0..=63).contains(&index) {
            return;
        }
        self.persistent_data[index as usize] = value;
    }
}

// ---------------------------------------------------------------------------
// Memory-mapped I/O (PICO-8 memory map)
// ---------------------------------------------------------------------------

//
// PICO-8 memory map (32 KB), matching the reference (fake-08 `PicoRam`):
//   0x0000-0x0FFF  Sprite sheet rows 0-63    (2 pixels per byte packed)
//   0x1000-0x1FFF  Sprite sheet rows 64-127  -- SHARED with map rows 32-63
//   0x2000-0x2FFF  Map rows 0-31             (1 byte per cell)
//   0x3000-0x30FF  Sprite flags              (256 bytes)
//   0x3100-0x42FF  Music + SFX               (owned by the audio module; 0 here)
//   0x4300-0x5DFF  General purpose           (6912 bytes, `user_data`)
//   0x5E00-0x5EFF  Persistent cartdata       (64 x 16.16 LE, `persistent_data`)
//   0x5F00-0x5F3F  Draw state                (palette, clip, pen, camera, fillp)
//   0x5F40-0x5FFF  Hardware state            (btnp repeat regs; rest unmapped)
//   0x6000-0x7FFF  Screen buffer             (2 pixels per byte packed)
//
// Pixel packing: LOW nibble = left (even) pixel, HIGH nibble = right pixel.

/// A memory region whose bytes map 1:1 (or 1:2 pixels) onto one of the
/// console's linear arrays; used for the `memcpy`/`memset` fast paths.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Linear {
    /// `sprites`, 2 pixels per byte (0x0000-0x1FFF).
    Sprites,
    /// `screen`, 2 pixels per byte (0x6000-0x7FFF).
    Screen,
    /// `map` rows 0-31 (0x2000-0x2FFF).
    MapLo,
    /// `flags` (0x3000-0x30FF).
    Flags,
    /// `user_data` (0x4300-0x5DFF).
    User,
}

/// Classify the byte range `[addr, addr + len)` if it lies entirely inside a
/// single linear region; returns the region and the byte offset within it.
fn linear_range(addr: usize, len: usize) -> Option<(Linear, usize)> {
    let end = addr + len;
    let regions: [(Linear, usize, usize); 5] = [
        (Linear::Sprites, 0, MEM_SPRITES_END as usize),
        (
            Linear::MapLo,
            MEM_MAP_START as usize,
            MEM_FLAGS_START as usize,
        ),
        (
            Linear::Flags,
            MEM_FLAGS_START as usize,
            MEM_FLAGS_START as usize + 256,
        ),
        (
            Linear::User,
            MEM_USER_START as usize,
            MEM_CARTDATA_START as usize,
        ),
        (
            Linear::Screen,
            MEM_SCREEN_START as usize,
            MEM_SCREEN_END as usize,
        ),
    ];
    regions
        .iter()
        .find(|&&(_, lo, hi)| addr >= lo && end <= hi)
        .map(|&(kind, lo, _)| (kind, addr - lo))
}

/// Pack two unpacked pixels into one memory byte (low nibble = left pixel).
#[inline]
fn pack_px(left: u8, right: u8) -> u8 {
    (left & 0x0F) | (right << 4)
}

impl Console {
    /// Read a single byte from the PICO-8 memory map.
    ///
    /// Addresses outside mapped regions return 0.
    pub fn peek(&self, addr: u16) -> u8 {
        match addr {
            // Sprite sheet (both halves): 2 pixels per byte.
            0x0000..=0x1FFF => {
                let px = addr as usize * 2;
                pack_px(self.sprites[px], self.sprites[px + 1])
            }
            // Map rows 0-31.
            0x2000..=0x2FFF => self.map[(addr - MEM_MAP_START) as usize],
            // Sprite flags.
            0x3000..=0x30FF => self.flags[(addr - MEM_FLAGS_START) as usize],
            // General purpose / user data.
            0x4300..=0x5DFF => self.user_data[(addr - MEM_USER_START) as usize],
            // Persistent cartdata: 64 x 16.16 fixed-point, little-endian.
            0x5E00..=0x5EFF => self.peek_cartdata((addr - MEM_CARTDATA_START) as usize),
            // Draw state hardware registers (camera, clip, palette, fillp...).
            0x5F00..=0x5F3F => self.peek_draw_state((addr - MEM_DRAW_STATE_START) as usize),
            // btnp auto-repeat registers.
            0x5F5C => self.btnp_repeat_delay,
            0x5F5D => self.btnp_repeat_interval,
            // Screen buffer: 2 pixels per byte.
            0x6000..=0x7FFF => {
                let px = (addr - MEM_SCREEN_START) as usize * 2;
                pack_px(self.screen[px], self.screen[px + 1])
            }
            // Music/SFX (audio module), rest of the hardware page, and
            // everything above the 32 KB map read as 0.
            _ => 0,
        }
    }

    /// Write a single byte to the PICO-8 memory map.
    ///
    /// Addresses outside mapped regions are silently ignored.
    pub fn poke(&mut self, addr: u16, val: u8) {
        match addr {
            // Sprite sheet; the upper half is aliased with map rows 32-63.
            0x0000..=0x1FFF => {
                let px = addr as usize * 2;
                self.sprites[px] = val & 0x0F;
                self.sprites[px + 1] = val >> 4;
                if addr >= MEM_SHARED_START {
                    self.map[addr as usize] = val;
                }
            }
            0x2000..=0x2FFF => self.map[(addr - MEM_MAP_START) as usize] = val,
            0x3000..=0x30FF => self.flags[(addr - MEM_FLAGS_START) as usize] = val,
            0x4300..=0x5DFF => self.user_data[(addr - MEM_USER_START) as usize] = val,
            0x5E00..=0x5EFF => self.poke_cartdata((addr - MEM_CARTDATA_START) as usize, val),
            0x5F00..=0x5F3F => self.poke_draw_state((addr - MEM_DRAW_STATE_START) as usize, val),
            0x5F5C => self.btnp_repeat_delay = val,
            0x5F5D => self.btnp_repeat_interval = val,
            0x6000..=0x7FFF => {
                let px = (addr - MEM_SCREEN_START) as usize * 2;
                self.screen[px] = val & 0x0F;
                self.screen[px + 1] = val >> 4;
            }
            _ => {}
        }
    }

    /// Read one byte of the persistent cartdata block (`0x5E00`-`0x5EFF`):
    /// slot `off / 4`, byte `off % 4` of its little-endian 16.16 value.
    #[inline]
    fn peek_cartdata(&self, off: usize) -> u8 {
        let bits = crate::fixed::to_bits(self.persistent_data[off / 4]);
        (bits >> (8 * (off % 4))) as u8
    }

    /// Write one byte of the persistent cartdata block (see `peek_cartdata`).
    fn poke_cartdata(&mut self, off: usize, val: u8) {
        let slot = off / 4;
        let shift = 8 * (off % 4);
        let bits = crate::fixed::to_bits(self.persistent_data[slot]);
        let bits = (bits & !(0xFF << shift)) | ((val as u32) << shift);
        self.persistent_data[slot] = crate::fixed::from_bits(bits);
    }

    /// Read `buf.len()` packed memory bytes from a linear region starting at
    /// byte offset `off` (pixel regions are packed 2 px -> 1 byte).
    fn read_linear(&self, kind: Linear, off: usize, buf: &mut [u8]) {
        match kind {
            Linear::Sprites | Linear::Screen => {
                let px = if kind == Linear::Sprites {
                    &self.sprites[off * 2..(off + buf.len()) * 2]
                } else {
                    &self.screen[off * 2..(off + buf.len()) * 2]
                };
                for (b, pair) in buf.iter_mut().zip(px.chunks_exact(2)) {
                    *b = pack_px(pair[0], pair[1]);
                }
            }
            Linear::MapLo => buf.copy_from_slice(&self.map[off..off + buf.len()]),
            Linear::Flags => buf.copy_from_slice(&self.flags[off..off + buf.len()]),
            Linear::User => buf.copy_from_slice(&self.user_data[off..off + buf.len()]),
        }
    }

    /// Write `buf` as packed memory bytes into a linear region at byte
    /// offset `off` (pixel regions are unpacked 1 byte -> 2 px). Keeps the
    /// shared map rows in sync when the upper sprite sheet is written.
    fn write_linear(&mut self, kind: Linear, off: usize, buf: &[u8]) {
        match kind {
            Linear::Sprites | Linear::Screen => {
                let px = if kind == Linear::Sprites {
                    &mut self.sprites[off * 2..(off + buf.len()) * 2]
                } else {
                    &mut self.screen[off * 2..(off + buf.len()) * 2]
                };
                for (&b, pair) in buf.iter().zip(px.chunks_exact_mut(2)) {
                    pair[0] = b & 0x0F;
                    pair[1] = b >> 4;
                }
                if kind == Linear::Sprites {
                    self.sync_map_range_from_sprites(off, off + buf.len());
                }
            }
            Linear::MapLo => self.map[off..off + buf.len()].copy_from_slice(buf),
            Linear::Flags => self.flags[off..off + buf.len()].copy_from_slice(buf),
            Linear::User => self.user_data[off..off + buf.len()].copy_from_slice(buf),
        }
    }

    /// Copy `len` bytes within the PICO-8 memory map (memmove semantics:
    /// overlapping ranges copy as if through a temporary buffer).
    ///
    /// When both ranges lie inside single linear regions the copy is done
    /// with slice operations (`copy_within` for same-region overlap, chunked
    /// pack/unpack across regions); otherwise it falls back to a per-byte
    /// `peek`/`poke` walk in the overlap-safe direction. No heap allocation.
    pub fn memcpy(&mut self, dest: u16, src: u16, len: u16) {
        if len == 0 || dest == src {
            return;
        }
        let (d, s, n) = (dest as usize, src as usize, len as usize);
        match (linear_range(s, n), linear_range(d, n)) {
            (Some((sk, so)), Some((dk, doff))) if sk == dk => {
                // Same array: `copy_within` is a memmove.
                match sk {
                    Linear::Sprites => {
                        self.sprites.copy_within(so * 2..(so + n) * 2, doff * 2);
                        self.sync_map_range_from_sprites(doff, doff + n);
                    }
                    Linear::Screen => self.screen.copy_within(so * 2..(so + n) * 2, doff * 2),
                    Linear::MapLo => self.map.copy_within(so..so + n, doff),
                    Linear::Flags => self.flags.copy_within(so..so + n, doff),
                    Linear::User => self.user_data.copy_within(so..so + n, doff),
                }
            }
            (Some((sk, so)), Some((dk, doff))) => {
                // Different arrays can't overlap: stream through a small
                // stack buffer of packed bytes, forward.
                let mut buf = [0u8; 128];
                let mut i = 0;
                while i < n {
                    let chunk = (n - i).min(buf.len());
                    self.read_linear(sk, so + i, &mut buf[..chunk]);
                    self.write_linear(dk, doff + i, &buf[..chunk]);
                    i += chunk;
                }
            }
            _ => {
                // Generic path (crosses regions or hits the draw-state /
                // cartdata / unmapped pages): byte-wise, copying backwards
                // when the destination starts above the source so an
                // overlapping range is never read after it was overwritten.
                if d > s {
                    for i in (0..len).rev() {
                        let b = self.peek(src.wrapping_add(i));
                        self.poke(dest.wrapping_add(i), b);
                    }
                } else {
                    for i in 0..len {
                        let b = self.peek(src.wrapping_add(i));
                        self.poke(dest.wrapping_add(i), b);
                    }
                }
            }
        }
    }

    /// Fill `len` bytes in the PICO-8 memory map starting at `dest` with `val`.
    pub fn memset(&mut self, dest: u16, val: u8, len: u16) {
        if len == 0 {
            return;
        }
        let (d, n) = (dest as usize, len as usize);
        match linear_range(d, n) {
            Some((Linear::Sprites, off)) | Some((Linear::Screen, off)) => {
                let is_sprites = d < MEM_SPRITES_END as usize;
                let px = if is_sprites {
                    &mut self.sprites[off * 2..(off + n) * 2]
                } else {
                    &mut self.screen[off * 2..(off + n) * 2]
                };
                for pair in px.chunks_exact_mut(2) {
                    pair[0] = val & 0x0F;
                    pair[1] = val >> 4;
                }
                if is_sprites {
                    self.sync_map_range_from_sprites(off, off + n);
                }
            }
            Some((Linear::MapLo, off)) => self.map[off..off + n].fill(val),
            Some((Linear::Flags, off)) => self.flags[off..off + n].fill(val),
            Some((Linear::User, off)) => self.user_data[off..off + n].fill(val),
            None => {
                for i in 0..len {
                    self.poke(dest.wrapping_add(i), val);
                }
            }
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

    /// Snapshot the currently loaded sprite sheet, map and flags into
    /// `cart_rom` so `reload()` / `cstore()` operate on the cart as loaded.
    /// Call once after copying cart data into the console.
    pub fn snapshot_cart_rom(&mut self) {
        for addr in 0..self.cart_rom.len() {
            self.cart_rom[addr] = self.peek(addr as u16);
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

    /// Read from the draw-state register page (`0x5F00`-`0x5F3F`). Offsets
    /// follow the reference `drawState_t` layout exactly.
    fn peek_draw_state(&self, offset: usize) -> u8 {
        match offset {
            // 0x00-0x0F: draw palette; bit 4 is the transparency flag.
            0x00..=0x0F => self.draw_pal[offset] | ((self.transparent[offset] as u8) << 4),
            // 0x10-0x1F: screen palette
            0x10..=0x1F => self.screen_pal[offset - 0x10],
            // 0x20-0x23: clip rect as begin/end edges (x0, y0, x1, y1).
            0x20 => self.clip_x as u8,
            0x21 => self.clip_y as u8,
            0x22 => (self.clip_x + self.clip_w) as u8,
            0x23 => (self.clip_y + self.clip_h) as u8,
            // 0x25: pen color (full byte; high nibble is the fillp alt color).
            0x25 => self.draw_color,
            // 0x26-0x27: text cursor
            0x26 => self.cursor_x as u8,
            0x27 => self.cursor_y as u8,
            // 0x28-0x2B: camera x/y as little-endian i16.
            0x28 => self.camera_x as u8,
            0x29 => (self.camera_x >> 8) as u8,
            0x2A => self.camera_y as u8,
            0x2B => (self.camera_y >> 8) as u8,
            // 0x31-0x32: fill pattern (LE), 0x33: fill transparency bit.
            0x31 => self.fill_pattern as u8,
            0x32 => (self.fill_pattern >> 8) as u8,
            0x33 => self.fill_pattern_transparency as u8,
            // 0x35: "line invalid" (no polyline start point).
            0x35 => self.line_end.is_none() as u8,
            // 0x38-0x3B: tline map wrap registers.
            0x38 => self.tline_map_w,
            0x39 => self.tline_map_h,
            0x3A => self.tline_map_x,
            0x3B => self.tline_map_y,
            // 0x3C-0x3F: last line endpoint (LE i16 x, y).
            0x3C..=0x3F => {
                let (lx, ly) = self.line_end.unwrap_or((0, 0));
                let v = if offset < 0x3E { lx } else { ly };
                (v >> (8 * (offset & 1))) as u8
            }
            _ => 0,
        }
    }

    /// Write to the draw-state register page (`0x5F00`-`0x5F3F`).
    fn poke_draw_state(&mut self, offset: usize, val: u8) {
        match offset {
            // 0x00-0x0F: draw palette (low nibble) + transparency (bit 4).
            0x00..=0x0F => {
                self.draw_pal[offset] = val & 0x0F;
                self.transparent[offset] = val & 0x10 != 0;
            }
            0x10..=0x1F => self.screen_pal[offset - 0x10] = val & 0x0F,
            // 0x20-0x23: clip edges; re-normalize so the stored rect keeps
            // the pre-clamped invariant.
            0x20..=0x23 => {
                let mut e = [
                    self.clip_x,
                    self.clip_y,
                    self.clip_x + self.clip_w,
                    self.clip_y + self.clip_h,
                ];
                e[offset - 0x20] = val as i32;
                self.set_clip_edges(e[0], e[1], e[2], e[3]);
            }
            0x25 => self.draw_color = val,
            0x26 => self.cursor_x = val as i32,
            0x27 => self.cursor_y = val as i32,
            // 0x28-0x2B: camera bytes; the pair is sign-extended from i16.
            0x28..=0x2B => {
                let cam = if offset < 0x2A {
                    &mut self.camera_x
                } else {
                    &mut self.camera_y
                };
                let mut bits = *cam as i16 as u16;
                if offset & 1 == 0 {
                    bits = (bits & 0xFF00) | val as u16;
                } else {
                    bits = (bits & 0x00FF) | ((val as u16) << 8);
                }
                *cam = bits as i16 as i32;
            }
            0x31 => self.fill_pattern = (self.fill_pattern & 0xFF00) | val as u16,
            0x32 => self.fill_pattern = (self.fill_pattern & 0x00FF) | ((val as u16) << 8),
            0x33 => self.fill_pattern_transparency = val & 1 != 0,
            0x35 => {
                if val != 0 {
                    self.line_end = None;
                } else if self.line_end.is_none() {
                    self.line_end = Some((0, 0));
                }
            }
            0x38 => self.tline_map_w = val,
            0x39 => self.tline_map_h = val,
            0x3A => self.tline_map_x = val,
            0x3B => self.tline_map_y = val,
            0x3C..=0x3F => {
                let (mut lx, mut ly) = self.line_end.unwrap_or((0, 0));
                let v = if offset < 0x3E { &mut lx } else { &mut ly };
                let mut bits = *v as i16 as u16;
                if offset & 1 == 0 {
                    bits = (bits & 0xFF00) | val as u16;
                } else {
                    bits = (bits & 0x00FF) | ((val as u16) << 8);
                }
                *v = bits as i16 as i32;
                self.line_end = Some((lx, ly));
            }
            _ => {}
        }
    }
}
