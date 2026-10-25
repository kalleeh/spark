//! Sprite editor tab: zoomed 8x8 pixel editor, sprite-sheet browser,
//! flag toggles and colour palette.

use macroquad::prelude::*;
use spark_core::console::Console;

use super::widgets::{arrow_selector, Mouse, Rect};
use super::MAIN_Y;

// ---------------------------------------------------------------------------
// Layout constants
// ---------------------------------------------------------------------------

const SPR_ZOOM: i32 = 8;
const SPR_ZOOM_X: i32 = 0;
const SPR_ZOOM_Y: i32 = MAIN_Y + 1;
const SPR_ZOOM_SIZE: i32 = 64; // 8 * 8

/// Color palette bar.
const PAL_Y: i32 = 112;
const PAL_SWATCH: i32 = 8;

/// Sprite sheet preview (mini).
const SHEET_X: i32 = 0;
const SHEET_Y: i32 = 76;
const SHEET_ROWS: usize = 4;
const SHEET_COLS: usize = 16;

const NUMBER_KEYS: [KeyCode; 10] = [
    KeyCode::Key0,
    KeyCode::Key1,
    KeyCode::Key2,
    KeyCode::Key3,
    KeyCode::Key4,
    KeyCode::Key5,
    KeyCode::Key6,
    KeyCode::Key7,
    KeyCode::Key8,
    KeyCode::Key9,
];

// ---------------------------------------------------------------------------
// Pure helpers
// ---------------------------------------------------------------------------

/// Compute the pixel origin (top-left corner) on the sprite sheet for a given sprite index.
/// The sprite sheet is 16 sprites wide and 16 sprites tall, each sprite 8x8 pixels.
pub(super) fn sprite_origin(sprite_idx: u8) -> (i32, i32) {
    let col = (sprite_idx as i32) % 16;
    let row = (sprite_idx as i32) / 16;
    (col * 8, row * 8)
}

/// Convert mouse coordinates in the zoomed sprite view to 8x8 pixel coordinates.
/// Returns `None` if the mouse is outside the zoomed view area.
fn zoom_to_sprite_pixel(mouse_x: i32, mouse_y: i32) -> Option<(i32, i32)> {
    if !(SPR_ZOOM_X..SPR_ZOOM_X + SPR_ZOOM_SIZE).contains(&mouse_x)
        || !(SPR_ZOOM_Y..SPR_ZOOM_Y + SPR_ZOOM_SIZE).contains(&mouse_y)
    {
        return None;
    }
    let px = (mouse_x - SPR_ZOOM_X) / SPR_ZOOM;
    let py = (mouse_y - SPR_ZOOM_Y) / SPR_ZOOM;
    if (0..8).contains(&px) && (0..8).contains(&py) {
        Some((px, py))
    } else {
        None
    }
}

/// Convert a click on the sprite sheet mini-view to a sprite index.
/// `page` is the first row of sprites visible in the sheet view.
/// Returns `None` if the click is outside the sheet area or the resulting index >= 256.
fn sheet_click_to_sprite(mouse_x: i32, mouse_y: i32, page: usize) -> Option<u8> {
    if mouse_x < SHEET_X
        || mouse_x >= SHEET_X + (SHEET_COLS * 8) as i32
        || mouse_y < SHEET_Y
        || mouse_y >= SHEET_Y + (SHEET_ROWS * 8) as i32
    {
        return None;
    }
    let col = ((mouse_x - SHEET_X) / 8) as usize;
    let row = ((mouse_y - SHEET_Y) / 8) as usize;
    let idx = (page + row) * 16 + col;
    if idx < 256 {
        Some(idx as u8)
    } else {
        None
    }
}

/// Convert a click on the color palette bar to a color index (0-15).
/// Returns `None` if the click is outside the palette area.
fn palette_click_to_color(mouse_x: i32, mouse_y: i32) -> Option<u8> {
    if !(PAL_Y..PAL_Y + PAL_SWATCH).contains(&mouse_y) || !(0..128).contains(&mouse_x) {
        return None;
    }
    let c = (mouse_x / PAL_SWATCH) as u8;
    if c < 16 {
        Some(c)
    } else {
        None
    }
}

/// Toggle a specific display bit in sprite flags.
/// `display_bit` is 0-7 where 0 is the leftmost (most significant) bit.
/// Returns the new flags value. If `display_bit` >= 8, flags are returned unchanged.
fn toggle_flag_bit(flags: u8, display_bit: u8) -> u8 {
    if display_bit >= 8 {
        return flags;
    }
    flags ^ (1 << (7 - display_bit))
}

/// Draw sprite `sprite_idx` at 1:1 scale with its top-left at `(dx, dy)`,
/// ignoring transparency (the editor shows every colour, including 0).
pub(super) fn blit_sprite(console: &mut Console, sprite_idx: u8, dx: i32, dy: i32) {
    let (sx, sy) = sprite_origin(sprite_idx);
    for py in 0..8_i32 {
        for px in 0..8_i32 {
            let c = console.sget(sx + px, sy + py);
            console.raw_pset(dx + px, dy + py, c);
        }
    }
}

// ---------------------------------------------------------------------------
// Sprite editor
// ---------------------------------------------------------------------------

pub struct SpriteEditor {
    pub current_sprite: u8,
    pub sprite_color: u8,
}

impl Default for SpriteEditor {
    fn default() -> Self {
        Self {
            current_sprite: 1,
            sprite_color: 7,
        }
    }
}

impl SpriteEditor {
    pub fn update(&mut self, console: &mut Console, mouse: &Mouse) {
        console.rectfill(0, MAIN_Y, 127, 127, Some(5));

        self.update_zoom_view(console, mouse);
        self.update_info_panel(console, mouse);
        self.update_sheet(console, mouse);
        self.update_palette(console, mouse);
    }

    /// Zoomed 64x64 view of the current sprite; left-drag paints, right-click picks.
    fn update_zoom_view(&mut self, console: &mut Console, mouse: &Mouse) {
        let (sx0, sy0) = sprite_origin(self.current_sprite);

        console.rectfill(
            SPR_ZOOM_X,
            SPR_ZOOM_Y,
            SPR_ZOOM_X + SPR_ZOOM_SIZE - 1,
            SPR_ZOOM_Y + SPR_ZOOM_SIZE - 1,
            Some(0),
        );

        for py in 0..8_i32 {
            for px in 0..8_i32 {
                let col = console.sget(sx0 + px, sy0 + py);
                let dx = SPR_ZOOM_X + px * SPR_ZOOM;
                let dy = SPR_ZOOM_Y + py * SPR_ZOOM;
                console.rectfill(dx, dy, dx + SPR_ZOOM - 2, dy + SPR_ZOOM - 2, Some(col));
            }
        }

        if mouse.down {
            if let Some((px, py)) = zoom_to_sprite_pixel(mouse.x, mouse.y) {
                console.sset(sx0 + px, sy0 + py, self.sprite_color);
            }
        }

        if is_mouse_button_pressed(MouseButton::Right) {
            if let Some((px, py)) = zoom_to_sprite_pixel(mouse.x, mouse.y) {
                self.sprite_color = console.sget(sx0 + px, sy0 + py);
            }
        }
    }

    /// Sprite number / colour readout, 1:1 preview, `<`/`>` navigation and flag bits.
    fn update_info_panel(&mut self, console: &mut Console, mouse: &Mouse) {
        let info_x = SPR_ZOOM_X + SPR_ZOOM_SIZE + 4;
        let info_y = SPR_ZOOM_Y;

        console.print(
            &format!("spr {}", self.current_sprite),
            Some(info_x),
            Some(info_y),
            Some(7),
        );
        console.print(
            &format!("col {}", self.sprite_color),
            Some(info_x),
            Some(info_y + 8),
            Some(self.sprite_color),
        );

        // Small 1:1 preview
        let preview_x = info_x;
        let preview_y = info_y + 18;
        console.rectfill(
            preview_x - 1,
            preview_y - 1,
            preview_x + 8,
            preview_y + 8,
            Some(0),
        );
        blit_sprite(console, self.current_sprite, preview_x, preview_y);

        // Navigation buttons
        let nav_y = info_y + 30;
        if let Some(idx) = arrow_selector(
            console,
            mouse,
            info_x,
            info_x + 12,
            nav_y,
            self.current_sprite as usize,
            256,
        ) {
            self.current_sprite = idx as u8;
        }

        // Sprite flags display
        let flags_y = info_y + 40;
        let fl = console.flags[self.current_sprite as usize];
        console.print(
            &format!("f:{:02x}", fl),
            Some(info_x),
            Some(flags_y),
            Some(13),
        );

        let flag_bit_y = flags_y + 7;
        for b in 0..8_u8 {
            let bx = info_x + b as i32 * 5;
            let on = (fl >> (7 - b)) & 1 == 1;
            let c: u8 = if on { 11 } else { 2 };
            console.rectfill(bx, flag_bit_y, bx + 3, flag_bit_y + 3, Some(c));
            if mouse.clicked(Rect::new(bx, flag_bit_y, 4, 4)) {
                console.flags[self.current_sprite as usize] =
                    toggle_flag_bit(console.flags[self.current_sprite as usize], b);
            }
        }
    }

    /// Mini sprite-sheet browser (4 rows of 16) with the current sprite outlined.
    fn update_sheet(&mut self, console: &mut Console, mouse: &Mouse) {
        console.rectfill(
            SHEET_X,
            SHEET_Y,
            SHEET_X + (SHEET_COLS * 8) as i32 - 1,
            SHEET_Y + (SHEET_ROWS * 8) as i32 - 1,
            Some(0),
        );

        let page = (self.current_sprite as usize / 16) / SHEET_ROWS * SHEET_ROWS;
        for row in 0..SHEET_ROWS {
            let sheet_row = page + row;
            if sheet_row >= 16 {
                break;
            }
            for col in 0..SHEET_COLS {
                let spr_idx = (sheet_row * 16 + col) as u8;
                let dx = SHEET_X + col as i32 * 8;
                let dy = SHEET_Y + row as i32 * 8;
                blit_sprite(console, spr_idx, dx, dy);
            }
        }

        let rel_row = self.current_sprite as usize / 16;
        if rel_row >= page && rel_row < page + SHEET_ROWS {
            let vis_row = rel_row - page;
            let vis_col = self.current_sprite as usize % 16;
            let hx = SHEET_X + vis_col as i32 * 8;
            let hy = SHEET_Y + vis_row as i32 * 8;
            console.rect(hx - 1, hy - 1, hx + 8, hy + 8, Some(7));
        }

        if mouse.pressed {
            if let Some(idx) = sheet_click_to_sprite(mouse.x, mouse.y, page) {
                self.current_sprite = idx;
            }
        }
    }

    /// 16-colour palette bar; click or press 0-9 to pick the drawing colour.
    fn update_palette(&mut self, console: &mut Console, mouse: &Mouse) {
        for c in 0..16_u8 {
            let px = c as i32 * PAL_SWATCH;
            console.rectfill(
                px,
                PAL_Y,
                px + PAL_SWATCH - 1,
                PAL_Y + PAL_SWATCH - 1,
                Some(c),
            );
        }

        let sel_px = self.sprite_color as i32 * PAL_SWATCH;
        console.rect(
            sel_px,
            PAL_Y,
            sel_px + PAL_SWATCH - 1,
            PAL_Y + PAL_SWATCH - 1,
            Some(7),
        );
        console.rect(
            sel_px + 1,
            PAL_Y + 1,
            sel_px + PAL_SWATCH - 2,
            PAL_Y + PAL_SWATCH - 2,
            Some(0),
        );

        if mouse.pressed {
            if let Some(c) = palette_click_to_color(mouse.x, mouse.y) {
                self.sprite_color = c;
            }
        }

        for (k, &key) in NUMBER_KEYS.iter().enumerate() {
            if is_key_pressed(key) {
                self.sprite_color = k as u8;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ===================================================================
    // sprite_origin
    // ===================================================================

    #[test]
    fn sprite_origin_index_0() {
        assert_eq!(sprite_origin(0), (0, 0));
    }

    #[test]
    fn sprite_origin_index_1() {
        // sprite 1 is column 1, row 0
        assert_eq!(sprite_origin(1), (8, 0));
    }

    #[test]
    fn sprite_origin_index_16() {
        // sprite 16 is column 0, row 1
        assert_eq!(sprite_origin(16), (0, 8));
    }

    #[test]
    fn sprite_origin_index_17() {
        // sprite 17 is column 1, row 1
        assert_eq!(sprite_origin(17), (8, 8));
    }

    #[test]
    fn sprite_origin_index_255() {
        // sprite 255 is column 15, row 15
        assert_eq!(sprite_origin(255), (120, 120));
    }

    // ===================================================================
    // zoom_to_sprite_pixel
    // ===================================================================

    #[test]
    fn zoom_to_sprite_pixel_top_left_corner() {
        // Top-left corner of the zoomed view
        assert_eq!(zoom_to_sprite_pixel(SPR_ZOOM_X, SPR_ZOOM_Y), Some((0, 0)));
    }

    #[test]
    fn zoom_to_sprite_pixel_bottom_right_corner() {
        // Bottom-right pixel in the last zoom cell
        assert_eq!(
            zoom_to_sprite_pixel(
                SPR_ZOOM_X + SPR_ZOOM_SIZE - 1,
                SPR_ZOOM_Y + SPR_ZOOM_SIZE - 1
            ),
            Some((7, 7))
        );
    }

    #[test]
    fn zoom_to_sprite_pixel_middle() {
        // Middle-ish: pixel 4,4 starts at offset 4*8 = 32
        assert_eq!(
            zoom_to_sprite_pixel(SPR_ZOOM_X + 4 * SPR_ZOOM, SPR_ZOOM_Y + 4 * SPR_ZOOM),
            Some((4, 4))
        );
    }

    #[test]
    fn zoom_to_sprite_pixel_first_cell_interior() {
        // Any position within the first zoom cell maps to (0, 0)
        assert_eq!(
            zoom_to_sprite_pixel(SPR_ZOOM_X + SPR_ZOOM - 1, SPR_ZOOM_Y + SPR_ZOOM - 1),
            Some((0, 0))
        );
    }

    #[test]
    fn zoom_to_sprite_pixel_out_of_bounds_left() {
        assert_eq!(zoom_to_sprite_pixel(SPR_ZOOM_X - 1, SPR_ZOOM_Y), None);
    }

    #[test]
    fn zoom_to_sprite_pixel_out_of_bounds_right() {
        assert_eq!(
            zoom_to_sprite_pixel(SPR_ZOOM_X + SPR_ZOOM_SIZE, SPR_ZOOM_Y),
            None
        );
    }

    #[test]
    fn zoom_to_sprite_pixel_out_of_bounds_above() {
        assert_eq!(zoom_to_sprite_pixel(SPR_ZOOM_X, SPR_ZOOM_Y - 1), None);
    }

    #[test]
    fn zoom_to_sprite_pixel_out_of_bounds_below() {
        assert_eq!(
            zoom_to_sprite_pixel(SPR_ZOOM_X, SPR_ZOOM_Y + SPR_ZOOM_SIZE),
            None
        );
    }

    // ===================================================================
    // sheet_click_to_sprite
    // ===================================================================

    #[test]
    fn sheet_click_to_sprite_top_left() {
        // page=0, click at top-left of sheet => row 0, col 0 => sprite 0
        assert_eq!(sheet_click_to_sprite(SHEET_X, SHEET_Y, 0), Some(0));
    }

    #[test]
    fn sheet_click_to_sprite_second_col() {
        // page=0, click one cell to the right => col 1 => sprite 1
        assert_eq!(sheet_click_to_sprite(SHEET_X + 8, SHEET_Y, 0), Some(1));
    }

    #[test]
    fn sheet_click_to_sprite_second_row() {
        // page=0, click one row down => row 1 => sprite 16
        assert_eq!(sheet_click_to_sprite(SHEET_X, SHEET_Y + 8, 0), Some(16));
    }

    #[test]
    fn sheet_click_to_sprite_with_page_offset() {
        // page=4 (starting at row 4), click at row 0, col 0 => (4+0)*16 + 0 = 64
        assert_eq!(sheet_click_to_sprite(SHEET_X, SHEET_Y, 4), Some(64));
    }

    #[test]
    fn sheet_click_to_sprite_with_page_and_offset() {
        // page=4, click at row 1, col 3 => (4+1)*16 + 3 = 83
        assert_eq!(
            sheet_click_to_sprite(SHEET_X + 3 * 8, SHEET_Y + 8, 4),
            Some(83)
        );
    }

    #[test]
    fn sheet_click_to_sprite_last_valid() {
        // page=12, row=3, col=15 => (12+3)*16 + 15 = 255
        assert_eq!(
            sheet_click_to_sprite(SHEET_X + 15 * 8, SHEET_Y + 3 * 8, 12),
            Some(255)
        );
    }

    #[test]
    fn sheet_click_to_sprite_outside_left() {
        assert_eq!(sheet_click_to_sprite(SHEET_X - 1, SHEET_Y, 0), None);
    }

    #[test]
    fn sheet_click_to_sprite_outside_below() {
        assert_eq!(
            sheet_click_to_sprite(SHEET_X, SHEET_Y + (SHEET_ROWS * 8) as i32, 0),
            None
        );
    }

    #[test]
    fn sheet_click_to_sprite_outside_right() {
        assert_eq!(
            sheet_click_to_sprite(SHEET_X + (SHEET_COLS * 8) as i32, SHEET_Y, 0),
            None
        );
    }

    // ===================================================================
    // palette_click_to_color
    // ===================================================================

    #[test]
    fn palette_click_to_color_first() {
        assert_eq!(palette_click_to_color(0, PAL_Y), Some(0));
    }

    #[test]
    fn palette_click_to_color_last() {
        // color 15 starts at x = 15*8 = 120
        assert_eq!(palette_click_to_color(15 * PAL_SWATCH, PAL_Y), Some(15));
    }

    #[test]
    fn palette_click_to_color_middle() {
        // color 7 starts at x = 7*8 = 56
        assert_eq!(
            palette_click_to_color(7 * PAL_SWATCH + 3, PAL_Y + 2),
            Some(7)
        );
    }

    #[test]
    fn palette_click_to_color_within_swatch() {
        // Inside the last pixel of swatch 0
        assert_eq!(
            palette_click_to_color(PAL_SWATCH - 1, PAL_Y + PAL_SWATCH - 1),
            Some(0)
        );
    }

    #[test]
    fn palette_click_to_color_above() {
        assert_eq!(palette_click_to_color(0, PAL_Y - 1), None);
    }

    #[test]
    fn palette_click_to_color_below() {
        assert_eq!(palette_click_to_color(0, PAL_Y + PAL_SWATCH), None);
    }

    #[test]
    fn palette_click_to_color_negative_x() {
        assert_eq!(palette_click_to_color(-1, PAL_Y), None);
    }

    #[test]
    fn palette_click_to_color_past_right_edge() {
        assert_eq!(palette_click_to_color(128, PAL_Y), None);
    }

    // ===================================================================
    // toggle_flag_bit
    // ===================================================================

    #[test]
    fn toggle_flag_bit_on() {
        // Start with 0, toggle bit 0 (MSB) on => 0b1000_0000 = 128
        assert_eq!(toggle_flag_bit(0x00, 0), 0x80);
    }

    #[test]
    fn toggle_flag_bit_off() {
        // Start with 0xFF, toggle bit 0 (MSB) off => 0b0111_1111 = 127
        assert_eq!(toggle_flag_bit(0xFF, 0), 0x7F);
    }

    #[test]
    fn toggle_flag_bit_lsb_on() {
        // Toggle display bit 7 (LSB) on
        assert_eq!(toggle_flag_bit(0x00, 7), 0x01);
    }

    #[test]
    fn toggle_flag_bit_lsb_off() {
        // Toggle display bit 7 (LSB) off
        assert_eq!(toggle_flag_bit(0xFF, 7), 0xFE);
    }

    #[test]
    fn toggle_flag_bit_middle() {
        // Toggle display bit 4 => bit position 3 => 0b0000_1000 = 8
        assert_eq!(toggle_flag_bit(0x00, 4), 0x08);
    }

    #[test]
    fn toggle_flag_bit_preserves_others() {
        // Start with 0b1010_0101, toggle display bit 2 (bit position 5) => XOR with 0b0010_0000
        assert_eq!(toggle_flag_bit(0b1010_0101, 2), 0b1000_0101);
    }

    #[test]
    fn toggle_flag_bit_out_of_range() {
        // display_bit 8 and above should return flags unchanged
        assert_eq!(toggle_flag_bit(0x42, 8), 0x42);
        assert_eq!(toggle_flag_bit(0x42, 255), 0x42);
    }

    #[test]
    fn toggle_flag_bit_double_toggle_restores() {
        let original: u8 = 0b1100_0011;
        let toggled = toggle_flag_bit(original, 3);
        let restored = toggle_flag_bit(toggled, 3);
        assert_eq!(restored, original);
    }
}
