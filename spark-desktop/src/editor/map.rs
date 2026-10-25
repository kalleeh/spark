//! Map editor tab: scrollable 16x14 tile view with a tile-picker bar.

use macroquad::prelude::*;
use spark_core::console::Console;

use super::sprite::blit_sprite;
use super::widgets::{alt_down, plus_minus_key_delta, wrap_step, Mouse, Rect};
use super::{MAIN_Y, SH, SW};

// ---------------------------------------------------------------------------
// Layout constants
// ---------------------------------------------------------------------------

const MAP_VIEW_X: i32 = 0;
const MAP_VIEW_Y: i32 = MAIN_Y + 1;
const MAP_COLS: usize = 16;
const MAP_ROWS: usize = 14;

// ---------------------------------------------------------------------------
// Pure helpers
// ---------------------------------------------------------------------------

/// Convert a click in the map view area to map tile coordinates, accounting for scroll.
/// Returns `None` if the click is outside the map view or the resulting tile is out of bounds.
fn map_view_to_tile(
    mouse_x: i32,
    mouse_y: i32,
    scroll_x: i32,
    scroll_y: i32,
) -> Option<(i32, i32)> {
    if mouse_x < MAP_VIEW_X
        || mouse_x >= MAP_VIEW_X + MAP_COLS as i32 * 8
        || mouse_y < MAP_VIEW_Y
        || mouse_y >= MAP_VIEW_Y + MAP_ROWS as i32 * 8
    {
        return None;
    }
    let col = (mouse_x - MAP_VIEW_X) / 8;
    let row = (mouse_y - MAP_VIEW_Y) / 8;
    let map_x = scroll_x + col;
    let map_y = scroll_y + row;
    if (0..128).contains(&map_x) && (0..64).contains(&map_y) {
        Some((map_x, map_y))
    } else {
        None
    }
}

// ---------------------------------------------------------------------------
// Map editor
// ---------------------------------------------------------------------------

pub struct MapEditor {
    pub scroll_x: i32,
    pub scroll_y: i32,
    pub current_tile: u8,
}

impl Default for MapEditor {
    fn default() -> Self {
        Self {
            scroll_x: 0,
            scroll_y: 0,
            current_tile: 1,
        }
    }
}

impl MapEditor {
    pub fn update(&mut self, console: &mut Console, mouse: &Mouse) {
        console.rectfill(0, MAIN_Y, 127, 127, Some(1));

        self.handle_scroll_keys();
        self.update_view(console, mouse);
        self.draw_info_bar(console);
        self.update_tile_bar(console, mouse);
        self.handle_tile_keys();
    }

    fn handle_scroll_keys(&mut self) {
        if alt_down() {
            return;
        }
        if is_key_pressed(KeyCode::Left) {
            self.scroll_x = (self.scroll_x - 1).max(0);
        }
        if is_key_pressed(KeyCode::Right) {
            self.scroll_x = (self.scroll_x + 1).min(128 - MAP_COLS as i32);
        }
        if is_key_pressed(KeyCode::Up) {
            self.scroll_y = (self.scroll_y - 1).max(0);
        }
        if is_key_pressed(KeyCode::Down) {
            self.scroll_y = (self.scroll_y + 1).min(64 - MAP_ROWS as i32);
        }
    }

    /// Draw the visible map window; left-drag paints the current tile,
    /// right-drag erases (tile 0).
    fn update_view(&mut self, console: &mut Console, mouse: &Mouse) {
        for row in 0..MAP_ROWS as i32 {
            for col in 0..MAP_COLS as i32 {
                let map_x = self.scroll_x + col;
                let map_y = self.scroll_y + row;
                if !(0..128).contains(&map_x) || !(0..64).contains(&map_y) {
                    continue;
                }
                let tile = console.mget(map_x, map_y);
                let dx = MAP_VIEW_X + col * 8;
                let dy = MAP_VIEW_Y + row * 8;

                if tile == 0 {
                    console.rectfill(dx, dy, dx + 7, dy + 7, Some(1));
                    for i in 0..8_i32 {
                        console.raw_pset(dx + i, dy, 2);
                        console.raw_pset(dx, dy + i, 2);
                    }
                } else {
                    blit_sprite(console, tile, dx, dy);
                }
            }
        }

        let right_down = is_mouse_button_down(MouseButton::Right);
        if mouse.down && !right_down {
            if let Some((map_x, map_y)) =
                map_view_to_tile(mouse.x, mouse.y, self.scroll_x, self.scroll_y)
            {
                console.mset(map_x, map_y, self.current_tile);
            }
        }
        if right_down {
            if let Some((map_x, map_y)) =
                map_view_to_tile(mouse.x, mouse.y, self.scroll_x, self.scroll_y)
            {
                console.mset(map_x, map_y, 0);
            }
        }
    }

    /// Scroll position / current tile readout plus a preview of the current tile.
    fn draw_info_bar(&self, console: &mut Console) {
        let info_str = format!(
            "({},{}) t:{}",
            self.scroll_x, self.scroll_y, self.current_tile
        );
        console.print(&info_str, Some(1), Some(MAIN_Y), Some(7));

        let preview_x = SW - 12;
        let preview_y = MAIN_Y;
        if self.current_tile > 0 {
            blit_sprite(console, self.current_tile, preview_x, preview_y);
        } else {
            console.rectfill(preview_x, preview_y, preview_x + 7, preview_y + 7, Some(0));
        }
    }

    /// Bottom bar showing the 16 tiles on the current tile's page.
    fn update_tile_bar(&mut self, console: &mut Console, mouse: &Mouse) {
        let tile_bar_y = SH - 10;
        console.rectfill(0, tile_bar_y - 1, 127, 127, Some(2));

        let tile_page = (self.current_tile as usize / 16) * 16;
        for i in 0..16_usize {
            let tile_idx = tile_page + i;
            if tile_idx >= 256 {
                break;
            }
            let dx = i as i32 * 8;
            let dy = tile_bar_y;

            if tile_idx == 0 {
                console.rectfill(dx, dy, dx + 7, dy + 7, Some(0));
                console.raw_pset(dx + 1, dy + 1, 5);
                console.raw_pset(dx + 6, dy + 1, 5);
                console.raw_pset(dx + 3, dy + 3, 5);
                console.raw_pset(dx + 4, dy + 3, 5);
                console.raw_pset(dx + 1, dy + 6, 5);
                console.raw_pset(dx + 6, dy + 6, 5);
            } else {
                blit_sprite(console, tile_idx as u8, dx, dy);
            }

            if tile_idx == self.current_tile as usize {
                console.rect(dx, dy, dx + 7, dy + 7, Some(7));
            }
        }

        if mouse.clicked(Rect::new(0, tile_bar_y, 128, 8)) {
            let idx = tile_page + (mouse.x / 8) as usize;
            if idx < 256 {
                self.current_tile = idx as u8;
            }
        }
    }

    /// `[` / `]` step a page (16 tiles); `-` / `+` step one tile. Both wrap.
    fn handle_tile_keys(&mut self) {
        let mut delta = plus_minus_key_delta();
        if is_key_pressed(KeyCode::LeftBracket) {
            delta -= 16;
        }
        if is_key_pressed(KeyCode::RightBracket) {
            delta += 16;
        }
        if delta != 0 {
            self.current_tile = wrap_step(self.current_tile as usize, 256, delta) as u8;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ===================================================================
    // map_view_to_tile
    // ===================================================================

    #[test]
    fn map_view_to_tile_top_left_no_scroll() {
        // Click at the top-left of the map view with no scroll
        assert_eq!(map_view_to_tile(MAP_VIEW_X, MAP_VIEW_Y, 0, 0), Some((0, 0)));
    }

    #[test]
    fn map_view_to_tile_basic() {
        // Click at column 2, row 3 with no scroll
        assert_eq!(
            map_view_to_tile(MAP_VIEW_X + 2 * 8, MAP_VIEW_Y + 3 * 8, 0, 0),
            Some((2, 3))
        );
    }

    #[test]
    fn map_view_to_tile_with_scroll() {
        // Scroll (10, 5), click at view column 0, row 0 => map tile (10, 5)
        assert_eq!(
            map_view_to_tile(MAP_VIEW_X, MAP_VIEW_Y, 10, 5),
            Some((10, 5))
        );
    }

    #[test]
    fn map_view_to_tile_with_scroll_offset() {
        // Scroll (10, 5), click at view column 3, row 2 => map tile (13, 7)
        assert_eq!(
            map_view_to_tile(MAP_VIEW_X + 3 * 8, MAP_VIEW_Y + 2 * 8, 10, 5),
            Some((13, 7))
        );
    }

    #[test]
    fn map_view_to_tile_out_of_bounds_left() {
        assert_eq!(map_view_to_tile(MAP_VIEW_X - 1, MAP_VIEW_Y, 0, 0), None);
    }

    #[test]
    fn map_view_to_tile_out_of_bounds_above() {
        assert_eq!(map_view_to_tile(MAP_VIEW_X, MAP_VIEW_Y - 1, 0, 0), None);
    }

    #[test]
    fn map_view_to_tile_out_of_bounds_right() {
        assert_eq!(
            map_view_to_tile(MAP_VIEW_X + MAP_COLS as i32 * 8, MAP_VIEW_Y, 0, 0),
            None
        );
    }

    #[test]
    fn map_view_to_tile_out_of_bounds_below() {
        assert_eq!(
            map_view_to_tile(MAP_VIEW_X, MAP_VIEW_Y + MAP_ROWS as i32 * 8, 0, 0),
            None
        );
    }

    #[test]
    fn map_view_to_tile_map_boundary_overflow() {
        // Scroll so that the resulting map coordinate exceeds the 128x64 map
        // scroll_x=120, click at view column 15 => map_x = 135, which is >= 128
        assert_eq!(
            map_view_to_tile(MAP_VIEW_X + 15 * 8, MAP_VIEW_Y, 120, 0),
            None
        );
    }

    #[test]
    fn map_view_to_tile_map_boundary_y_overflow() {
        // scroll_y=60, click at view row 13 => map_y = 73, which is >= 64
        assert_eq!(
            map_view_to_tile(MAP_VIEW_X, MAP_VIEW_Y + 13 * 8, 0, 60),
            None
        );
    }

    #[test]
    fn map_view_to_tile_last_valid_cell() {
        // Last valid view cell: column MAP_COLS-1, row MAP_ROWS-1, no scroll
        assert_eq!(
            map_view_to_tile(
                MAP_VIEW_X + (MAP_COLS as i32 - 1) * 8,
                MAP_VIEW_Y + (MAP_ROWS as i32 - 1) * 8,
                0,
                0
            ),
            Some((MAP_COLS as i32 - 1, MAP_ROWS as i32 - 1))
        );
    }

    #[test]
    fn map_view_to_tile_edge_pixel_within_cell() {
        // Click at the last pixel within the first cell (x = 7, y = 7 within view)
        assert_eq!(
            map_view_to_tile(MAP_VIEW_X + 7, MAP_VIEW_Y + 7, 0, 0),
            Some((0, 0))
        );
    }
}
