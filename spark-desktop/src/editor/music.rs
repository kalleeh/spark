//! Music pattern editor tab: 64-pattern list with per-channel SFX assignment
//! and loop/end/stop flags.

use macroquad::prelude::*;
use spark_core::audio::{MusicPattern, NUM_MUSIC};
use spark_core::console::Console;

use super::widgets::{alt_down, step_buttons, wrap_step, Mouse, Rect};
use super::{CHAR_H, CHAR_W, MAIN_Y, SH};

/// Number of pattern rows visible at once.
const VISIBLE_ROWS: usize = 16;
/// Height of each pattern row in pixels.
const ROW_H: i32 = CHAR_H;
/// Y position where the pattern list starts.
const LIST_Y: i32 = MAIN_Y + 1;

/// Bit 6 of a channel byte: set means the channel is *disabled*.
const CH_DISABLED_BIT: u8 = 0x40;
/// Low 6 bits of a channel byte: SFX index.
const CH_SFX_MASK: u8 = 0x3F;

/// Pattern flag bits with their button label, x position and "on" colour.
const FLAG_BUTTONS: [(u8, &str, i32, u8); 3] =
    [(1, "LOOP", 1, 12), (2, "END", 28, 14), (4, "STOP", 50, 8)];

#[derive(Default)]
pub struct MusicEditor {
    pub current_music: usize,
    scroll: usize,
}

impl MusicEditor {
    /// Edits `music_data` in place.
    pub fn update(
        &mut self,
        console: &mut Console,
        mouse: &Mouse,
        music_data: &mut [MusicPattern; NUM_MUSIC],
    ) {
        console.rectfill(0, MAIN_Y, 127, 127, Some(0));

        self.handle_nav_keys();
        self.update_pattern_list(console, mouse, music_data);
        Self::update_detail_panel(
            console,
            mouse,
            self.current_music,
            &mut music_data[self.current_music],
        );
    }

    /// Up/Down move the selection (clamped, no wrap) and keep it visible.
    fn handle_nav_keys(&mut self) {
        if !alt_down() {
            if is_key_pressed(KeyCode::Up) && self.current_music > 0 {
                self.current_music -= 1;
            }
            if is_key_pressed(KeyCode::Down) && self.current_music < NUM_MUSIC - 1 {
                self.current_music += 1;
            }
        }

        if self.current_music < self.scroll {
            self.scroll = self.current_music;
        }
        if self.current_music >= self.scroll + VISIBLE_ROWS {
            self.scroll = self.current_music - VISIBLE_ROWS + 1;
        }
    }

    /// One row per pattern: index, 4 channel SFX numbers, L/E/S flags.
    fn update_pattern_list(
        &mut self,
        console: &mut Console,
        mouse: &Mouse,
        music_data: &[MusicPattern; NUM_MUSIC],
    ) {
        for i in 0..VISIBLE_ROWS {
            let pat_idx = self.scroll + i;
            if pat_idx >= NUM_MUSIC {
                break;
            }
            let py = LIST_Y + i as i32 * ROW_H;
            let pat = &music_data[pat_idx];

            let is_selected = pat_idx == self.current_music;
            if is_selected {
                console.rectfill(0, py, 127, py + ROW_H - 2, Some(1));
            }

            let idx_color: u8 = if is_selected { 7 } else { 13 };
            console.print(
                &format!("{:02}", pat_idx),
                Some(1),
                Some(py),
                Some(idx_color),
            );

            for ch in 0..4_usize {
                let cx = 14 + ch as i32 * 18;
                let enabled = pat.channel_enabled(ch);
                let ch_color: u8 = if enabled { 11 } else { 5 };
                let ch_label = if enabled {
                    format!("{:02}", pat.sfx_index(ch))
                } else {
                    "--".to_string()
                };
                console.print(&ch_label, Some(cx), Some(py), Some(ch_color));
            }

            let flag_x: i32 = 88;
            if pat.is_loop_start() {
                console.print("L", Some(flag_x), Some(py), Some(12));
            }
            if pat.is_loop_end() {
                console.print("E", Some(flag_x + 6), Some(py), Some(14));
            }
            if pat.is_stop() {
                console.print("S", Some(flag_x + 12), Some(py), Some(8));
            }

            if mouse.clicked(Rect::new(0, py, 128, ROW_H)) {
                self.current_music = pat_idx;
            }
        }
    }

    /// Bottom panel: per-channel enable toggle and SFX `-`/`+`, then flag buttons.
    fn update_detail_panel(
        console: &mut Console,
        mouse: &Mouse,
        pat_idx: usize,
        pat: &mut MusicPattern,
    ) {
        let detail_y = LIST_Y + VISIBLE_ROWS as i32 * ROW_H + 2;
        if detail_y + 20 > SH {
            return;
        }

        console.print(
            &format!("PAT {:02}", pat_idx),
            Some(1),
            Some(detail_y),
            Some(7),
        );

        for (ch, ch_byte) in pat.channels.iter_mut().enumerate() {
            let cy = detail_y + 8 + ch as i32 * 8;
            let enabled = (*ch_byte & CH_DISABLED_BIT) == 0;
            let sfx_idx = *ch_byte & CH_SFX_MASK;

            console.print(&format!("{}:", ch + 1), Some(1), Some(cy), Some(6));

            // Enabled/disabled toggle
            let tog_x: i32 = 12;
            let tog_color: u8 = if enabled { 11 } else { 2 };
            console.rectfill(tog_x, cy, tog_x + 5, cy + 5, Some(tog_color));
            if mouse.clicked(Rect::new(tog_x, cy, 6, 6)) {
                *ch_byte ^= CH_DISABLED_BIT;
            }

            // SFX index with -/+ (wraps 0..63, preserves the high bits)
            let sfx_color: u8 = if enabled { 7 } else { 5 };
            console.print(
                &format!("{:02}", sfx_idx),
                Some(22),
                Some(cy),
                Some(sfx_color),
            );
            let delta = step_buttons(console, mouse, 34, 42, cy, ("-", "+"), 7);
            if delta != 0 {
                let new_idx = wrap_step(sfx_idx as usize, 64, delta) as u8;
                *ch_byte = (*ch_byte & !CH_SFX_MASK) | new_idx;
            }
        }

        let flag_btn_y = detail_y + 8 + 4 * 8;
        if flag_btn_y + 6 <= SH {
            for &(bit, label, x, on_color) in FLAG_BUTTONS.iter() {
                let c: u8 = if pat.flags & bit != 0 { on_color } else { 2 };
                console.print(label, Some(x), Some(flag_btn_y), Some(c));
                if mouse.clicked(Rect::new(
                    x,
                    flag_btn_y,
                    label.len() as i32 * CHAR_W,
                    CHAR_H,
                )) {
                    pat.flags ^= bit;
                }
            }
        }
    }
}
