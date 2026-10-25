//! "LOAD .P8" overlay: lists `.p8` files in the current directory.

use macroquad::prelude::*;
use spark_core::console::Console;

use super::widgets::{Mouse, Rect};
use super::CHAR_H;

/// Number of file names shown at once.
const VISIBLE: usize = 12;
/// Longest file name (in characters) that fits inside the overlay.
const MAX_NAME_CHARS: usize = 22;

#[derive(Default)]
pub struct FilePicker {
    pub visible: bool,
    files: Vec<String>,
    scroll: usize,
}

impl FilePicker {
    /// Scan the current directory for `.p8` files and show the overlay.
    pub fn open(&mut self) {
        self.files.clear();
        if let Ok(entries) = std::fs::read_dir(".") {
            for entry in entries.flatten() {
                if let Some(name) = entry.file_name().to_str() {
                    if name.ends_with(".p8") {
                        self.files.push(name.to_string());
                    }
                }
            }
        }
        self.files.sort();
        self.scroll = 0;
        self.visible = true;
    }

    /// Draw the overlay and handle input. Returns the chosen file name (and
    /// hides the picker) when the user clicks an entry.
    pub fn update(&mut self, console: &mut Console, mouse: &Mouse) -> Option<String> {
        if is_key_pressed(KeyCode::Escape) {
            self.visible = false;
            return None;
        }

        console.rectfill(4, 10, 123, 118, Some(0));
        console.rect(4, 10, 123, 118, Some(7));
        console.print("LOAD .P8", Some(8), Some(12), Some(7));

        if self.files.is_empty() {
            console.print("no .p8 files", Some(8), Some(24), Some(8));
            console.print("in current dir", Some(8), Some(32), Some(8));
            console.print("esc to close", Some(8), Some(48), Some(13));
            return None;
        }

        if is_key_pressed(KeyCode::Up) && self.scroll > 0 {
            self.scroll -= 1;
        }
        if is_key_pressed(KeyCode::Down) {
            let max_scroll = self.files.len().saturating_sub(VISIBLE);
            if self.scroll < max_scroll {
                self.scroll += 1;
            }
        }

        for i in 0..VISIBLE {
            let file_idx = self.scroll + i;
            if file_idx >= self.files.len() {
                break;
            }
            let fy = 22 + i as i32 * CHAR_H;
            let name = &self.files[file_idx];
            let display: String = name.chars().take(MAX_NAME_CHARS).collect();
            console.print(&display, Some(8), Some(fy), Some(6));

            if mouse.clicked(Rect::new(8, fy, 112, CHAR_H)) {
                let chosen = name.clone();
                self.visible = false;
                return Some(chosen);
            }
        }

        // Scroll indicators
        if self.scroll > 0 {
            console.print("\x18", Some(118), Some(22), Some(13)); // up arrow
        }
        if self.scroll + VISIBLE < self.files.len() {
            console.print("\x19", Some(118), Some(110), Some(13)); // down arrow
        }

        None
    }
}
