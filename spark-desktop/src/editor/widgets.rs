//! Small shared UI helpers: mouse state, rectangle hit-testing, arrow/step
//! button widgets, and keyboard-modifier queries.

use macroquad::prelude::*;
use spark_core::console::Console;

use super::{CHAR_H, CHAR_W};

// ---------------------------------------------------------------------------
// Geometry
// ---------------------------------------------------------------------------

/// Axis-aligned rectangle in 128x128 screen pixels. `x..x+w` / `y..y+h` are
/// half-open, matching the `>= a && < b` hit tests used throughout the editor.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Rect {
    pub x: i32,
    pub y: i32,
    pub w: i32,
    pub h: i32,
}

impl Rect {
    pub const fn new(x: i32, y: i32, w: i32, h: i32) -> Self {
        Self { x, y, w, h }
    }

    pub fn contains(&self, px: i32, py: i32) -> bool {
        px >= self.x && px < self.x + self.w && py >= self.y && py < self.y + self.h
    }
}

/// Hit box for a single printed glyph at `(x, y)` (a little wider than the
/// glyph so arrow buttons are easy to click).
pub fn glyph_rect(x: i32, y: i32) -> Rect {
    Rect::new(x, y, CHAR_W + 2, CHAR_H)
}

// ---------------------------------------------------------------------------
// Mouse
// ---------------------------------------------------------------------------

/// Mouse state in 128x128 screen coordinates, sampled once per frame.
#[derive(Debug, Default, Clone, Copy)]
pub struct Mouse {
    pub x: i32,
    pub y: i32,
    /// Left button went down this frame.
    pub pressed: bool,
    /// Left button is currently held.
    pub down: bool,
}

impl Mouse {
    /// Sample macroquad's mouse and convert to screen pixels using the
    /// letterbox offset/scale supplied by the host.
    pub fn poll(&mut self, offset_x: f32, offset_y: f32, scale: f32) {
        let (mx, my) = mouse_position();
        if scale > 0.0 {
            self.x = ((mx - offset_x) / scale) as i32;
            self.y = ((my - offset_y) / scale) as i32;
        }
        self.pressed = is_mouse_button_pressed(MouseButton::Left);
        self.down = is_mouse_button_down(MouseButton::Left);
    }

    /// True if the left button was pressed this frame inside `rect`.
    pub fn clicked(&self, rect: Rect) -> bool {
        self.pressed && rect.contains(self.x, self.y)
    }
}

// ---------------------------------------------------------------------------
// Widgets
// ---------------------------------------------------------------------------

/// Draw a pair of single-glyph buttons (e.g. `-`/`+` or `<`/`>`) on row `y`
/// and return `-1`, `0` or `1` depending on which one was clicked this frame.
pub fn step_buttons(
    console: &mut Console,
    mouse: &Mouse,
    left_x: i32,
    right_x: i32,
    y: i32,
    glyphs: (&str, &str),
    color: u8,
) -> i32 {
    console.print(glyphs.0, Some(left_x), Some(y), Some(color));
    console.print(glyphs.1, Some(right_x), Some(y), Some(color));
    if mouse.clicked(glyph_rect(left_x, y)) {
        -1
    } else if mouse.clicked(glyph_rect(right_x, y)) {
        1
    } else {
        0
    }
}

/// Step `idx` by `delta` within `0..n`, wrapping at both ends.
pub fn wrap_step(idx: usize, n: usize, delta: i32) -> usize {
    debug_assert!(n > 0);
    let n = n as i64;
    ((idx as i64 + delta as i64).rem_euclid(n)) as usize
}

/// `<` / `>` index selector with wraparound over `0..n`. The caller draws any
/// label; this only draws the two arrows at `left_x` / `right_x` on row `y`.
/// Returns the new index if an arrow was clicked.
pub fn arrow_selector(
    console: &mut Console,
    mouse: &Mouse,
    left_x: i32,
    right_x: i32,
    y: i32,
    idx: usize,
    n: usize,
) -> Option<usize> {
    let delta = step_buttons(console, mouse, left_x, right_x, y, ("<", ">"), 7);
    if delta == 0 {
        None
    } else {
        Some(wrap_step(idx, n, delta))
    }
}

// ---------------------------------------------------------------------------
// Keyboard modifiers
// ---------------------------------------------------------------------------

/// Ctrl (or Cmd on macOS) is held.
pub fn ctrl_down() -> bool {
    is_key_down(KeyCode::LeftControl)
        || is_key_down(KeyCode::RightControl)
        || is_key_down(KeyCode::LeftSuper)
        || is_key_down(KeyCode::RightSuper)
}

pub fn shift_down() -> bool {
    is_key_down(KeyCode::LeftShift) || is_key_down(KeyCode::RightShift)
}

pub fn alt_down() -> bool {
    is_key_down(KeyCode::LeftAlt) || is_key_down(KeyCode::RightAlt)
}

/// `-` / `+` (main row or keypad) pressed this frame, as a step delta.
pub fn plus_minus_key_delta() -> i32 {
    let mut d = 0;
    if is_key_pressed(KeyCode::Minus) || is_key_pressed(KeyCode::KpSubtract) {
        d -= 1;
    }
    if is_key_pressed(KeyCode::Equal) || is_key_pressed(KeyCode::KpAdd) {
        d += 1;
    }
    d
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rect_contains_is_half_open() {
        let r = Rect::new(10, 20, 5, 6);
        assert!(r.contains(10, 20));
        assert!(r.contains(14, 25));
        assert!(!r.contains(15, 20));
        assert!(!r.contains(10, 26));
        assert!(!r.contains(9, 20));
        assert!(!r.contains(10, 19));
    }

    #[test]
    fn mouse_clicked_requires_press_and_hit() {
        let r = Rect::new(0, 0, 8, 8);
        let m = Mouse {
            x: 3,
            y: 3,
            pressed: true,
            down: true,
        };
        assert!(m.clicked(r));
        let m = Mouse {
            pressed: false,
            ..m
        };
        assert!(!m.clicked(r));
        let m = Mouse {
            x: 9,
            pressed: true,
            ..m
        };
        assert!(!m.clicked(r));
    }

    #[test]
    fn wrap_step_wraps_both_ends() {
        assert_eq!(wrap_step(0, 64, -1), 63);
        assert_eq!(wrap_step(63, 64, 1), 0);
        assert_eq!(wrap_step(5, 64, 1), 6);
        assert_eq!(wrap_step(5, 64, -1), 4);
        assert_eq!(wrap_step(0, 256, -16), 240);
        assert_eq!(wrap_step(250, 256, 16), 10);
    }
}
