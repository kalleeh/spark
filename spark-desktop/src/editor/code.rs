//! Code editor tab: text editing with selection, undo/redo, line clipboard,
//! incremental search, auto-indent and syntax highlighting.

use macroquad::prelude::*;
use spark_core::console::Console;

use super::highlight::{count_tokens, token_color, tokenize_line};
use super::widgets::{alt_down, ctrl_down, shift_down, Mouse};
use super::{CHAR_H, CHAR_W, MAIN_Y, SH, SW, TOP_BAR_H};

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

/// Line-number gutter width in characters (e.g. " 1 ").
const GUTTER_CHARS: usize = 3;
const GUTTER_PX: i32 = GUTTER_CHARS as i32 * CHAR_W;

/// Visible columns and rows.
const CODE_COLS: usize = ((SW - GUTTER_PX) / CHAR_W) as usize;
const CODE_ROWS: usize = ((SH - MAIN_Y) / CHAR_H) as usize;

/// Selection highlight color (PICO-8 dark blue).
const SELECTION_COLOR: u8 = 1;
/// Search match highlight color (PICO-8 orange).
const SEARCH_MATCH_COLOR: u8 = 9;
/// Current search match highlight color (PICO-8 red).
const SEARCH_CURRENT_COLOR: u8 = 8;

/// PICO-8 character limit.
const PICO8_CHAR_LIMIT: usize = 65535;
/// PICO-8 token limit.
const PICO8_TOKEN_LIMIT: usize = 8192;

/// Cursor blink half-period in frames.
const BLINK_HALF: u32 = 20;

/// Maximum entries in the undo stack.
const UNDO_MAX: usize = 100;

// Key repeat
const REPEAT_DELAY: u32 = 18;
const REPEAT_SLOW: u32 = 6;
const REPEAT_FAST: u32 = 2;
const REPEAT_RAMP: u32 = 60;

const K_LEFT: usize = 0;
const K_RIGHT: usize = 1;
const K_UP: usize = 2;
const K_DOWN: usize = 3;
const K_HOME: usize = 4;
const K_END: usize = 5;
const K_BKSP: usize = 6;
const K_DEL: usize = 7;
const K_ENTER: usize = 8;
const K_TAB: usize = 9;
const NUM_REPEAT_KEYS: usize = 10;

const REPEATABLE_KEYS: [KeyCode; NUM_REPEAT_KEYS] = [
    KeyCode::Left,
    KeyCode::Right,
    KeyCode::Up,
    KeyCode::Down,
    KeyCode::Home,
    KeyCode::End,
    KeyCode::Backspace,
    KeyCode::Delete,
    KeyCode::Enter,
    KeyCode::Tab,
];

/// A cursor movement: `(code, cursor_pos) -> new_cursor_pos`.
type CursorMove = fn(&str, usize) -> usize;

/// Cursor-movement keys and the movement each performs.
const NAV_KEYS: [(usize, CursorMove); 6] = [
    (K_LEFT, move_left),
    (K_RIGHT, move_right),
    (K_UP, move_up),
    (K_DOWN, move_down),
    (K_HOME, move_line_start),
    (K_END, move_line_end),
];

// ---------------------------------------------------------------------------
// Cursor math
// ---------------------------------------------------------------------------

/// Split a code string into lines.
fn split_lines(code: &str) -> Vec<&str> {
    if code.is_empty() {
        vec![""]
    } else {
        code.split('\n').collect()
    }
}

/// Given a cursor byte-offset and pre-split lines, return (line_index, column_index).
fn cursor_to_line_col(cursor_pos: usize, lines: &[&str]) -> (usize, usize) {
    let mut remaining = cursor_pos;
    for (i, line) in lines.iter().enumerate() {
        if remaining <= line.len() {
            return (i, remaining);
        }
        // +1 accounts for the '\n' separator that was removed by split
        remaining = remaining.saturating_sub(line.len() + 1);
    }
    let last = lines.len().saturating_sub(1);
    (last, lines.get(last).map_or(0, |l| l.len()))
}

/// Convert (line, column) back to a byte offset, clamped to the code length.
fn line_col_to_pos(lines: &[&str], line: usize, col: usize, code_len: usize) -> usize {
    let line = line.min(lines.len().saturating_sub(1));
    let mut pos: usize = 0;
    for l in lines.iter().take(line) {
        pos += l.len() + 1;
    }
    let max_col = lines.get(line).map_or(0, |l| l.len());
    pos += col.min(max_col);
    pos.min(code_len)
}

/// Byte offset of the first character of `line` (0 if `line` is 0).
fn line_start_offset(lines: &[&str], line: usize) -> usize {
    lines.iter().take(line).map(|l| l.len() + 1).sum()
}

/// One character left (respecting UTF-8 boundaries).
fn move_left(code: &str, pos: usize) -> usize {
    let mut p = pos.saturating_sub(1);
    while p > 0 && !code.is_char_boundary(p) {
        p -= 1;
    }
    p
}

/// One character right (respecting UTF-8 boundaries).
fn move_right(code: &str, pos: usize) -> usize {
    if pos >= code.len() {
        return pos;
    }
    let mut p = pos + 1;
    while p < code.len() && !code.is_char_boundary(p) {
        p += 1;
    }
    p
}

fn move_up(code: &str, pos: usize) -> usize {
    let lines = split_lines(code);
    let (line, col) = cursor_to_line_col(pos, &lines);
    if line > 0 {
        line_col_to_pos(&lines, line - 1, col, code.len())
    } else {
        pos
    }
}

fn move_down(code: &str, pos: usize) -> usize {
    let lines = split_lines(code);
    let (line, col) = cursor_to_line_col(pos, &lines);
    if line + 1 < lines.len() {
        line_col_to_pos(&lines, line + 1, col, code.len())
    } else {
        pos
    }
}

fn move_line_start(code: &str, pos: usize) -> usize {
    let lines = split_lines(code);
    let (line, _) = cursor_to_line_col(pos, &lines);
    line_col_to_pos(&lines, line, 0, code.len())
}

fn move_line_end(code: &str, pos: usize) -> usize {
    let lines = split_lines(code);
    let (line, _) = cursor_to_line_col(pos, &lines);
    let len = lines.get(line).map_or(0, |l| l.len());
    line_col_to_pos(&lines, line, len, code.len())
}

// ---------------------------------------------------------------------------
// Selection helpers
// ---------------------------------------------------------------------------

/// Return the ordered (start, end) byte offsets for a selection range.
/// `sel_start` is where the selection began, `cursor_pos` is the current cursor.
fn selection_range(sel_start: usize, cursor_pos: usize) -> (usize, usize) {
    if sel_start <= cursor_pos {
        (sel_start, cursor_pos)
    } else {
        (cursor_pos, sel_start)
    }
}

/// Extract the selected text from a code string given a selection range.
fn selected_text(code: &str, sel_start: usize, cursor_pos: usize) -> &str {
    let (lo, hi) = selection_range(sel_start, cursor_pos);
    let lo = lo.min(code.len());
    let hi = hi.min(code.len());
    &code[lo..hi]
}

/// Delete the selected range from a code string, returning the new cursor position.
fn delete_selection(code: &mut String, sel_start: usize, cursor_pos: usize) -> usize {
    let (lo, hi) = selection_range(sel_start, cursor_pos);
    let lo = lo.min(code.len());
    let hi = hi.min(code.len());
    code.drain(lo..hi);
    lo.min(code.len())
}

// ---------------------------------------------------------------------------
// Search helpers
// ---------------------------------------------------------------------------

/// Find all byte offsets of non-overlapping occurrences of `query` in `code`.
/// Returns an empty vec if `query` is empty.
fn find_all_matches(code: &str, query: &str) -> Vec<usize> {
    if query.is_empty() {
        return Vec::new();
    }
    let query_lower = query.to_ascii_lowercase();
    let code_lower = code.to_ascii_lowercase();
    let mut results = Vec::new();
    let mut start = 0;
    while let Some(pos) = code_lower[start..].find(&query_lower) {
        results.push(start + pos);
        start += pos + query_lower.len();
    }
    results
}

// ---------------------------------------------------------------------------
// Auto-indent helpers
// ---------------------------------------------------------------------------

/// Return the leading whitespace of a given line.
fn leading_whitespace(line: &str) -> &str {
    let trimmed = line.trim_start();
    &line[..line.len() - trimmed.len()]
}

/// Check if a line ends with a keyword that warrants extra indentation.
/// Strips trailing whitespace and comments before checking.
fn ends_with_indent_keyword(line: &str) -> bool {
    // Strip trailing comment first
    let stripped = if let Some(pos) = line.find("--") {
        &line[..pos]
    } else {
        line
    };
    let trimmed = stripped.trim_end();
    if trimmed.is_empty() {
        return false;
    }
    // Check for keywords at end
    let indent_suffixes: &[&str] = &["then", "do", "else", "repeat"];
    for suffix in indent_suffixes {
        if trimmed.ends_with(suffix) {
            // Make sure it's a word boundary (not part of a larger identifier)
            let before = trimmed.len() - suffix.len();
            if before == 0 {
                return true;
            }
            let prev_char = trimmed.as_bytes()[before - 1];
            if !prev_char.is_ascii_alphanumeric() && prev_char != b'_' {
                return true;
            }
        }
    }
    // Check for function(...) at end  (the line ends with ")" preceded by "function" somewhere)
    if trimmed.ends_with(')') {
        // Look for "function" followed by "(" in this line
        if let Some(pos) = trimmed.rfind("function") {
            let after_fn = pos + "function".len();
            // Ensure "function" is at a word boundary
            let word_boundary = pos == 0
                || (!trimmed.as_bytes()[pos - 1].is_ascii_alphanumeric()
                    && trimmed.as_bytes()[pos - 1] != b'_');
            if word_boundary {
                // Check there's a '(' after "function"
                if trimmed[after_fn..].contains('(') {
                    return true;
                }
            }
        }
    }
    false
}

/// Discard any buffered typed characters (after a shortcut consumed the keypress).
fn drain_chars() {
    while get_char_pressed().is_some() {}
}

// ---------------------------------------------------------------------------
// Code editor
// ---------------------------------------------------------------------------

/// Editing state for the code tab. The text itself lives in `Editor::code`
/// and is passed into every method, so the host can read/replace it freely.
pub struct CodeEditor {
    pub cursor_pos: usize,
    pub scroll_y: usize,
    pub selection_start: Option<usize>,

    key_timers: [u32; NUM_REPEAT_KEYS],

    // Undo/redo stacks: each entry is (code_snapshot, cursor_pos)
    undo_stack: Vec<(String, usize)>,
    redo_stack: Vec<(String, usize)>,
    // Edit coalescing: the code state at the last undo push, so rapid typing
    // collapses into one undo entry.
    last_undo_code: String,

    search_active: bool,
    search_query: String,
    search_results: Vec<usize>, // byte offsets of matches
    search_current: usize,      // index into search_results

    // Internal clipboard (line-based copy/paste). System clipboard integration
    // could use the `arboard` crate; for now it only works within the session.
    clipboard: String,
}

impl CodeEditor {
    /// Fresh editor state for `code` (no undo history, cursor at 0).
    pub fn new(code: &str) -> Self {
        Self {
            cursor_pos: 0,
            scroll_y: 0,
            selection_start: None,
            key_timers: [0; NUM_REPEAT_KEYS],
            undo_stack: Vec::new(),
            redo_stack: Vec::new(),
            last_undo_code: code.to_string(),
            search_active: false,
            search_query: String::new(),
            search_results: Vec::new(),
            search_current: 0,
            clipboard: String::new(),
        }
    }

    // -----------------------------------------------------------------------
    // Per-frame entry point
    // -----------------------------------------------------------------------

    pub fn update(&mut self, console: &mut Console, code: &mut String, mouse: &Mouse, frame: u32) {
        console.rectfill(0, MAIN_Y, 127, 127, Some(1));

        self.handle_input(code);

        let lines = split_lines(code);
        let (cursor_line, cursor_col) = cursor_to_line_col(self.cursor_pos, &lines);
        self.scroll_to_cursor(cursor_line);

        self.draw_lines(console, &lines);
        self.draw_cursor(console, cursor_line, cursor_col, frame);

        // Mouse click to position cursor (clears selection)
        if mouse.pressed && mouse.y >= MAIN_Y && mouse.x >= GUTTER_PX {
            let click_line = self.scroll_y + ((mouse.y - MAIN_Y) / CHAR_H) as usize;
            let click_col = ((mouse.x - GUTTER_PX) / CHAR_W) as usize;
            self.cursor_pos = line_col_to_pos(&lines, click_line, click_col, code.len());
            self.selection_start = None;
        }

        if self.search_active {
            self.draw_search_bar(console);
        }
        draw_token_count(console, code);
    }

    fn scroll_to_cursor(&mut self, cursor_line: usize) {
        if cursor_line < self.scroll_y {
            self.scroll_y = cursor_line;
        }
        if cursor_line >= self.scroll_y + CODE_ROWS {
            self.scroll_y = cursor_line - CODE_ROWS + 1;
        }
    }

    // -----------------------------------------------------------------------
    // Drawing
    // -----------------------------------------------------------------------

    /// Selection/search highlights, gutter and syntax-coloured text for every
    /// visible row.
    fn draw_lines(&self, console: &mut Console, lines: &[&str]) {
        let sel_range = self
            .selection_start
            .map(|ss| selection_range(ss, self.cursor_pos));

        // Reserve the bottom row for the search bar when it is open.
        let visible_rows = if self.search_active {
            CODE_ROWS.saturating_sub(1)
        } else {
            CODE_ROWS
        };

        let mut line_byte_start = line_start_offset(lines, self.scroll_y);
        for i in 0..visible_rows {
            let line_idx = self.scroll_y + i;
            let Some(&line) = lines.get(line_idx) else {
                break;
            };
            let py = MAIN_Y + i as i32 * CHAR_H;
            let line_byte_end = line_byte_start + line.len();

            if let Some((sel_lo, sel_hi)) = sel_range {
                draw_selection_row(console, py, line, line_byte_start, sel_lo, sel_hi);
            }
            if self.search_active && !self.search_query.is_empty() {
                self.draw_search_row(console, py, line, line_byte_start);
            }

            // Line number gutter
            let num_str = format!("{:>3} ", line_idx + 1);
            console.print(&num_str, Some(0), Some(py), Some(13));

            // Syntax-highlighted code
            for tok in &tokenize_line(line) {
                if tok.start >= CODE_COLS {
                    break;
                }
                let max_chars = CODE_COLS - tok.start;
                let visible: String = line[tok.start..tok.end].chars().take(max_chars).collect();
                let px = GUTTER_PX + tok.start as i32 * CHAR_W;
                console.print(&visible, Some(px), Some(py), Some(token_color(tok.kind)));
            }

            line_byte_start = line_byte_end + 1;
        }
    }

    /// Highlight every search match that overlaps this row.
    fn draw_search_row(&self, console: &mut Console, py: i32, line: &str, line_byte_start: usize) {
        let query_len = self.search_query.len();
        let line_byte_end = line_byte_start + line.len();
        for (match_idx, &match_offset) in self.search_results.iter().enumerate() {
            let match_end = match_offset + query_len;
            if match_offset >= line_byte_end || match_end <= line_byte_start {
                continue;
            }
            let col_start = match_offset.saturating_sub(line_byte_start).min(CODE_COLS);
            let col_end = if match_end >= line_byte_end {
                line.len()
            } else {
                match_end - line_byte_start
            }
            .min(CODE_COLS);
            if col_end > col_start {
                let color = if match_idx == self.search_current {
                    SEARCH_CURRENT_COLOR
                } else {
                    SEARCH_MATCH_COLOR
                };
                fill_cols(console, py, col_start, col_end, color);
            }
        }
    }

    fn draw_cursor(
        &self,
        console: &mut Console,
        cursor_line: usize,
        cursor_col: usize,
        frame: u32,
    ) {
        let blink_on = (frame % (BLINK_HALF * 2)) < BLINK_HALF;
        if !blink_on {
            return;
        }
        let cx = GUTTER_PX + cursor_col as i32 * CHAR_W;
        let cy = MAIN_Y + (cursor_line as i32 - self.scroll_y as i32) * CHAR_H;
        if cy >= MAIN_Y && cy + CHAR_H <= SH {
            console.rectfill(cx, cy, cx, cy + CHAR_H - 2, Some(12));
        }
    }

    /// "f:<query>" bar with match counter at the bottom of the code area.
    fn draw_search_bar(&self, console: &mut Console) {
        let bar_y = SH - CHAR_H;
        console.rectfill(0, bar_y, 127, 127, Some(0));
        let query_display: String = self.search_query.chars().take(14).collect();
        console.print(
            &format!("f:{}", query_display),
            Some(1),
            Some(bar_y),
            Some(7),
        );

        let (count_str, color) = if !self.search_results.is_empty() {
            (
                format!("{}/{}", self.search_current + 1, self.search_results.len()),
                11,
            )
        } else if !self.search_query.is_empty() {
            ("0/0".to_string(), 8)
        } else {
            return;
        };
        let count_x = SW - count_str.len() as i32 * CHAR_W - 1;
        console.print(&count_str, Some(count_x), Some(bar_y), Some(color));
    }

    // -----------------------------------------------------------------------
    // Key repeat
    // -----------------------------------------------------------------------

    fn update_key_timers(&mut self) {
        for (i, key) in REPEATABLE_KEYS.iter().enumerate() {
            let down = if i == K_ENTER {
                is_key_down(KeyCode::Enter) || is_key_down(KeyCode::KpEnter)
            } else {
                is_key_down(*key)
            };
            if down {
                self.key_timers[i] += 1;
            } else {
                self.key_timers[i] = 0;
            }
        }
    }

    /// True on the frame a key is first pressed, then again at an accelerating
    /// repeat rate while it is held.
    fn key_fire(&self, idx: usize) -> bool {
        let t = self.key_timers[idx];
        if t == 0 {
            return false;
        }
        if t == 1 {
            return true;
        }
        if t <= REPEAT_DELAY {
            return false;
        }
        let since = t - REPEAT_DELAY;
        let frac = (since as f32 / REPEAT_RAMP as f32).min(1.0);
        let interval = REPEAT_SLOW as f32 - (REPEAT_SLOW as f32 - REPEAT_FAST as f32) * frac;
        let interval = (interval.round() as u32).max(1);
        since.is_multiple_of(interval)
    }

    // -----------------------------------------------------------------------
    // Input dispatch
    // -----------------------------------------------------------------------

    fn handle_input(&mut self, code: &mut String) {
        self.update_key_timers();

        let ctrl = ctrl_down();
        let shift = shift_down();

        // Ctrl+F toggles search mode
        if ctrl && is_key_pressed(KeyCode::F) {
            if self.search_active {
                self.close_search();
            } else {
                self.search_active = true;
            }
            drain_chars();
            return;
        }

        if self.search_active {
            self.handle_search_input(code);
            return;
        }

        if alt_down() {
            return;
        }

        if ctrl && self.handle_shortcuts(code, shift) {
            return;
        }

        // Typed characters that arrive on the same frame as a navigation /
        // editing key are echoes of that key and must not be inserted.
        let nav_key =
            (0..NUM_REPEAT_KEYS).any(|i| self.key_fire(i)) || is_key_pressed(KeyCode::Escape);

        self.handle_navigation(code, shift);
        self.handle_editing(code, ctrl, nav_key);
    }

    /// Ctrl+A/Z/Y/C/X/V. Returns true if a shortcut fired.
    fn handle_shortcuts(&mut self, code: &mut String, shift: bool) -> bool {
        let handled = if is_key_pressed(KeyCode::A) {
            self.selection_start = Some(0);
            self.cursor_pos = code.len();
            true
        } else if is_key_pressed(KeyCode::Z) && !shift {
            self.selection_start = None;
            self.push_undo_if_changed(code);
            self.perform_undo(code);
            true
        } else if is_key_pressed(KeyCode::Y) || (is_key_pressed(KeyCode::Z) && shift) {
            self.selection_start = None;
            self.perform_redo(code);
            true
        } else if is_key_pressed(KeyCode::C) {
            self.copy(code);
            true
        } else if is_key_pressed(KeyCode::X) {
            self.cut(code);
            true
        } else if is_key_pressed(KeyCode::V) {
            self.paste(code);
            true
        } else {
            false
        };
        if handled {
            drain_chars();
        }
        handled
    }

    /// Arrow/Home/End movement; Shift extends the selection, otherwise clears it.
    fn handle_navigation(&mut self, code: &str, shift: bool) {
        for &(key, mv) in NAV_KEYS.iter() {
            if self.key_fire(key) {
                if shift {
                    self.begin_or_extend_selection();
                } else {
                    self.selection_start = None;
                }
                self.cursor_pos = mv(code, self.cursor_pos);
            }
        }
    }

    /// Backspace / Delete / Enter / Tab / typed characters.
    fn handle_editing(&mut self, code: &mut String, ctrl: bool, nav_key: bool) {
        if self.key_fire(K_BKSP) {
            if self.delete_selection_if_active(code) {
                return;
            }
            if self.cursor_pos > 0 {
                self.push_undo_if_changed(code);
                let prev = move_left(code, self.cursor_pos);
                code.remove(prev);
                self.cursor_pos = prev;
                return;
            }
        }

        if self.key_fire(K_DEL) {
            if self.delete_selection_if_active(code) {
                return;
            }
            if self.cursor_pos < code.len() {
                self.push_undo_if_changed(code);
                code.remove(self.cursor_pos);
                return;
            }
        }

        if self.key_fire(K_ENTER) {
            self.delete_selection_if_active(code);
            self.push_undo_if_changed(code);
            self.insert_newline_with_indent(code);
            return;
        }

        if self.key_fire(K_TAB) && !ctrl {
            self.delete_selection_if_active(code);
            self.push_undo_if_changed(code);
            code.insert(self.cursor_pos, ' ');
            self.cursor_pos += 1;
            return;
        }

        if !ctrl {
            let mut inserted = false;
            while let Some(ch) = get_char_pressed() {
                if nav_key {
                    continue;
                }
                if (' '..='~').contains(&ch) {
                    if !inserted {
                        self.delete_selection_if_active(code);
                        self.push_undo_if_changed(code);
                        inserted = true;
                    }
                    code.insert(self.cursor_pos, ch);
                    self.cursor_pos += ch.len_utf8();
                }
            }
        }
    }

    /// Insert a newline carrying over the current line's indentation, plus two
    /// extra spaces after `then`/`do`/`else`/`repeat`/`function(...)`.
    fn insert_newline_with_indent(&mut self, code: &mut String) {
        let indent = {
            let lines = split_lines(code);
            let (cur_line, _) = cursor_to_line_col(self.cursor_pos, &lines);
            let current_line_text = lines.get(cur_line).copied().unwrap_or("");
            let extra = if ends_with_indent_keyword(current_line_text) {
                "  "
            } else {
                ""
            };
            format!("{}{}", leading_whitespace(current_line_text), extra)
        };
        let insert_str = format!("\n{}", indent);
        code.insert_str(self.cursor_pos, &insert_str);
        self.cursor_pos += insert_str.len();
    }

    // -----------------------------------------------------------------------
    // Selection
    // -----------------------------------------------------------------------

    /// True if a non-empty selection exists.
    fn has_selection(&self) -> bool {
        self.selection_start.is_some_and(|ss| ss != self.cursor_pos)
    }

    /// If a non-empty selection is active, delete it (pushing undo) and return
    /// true. Always clears `selection_start`.
    fn delete_selection_if_active(&mut self, code: &mut String) -> bool {
        let Some(ss) = self.selection_start.take() else {
            return false;
        };
        if ss == self.cursor_pos {
            return false;
        }
        self.push_undo_if_changed(code);
        self.cursor_pos = delete_selection(code, ss, self.cursor_pos);
        true
    }

    /// Begin a selection at the cursor if none is active.
    fn begin_or_extend_selection(&mut self) {
        if self.selection_start.is_none() {
            self.selection_start = Some(self.cursor_pos);
        }
    }

    // -----------------------------------------------------------------------
    // Search
    // -----------------------------------------------------------------------

    fn close_search(&mut self) {
        self.search_active = false;
        self.search_query.clear();
        self.search_results.clear();
        self.search_current = 0;
    }

    /// Keyboard handling while the search bar is open.
    fn handle_search_input(&mut self, code: &str) {
        let ctrl = ctrl_down();
        let shift = shift_down();

        if is_key_pressed(KeyCode::Escape) {
            self.close_search();
            drain_chars();
            return;
        }

        // Enter / Ctrl+G: next match; Shift+Enter: previous match
        if is_key_pressed(KeyCode::Enter)
            || is_key_pressed(KeyCode::KpEnter)
            || (ctrl && is_key_pressed(KeyCode::G))
        {
            if !self.search_results.is_empty() {
                let n = self.search_results.len();
                self.search_current = if shift && !ctrl {
                    (self.search_current + n - 1) % n
                } else {
                    (self.search_current + 1) % n
                };
                self.cursor_pos = self.search_results[self.search_current];
            }
            drain_chars();
            return;
        }

        if is_key_pressed(KeyCode::Backspace) {
            self.search_query.pop();
            self.update_search_results(code);
            drain_chars();
            return;
        }

        let mut changed = false;
        while let Some(ch) = get_char_pressed() {
            if (' '..='~').contains(&ch) {
                self.search_query.push(ch);
                changed = true;
            }
        }
        if changed {
            self.update_search_results(code);
        }
    }

    /// Recompute search results from the current query and code, and move the
    /// cursor to the current match.
    fn update_search_results(&mut self, code: &str) {
        self.search_results = find_all_matches(code, &self.search_query);
        if self.search_results.is_empty() {
            self.search_current = 0;
        } else {
            if self.search_current >= self.search_results.len() {
                self.search_current = 0;
            }
            self.cursor_pos = self.search_results[self.search_current];
        }
    }

    // -----------------------------------------------------------------------
    // Undo / redo
    // -----------------------------------------------------------------------

    fn trim_undo_stack(&mut self) {
        if self.undo_stack.len() > UNDO_MAX {
            self.undo_stack.remove(0);
        }
    }

    /// Save a specific snapshot onto the undo stack, clear redo, and cap size.
    fn push_undo_snapshot(&mut self, snapshot: String, cursor: usize) {
        self.undo_stack.push((snapshot, cursor));
        self.redo_stack.clear();
        self.trim_undo_stack();
    }

    /// Push the current (pre-mutation) state onto the undo stack. Call right
    /// before a destructive edit (cut, paste, etc.).
    fn push_undo(&mut self, code: &str) {
        self.push_undo_snapshot(code.to_string(), self.cursor_pos);
        self.last_undo_code = code.to_string();
    }

    /// Push the *previous checkpoint* onto the undo stack if the code has
    /// changed since then, coalescing rapid typing into fewer undo entries.
    fn push_undo_if_changed(&mut self, code: &str) {
        if code != self.last_undo_code {
            self.push_undo_snapshot(self.last_undo_code.clone(), self.cursor_pos);
            self.last_undo_code = code.to_string();
        }
    }

    fn perform_undo(&mut self, code: &mut String) {
        if let Some((snap_code, snap_cursor)) = self.undo_stack.pop() {
            self.redo_stack.push((code.clone(), self.cursor_pos));
            self.restore_snapshot(code, snap_code, snap_cursor);
        }
    }

    fn perform_redo(&mut self, code: &mut String) {
        if let Some((snap_code, snap_cursor)) = self.redo_stack.pop() {
            self.undo_stack.push((code.clone(), self.cursor_pos));
            self.trim_undo_stack();
            self.restore_snapshot(code, snap_code, snap_cursor);
        }
    }

    fn restore_snapshot(&mut self, code: &mut String, snap_code: String, snap_cursor: usize) {
        *code = snap_code;
        self.cursor_pos = snap_cursor.min(code.len());
        self.last_undo_code = code.clone();
    }

    // -----------------------------------------------------------------------
    // Clipboard
    // -----------------------------------------------------------------------

    /// Ctrl+C: copy the selection, or the current line if none.
    fn copy(&mut self, code: &str) {
        if self.has_selection() {
            let ss = self.selection_start.unwrap_or(self.cursor_pos);
            self.clipboard = selected_text(code, ss, self.cursor_pos).to_string();
        } else {
            self.clipboard_copy_line(code);
        }
    }

    /// Ctrl+X: cut the selection, or the current line if none.
    fn cut(&mut self, code: &mut String) {
        if self.has_selection() {
            let ss = self.selection_start.unwrap_or(self.cursor_pos);
            self.clipboard = selected_text(code, ss, self.cursor_pos).to_string();
            self.delete_selection_if_active(code);
        } else {
            self.selection_start = None;
            self.clipboard_cut_line(code);
        }
    }

    /// Ctrl+V: replace the selection with the clipboard, or paste as a new line.
    fn paste(&mut self, code: &mut String) {
        if self.clipboard.is_empty() {
            return;
        }
        if self.has_selection() {
            let ss = self.selection_start.take().unwrap_or(self.cursor_pos);
            self.push_undo(code);
            self.cursor_pos = delete_selection(code, ss, self.cursor_pos);
            code.insert_str(self.cursor_pos, &self.clipboard);
            self.cursor_pos += self.clipboard.len();
        } else {
            self.selection_start = None;
            self.clipboard_paste(code);
        }
    }

    /// Copy the current line to the internal clipboard.
    fn clipboard_copy_line(&mut self, code: &str) {
        let lines = split_lines(code);
        let (line_idx, _) = cursor_to_line_col(self.cursor_pos, &lines);
        if let Some(line) = lines.get(line_idx) {
            self.clipboard = line.to_string();
        }
    }

    /// Cut the current line: copy to clipboard and delete it (with its newline).
    fn clipboard_cut_line(&mut self, code: &mut String) {
        self.push_undo(code);

        let lines = split_lines(code);
        let (line_idx, _) = cursor_to_line_col(self.cursor_pos, &lines);
        if let Some(line) = lines.get(line_idx) {
            self.clipboard = line.to_string();
        }

        let mut line_start = line_start_offset(&lines, line_idx);
        let mut line_end = line_start + lines.get(line_idx).map_or(0, |l| l.len());
        if line_end < code.len() && code.as_bytes()[line_end] == b'\n' {
            line_end += 1;
        } else {
            // Last line without trailing newline: remove the preceding newline
            // instead to avoid leaving a trailing empty line.
            line_start = line_start.saturating_sub(1);
        }
        code.drain(line_start..line_end);
        self.cursor_pos = line_start.min(code.len());
    }

    /// Paste the clipboard as a new line below the current one.
    fn clipboard_paste(&mut self, code: &mut String) {
        if self.clipboard.is_empty() {
            return;
        }
        self.push_undo(code);

        let lines = split_lines(code);
        let (line_idx, _) = cursor_to_line_col(self.cursor_pos, &lines);
        let mut insert_pos = line_start_offset(&lines, line_idx + 1);
        if insert_pos > code.len() {
            // On the last line without a trailing newline: add one first.
            code.push('\n');
            insert_pos = code.len();
        }
        let paste_text = format!("{}\n", self.clipboard);
        code.insert_str(insert_pos, &paste_text);
        self.cursor_pos = insert_pos;
    }
}

// ---------------------------------------------------------------------------
// Drawing helpers
// ---------------------------------------------------------------------------

/// Fill the text-row background from column `col_start` to `col_end` (exclusive).
fn fill_cols(console: &mut Console, py: i32, col_start: usize, col_end: usize, color: u8) {
    let hx1 = GUTTER_PX + col_start as i32 * CHAR_W;
    let hx2 = GUTTER_PX + col_end as i32 * CHAR_W - 1;
    console.rectfill(hx1, py, hx2, py + CHAR_H - 2, Some(color));
}

/// Highlight the part of `[sel_lo, sel_hi)` that falls on this row. A
/// selection running past the end of the line also covers the newline (one
/// extra column).
fn draw_selection_row(
    console: &mut Console,
    py: i32,
    line: &str,
    line_byte_start: usize,
    sel_lo: usize,
    sel_hi: usize,
) {
    let line_byte_end = line_byte_start + line.len();
    if sel_lo >= sel_hi || sel_lo > line_byte_end || sel_hi <= line_byte_start {
        return;
    }
    let col_start = sel_lo.saturating_sub(line_byte_start).min(CODE_COLS);
    let col_end = if sel_hi > line_byte_end {
        line.len() + 1
    } else {
        sel_hi - line_byte_start
    }
    .min(CODE_COLS + 1);
    if col_end > col_start {
        fill_cols(console, py, col_start, col_end, SELECTION_COLOR);
    }
}

/// "c:<chars> t:~<tokens>" in the top-right of the top bar; pink when over
/// the PICO-8 limits.
fn draw_token_count(console: &mut Console, code: &str) {
    let char_count = code.len();
    let token_count = count_tokens(code);
    let count_str = format!("c:{} t:~{}", char_count, token_count);
    let cx = SW - count_str.len() as i32 * CHAR_W - 1;
    console.rectfill(cx - 1, 0, SW - 1, TOP_BAR_H - 2, Some(8));
    let over_limit = char_count > PICO8_CHAR_LIMIT || token_count > PICO8_TOKEN_LIMIT;
    let fg: u8 = if over_limit { 14 } else { 7 };
    console.print(&count_str, Some(cx), Some(1), Some(fg));
}

#[cfg(test)]
mod tests {
    use super::*;

    // ===================================================================
    // split_lines
    // ===================================================================

    #[test]
    fn split_lines_empty() {
        let lines = split_lines("");
        assert_eq!(lines, vec![""]);
    }

    #[test]
    fn split_lines_single_line() {
        let lines = split_lines("hello");
        assert_eq!(lines, vec!["hello"]);
    }

    #[test]
    fn split_lines_multiple_lines() {
        let lines = split_lines("aaa\nbbb\nccc");
        assert_eq!(lines, vec!["aaa", "bbb", "ccc"]);
    }

    #[test]
    fn split_lines_trailing_newline() {
        let lines = split_lines("line1\nline2\n");
        assert_eq!(lines, vec!["line1", "line2", ""]);
    }

    // ===================================================================
    // cursor_to_line_col
    // ===================================================================

    #[test]
    fn cursor_to_line_col_start() {
        let lines = split_lines("abc\ndef");
        assert_eq!(cursor_to_line_col(0, &lines), (0, 0));
    }

    #[test]
    fn cursor_to_line_col_middle_of_first_line() {
        let lines = split_lines("abc\ndef");
        assert_eq!(cursor_to_line_col(2, &lines), (0, 2));
    }

    #[test]
    fn cursor_to_line_col_end_of_first_line() {
        let lines = split_lines("abc\ndef");
        assert_eq!(cursor_to_line_col(3, &lines), (0, 3));
    }

    #[test]
    fn cursor_to_line_col_second_line_start() {
        let lines = split_lines("abc\ndef");
        // position 4 = past "abc\n"
        assert_eq!(cursor_to_line_col(4, &lines), (1, 0));
    }

    #[test]
    fn cursor_to_line_col_second_line_middle() {
        let lines = split_lines("abc\ndef");
        assert_eq!(cursor_to_line_col(5, &lines), (1, 1));
    }

    #[test]
    fn cursor_to_line_col_past_end() {
        let lines = split_lines("abc\ndef");
        // total length = 7, cursor at 100 should clamp to end of last line
        assert_eq!(cursor_to_line_col(100, &lines), (1, 3));
    }

    // ===================================================================
    // line_col_to_pos
    // ===================================================================

    #[test]
    fn line_col_to_pos_basic() {
        let code = "abc\ndef";
        let lines = split_lines(code);
        // line 0, col 2 => position 2
        assert_eq!(line_col_to_pos(&lines, 0, 2, code.len()), 2);
    }

    #[test]
    fn line_col_to_pos_second_line() {
        let code = "abc\ndef";
        let lines = split_lines(code);
        // line 1, col 1 => position 5 (past "abc\n" + 1)
        assert_eq!(line_col_to_pos(&lines, 1, 1, code.len()), 5);
    }

    #[test]
    fn line_col_to_pos_clamped_column() {
        let code = "abc\ndef";
        let lines = split_lines(code);
        // line 0, col 999 should clamp to end of line 0 (pos 3)
        assert_eq!(line_col_to_pos(&lines, 0, 999, code.len()), 3);
    }

    #[test]
    fn line_col_to_pos_clamped_line() {
        let code = "abc\ndef";
        let lines = split_lines(code);
        // line 999 should clamp to last line (line 1), col 0 => position 4
        assert_eq!(line_col_to_pos(&lines, 999, 0, code.len()), 4);
    }

    #[test]
    fn line_col_to_pos_clamped_to_code_len() {
        let code = "abc\ndef";
        let lines = split_lines(code);
        // line 999, col 999 => should not exceed code.len() = 7
        assert_eq!(line_col_to_pos(&lines, 999, 999, code.len()), code.len());
    }

    // ===================================================================
    // Round-trip: cursor_to_line_col <-> line_col_to_pos
    // ===================================================================

    #[test]
    fn round_trip_cursor_line_col() {
        let code = "hello\nworld\nfoo";
        let lines = split_lines(code);
        for pos in 0..=code.len() {
            let (line, col) = cursor_to_line_col(pos, &lines);
            let recovered = line_col_to_pos(&lines, line, col, code.len());
            assert_eq!(
                recovered, pos,
                "round-trip failed for pos={} -> ({},{}) -> {}",
                pos, line, col, recovered
            );
        }
    }

    #[test]
    fn round_trip_line_col_cursor() {
        let code = "ab\ncde\nf";
        let lines = split_lines(code);
        for line in 0..lines.len() {
            for col in 0..=lines[line].len() {
                let pos = line_col_to_pos(&lines, line, col, code.len());
                let (rline, rcol) = cursor_to_line_col(pos, &lines);
                assert_eq!(
                    (rline, rcol),
                    (line, col),
                    "round-trip failed for ({},{}) -> {} -> ({},{})",
                    line,
                    col,
                    pos,
                    rline,
                    rcol
                );
            }
        }
    }

    // ===================================================================
    // Selection range calculation
    // ===================================================================

    #[test]
    fn selection_range_forward() {
        // selection_start < cursor_pos
        assert_eq!(selection_range(3, 10), (3, 10));
    }

    #[test]
    fn selection_range_backward() {
        // selection_start > cursor_pos
        assert_eq!(selection_range(10, 3), (3, 10));
    }

    #[test]
    fn selection_range_same() {
        // selection_start == cursor_pos (empty selection)
        assert_eq!(selection_range(5, 5), (5, 5));
    }

    #[test]
    fn selection_range_zero() {
        assert_eq!(selection_range(0, 0), (0, 0));
    }

    // ===================================================================
    // Selected text extraction
    // ===================================================================

    #[test]
    fn selected_text_forward() {
        let code = "hello world";
        assert_eq!(selected_text(code, 0, 5), "hello");
    }

    #[test]
    fn selected_text_backward() {
        let code = "hello world";
        assert_eq!(selected_text(code, 5, 0), "hello");
    }

    #[test]
    fn selected_text_empty() {
        let code = "hello world";
        assert_eq!(selected_text(code, 3, 3), "");
    }

    #[test]
    fn selected_text_full() {
        let code = "hello";
        assert_eq!(selected_text(code, 0, 5), "hello");
    }

    #[test]
    fn selected_text_with_newline() {
        let code = "abc\ndef";
        assert_eq!(selected_text(code, 2, 5), "c\nd");
    }

    #[test]
    fn selected_text_clamped_to_code_len() {
        let code = "hi";
        // Out of bounds should clamp
        assert_eq!(selected_text(code, 0, 100), "hi");
    }

    // ===================================================================
    // Delete selection logic
    // ===================================================================

    #[test]
    fn delete_selection_forward() {
        let mut code = "hello world".to_string();
        let new_cursor = delete_selection(&mut code, 5, 11);
        assert_eq!(code, "hello");
        assert_eq!(new_cursor, 5);
    }

    #[test]
    fn delete_selection_backward() {
        let mut code = "hello world".to_string();
        let new_cursor = delete_selection(&mut code, 11, 5);
        assert_eq!(code, "hello");
        assert_eq!(new_cursor, 5);
    }

    #[test]
    fn delete_selection_empty() {
        let mut code = "hello".to_string();
        let new_cursor = delete_selection(&mut code, 3, 3);
        assert_eq!(code, "hello");
        assert_eq!(new_cursor, 3);
    }

    #[test]
    fn delete_selection_all() {
        let mut code = "hello".to_string();
        let new_cursor = delete_selection(&mut code, 0, 5);
        assert_eq!(code, "");
        assert_eq!(new_cursor, 0);
    }

    #[test]
    fn delete_selection_with_newlines() {
        let mut code = "abc\ndef\nghi".to_string();
        // Delete "c\ndef\ng"
        let new_cursor = delete_selection(&mut code, 2, 9);
        assert_eq!(code, "abhi");
        assert_eq!(new_cursor, 2);
    }

    #[test]
    fn delete_selection_from_start() {
        let mut code = "hello world".to_string();
        let new_cursor = delete_selection(&mut code, 0, 6);
        assert_eq!(code, "world");
        assert_eq!(new_cursor, 0);
    }

    // ===================================================================
    // Search: find_all_matches
    // ===================================================================

    #[test]
    fn search_find_occurrences_basic() {
        let code = "hello world hello";
        let results = find_all_matches(code, "hello");
        assert_eq!(results, vec![0, 12]);
    }

    #[test]
    fn search_find_occurrences_correct_byte_offsets() {
        let code = "abc abc abc";
        let results = find_all_matches(code, "abc");
        assert_eq!(results, vec![0, 4, 8]);
        // Verify each offset points to the right substring
        for &offset in &results {
            assert_eq!(&code[offset..offset + 3], "abc");
        }
    }

    #[test]
    fn search_find_case_insensitive() {
        let code = "Hello HELLO hello";
        let results = find_all_matches(code, "hello");
        assert_eq!(results.len(), 3);
    }

    #[test]
    fn search_find_no_match() {
        let code = "hello world";
        let results = find_all_matches(code, "xyz");
        assert!(results.is_empty());
    }

    #[test]
    fn search_empty_query_returns_no_results() {
        let code = "hello world";
        let results = find_all_matches(code, "");
        assert!(results.is_empty());
    }

    #[test]
    fn search_empty_code() {
        let results = find_all_matches("", "hello");
        assert!(results.is_empty());
    }

    #[test]
    fn search_single_char() {
        let code = "abacada";
        let results = find_all_matches(code, "a");
        assert_eq!(results, vec![0, 2, 4, 6]);
    }

    #[test]
    fn search_overlapping_not_found() {
        // Non-overlapping search: "aa" in "aaaa" should find offsets 0, 2
        let code = "aaaa";
        let results = find_all_matches(code, "aa");
        assert_eq!(results, vec![0, 2]);
    }

    #[test]
    fn search_multiline() {
        let code = "line1\nfoo\nline2\nfoo";
        let results = find_all_matches(code, "foo");
        assert_eq!(results, vec![6, 16]);
    }

    // ===================================================================
    // Auto-indent: leading_whitespace
    // ===================================================================

    #[test]
    fn leading_whitespace_none() {
        assert_eq!(leading_whitespace("hello"), "");
    }

    #[test]
    fn leading_whitespace_spaces() {
        assert_eq!(leading_whitespace("  hello"), "  ");
    }

    #[test]
    fn leading_whitespace_all_spaces() {
        assert_eq!(leading_whitespace("    "), "    ");
    }

    #[test]
    fn leading_whitespace_empty() {
        assert_eq!(leading_whitespace(""), "");
    }

    // ===================================================================
    // Auto-indent: ends_with_indent_keyword
    // ===================================================================

    #[test]
    fn indent_after_then() {
        assert!(ends_with_indent_keyword("if x then"));
    }

    #[test]
    fn indent_after_do() {
        assert!(ends_with_indent_keyword("for i=1,10 do"));
    }

    #[test]
    fn indent_after_else() {
        assert!(ends_with_indent_keyword("else"));
    }

    #[test]
    fn indent_after_repeat() {
        assert!(ends_with_indent_keyword("repeat"));
    }

    #[test]
    fn indent_after_function_parens() {
        assert!(ends_with_indent_keyword("function _init()"));
    }

    #[test]
    fn indent_after_function_with_args() {
        assert!(ends_with_indent_keyword("function foo(x, y)"));
    }

    #[test]
    fn no_indent_after_end() {
        assert!(!ends_with_indent_keyword("end"));
    }

    #[test]
    fn no_indent_after_plain_statement() {
        assert!(!ends_with_indent_keyword("x = 1"));
    }

    #[test]
    fn indent_after_then_with_comment() {
        assert!(ends_with_indent_keyword("if x then -- check"));
    }

    #[test]
    fn no_indent_partial_keyword() {
        // "mythen" should not trigger indent (it ends with "then" but is part of an identifier)
        assert!(!ends_with_indent_keyword("mythen"));
    }

    // ===================================================================
    // Cursor movement
    // ===================================================================

    #[test]
    fn move_left_right_at_bounds() {
        assert_eq!(move_left("abc", 0), 0);
        assert_eq!(move_left("abc", 2), 1);
        assert_eq!(move_right("abc", 3), 3);
        assert_eq!(move_right("abc", 1), 2);
    }

    #[test]
    fn move_left_right_respect_utf8() {
        let code = "aé b"; // 'é' is 2 bytes
        assert_eq!(move_right(code, 1), 3);
        assert_eq!(move_left(code, 3), 1);
    }

    #[test]
    fn move_up_down_keep_column_and_clamp() {
        let code = "abcd\nxy\nlonger";
        // From col 3 on line 2 ("longer") up to "xy" clamps to col 2.
        assert_eq!(move_up(code, 8 + 3), 5 + 2);
        // From line 0 up stays put.
        assert_eq!(move_up(code, 2), 2);
        // Down from "abcd" col 3 to "xy" clamps to col 2.
        assert_eq!(move_down(code, 3), 5 + 2);
        // Down from the last line stays put.
        assert_eq!(move_down(code, 10), 10);
    }

    #[test]
    fn move_line_start_end() {
        let code = "abc\ndefg";
        assert_eq!(move_line_start(code, 6), 4);
        assert_eq!(move_line_end(code, 4), 8);
        assert_eq!(move_line_end(code, 1), 3);
    }

    // ===================================================================
    // Undo/Redo stack operations
    // ===================================================================

    /// Helper: create a CodeEditor with specific code for testing.
    fn editor_with_code(code: &str) -> (CodeEditor, String) {
        (CodeEditor::new(code), code.to_string())
    }

    #[test]
    fn undo_push_and_pop() {
        let (mut ed, mut code) = editor_with_code("hello");
        // Simulate an edit: change the code, then push undo
        let old_code = code.clone();
        code = "hello world".to_string();
        ed.cursor_pos = 11;
        ed.push_undo_if_changed(&code);
        // Undo stack should have 1 entry (the old state)
        assert_eq!(ed.undo_stack.len(), 1);
        assert_eq!(ed.undo_stack[0].0, old_code);

        // Now perform undo
        ed.perform_undo(&mut code);
        assert_eq!(code, "hello");
        assert_eq!(ed.undo_stack.len(), 0);
        assert_eq!(ed.redo_stack.len(), 1);
    }

    #[test]
    fn redo_after_undo() {
        let (mut ed, _) = editor_with_code("aaa");
        let mut code = "bbb".to_string();
        ed.cursor_pos = 3;
        ed.push_undo_if_changed(&code);
        ed.perform_undo(&mut code);
        assert_eq!(code, "aaa");
        ed.perform_redo(&mut code);
        assert_eq!(code, "bbb");
        assert_eq!(ed.undo_stack.len(), 1);
        assert_eq!(ed.redo_stack.len(), 0);
    }

    #[test]
    fn undo_on_empty_stack() {
        let (mut ed, mut code) = editor_with_code("test");
        ed.perform_undo(&mut code);
        assert_eq!(code, "test");
        assert_eq!(ed.redo_stack.len(), 0);
    }

    #[test]
    fn redo_on_empty_stack() {
        let (mut ed, mut code) = editor_with_code("test");
        ed.perform_redo(&mut code);
        assert_eq!(code, "test");
        assert_eq!(ed.undo_stack.len(), 0);
    }

    #[test]
    fn new_edit_clears_redo_stack() {
        let (mut ed, _) = editor_with_code("aaa");
        let mut code = "bbb".to_string();
        ed.push_undo_if_changed(&code);
        // undo to get "aaa" back, "bbb" goes to redo
        ed.perform_undo(&mut code);
        assert_eq!(ed.redo_stack.len(), 1);
        // Now make a new edit
        code = "ccc".to_string();
        ed.push_undo_if_changed(&code);
        assert_eq!(ed.redo_stack.len(), 0);
    }

    #[test]
    fn undo_stack_size_limit() {
        let (mut ed, _) = editor_with_code("start");
        for i in 0..UNDO_MAX + 20 {
            let code = format!("version_{}", i);
            ed.push_undo_if_changed(&code);
        }
        assert!(ed.undo_stack.len() <= UNDO_MAX);
    }

    #[test]
    fn push_undo_no_duplicate() {
        let (mut ed, code) = editor_with_code("hello");
        // Push without changing code -- should not add to stack
        ed.push_undo_if_changed(&code);
        assert_eq!(ed.undo_stack.len(), 0);
    }

    // ===================================================================
    // Clipboard copy/paste of lines
    // ===================================================================

    #[test]
    fn clipboard_copy_line_basic() {
        let (mut ed, code) = editor_with_code("line1\nline2\nline3");
        ed.cursor_pos = 6; // start of "line2"
        ed.clipboard_copy_line(&code);
        assert_eq!(ed.clipboard, "line2");
    }

    #[test]
    fn clipboard_copy_first_line() {
        let (mut ed, code) = editor_with_code("first\nsecond");
        ed.cursor_pos = 0;
        ed.clipboard_copy_line(&code);
        assert_eq!(ed.clipboard, "first");
    }

    #[test]
    fn clipboard_copy_last_line() {
        let (mut ed, code) = editor_with_code("a\nb\nc");
        ed.cursor_pos = 4; // at "c"
        ed.clipboard_copy_line(&code);
        assert_eq!(ed.clipboard, "c");
    }

    #[test]
    fn clipboard_cut_line_removes_line() {
        let (mut ed, mut code) = editor_with_code("line1\nline2\nline3");
        ed.cursor_pos = 6; // start of "line2"
        ed.clipboard_cut_line(&mut code);
        assert_eq!(ed.clipboard, "line2");
        // "line2\n" should be removed
        assert_eq!(code, "line1\nline3");
    }

    #[test]
    fn clipboard_cut_last_line() {
        let (mut ed, mut code) = editor_with_code("aaa\nbbb");
        ed.cursor_pos = 4; // at "bbb"
        ed.clipboard_cut_line(&mut code);
        assert_eq!(ed.clipboard, "bbb");
        assert_eq!(code, "aaa");
    }

    #[test]
    fn clipboard_paste_inserts_line() {
        let (mut ed, mut code) = editor_with_code("line1\nline3");
        ed.clipboard = "line2".to_string();
        ed.cursor_pos = 0; // on "line1"
        ed.clipboard_paste(&mut code);
        // Should insert "line2\n" after line1
        assert_eq!(code, "line1\nline2\nline3");
    }

    #[test]
    fn clipboard_paste_empty_clipboard() {
        let (mut ed, mut code) = editor_with_code("hello");
        ed.clipboard = String::new();
        let original = code.clone();
        ed.clipboard_paste(&mut code);
        assert_eq!(code, original);
    }

    #[test]
    fn clipboard_cut_pushes_undo() {
        let (mut ed, mut code) = editor_with_code("aaa\nbbb");
        ed.cursor_pos = 4;
        ed.clipboard_cut_line(&mut code);
        assert_eq!(ed.undo_stack.len(), 1);
        assert_eq!(ed.undo_stack[0].0, "aaa\nbbb");
    }

    #[test]
    fn clipboard_paste_pushes_undo() {
        let (mut ed, mut code) = editor_with_code("hello");
        ed.clipboard = "world".to_string();
        ed.cursor_pos = 0;
        ed.clipboard_paste(&mut code);
        assert_eq!(ed.undo_stack.len(), 1);
        assert_eq!(ed.undo_stack[0].0, "hello");
    }

    #[test]
    fn copy_prefers_selection_over_line() {
        let (mut ed, code) = editor_with_code("hello world");
        ed.selection_start = Some(6);
        ed.cursor_pos = 11;
        ed.copy(&code);
        assert_eq!(ed.clipboard, "world");
    }

    #[test]
    fn cut_selection_removes_it_and_pushes_undo() {
        let (mut ed, mut code) = editor_with_code("hello world");
        ed.selection_start = Some(5);
        ed.cursor_pos = 11;
        ed.cut(&mut code);
        assert_eq!(ed.clipboard, " world");
        assert_eq!(code, "hello");
        assert_eq!(ed.cursor_pos, 5);
        assert_eq!(ed.selection_start, None);
        // Deleting a selection coalesces with typing: the pre-cut checkpoint is
        // kept in `last_undo_code` and pushed by the next edit.
        assert_eq!(ed.last_undo_code, "hello world");
        code.push('!');
        ed.push_undo_if_changed(&code);
        assert_eq!(ed.undo_stack.len(), 1);
        assert_eq!(ed.undo_stack[0].0, "hello world");
    }

    #[test]
    fn paste_replaces_selection_inline() {
        let (mut ed, mut code) = editor_with_code("hello world");
        ed.clipboard = "there".to_string();
        ed.selection_start = Some(6);
        ed.cursor_pos = 11;
        ed.paste(&mut code);
        assert_eq!(code, "hello there");
        assert_eq!(ed.cursor_pos, 11);
        assert_eq!(ed.selection_start, None);
    }

    // ===================================================================
    // Selection state
    // ===================================================================

    #[test]
    fn selection_cleared_on_construction() {
        let (ed, _) = editor_with_code("");
        assert_eq!(ed.selection_start, None);
    }

    #[test]
    fn delete_selection_if_active_handles_empty_and_real_selection() {
        let (mut ed, mut code) = editor_with_code("hello world");
        // Empty selection: cleared, nothing deleted.
        ed.selection_start = Some(3);
        ed.cursor_pos = 3;
        assert!(!ed.delete_selection_if_active(&mut code));
        assert_eq!(ed.selection_start, None);
        assert_eq!(code, "hello world");
        // Backward selection: deleted, cursor at the low end.
        ed.selection_start = Some(11);
        ed.cursor_pos = 5;
        assert!(ed.delete_selection_if_active(&mut code));
        assert_eq!(code, "hello");
        assert_eq!(ed.cursor_pos, 5);
        assert_eq!(ed.selection_start, None);
    }

    // ===================================================================
    // Search: next/previous wrapping
    // ===================================================================

    #[test]
    fn search_next_wraps_around() {
        let (mut ed, code) = editor_with_code("abc abc abc");
        ed.search_active = true;
        ed.search_query = "abc".to_string();
        ed.update_search_results(&code);
        assert_eq!(ed.search_results.len(), 3);
        assert_eq!(ed.search_current, 0);

        ed.search_current = (ed.search_current + 1) % ed.search_results.len();
        assert_eq!(ed.search_current, 1);
        ed.search_current = (ed.search_current + 1) % ed.search_results.len();
        assert_eq!(ed.search_current, 2);
        // Wrap around to 0
        ed.search_current = (ed.search_current + 1) % ed.search_results.len();
        assert_eq!(ed.search_current, 0);
    }

    #[test]
    fn search_previous_wraps_around() {
        let (mut ed, code) = editor_with_code("abc abc abc");
        ed.search_active = true;
        ed.search_query = "abc".to_string();
        ed.update_search_results(&code);
        let n = ed.search_results.len();
        assert_eq!(n, 3);

        // Start at 0, go previous => wraps to last
        ed.search_current = (ed.search_current + n - 1) % n;
        assert_eq!(ed.search_current, 2);
        ed.search_current = (ed.search_current + n - 1) % n;
        assert_eq!(ed.search_current, 1);
    }

    #[test]
    fn search_cursor_jumps_to_match() {
        let (mut ed, code) = editor_with_code("hello world hello");
        ed.search_active = true;
        ed.search_query = "hello".to_string();
        ed.update_search_results(&code);
        assert_eq!(ed.search_results.len(), 2);
        // cursor should be at first match
        assert_eq!(ed.cursor_pos, 0);

        ed.search_current = (ed.search_current + 1) % ed.search_results.len();
        ed.cursor_pos = ed.search_results[ed.search_current];
        assert_eq!(ed.cursor_pos, 12);
    }

    // ===================================================================
    // Auto-indent: integration via editor
    // ===================================================================

    #[test]
    fn auto_indent_matches_previous_line() {
        let (mut ed, mut code) = editor_with_code("  hello");
        ed.cursor_pos = 7; // end of "  hello"
        ed.insert_newline_with_indent(&mut code);
        assert_eq!(code, "  hello\n  ");
        // Cursor should be at position 10 (after the indent)
        assert_eq!(ed.cursor_pos, 10);
    }

    #[test]
    fn auto_indent_extra_after_then() {
        let (mut ed, mut code) = editor_with_code("  if x then");
        ed.cursor_pos = 11; // end of "  if x then"
        ed.insert_newline_with_indent(&mut code);
        assert_eq!(code, "  if x then\n    ");
        assert_eq!(ed.cursor_pos, 16);
    }

    #[test]
    fn auto_indent_extra_after_do() {
        let (mut ed, mut code) = editor_with_code("for i=1,10 do");
        ed.cursor_pos = code.len();
        ed.insert_newline_with_indent(&mut code);
        assert_eq!(code, "for i=1,10 do\n  ");
    }

    #[test]
    fn auto_indent_extra_after_function() {
        let (mut ed, mut code) = editor_with_code("function _init()");
        ed.cursor_pos = code.len();
        ed.insert_newline_with_indent(&mut code);
        assert_eq!(code, "function _init()\n  ");
    }

    // ===================================================================
    // Search state
    // ===================================================================

    #[test]
    fn search_fields_default() {
        let (ed, _) = editor_with_code("");
        assert!(!ed.search_active);
        assert!(ed.search_query.is_empty());
        assert!(ed.search_results.is_empty());
        assert_eq!(ed.search_current, 0);
    }

    #[test]
    fn search_update_results_populates() {
        let (mut ed, code) = editor_with_code("foo bar foo baz foo");
        ed.search_query = "foo".to_string();
        ed.update_search_results(&code);
        assert_eq!(ed.search_results.len(), 3);
        assert_eq!(ed.search_results[0], 0);
        assert_eq!(ed.search_results[1], 8);
        assert_eq!(ed.search_results[2], 16);
    }

    #[test]
    fn search_update_results_empty_query() {
        let (mut ed, code) = editor_with_code("foo bar");
        ed.search_query = String::new();
        ed.update_search_results(&code);
        assert!(ed.search_results.is_empty());
    }

    #[test]
    fn close_search_clears_state() {
        let (mut ed, _) = editor_with_code("test code");
        ed.search_active = true;
        ed.search_query = "test".to_string();
        ed.search_results = vec![0];
        ed.search_current = 0;

        ed.close_search();

        assert!(!ed.search_active);
        assert!(ed.search_query.is_empty());
        assert!(ed.search_results.is_empty());
        assert_eq!(ed.search_current, 0);
    }
}
