use macroquad::prelude::*;
use spark_core::audio::{Sfx, MusicPattern, NUM_SFX, NUM_MUSIC, NOTES_PER_SFX};
use spark_core::console::Console;

// ---------------------------------------------------------------------------
// Public types
// ---------------------------------------------------------------------------

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum EditorTab {
    Code,
    Sprite,
    Map,
    Sfx,
    Music,
}

pub enum EditorAction {
    None,
    RunGame,
    SaveCart,
    SaveCartAs,
    NewCart,
    LoadCart,
}

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

const CHAR_W: i32 = spark_core::font::CHAR_W;
const CHAR_H: i32 = spark_core::font::CHAR_H;

/// Top bar occupies rows 0..=6 (7 pixels tall).
const TOP_BAR_H: i32 = 7;
/// Main editing area starts at this Y.
const MAIN_Y: i32 = TOP_BAR_H;
/// Screen dimensions.
const SW: i32 = 128;
const SH: i32 = 128;

/// Line-number gutter width in characters (e.g. " 1 ").
const GUTTER_CHARS: usize = 3;
const GUTTER_PX: i32 = GUTTER_CHARS as i32 * CHAR_W;

/// Code editor: visible columns and rows.
const CODE_COLS: usize = ((SW - GUTTER_PX) / CHAR_W) as usize;
const CODE_ROWS: usize = ((SH - MAIN_Y) / CHAR_H) as usize;

/// Sprite editor constants.
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

/// Map editor constants.
const MAP_VIEW_X: i32 = 0;
const MAP_VIEW_Y: i32 = MAIN_Y + 1;
const MAP_COLS: usize = 16;
const MAP_ROWS: usize = 14;

/// SFX editor constants.
const SFX_ROLL_X: i32 = 0;
const SFX_ROLL_Y: i32 = 18;
const SFX_ROLL_W: i32 = 128;
const SFX_ROLL_H: i32 = 64;
const SFX_NOTE_W: i32 = 4; // 32 notes * 4 = 128 pixels wide

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

// ---------------------------------------------------------------------------
// Default source code
// ---------------------------------------------------------------------------

const DEFAULT_CODE: &str = "\
-- spark fantasy console
-- write your game here!

function _init()
 cls()
end

function _update()
end

function _draw()
 cls(1)
 print(\"welcome to spark!\",16,60,7)
end
";

// ---------------------------------------------------------------------------
// Free helper functions for cursor math (avoids borrow-checker issues)
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
    for i in 0..line {
        pos += lines[i].len() + 1;
    }
    let max_col = lines.get(line).map_or(0, |l| l.len());
    pos += col.min(max_col);
    pos.min(code_len)
}

// ---------------------------------------------------------------------------
// Free helper functions for sprite / map math
// ---------------------------------------------------------------------------

/// Compute the pixel origin (top-left corner) on the sprite sheet for a given sprite index.
/// The sprite sheet is 16 sprites wide and 16 sprites tall, each sprite 8x8 pixels.
fn sprite_origin(sprite_idx: u8) -> (i32, i32) {
    let col = (sprite_idx as i32) % 16;
    let row = (sprite_idx as i32) / 16;
    (col * 8, row * 8)
}

/// Convert mouse coordinates in the zoomed sprite view to 8x8 pixel coordinates.
/// Returns `None` if the mouse is outside the zoomed view area.
fn zoom_to_sprite_pixel(mouse_x: i32, mouse_y: i32) -> Option<(i32, i32)> {
    if mouse_x < SPR_ZOOM_X
        || mouse_x >= SPR_ZOOM_X + SPR_ZOOM_SIZE
        || mouse_y < SPR_ZOOM_Y
        || mouse_y >= SPR_ZOOM_Y + SPR_ZOOM_SIZE
    {
        return None;
    }
    let px = (mouse_x - SPR_ZOOM_X) / SPR_ZOOM;
    let py = (mouse_y - SPR_ZOOM_Y) / SPR_ZOOM;
    if px >= 0 && px < 8 && py >= 0 && py < 8 {
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
    if mouse_y < PAL_Y || mouse_y >= PAL_Y + PAL_SWATCH || mouse_x < 0 || mouse_x >= 128 {
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
    if map_x >= 0 && map_x < 128 && map_y >= 0 && map_y < 64 {
        Some((map_x, map_y))
    } else {
        None
    }
}

// ---------------------------------------------------------------------------
// Free helper functions for SFX editor
// ---------------------------------------------------------------------------

/// Convert a pitch value (0-63) to a Y pixel coordinate within the piano roll area.
/// Pitch 0 is at the bottom, pitch 63 is at the top.
fn sfx_pitch_to_y(pitch: u8) -> i32 {
    let clamped = (pitch as i32).min(63).max(0);
    SFX_ROLL_Y + SFX_ROLL_H - 1 - (clamped * SFX_ROLL_H / 64)
}

/// Convert a click in the SFX piano roll area to a (note_index, pitch) pair.
/// Returns None if the click is outside the piano roll area.
fn sfx_click_to_note(mouse_x: i32, mouse_y: i32) -> Option<(usize, u8)> {
    if mouse_x < SFX_ROLL_X
        || mouse_x >= SFX_ROLL_X + SFX_ROLL_W
        || mouse_y < SFX_ROLL_Y
        || mouse_y >= SFX_ROLL_Y + SFX_ROLL_H
    {
        return None;
    }
    let note_idx = ((mouse_x - SFX_ROLL_X) / SFX_NOTE_W) as usize;
    if note_idx >= NOTES_PER_SFX {
        return None;
    }
    let rel_y = mouse_y - SFX_ROLL_Y;
    let pitch = ((SFX_ROLL_H - 1 - rel_y) * 64 / SFX_ROLL_H) as u8;
    let pitch = pitch.min(63);
    Some((note_idx, pitch))
}

/// Map a waveform index (0-7) to a PICO-8 display color.
fn waveform_color(waveform: u8) -> u8 {
    match waveform & 0x07 {
        0 => 12, // triangle - light blue
        1 => 13, // tilted saw - indigo
        2 => 9,  // saw - orange
        3 => 11, // square - green
        4 => 10, // pulse - yellow
        5 => 14, // organ - pink
        6 => 8,  // noise - red
        7 => 15, // phaser - peach
        _ => 6,
    }
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
fn selected_text<'a>(code: &'a str, sel_start: usize, cursor_pos: usize) -> &'a str {
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
// Token counting
// ---------------------------------------------------------------------------

/// Keywords and operators that count as free punctuation in PICO-8 (0 tokens).
/// Parentheses, commas, semicolons, and dots are free.
const FREE_PUNCTUATION: &[u8] = b"(),.;:[]{}";

/// Approximate the PICO-8 token count for `code`.
/// This uses the syntax highlighting tokenizer per line and counts tokens
/// according to PICO-8 rules:
/// - Comments and whitespace = 0 tokens
/// - Each keyword, identifier, number, string literal = 1 token
/// - Each operator = 1 token
/// - Free punctuation (parentheses, commas, semicolons, colons, dots, brackets) = 0 tokens
fn count_tokens(code: &str) -> usize {
    let mut count: usize = 0;
    for line in code.split('\n') {
        let tokens = tokenize_line(line);
        for tok in &tokens {
            match tok.kind {
                TokenKind::Comment => {} // free
                TokenKind::Keyword | TokenKind::ApiFunc | TokenKind::Number | TokenKind::StringLit => {
                    count += 1;
                }
                TokenKind::Default => {
                    // Count identifiers and operators within Default tokens.
                    // Whitespace and free punctuation are free.
                    let text = &line[tok.start..tok.end];
                    for ch in text.chars() {
                        if ch.is_whitespace() {
                            continue;
                        }
                        if FREE_PUNCTUATION.contains(&(ch as u8)) {
                            continue;
                        }
                        // Operators and other non-free punctuation each cost 1 token
                        if !ch.is_ascii_alphanumeric() && ch != '_' {
                            count += 1;
                            continue;
                        }
                        // If we encounter an alphanumeric char in a Default token,
                        // it's an identifier (already counted as one Default span).
                        // We count the whole span as 1 token and break.
                        count += 1;
                        break;
                    }
                }
            }
        }
    }
    count
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

// ---------------------------------------------------------------------------
// Syntax highlighting
// ---------------------------------------------------------------------------

/// Token categories for syntax highlighting.
#[derive(Debug, PartialEq, Clone, Copy)]
enum TokenKind {
    Default,  // color 6 (light gray)
    Comment,  // color 13 (dark gray/blue)
    StringLit, // color 4 (brown)
    Number,   // color 14 (pink/mauve)
    Keyword,  // color 12 (light blue)
    ApiFunc,  // color 9 (orange)
}

/// A span of characters on one line sharing the same highlight color.
#[derive(Debug, PartialEq)]
struct Token {
    kind: TokenKind,
    start: usize,
    end: usize, // exclusive byte offset within the line
}

/// Lua keywords recognized by the highlighter.
const LUA_KEYWORDS: &[&str] = &[
    "if", "then", "else", "elseif", "end", "for", "while", "do",
    "repeat", "until", "function", "return", "local", "and", "or",
    "not", "in", "true", "false", "nil", "break", "goto",
];

/// PICO-8 API function names recognized by the highlighter.
const PICO8_API: &[&str] = &[
    "cls", "pset", "print", "spr", "map", "sfx", "music", "btn", "btnp",
    "rnd", "sin", "cos", "atan2", "sqrt", "abs", "flr", "ceil", "min",
    "max", "mid", "sgn", "band", "bor", "bxor", "bnot", "shl", "shr",
    "sub", "add", "del", "count", "foreach", "all", "tostr", "tonum",
    "peek", "poke", "memcpy", "memset", "stat", "camera", "clip", "pal",
    "palt", "color", "circ", "circfill", "rect", "rectfill", "line",
    "sget", "sset", "fget", "fset", "mget", "mset", "sspr",
];

/// Return true if `c` can appear in a Lua identifier.
fn is_ident_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

/// Tokenize a single line for syntax highlighting.
/// Returns a list of non-overlapping, contiguous tokens covering the line.
fn tokenize_line(line: &str) -> Vec<Token> {
    let bytes = line.as_bytes();
    let len = bytes.len();
    let mut tokens: Vec<Token> = Vec::new();
    let mut i: usize = 0;

    while i < len {
        // --- Comment: -- to end of line ---
        if i + 1 < len && bytes[i] == b'-' && bytes[i + 1] == b'-' {
            tokens.push(Token { kind: TokenKind::Comment, start: i, end: len });
            i = len;
            continue;
        }

        // --- String literal: "..." or '...' ---
        if bytes[i] == b'"' || bytes[i] == b'\'' {
            let quote = bytes[i];
            let start = i;
            i += 1;
            while i < len && bytes[i] != quote {
                if bytes[i] == b'\\' && i + 1 < len {
                    i += 1; // skip escaped char
                }
                i += 1;
            }
            if i < len {
                i += 1; // skip closing quote
            }
            tokens.push(Token { kind: TokenKind::StringLit, start, end: i });
            continue;
        }

        // --- Number: digit sequences, hex 0x... ---
        if bytes[i].is_ascii_digit()
            || (bytes[i] == b'.' && i + 1 < len && bytes[i + 1].is_ascii_digit())
        {
            let start = i;
            if bytes[i] == b'0' && i + 1 < len && (bytes[i + 1] == b'x' || bytes[i + 1] == b'X')
            {
                // hex number
                i += 2;
                while i < len && bytes[i].is_ascii_hexdigit() {
                    i += 1;
                }
            } else {
                // decimal, possibly with dot
                while i < len && (bytes[i].is_ascii_digit() || bytes[i] == b'.') {
                    i += 1;
                }
            }
            tokens.push(Token { kind: TokenKind::Number, start, end: i });
            continue;
        }

        // --- Identifier (keyword / API / default) ---
        if bytes[i].is_ascii_alphabetic() || bytes[i] == b'_' {
            let start = i;
            while i < len && is_ident_char(bytes[i] as char) {
                i += 1;
            }
            let word = &line[start..i];
            let kind = if LUA_KEYWORDS.contains(&word) {
                TokenKind::Keyword
            } else if PICO8_API.contains(&word) {
                TokenKind::ApiFunc
            } else {
                TokenKind::Default
            };
            tokens.push(Token { kind, start, end: i });
            continue;
        }

        // --- Default: single character (punctuation, whitespace, etc.) ---
        let start = i;
        i += 1;
        // Merge consecutive default characters
        while i < len
            && bytes[i] != b'-'
            && bytes[i] != b'"'
            && bytes[i] != b'\''
            && !bytes[i].is_ascii_alphanumeric()
            && bytes[i] != b'_'
            && !(bytes[i] == b'.' && i + 1 < len && bytes[i + 1].is_ascii_digit())
        {
            i += 1;
        }
        tokens.push(Token { kind: TokenKind::Default, start, end: i });
    }

    tokens
}

/// Map a TokenKind to a PICO-8 color index.
fn token_color(kind: TokenKind) -> u8 {
    match kind {
        TokenKind::Default => 6,
        TokenKind::Comment => 13,
        TokenKind::StringLit => 4,
        TokenKind::Number => 14,
        TokenKind::Keyword => 12,
        TokenKind::ApiFunc => 9,
    }
}

// ---------------------------------------------------------------------------
// Key repeat
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------
// Editor struct
// ---------------------------------------------------------------------------

pub struct Editor {
    pub tab: EditorTab,
    pub code: String,

    // Code editor state
    pub cursor_pos: usize,
    pub scroll_y: usize,

    // Text selection state
    pub selection_start: Option<usize>,

    // Sprite editor state
    pub current_sprite: u8,
    pub sprite_color: u8,

    // Map editor state
    pub map_scroll_x: i32,
    pub map_scroll_y: i32,
    pub map_current_tile: u8,

    // SFX editor state
    pub current_sfx: usize,
    pub sfx_data: [Sfx; NUM_SFX],
    pub music_data: [MusicPattern; NUM_MUSIC],

    // Mouse state (in 128x128 coordinates)
    pub mouse_x: i32,
    pub mouse_y: i32,
    pub mouse_pressed: bool,
    pub mouse_down: bool,

    // Screen scale info (set by main loop)
    pub screen_offset_x: f32,
    pub screen_offset_y: f32,
    pub screen_scale: f32,

    // Internal
    frame: u32,
    key_timers: [u32; NUM_REPEAT_KEYS],

    // Undo/redo stacks: each entry is (code_snapshot, cursor_pos)
    undo_stack: Vec<(String, usize)>,
    redo_stack: Vec<(String, usize)>,

    // Search state
    search_active: bool,
    search_query: String,
    search_results: Vec<usize>,  // byte offsets of matches
    search_current: usize,       // index into search_results

    // Internal clipboard (line-based copy/paste)
    // NOTE: System clipboard integration could be improved by using the `arboard`
    // crate or macroquad's clipboard API if available. Currently we use an internal
    // clipboard only, which works within the editor session.
    clipboard: String,

    // Edit coalescing: tracks the code state at last undo push to avoid
    // pushing identical consecutive snapshots.
    last_undo_code: String,

    // Music editor state
    pub current_music: usize,
    music_scroll: usize,

    // File picker state
    pub show_file_picker: bool,
    file_list: Vec<String>,
    file_picker_scroll: usize,

    /// When a file is selected from the picker, its path is stored here
    /// for main.rs to consume via `.take()`.
    pub pending_load: Option<String>,
}

// ---------------------------------------------------------------------------
// Implementation
// ---------------------------------------------------------------------------

impl Editor {
    pub fn new() -> Self {
        Self {
            tab: EditorTab::Code,
            code: DEFAULT_CODE.to_string(),

            cursor_pos: 0,
            scroll_y: 0,

            selection_start: None,

            current_sprite: 1,
            sprite_color: 7,

            map_scroll_x: 0,
            map_scroll_y: 0,
            map_current_tile: 1,

            current_sfx: 0,
            sfx_data: [Sfx::default(); NUM_SFX],
            music_data: [MusicPattern::default(); NUM_MUSIC],

            mouse_x: 0,
            mouse_y: 0,
            mouse_pressed: false,
            mouse_down: false,

            screen_offset_x: 0.0,
            screen_offset_y: 0.0,
            screen_scale: 1.0,

            frame: 0,
            key_timers: [0; NUM_REPEAT_KEYS],

            search_active: false,
            search_query: String::new(),
            search_results: Vec::new(),
            search_current: 0,

            undo_stack: Vec::new(),
            redo_stack: Vec::new(),
            clipboard: String::new(),
            last_undo_code: DEFAULT_CODE.to_string(),

            current_music: 0,
            music_scroll: 0,

            show_file_picker: false,
            file_list: Vec::new(),
            file_picker_scroll: 0,
            pending_load: None,
        }
    }

    // =======================================================================
    // Main update entry-point
    // =======================================================================

    /// Update editor state and draw to console. Returns an action for the host.
    pub fn update(&mut self, console: &mut Console) -> EditorAction {
        self.frame = self.frame.wrapping_add(1);

        // --- Convert mouse coordinates ---
        let (mx, my) = mouse_position();
        if self.screen_scale > 0.0 {
            self.mouse_x = ((mx - self.screen_offset_x) / self.screen_scale) as i32;
            self.mouse_y = ((my - self.screen_offset_y) / self.screen_scale) as i32;
        }
        self.mouse_pressed = is_mouse_button_pressed(MouseButton::Left);
        self.mouse_down = is_mouse_button_down(MouseButton::Left);

        // --- Reset draw state ---
        console.reset_draw_state();
        console.camera(0, 0);
        console.clip_rect(0, 0, 128, 128);

        // --- Global key shortcuts ---
        let ctrl_or_cmd = is_key_down(KeyCode::LeftControl)
            || is_key_down(KeyCode::RightControl)
            || is_key_down(KeyCode::LeftSuper)
            || is_key_down(KeyCode::RightSuper);
        let shift = is_key_down(KeyCode::LeftShift) || is_key_down(KeyCode::RightShift);
        if ctrl_or_cmd && is_key_pressed(KeyCode::R) {
            return EditorAction::RunGame;
        }
        if ctrl_or_cmd && shift && is_key_pressed(KeyCode::S) {
            return EditorAction::SaveCartAs;
        }
        if ctrl_or_cmd && !shift && is_key_pressed(KeyCode::S) {
            return EditorAction::SaveCart;
        }
        if ctrl_or_cmd && is_key_pressed(KeyCode::N) {
            return EditorAction::NewCart;
        }
        if ctrl_or_cmd && is_key_pressed(KeyCode::O) {
            // Open file picker: scan current directory for .p8 files
            self.file_list.clear();
            if let Ok(entries) = std::fs::read_dir(".") {
                for entry in entries.flatten() {
                    if let Some(name) = entry.file_name().to_str() {
                        if name.ends_with(".p8") {
                            self.file_list.push(name.to_string());
                        }
                    }
                }
            }
            self.file_list.sort();
            self.file_picker_scroll = 0;
            self.show_file_picker = true;
        }

        // Alt+Left / Alt+Right => cycle tabs
        let alt = is_key_down(KeyCode::LeftAlt) || is_key_down(KeyCode::RightAlt);
        if alt {
            if is_key_pressed(KeyCode::Left) {
                self.tab = match self.tab {
                    EditorTab::Code => EditorTab::Music,
                    EditorTab::Sprite => EditorTab::Code,
                    EditorTab::Map => EditorTab::Sprite,
                    EditorTab::Sfx => EditorTab::Map,
                    EditorTab::Music => EditorTab::Sfx,
                };
            }
            if is_key_pressed(KeyCode::Right) {
                self.tab = match self.tab {
                    EditorTab::Code => EditorTab::Sprite,
                    EditorTab::Sprite => EditorTab::Map,
                    EditorTab::Map => EditorTab::Sfx,
                    EditorTab::Sfx => EditorTab::Music,
                    EditorTab::Music => EditorTab::Code,
                };
            }
        }

        // --- Clear screen ---
        console.cls(Some(0));

        // --- Draw the top bar & handle clicks ---
        let action = self.draw_top_bar(console);
        if let EditorAction::RunGame = action {
            return action;
        }

        // --- Draw the active tab ---
        match self.tab {
            EditorTab::Code => self.update_code(console),
            EditorTab::Sprite => self.update_sprite(console),
            EditorTab::Map => self.update_map(console),
            EditorTab::Sfx => self.update_sfx(console),
            EditorTab::Music => self.update_music(console),
        }

        // --- File picker overlay ---
        if self.show_file_picker {
            return self.update_file_picker(console);
        }

        EditorAction::None
    }

    // =======================================================================
    // Top bar (tab buttons + run button)
    // =======================================================================

    fn draw_top_bar(&mut self, console: &mut Console) -> EditorAction {
        console.rectfill(0, 0, 127, TOP_BAR_H - 1, Some(8));

        let tabs: &[(&str, EditorTab)] = &[
            ("CODE", EditorTab::Code),
            ("SPR", EditorTab::Sprite),
            ("MAP", EditorTab::Map),
            ("SFX", EditorTab::Sfx),
            ("MUS", EditorTab::Music),
        ];

        let mut tx: i32 = 1;
        for &(label, tab_id) in tabs {
            let w = label.len() as i32 * CHAR_W + 4;
            let selected = self.tab == tab_id;
            let bg: u8 = if selected { 7 } else { 2 };
            let fg: u8 = if selected { 0 } else { 7 };
            console.rectfill(tx, 0, tx + w - 1, TOP_BAR_H - 2, Some(bg));
            console.print(label, Some(tx + 2), Some(1), Some(fg));

            if self.mouse_pressed
                && self.mouse_x >= tx
                && self.mouse_x < tx + w
                && self.mouse_y >= 0
                && self.mouse_y < TOP_BAR_H
            {
                self.tab = tab_id;
            }

            tx += w + 2;
        }

        // --- Run button ---
        let run_x = SW - 12;
        let run_y: i32 = 1;
        console.rectfill(run_x - 1, run_y - 1, run_x + 6, run_y + 5, Some(0));
        for i in 0..5_i32 {
            let half = if i <= 2 { i } else { 4 - i };
            for x in run_x..=(run_x + half * 2) {
                console.raw_pset(x, run_y + i, 11);
            }
        }

        if self.mouse_pressed
            && self.mouse_x >= run_x - 1
            && self.mouse_x <= run_x + 6
            && self.mouse_y >= 0
            && self.mouse_y < TOP_BAR_H
        {
            return EditorAction::RunGame;
        }

        EditorAction::None
    }

    // =======================================================================
    // CODE EDITOR
    // =======================================================================

    fn update_code(&mut self, console: &mut Console) {
        console.rectfill(0, MAIN_Y, 127, 127, Some(1));

        self.handle_code_input();

        let lines = split_lines(&self.code);
        let (cursor_line, cursor_col) = cursor_to_line_col(self.cursor_pos, &lines);

        if cursor_line < self.scroll_y {
            self.scroll_y = cursor_line;
        }
        if cursor_line >= self.scroll_y + CODE_ROWS {
            self.scroll_y = cursor_line - CODE_ROWS + 1;
        }

        // Pre-compute selection range if active
        let sel_range = self.selection_start.map(|ss| selection_range(ss, self.cursor_pos));

        // Pre-compute search match info: clone to avoid borrow issues
        let search_active = self.search_active;
        let search_results: Vec<usize> = self.search_results.clone();
        let search_current = self.search_current;
        let search_query_len = self.search_query.len();

        // Determine how many visible code rows we have (reserve space for search bar)
        let visible_rows = if search_active { CODE_ROWS.saturating_sub(1) } else { CODE_ROWS };

        for i in 0..visible_rows {
            let line_idx = self.scroll_y + i;
            let py = MAIN_Y + i as i32 * CHAR_H;

            if line_idx < lines.len() {
                let line = lines[line_idx];

                // Compute the byte offset of this line's start in the code
                let mut line_byte_start: usize = 0;
                for li in 0..line_idx {
                    line_byte_start += lines[li].len() + 1;
                }
                let line_byte_end = line_byte_start + line.len();

                // Draw selection highlight behind text
                if let Some((sel_lo, sel_hi)) = sel_range {
                    if sel_lo < sel_hi && sel_lo < line_byte_end + 1 && sel_hi > line_byte_start {
                        // Selection overlaps this line
                        let col_start = if sel_lo <= line_byte_start {
                            0
                        } else {
                            sel_lo - line_byte_start
                        };
                        // If selection extends past the end of line text, include
                        // the newline character visually (extend by 1 col)
                        let col_end = if sel_hi >= line_byte_end + 1 {
                            line.len() + 1
                        } else if sel_hi >= line_byte_end {
                            line.len()
                        } else {
                            sel_hi - line_byte_start
                        };
                        let col_start = col_start.min(CODE_COLS);
                        let col_end = col_end.min(CODE_COLS + 1);
                        if col_end > col_start {
                            let hx1 = GUTTER_PX + col_start as i32 * CHAR_W;
                            let hx2 = GUTTER_PX + col_end as i32 * CHAR_W - 1;
                            console.rectfill(hx1, py, hx2, py + CHAR_H - 2, Some(SELECTION_COLOR));
                        }
                    }
                }

                // Draw search match highlights behind text
                if search_active && search_query_len > 0 {
                    for (match_idx, &match_offset) in search_results.iter().enumerate() {
                        let match_end = match_offset + search_query_len;
                        // Check if this match overlaps with this line
                        if match_offset < line_byte_end && match_end > line_byte_start {
                            let col_start = if match_offset <= line_byte_start {
                                0
                            } else {
                                match_offset - line_byte_start
                            };
                            let col_end = if match_end >= line_byte_end {
                                line.len()
                            } else {
                                match_end - line_byte_start
                            };
                            let col_start = col_start.min(CODE_COLS);
                            let col_end = col_end.min(CODE_COLS);
                            if col_end > col_start {
                                let hx1 = GUTTER_PX + col_start as i32 * CHAR_W;
                                let hx2 = GUTTER_PX + col_end as i32 * CHAR_W - 1;
                                let color = if match_idx == search_current {
                                    SEARCH_CURRENT_COLOR
                                } else {
                                    SEARCH_MATCH_COLOR
                                };
                                console.rectfill(hx1, py, hx2, py + CHAR_H - 2, Some(color));
                            }
                        }
                    }
                }

                // Draw line number gutter
                let num_str = format!("{:>3} ", line_idx + 1);
                console.print(&num_str, Some(0), Some(py), Some(13));

                // Draw syntax-highlighted code
                let tokens = tokenize_line(line);
                for tok in &tokens {
                    let tok_text = &line[tok.start..tok.end];
                    let char_col = tok.start;
                    if char_col >= CODE_COLS {
                        break;
                    }
                    let max_chars = CODE_COLS - char_col;
                    let visible: String = tok_text.chars().take(max_chars).collect();
                    let px = GUTTER_PX + char_col as i32 * CHAR_W;
                    console.print(&visible, Some(px), Some(py), Some(token_color(tok.kind)));
                }
            }
        }

        // Cursor blink
        let blink_on = (self.frame % (BLINK_HALF * 2)) < BLINK_HALF;
        if blink_on {
            let cx = GUTTER_PX + cursor_col as i32 * CHAR_W;
            let cy = MAIN_Y + (cursor_line as i32 - self.scroll_y as i32) * CHAR_H;
            if cy >= MAIN_Y && cy + CHAR_H <= SH {
                console.rectfill(cx, cy, cx, cy + CHAR_H - 2, Some(12));
            }
        }

        // Mouse click to position cursor (clears selection)
        if self.mouse_pressed && self.mouse_y >= MAIN_Y && self.mouse_x >= GUTTER_PX {
            let click_line = self.scroll_y + ((self.mouse_y - MAIN_Y) / CHAR_H) as usize;
            let click_col = ((self.mouse_x - GUTTER_PX) / CHAR_W) as usize;
            self.cursor_pos = line_col_to_pos(&lines, click_line, click_col, self.code.len());
            self.selection_start = None;
        }

        // --- Search bar overlay at bottom of code area ---
        if self.search_active {
            let bar_y = SH - CHAR_H;
            console.rectfill(0, bar_y, 127, 127, Some(0));
            // "Find: {query}" with match count
            let query_display: String = self.search_query.chars().take(14).collect();
            let find_label = format!("f:{}", query_display);
            console.print(&find_label, Some(1), Some(bar_y), Some(7));

            if !self.search_results.is_empty() {
                let count_str = format!(
                    "{}/{}",
                    self.search_current + 1,
                    self.search_results.len()
                );
                let count_x = SW - count_str.len() as i32 * CHAR_W - 1;
                console.print(&count_str, Some(count_x), Some(bar_y), Some(11));
            } else if !self.search_query.is_empty() {
                let no_match = "0/0";
                let nm_x = SW - no_match.len() as i32 * CHAR_W - 1;
                console.print(no_match, Some(nm_x), Some(bar_y), Some(8));
            }
        }

        // --- Token / character count display in top-right ---
        if self.tab == EditorTab::Code {
            let char_count = self.code.len();
            let token_count = count_tokens(&self.code);
            let count_str = format!("c:{} t:~{}", char_count, token_count);
            let cx = SW - count_str.len() as i32 * CHAR_W - 1;
            // Draw over the top bar area (right side)
            console.rectfill(cx - 1, 0, SW - 1, TOP_BAR_H - 2, Some(8));
            let color: u8 = if char_count > PICO8_CHAR_LIMIT || token_count > PICO8_TOKEN_LIMIT {
                8 // red (will show on red bg... use white instead)
            } else {
                7
            };
            let fg = if char_count > PICO8_CHAR_LIMIT || token_count > PICO8_TOKEN_LIMIT {
                14 // pink for warning
            } else {
                color
            };
            console.print(&count_str, Some(cx), Some(1), Some(fg));
        }
    }

    // -----------------------------------------------------------------------
    // Key repeat helpers
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
        since % interval == 0
    }

    // -----------------------------------------------------------------------
    // Code input handling
    // -----------------------------------------------------------------------

    /// Save a specific snapshot onto the undo stack, clear redo, and cap size.
    fn push_undo_snapshot(&mut self, snapshot: String, cursor: usize) {
        self.undo_stack.push((snapshot, cursor));
        self.redo_stack.clear();
        if self.undo_stack.len() > UNDO_MAX {
            self.undo_stack.remove(0);
        }
    }

    /// Push the current (pre-mutation) state onto the undo stack.
    /// Call this right before a destructive edit (cut, paste, etc.)
    /// so that the current code is preserved as the undo target.
    /// Updates `last_undo_code` after pushing.
    fn push_undo(&mut self) {
        self.push_undo_snapshot(self.code.clone(), self.cursor_pos);
        self.last_undo_code = self.code.clone();
    }

    /// Push the *previous checkpoint* onto the undo stack if the code has
    /// changed since the last checkpoint. Used to coalesce rapid typing
    /// into fewer undo entries. After pushing, updates the checkpoint.
    fn push_undo_if_changed(&mut self) {
        if self.code != self.last_undo_code {
            self.push_undo_snapshot(self.last_undo_code.clone(), self.cursor_pos);
            self.last_undo_code = self.code.clone();
        }
    }

    /// Perform an undo: pop from undo_stack, push current to redo_stack, restore.
    fn perform_undo(&mut self) {
        if let Some((snap_code, snap_cursor)) = self.undo_stack.pop() {
            self.redo_stack
                .push((self.code.clone(), self.cursor_pos));
            self.code = snap_code;
            self.cursor_pos = snap_cursor.min(self.code.len());
            self.last_undo_code = self.code.clone();
        }
    }

    /// Perform a redo: pop from redo_stack, push current to undo_stack, restore.
    fn perform_redo(&mut self) {
        if let Some((snap_code, snap_cursor)) = self.redo_stack.pop() {
            self.undo_stack
                .push((self.code.clone(), self.cursor_pos));
            if self.undo_stack.len() > UNDO_MAX {
                self.undo_stack.remove(0);
            }
            self.code = snap_code;
            self.cursor_pos = snap_cursor.min(self.code.len());
            self.last_undo_code = self.code.clone();
        }
    }

    /// Copy the current line to the internal clipboard.
    fn clipboard_copy_line(&mut self) {
        let lines = split_lines(&self.code);
        let (line_idx, _) = cursor_to_line_col(self.cursor_pos, &lines);
        if let Some(line) = lines.get(line_idx) {
            self.clipboard = line.to_string();
        }
    }

    /// Cut the current line: copy to clipboard and delete it.
    fn clipboard_cut_line(&mut self) {
        self.push_undo();

        let lines = split_lines(&self.code);
        let (line_idx, _) = cursor_to_line_col(self.cursor_pos, &lines);
        if let Some(line) = lines.get(line_idx) {
            self.clipboard = line.to_string();
        }

        // Compute byte range to delete: start of line to start of next line
        // (including the trailing newline if present).
        let mut line_start: usize = 0;
        for i in 0..line_idx {
            line_start += lines[i].len() + 1;
        }
        let line_text_len = lines.get(line_idx).map_or(0, |l| l.len());
        let mut line_end = line_start + line_text_len;
        // Include the newline after this line if it exists
        if line_end < self.code.len() && self.code.as_bytes()[line_end] == b'\n' {
            line_end += 1;
        } else if line_start > 0 {
            // If this is the last line (no trailing newline), remove
            // the preceding newline instead to avoid a trailing empty line.
            line_start -= 1;
        }
        self.code.drain(line_start..line_end);
        self.cursor_pos = line_start.min(self.code.len());
    }

    /// Paste the clipboard contents at the cursor position.
    fn clipboard_paste(&mut self) {
        if self.clipboard.is_empty() {
            return;
        }
        self.push_undo();

        // Insert clipboard text followed by a newline on a new line
        // below the current line.
        let lines = split_lines(&self.code);
        let (line_idx, _) = cursor_to_line_col(self.cursor_pos, &lines);
        let mut insert_pos: usize = 0;
        for i in 0..=line_idx {
            insert_pos += lines[i].len() + 1;
        }
        // Clamp in case we are on the last line without trailing newline
        if insert_pos > self.code.len() {
            // Need to add a newline first
            self.code.push('\n');
            insert_pos = self.code.len();
        }
        let paste_text = format!("{}\n", self.clipboard);
        self.code.insert_str(insert_pos, &paste_text);
        // Place cursor at start of pasted line
        self.cursor_pos = insert_pos;
    }

    /// Helper: if selection is active, delete it and return true.
    /// Also clears selection_start. Pushes undo before deleting.
    fn delete_selection_if_active(&mut self) -> bool {
        if let Some(ss) = self.selection_start {
            if ss != self.cursor_pos {
                self.push_undo_if_changed();
                self.cursor_pos = delete_selection(&mut self.code, ss, self.cursor_pos);
                self.selection_start = None;
                return true;
            }
            self.selection_start = None;
        }
        false
    }

    /// Helper: begin or extend selection when Shift is held.
    /// If no selection is active, set selection_start to the current cursor_pos.
    fn begin_or_extend_selection(&mut self) {
        if self.selection_start.is_none() {
            self.selection_start = Some(self.cursor_pos);
        }
    }

    // -----------------------------------------------------------------------
    // Search input handling (when search bar is active)
    // -----------------------------------------------------------------------

    fn handle_search_input(&mut self) {
        let ctrl = is_key_down(KeyCode::LeftControl)
            || is_key_down(KeyCode::RightControl)
            || is_key_down(KeyCode::LeftSuper)
            || is_key_down(KeyCode::RightSuper);
        let shift = is_key_down(KeyCode::LeftShift) || is_key_down(KeyCode::RightShift);

        // Escape: close search
        if is_key_pressed(KeyCode::Escape) {
            self.search_active = false;
            self.search_query.clear();
            self.search_results.clear();
            self.search_current = 0;
            while get_char_pressed().is_some() {}
            return;
        }

        // Enter or Ctrl+G: jump to next match
        if is_key_pressed(KeyCode::Enter) || is_key_pressed(KeyCode::KpEnter)
            || (ctrl && is_key_pressed(KeyCode::G))
        {
            if !self.search_results.is_empty() {
                if shift && !ctrl {
                    // Shift+Enter: previous match
                    if self.search_current == 0 {
                        self.search_current = self.search_results.len() - 1;
                    } else {
                        self.search_current -= 1;
                    }
                } else {
                    // Next match
                    self.search_current = (self.search_current + 1) % self.search_results.len();
                }
                self.cursor_pos = self.search_results[self.search_current];
            }
            while get_char_pressed().is_some() {}
            return;
        }

        // Backspace: remove last char from query
        if is_key_pressed(KeyCode::Backspace) {
            self.search_query.pop();
            self.update_search_results();
            while get_char_pressed().is_some() {}
            return;
        }

        // Type characters into query
        let mut changed = false;
        while let Some(ch) = get_char_pressed() {
            if ch >= ' ' && ch <= '~' {
                self.search_query.push(ch);
                changed = true;
            }
        }
        if changed {
            self.update_search_results();
        }
    }

    /// Recompute search results from the current query and code.
    fn update_search_results(&mut self) {
        self.search_results = find_all_matches(&self.code, &self.search_query);
        if self.search_results.is_empty() {
            self.search_current = 0;
        } else {
            // Clamp current index
            if self.search_current >= self.search_results.len() {
                self.search_current = 0;
            }
            // Jump cursor to current match
            self.cursor_pos = self.search_results[self.search_current];
        }
    }

    fn handle_code_input(&mut self) {
        self.update_key_timers();

        let ctrl = is_key_down(KeyCode::LeftControl)
            || is_key_down(KeyCode::RightControl)
            || is_key_down(KeyCode::LeftSuper)
            || is_key_down(KeyCode::RightSuper);
        let alt = is_key_down(KeyCode::LeftAlt) || is_key_down(KeyCode::RightAlt);
        let shift = is_key_down(KeyCode::LeftShift) || is_key_down(KeyCode::RightShift);

        // --- Ctrl+F: Toggle search mode ---
        if ctrl && is_key_pressed(KeyCode::F) {
            self.search_active = !self.search_active;
            if !self.search_active {
                self.search_query.clear();
                self.search_results.clear();
                self.search_current = 0;
            }
            while get_char_pressed().is_some() {}
            return;
        }

        // If search is active, delegate input to search handler
        if self.search_active {
            self.handle_search_input();
            return;
        }

        if alt {
            return;
        }

        // --- Ctrl+A: Select all ---
        if ctrl && is_key_pressed(KeyCode::A) {
            self.selection_start = Some(0);
            self.cursor_pos = self.code.len();
            while get_char_pressed().is_some() {}
            return;
        }

        // --- Ctrl+Z: Undo ---
        if ctrl && is_key_pressed(KeyCode::Z) && !shift {
            self.selection_start = None;
            self.push_undo_if_changed();
            self.perform_undo();
            while get_char_pressed().is_some() {}
            return;
        }

        // --- Ctrl+Y or Ctrl+Shift+Z: Redo ---
        if ctrl && (is_key_pressed(KeyCode::Y) || (is_key_pressed(KeyCode::Z) && shift)) {
            self.selection_start = None;
            self.perform_redo();
            while get_char_pressed().is_some() {}
            return;
        }

        // --- Ctrl+C: Copy (selection or line) ---
        if ctrl && is_key_pressed(KeyCode::C) {
            if let Some(ss) = self.selection_start {
                if ss != self.cursor_pos {
                    self.clipboard = selected_text(&self.code, ss, self.cursor_pos).to_string();
                } else {
                    self.clipboard_copy_line();
                }
            } else {
                self.clipboard_copy_line();
            }
            while get_char_pressed().is_some() {}
            return;
        }

        // --- Ctrl+X: Cut (selection or line) ---
        if ctrl && is_key_pressed(KeyCode::X) {
            if let Some(ss) = self.selection_start {
                if ss != self.cursor_pos {
                    self.clipboard = selected_text(&self.code, ss, self.cursor_pos).to_string();
                    self.delete_selection_if_active();
                } else {
                    self.selection_start = None;
                    self.clipboard_cut_line();
                }
            } else {
                self.clipboard_cut_line();
            }
            while get_char_pressed().is_some() {}
            return;
        }

        // --- Ctrl+V: Paste (replace selection or insert at cursor) ---
        if ctrl && is_key_pressed(KeyCode::V) {
            if self.clipboard.is_empty() {
                while get_char_pressed().is_some() {}
                return;
            }
            if let Some(ss) = self.selection_start {
                if ss != self.cursor_pos {
                    // Replace selection with clipboard
                    self.push_undo();
                    self.cursor_pos = delete_selection(&mut self.code, ss, self.cursor_pos);
                    self.selection_start = None;
                    let clip = self.clipboard.clone();
                    self.code.insert_str(self.cursor_pos, &clip);
                    self.cursor_pos += clip.len();
                } else {
                    self.selection_start = None;
                    self.clipboard_paste();
                }
            } else {
                self.clipboard_paste();
            }
            while get_char_pressed().is_some() {}
            return;
        }

        let nav_key = (0..NUM_REPEAT_KEYS).any(|i| self.key_fire(i))
            || is_key_pressed(KeyCode::Escape);

        // --- Navigation keys with Shift selection support ---
        if self.key_fire(K_LEFT) {
            if shift {
                self.begin_or_extend_selection();
            } else {
                self.selection_start = None;
            }
            if self.cursor_pos > 0 {
                let mut p = self.cursor_pos - 1;
                while p > 0 && !self.code.is_char_boundary(p) {
                    p -= 1;
                }
                self.cursor_pos = p;
            }
        }

        if self.key_fire(K_RIGHT) {
            if shift {
                self.begin_or_extend_selection();
            } else {
                self.selection_start = None;
            }
            if self.cursor_pos < self.code.len() {
                let mut p = self.cursor_pos + 1;
                while p < self.code.len() && !self.code.is_char_boundary(p) {
                    p += 1;
                }
                self.cursor_pos = p;
            }
        }

        if self.key_fire(K_UP) {
            if shift {
                self.begin_or_extend_selection();
            } else {
                self.selection_start = None;
            }
            let lines = split_lines(&self.code);
            let (line, col) = cursor_to_line_col(self.cursor_pos, &lines);
            if line > 0 {
                self.cursor_pos = line_col_to_pos(&lines, line - 1, col, self.code.len());
            }
        }

        if self.key_fire(K_DOWN) {
            if shift {
                self.begin_or_extend_selection();
            } else {
                self.selection_start = None;
            }
            let lines = split_lines(&self.code);
            let (line, col) = cursor_to_line_col(self.cursor_pos, &lines);
            if line + 1 < lines.len() {
                self.cursor_pos = line_col_to_pos(&lines, line + 1, col, self.code.len());
            }
        }

        if self.key_fire(K_HOME) {
            if shift {
                self.begin_or_extend_selection();
            } else {
                self.selection_start = None;
            }
            let lines = split_lines(&self.code);
            let (line, _) = cursor_to_line_col(self.cursor_pos, &lines);
            self.cursor_pos = line_col_to_pos(&lines, line, 0, self.code.len());
        }

        if self.key_fire(K_END) {
            if shift {
                self.begin_or_extend_selection();
            } else {
                self.selection_start = None;
            }
            let lines = split_lines(&self.code);
            let (line, _) = cursor_to_line_col(self.cursor_pos, &lines);
            let len = lines.get(line).map_or(0, |l| l.len());
            self.cursor_pos = line_col_to_pos(&lines, line, len, self.code.len());
        }

        // --- Backspace: delete selection or single char ---
        if self.key_fire(K_BKSP) {
            if self.delete_selection_if_active() {
                return;
            }
            if self.cursor_pos > 0 {
                self.push_undo_if_changed();
                let mut prev = self.cursor_pos - 1;
                while prev > 0 && !self.code.is_char_boundary(prev) {
                    prev -= 1;
                }
                self.code.remove(prev);
                self.cursor_pos = prev;
                return;
            }
        }

        // --- Delete: delete selection or single char ---
        if self.key_fire(K_DEL) {
            if self.delete_selection_if_active() {
                return;
            }
            if self.cursor_pos < self.code.len() {
                self.push_undo_if_changed();
                self.code.remove(self.cursor_pos);
                return;
            }
        }

        // --- Enter: replace selection or insert newline with auto-indent ---
        if self.key_fire(K_ENTER) {
            self.delete_selection_if_active();
            self.push_undo_if_changed();

            // Determine indentation from the current line
            let lines = split_lines(&self.code);
            let (cur_line, _) = cursor_to_line_col(self.cursor_pos, &lines);
            let current_line_text = lines.get(cur_line).copied().unwrap_or("");
            let base_indent = leading_whitespace(current_line_text).to_string();

            // Check if current line ends with an indent-triggering keyword
            let extra_indent = if ends_with_indent_keyword(current_line_text) {
                "  "
            } else {
                ""
            };

            let indent = format!("{}{}", base_indent, extra_indent);
            let insert_str = format!("\n{}", indent);
            self.code.insert_str(self.cursor_pos, &insert_str);
            self.cursor_pos += insert_str.len();
            return;
        }

        // --- Tab: replace selection or insert space ---
        if self.key_fire(K_TAB) && !ctrl {
            self.delete_selection_if_active();
            self.push_undo_if_changed();
            self.code.insert(self.cursor_pos, ' ');
            self.cursor_pos += 1;
            return;
        }

        // --- Character input: replace selection or insert ---
        if !ctrl {
            let mut inserted = false;
            while let Some(ch) = get_char_pressed() {
                if nav_key {
                    continue;
                }
                if ch >= ' ' && ch <= '~' {
                    if !inserted {
                        self.delete_selection_if_active();
                        self.push_undo_if_changed();
                        inserted = true;
                    }
                    self.code.insert(self.cursor_pos, ch);
                    self.cursor_pos += ch.len_utf8();
                }
            }
        }
    }

    // =======================================================================
    // SPRITE EDITOR
    // =======================================================================

    fn update_sprite(&mut self, console: &mut Console) {
        console.rectfill(0, MAIN_Y, 127, 127, Some(5));

        let (sx0, sy0) = sprite_origin(self.current_sprite);

        // ---- Zoomed sprite view (64x64 area) ----
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

        if self.mouse_down {
            if let Some((px, py)) = zoom_to_sprite_pixel(self.mouse_x, self.mouse_y) {
                console.sset(sx0 + px, sy0 + py, self.sprite_color);
            }
        }

        if is_mouse_button_pressed(MouseButton::Right) {
            if let Some((px, py)) = zoom_to_sprite_pixel(self.mouse_x, self.mouse_y) {
                self.sprite_color = console.sget(sx0 + px, sy0 + py);
            }
        }

        // ---- Info panel ----
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
        for py in 0..8_i32 {
            for px in 0..8_i32 {
                let col = console.sget(sx0 + px, sy0 + py);
                console.raw_pset(preview_x + px, preview_y + py, col);
            }
        }

        // Navigation buttons
        let nav_y = info_y + 30;
        console.print("<", Some(info_x), Some(nav_y), Some(7));
        console.print(">", Some(info_x + 12), Some(nav_y), Some(7));

        if self.mouse_pressed && self.mouse_y >= nav_y && self.mouse_y < nav_y + CHAR_H {
            if self.mouse_x >= info_x && self.mouse_x < info_x + 6 {
                self.current_sprite = self.current_sprite.wrapping_sub(1);
            } else if self.mouse_x >= info_x + 12 && self.mouse_x < info_x + 18 {
                self.current_sprite = self.current_sprite.wrapping_add(1);
            }
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
            if self.mouse_pressed
                && self.mouse_x >= bx
                && self.mouse_x < bx + 4
                && self.mouse_y >= flag_bit_y
                && self.mouse_y < flag_bit_y + 4
            {
                console.flags[self.current_sprite as usize] =
                    toggle_flag_bit(console.flags[self.current_sprite as usize], b);
            }
        }

        // ---- Sprite sheet mini view ----
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
                let spr_idx = sheet_row * 16 + col;
                let ssrc_x = (spr_idx % 16) * 8;
                let ssrc_y = (spr_idx / 16) * 8;
                let dx = SHEET_X + col as i32 * 8;
                let dy = SHEET_Y + row as i32 * 8;
                for py in 0..8_i32 {
                    for px in 0..8_i32 {
                        let c = console.sget(ssrc_x as i32 + px, ssrc_y as i32 + py);
                        console.raw_pset(dx + px, dy + py, c);
                    }
                }
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

        if self.mouse_pressed {
            if let Some(idx) = sheet_click_to_sprite(self.mouse_x, self.mouse_y, page) {
                self.current_sprite = idx;
            }
        }

        // ---- Color palette ----
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

        if self.mouse_pressed {
            if let Some(c) = palette_click_to_color(self.mouse_x, self.mouse_y) {
                self.sprite_color = c;
            }
        }

        let number_keys = [
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
        for (k, &key) in number_keys.iter().enumerate() {
            if is_key_pressed(key) {
                self.sprite_color = k as u8;
            }
        }
    }

    // =======================================================================
    // MAP EDITOR
    // =======================================================================

    fn update_map(&mut self, console: &mut Console) {
        console.rectfill(0, MAIN_Y, 127, 127, Some(1));

        let alt = is_key_down(KeyCode::LeftAlt) || is_key_down(KeyCode::RightAlt);
        if !alt {
            if is_key_pressed(KeyCode::Left) {
                self.map_scroll_x = (self.map_scroll_x - 1).max(0);
            }
            if is_key_pressed(KeyCode::Right) {
                self.map_scroll_x = (self.map_scroll_x + 1).min(128 - MAP_COLS as i32);
            }
            if is_key_pressed(KeyCode::Up) {
                self.map_scroll_y = (self.map_scroll_y - 1).max(0);
            }
            if is_key_pressed(KeyCode::Down) {
                self.map_scroll_y = (self.map_scroll_y + 1).min(64 - MAP_ROWS as i32);
            }
        }

        for row in 0..MAP_ROWS as i32 {
            for col in 0..MAP_COLS as i32 {
                let map_x = self.map_scroll_x + col;
                let map_y = self.map_scroll_y + row;
                if map_x < 0 || map_x >= 128 || map_y < 0 || map_y >= 64 {
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
                    let (spr_px, spr_py) = sprite_origin(tile);
                    for py in 0..8_i32 {
                        for px in 0..8_i32 {
                            let c = console.sget(spr_px + px, spr_py + py);
                            console.raw_pset(dx + px, dy + py, c);
                        }
                    }
                }
            }
        }

        if self.mouse_down && !is_mouse_button_down(MouseButton::Right) {
            if let Some((map_x, map_y)) = map_view_to_tile(
                self.mouse_x,
                self.mouse_y,
                self.map_scroll_x,
                self.map_scroll_y,
            ) {
                console.mset(map_x, map_y, self.map_current_tile);
            }
        }

        if is_mouse_button_down(MouseButton::Right) {
            if let Some((map_x, map_y)) = map_view_to_tile(
                self.mouse_x,
                self.mouse_y,
                self.map_scroll_x,
                self.map_scroll_y,
            ) {
                console.mset(map_x, map_y, 0);
            }
        }

        // ---- Top info bar ----
        let info_str = format!(
            "({},{}) t:{}",
            self.map_scroll_x, self.map_scroll_y, self.map_current_tile
        );
        console.print(&info_str, Some(1), Some(MAIN_Y), Some(7));

        let preview_x = SW - 12;
        let preview_y = MAIN_Y;
        if self.map_current_tile > 0 {
            let (tc, tr) = sprite_origin(self.map_current_tile);
            for py in 0..8_i32 {
                for px in 0..8_i32 {
                    let c = console.sget(tc + px, tr + py);
                    console.raw_pset(preview_x + px, preview_y + py, c);
                }
            }
        } else {
            console.rectfill(preview_x, preview_y, preview_x + 7, preview_y + 7, Some(0));
        }

        // ---- Bottom tile selector bar ----
        let tile_bar_y = SH - 10;
        console.rectfill(0, tile_bar_y - 1, 127, 127, Some(2));

        let tile_page = (self.map_current_tile as usize / 16) * 16;
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
                let (tc, tr) = sprite_origin(tile_idx as u8);
                for py in 0..8_i32 {
                    for px in 0..8_i32 {
                        let c = console.sget(tc + px, tr + py);
                        console.raw_pset(dx + px, dy + py, c);
                    }
                }
            }

            if tile_idx == self.map_current_tile as usize {
                console.rect(dx, dy, dx + 7, dy + 7, Some(7));
            }
        }

        if self.mouse_pressed
            && self.mouse_y >= tile_bar_y
            && self.mouse_y < tile_bar_y + 8
            && self.mouse_x >= 0
            && self.mouse_x < 128
        {
            let idx = tile_page + (self.mouse_x / 8) as usize;
            if idx < 256 {
                self.map_current_tile = idx as u8;
            }
        }

        if is_key_pressed(KeyCode::LeftBracket) {
            self.map_current_tile = self.map_current_tile.wrapping_sub(16);
        }
        if is_key_pressed(KeyCode::RightBracket) {
            self.map_current_tile = self.map_current_tile.wrapping_add(16);
        }

        if is_key_pressed(KeyCode::Minus) || is_key_pressed(KeyCode::KpSubtract) {
            self.map_current_tile = self.map_current_tile.wrapping_sub(1);
        }
        if is_key_pressed(KeyCode::Equal) || is_key_pressed(KeyCode::KpAdd) {
            self.map_current_tile = self.map_current_tile.wrapping_add(1);
        }
    }

    // =======================================================================
    // SFX EDITOR
    // =======================================================================

    fn update_sfx(&mut self, console: &mut Console) {
        console.rectfill(0, MAIN_Y, 127, 127, Some(0));

        // ---- Top: SFX index selector ----
        let sel_y = MAIN_Y + 1;
        console.print("<", Some(1), Some(sel_y), Some(7));
        let sfx_label = format!("SFX {:02}", self.current_sfx);
        console.print(&sfx_label, Some(10), Some(sel_y), Some(7));
        console.print(">", Some(50), Some(sel_y), Some(7));

        if self.mouse_pressed && self.mouse_y >= sel_y && self.mouse_y < sel_y + CHAR_H {
            if self.mouse_x >= 1 && self.mouse_x < 8 {
                // Left arrow
                if self.current_sfx > 0 {
                    self.current_sfx -= 1;
                } else {
                    self.current_sfx = NUM_SFX - 1;
                }
            } else if self.mouse_x >= 50 && self.mouse_x < 57 {
                // Right arrow
                if self.current_sfx < NUM_SFX - 1 {
                    self.current_sfx += 1;
                } else {
                    self.current_sfx = 0;
                }
            }
        }

        // Keyboard navigation for SFX index
        let alt = is_key_down(KeyCode::LeftAlt) || is_key_down(KeyCode::RightAlt);
        if !alt {
            if is_key_pressed(KeyCode::Minus) || is_key_pressed(KeyCode::KpSubtract) {
                if self.current_sfx > 0 {
                    self.current_sfx -= 1;
                } else {
                    self.current_sfx = NUM_SFX - 1;
                }
            }
            if is_key_pressed(KeyCode::Equal) || is_key_pressed(KeyCode::KpAdd) {
                if self.current_sfx < NUM_SFX - 1 {
                    self.current_sfx += 1;
                } else {
                    self.current_sfx = 0;
                }
            }
        }

        let sfx = &self.sfx_data[self.current_sfx];

        // ---- Middle: Piano roll display ----
        // Background grid
        console.rectfill(
            SFX_ROLL_X,
            SFX_ROLL_Y,
            SFX_ROLL_X + SFX_ROLL_W - 1,
            SFX_ROLL_Y + SFX_ROLL_H - 1,
            Some(1),
        );

        // Draw vertical grid lines every 4 notes
        for n in (0..NOTES_PER_SFX).step_by(4) {
            let x = SFX_ROLL_X + n as i32 * SFX_NOTE_W;
            for y in SFX_ROLL_Y..SFX_ROLL_Y + SFX_ROLL_H {
                console.raw_pset(x, y, 2);
            }
        }

        // Draw horizontal grid lines at each octave (every 12 semitones)
        for octave_pitch in (0..64).step_by(12) {
            let y = sfx_pitch_to_y(octave_pitch);
            if y >= SFX_ROLL_Y && y < SFX_ROLL_Y + SFX_ROLL_H {
                for x in SFX_ROLL_X..SFX_ROLL_X + SFX_ROLL_W {
                    console.raw_pset(x, y, 2);
                }
            }
        }

        // Draw notes
        for n in 0..NOTES_PER_SFX {
            let note = &sfx.notes[n];
            if note.volume == 0 {
                continue;
            }
            let x = SFX_ROLL_X + n as i32 * SFX_NOTE_W;
            let y = sfx_pitch_to_y(note.pitch);
            let color = waveform_color(note.waveform);

            // Draw a bar whose height is proportional to volume (1-7)
            let bar_h = (note.volume as i32).max(1);
            let y_top = (y - bar_h + 1).max(SFX_ROLL_Y);
            let y_bot = y.min(SFX_ROLL_Y + SFX_ROLL_H - 1);
            console.rectfill(x + 1, y_top, x + SFX_NOTE_W - 1, y_bot, Some(color));
        }

        // ---- Clicking on the piano roll to set note pitch ----
        if self.mouse_down {
            if let Some((note_idx, pitch)) = sfx_click_to_note(self.mouse_x, self.mouse_y) {
                let sfx = &mut self.sfx_data[self.current_sfx];
                sfx.notes[note_idx].pitch = pitch;
                if sfx.notes[note_idx].volume == 0 {
                    sfx.notes[note_idx].volume = 5; // Default volume
                    sfx.notes[note_idx].waveform = 0; // Default triangle
                }
            }
        }

        // Right-click to clear a note
        if is_mouse_button_down(MouseButton::Right) {
            if let Some((note_idx, _)) = sfx_click_to_note(self.mouse_x, self.mouse_y) {
                self.sfx_data[self.current_sfx].notes[note_idx].volume = 0;
            }
        }

        // ---- Bottom: SFX properties ----
        let sfx = &self.sfx_data[self.current_sfx];
        let prop_y = SFX_ROLL_Y + SFX_ROLL_H + 2;
        let speed_str = format!("spd:{}", sfx.speed);
        console.print(&speed_str, Some(1), Some(prop_y), Some(6));

        let loop_str = format!("loop:{}-{}", sfx.loop_start, sfx.loop_end);
        console.print(&loop_str, Some(48), Some(prop_y), Some(6));

        // Waveform legend
        let legend_y = prop_y + 8;
        let waveform_names = ["tri", "tsaw", "saw", "sq", "pls", "org", "noi", "pha"];
        let mut lx: i32 = 1;
        for (i, name) in waveform_names.iter().enumerate() {
            let c = waveform_color(i as u8);
            console.print(name, Some(lx), Some(legend_y), Some(c));
            lx += name.len() as i32 * CHAR_W + 2;
        }

        // ---- Speed +/- buttons ----
        let spd_btn_y = prop_y + 16;
        console.print("spd", Some(1), Some(spd_btn_y), Some(6));
        console.print("-", Some(20), Some(spd_btn_y), Some(7));
        console.print("+", Some(28), Some(spd_btn_y), Some(7));

        if self.mouse_pressed && self.mouse_y >= spd_btn_y && self.mouse_y < spd_btn_y + CHAR_H {
            if self.mouse_x >= 20 && self.mouse_x < 26 {
                let sfx = &mut self.sfx_data[self.current_sfx];
                sfx.speed = sfx.speed.saturating_sub(1).max(1);
            } else if self.mouse_x >= 28 && self.mouse_x < 34 {
                let sfx = &mut self.sfx_data[self.current_sfx];
                sfx.speed = sfx.speed.saturating_add(1);
            }
        }

        // ---- Waveform selector buttons ----
        let wav_btn_y = spd_btn_y + 8;
        console.print("wav", Some(1), Some(wav_btn_y), Some(6));
        for i in 0..8_u8 {
            let bx = 20 + i as i32 * 8;
            let c = waveform_color(i);
            console.rectfill(bx, wav_btn_y, bx + 6, wav_btn_y + 5, Some(c));
            let label = format!("{}", i);
            console.print(&label, Some(bx + 1), Some(wav_btn_y), Some(0));

            if self.mouse_pressed
                && self.mouse_x >= bx
                && self.mouse_x < bx + 7
                && self.mouse_y >= wav_btn_y
                && self.mouse_y < wav_btn_y + 6
            {
                // Set all notes with volume > 0 to this waveform
                let sfx = &mut self.sfx_data[self.current_sfx];
                for note in sfx.notes.iter_mut() {
                    if note.volume > 0 {
                        note.waveform = i;
                    }
                }
            }
        }
    }

    // =======================================================================
    // MUSIC PATTERN EDITOR
    // =======================================================================

    fn update_music(&mut self, console: &mut Console) {
        console.rectfill(0, MAIN_Y, 127, 127, Some(0));

        /// Number of pattern rows visible at once.
        const VISIBLE_ROWS: usize = 16;
        /// Height of each pattern row in pixels.
        const ROW_H: i32 = CHAR_H;
        /// Y position where the pattern list starts.
        const LIST_Y: i32 = MAIN_Y + 1;

        // ---- Keyboard navigation for pattern list ----
        let alt = is_key_down(KeyCode::LeftAlt) || is_key_down(KeyCode::RightAlt);
        if !alt {
            if is_key_pressed(KeyCode::Up) && self.current_music > 0 {
                self.current_music -= 1;
            }
            if is_key_pressed(KeyCode::Down) && self.current_music < NUM_MUSIC - 1 {
                self.current_music += 1;
            }
        }

        // Keep selected pattern visible
        if self.current_music < self.music_scroll {
            self.music_scroll = self.current_music;
        }
        if self.current_music >= self.music_scroll + VISIBLE_ROWS {
            self.music_scroll = self.current_music - VISIBLE_ROWS + 1;
        }

        // ---- Draw pattern list (left side, 16 rows) ----
        // Each row: pattern index, 4 channel SFX numbers, flags
        for i in 0..VISIBLE_ROWS {
            let pat_idx = self.music_scroll + i;
            if pat_idx >= NUM_MUSIC {
                break;
            }
            let py = LIST_Y + i as i32 * ROW_H;
            let pat = &self.music_data[pat_idx];

            // Highlight selected pattern
            let is_selected = pat_idx == self.current_music;
            if is_selected {
                console.rectfill(0, py, 127, py + ROW_H - 2, Some(1));
            }

            // Pattern index
            let idx_color: u8 = if is_selected { 7 } else { 13 };
            let idx_label = format!("{:02}", pat_idx);
            console.print(&idx_label, Some(1), Some(py), Some(idx_color));

            // 4 channel SFX indices
            for ch in 0..4_usize {
                let cx = 14 + ch as i32 * 18;
                let enabled = pat.channel_enabled(ch);
                let sfx_idx = pat.sfx_index(ch);
                let ch_color: u8 = if enabled { 11 } else { 5 };
                let ch_label = if enabled {
                    format!("{:02}", sfx_idx)
                } else {
                    "--".to_string()
                };
                console.print(&ch_label, Some(cx), Some(py), Some(ch_color));
            }

            // Flags: L(oop start), E(nd), S(top)
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

            // Click to select pattern
            if self.mouse_pressed
                && self.mouse_y >= py
                && self.mouse_y < py + ROW_H
                && self.mouse_x >= 0
                && self.mouse_x < 128
            {
                self.current_music = pat_idx;
            }
        }

        // ---- Detail panel for selected pattern (bottom area) ----
        let detail_y = LIST_Y + VISIBLE_ROWS as i32 * ROW_H + 2;
        if detail_y + 20 <= SH {
            // Copy channel data out to avoid borrow conflicts
            let pat_channels = self.music_data[self.current_music].channels;
            let label = format!("PAT {:02}", self.current_music);
            console.print(&label, Some(1), Some(detail_y), Some(7));

            // Channel editors with +/- buttons
            for ch in 0..4_usize {
                let cy = detail_y + 8 + ch as i32 * 8;
                let ch_byte = pat_channels[ch];
                let enabled = (ch_byte & 0x40) == 0;
                let sfx_idx = ch_byte & 0x3F;

                // Channel label
                let ch_label = format!("{}:", ch + 1);
                console.print(&ch_label, Some(1), Some(cy), Some(6));

                // Toggle enabled/disabled button
                let tog_x: i32 = 12;
                let tog_color: u8 = if enabled { 11 } else { 2 };
                console.rectfill(tog_x, cy, tog_x + 5, cy + 5, Some(tog_color));
                if self.mouse_pressed
                    && self.mouse_x >= tog_x
                    && self.mouse_x < tog_x + 6
                    && self.mouse_y >= cy
                    && self.mouse_y < cy + 6
                {
                    // Toggle bit 6 (enabled/disabled)
                    self.music_data[self.current_music].channels[ch] ^= 0x40;
                }

                // SFX index display
                let sfx_x: i32 = 22;
                let sfx_label = format!("{:02}", sfx_idx);
                let sfx_color: u8 = if enabled { 7 } else { 5 };
                console.print(&sfx_label, Some(sfx_x), Some(cy), Some(sfx_color));

                // - button
                let minus_x: i32 = 34;
                console.print("-", Some(minus_x), Some(cy), Some(7));
                if self.mouse_pressed
                    && self.mouse_x >= minus_x
                    && self.mouse_x < minus_x + 6
                    && self.mouse_y >= cy
                    && self.mouse_y < cy + 6
                {
                    let cur = self.music_data[self.current_music].channels[ch];
                    let idx = cur & 0x3F;
                    let hi_bits = cur & 0xC0;
                    let new_idx = if idx > 0 { idx - 1 } else { 63 };
                    self.music_data[self.current_music].channels[ch] = hi_bits | new_idx;
                }

                // + button
                let plus_x: i32 = 42;
                console.print("+", Some(plus_x), Some(cy), Some(7));
                if self.mouse_pressed
                    && self.mouse_x >= plus_x
                    && self.mouse_x < plus_x + 6
                    && self.mouse_y >= cy
                    && self.mouse_y < cy + 6
                {
                    let cur = self.music_data[self.current_music].channels[ch];
                    let idx = cur & 0x3F;
                    let hi_bits = cur & 0xC0;
                    let new_idx = if idx < 63 { idx + 1 } else { 0 };
                    self.music_data[self.current_music].channels[ch] = hi_bits | new_idx;
                }
            }

            // ---- Flag toggle buttons ----
            let flag_btn_y = detail_y + 8 + 4 * 8;
            if flag_btn_y + 6 <= SH {
                let pat_flags = self.music_data[self.current_music].flags;

                // Loop start button
                let ls_x: i32 = 1;
                let ls_on = pat_flags & 1 != 0;
                let ls_c: u8 = if ls_on { 12 } else { 2 };
                console.print("LOOP", Some(ls_x), Some(flag_btn_y), Some(ls_c));
                if self.mouse_pressed
                    && self.mouse_x >= ls_x
                    && self.mouse_x < ls_x + 4 * CHAR_W
                    && self.mouse_y >= flag_btn_y
                    && self.mouse_y < flag_btn_y + CHAR_H
                {
                    self.music_data[self.current_music].flags ^= 1;
                }

                // Loop end button
                let le_x: i32 = 28;
                let le_on = pat_flags & 2 != 0;
                let le_c: u8 = if le_on { 14 } else { 2 };
                console.print("END", Some(le_x), Some(flag_btn_y), Some(le_c));
                if self.mouse_pressed
                    && self.mouse_x >= le_x
                    && self.mouse_x < le_x + 3 * CHAR_W
                    && self.mouse_y >= flag_btn_y
                    && self.mouse_y < flag_btn_y + CHAR_H
                {
                    self.music_data[self.current_music].flags ^= 2;
                }

                // Stop button
                let st_x: i32 = 50;
                let st_on = pat_flags & 4 != 0;
                let st_c: u8 = if st_on { 8 } else { 2 };
                console.print("STOP", Some(st_x), Some(flag_btn_y), Some(st_c));
                if self.mouse_pressed
                    && self.mouse_x >= st_x
                    && self.mouse_x < st_x + 4 * CHAR_W
                    && self.mouse_y >= flag_btn_y
                    && self.mouse_y < flag_btn_y + CHAR_H
                {
                    self.music_data[self.current_music].flags ^= 4;
                }
            }
        }
    }

    // =======================================================================
    // FILE PICKER OVERLAY
    // =======================================================================

    fn update_file_picker(&mut self, console: &mut Console) -> EditorAction {
        // Escape closes the picker
        if is_key_pressed(KeyCode::Escape) {
            self.show_file_picker = false;
            return EditorAction::None;
        }

        // Draw overlay background
        console.rectfill(4, 10, 123, 118, Some(0));
        console.rect(4, 10, 123, 118, Some(7));
        console.print("LOAD .P8", Some(8), Some(12), Some(7));

        if self.file_list.is_empty() {
            console.print("no .p8 files", Some(8), Some(24), Some(8));
            console.print("in current dir", Some(8), Some(32), Some(8));
            console.print("esc to close", Some(8), Some(48), Some(13));
            return EditorAction::None;
        }

        // Scroll with up/down keys
        if is_key_pressed(KeyCode::Up) && self.file_picker_scroll > 0 {
            self.file_picker_scroll -= 1;
        }
        if is_key_pressed(KeyCode::Down) {
            let max_scroll = self.file_list.len().saturating_sub(12);
            if self.file_picker_scroll < max_scroll {
                self.file_picker_scroll += 1;
            }
        }

        // Display up to 12 files
        let visible = 12;
        for i in 0..visible {
            let file_idx = self.file_picker_scroll + i;
            if file_idx >= self.file_list.len() {
                break;
            }
            let fy = 22 + i as i32 * CHAR_H;
            let name = &self.file_list[file_idx];

            // Truncate display name to fit in the overlay
            let max_chars = 22;
            let display: String = name.chars().take(max_chars).collect();
            console.print(&display, Some(8), Some(fy), Some(6));

            // Click to select
            if self.mouse_pressed
                && self.mouse_x >= 8
                && self.mouse_x < 120
                && self.mouse_y >= fy
                && self.mouse_y < fy + CHAR_H
            {
                self.pending_load = Some(name.clone());
                self.show_file_picker = false;
                return EditorAction::LoadCart;
            }
        }

        // Scroll indicators
        if self.file_picker_scroll > 0 {
            console.print("\x18", Some(118), Some(22), Some(13)); // up arrow
        }
        if self.file_picker_scroll + visible < self.file_list.len() {
            console.print("\x19", Some(118), Some(110), Some(13)); // down arrow
        }

        EditorAction::None
    }

    // =======================================================================
    // Reset editor to defaults (for NewCart)
    // =======================================================================

    pub fn reset_to_defaults(&mut self) {
        self.code = DEFAULT_CODE.to_string();
        self.cursor_pos = 0;
        self.scroll_y = 0;
        self.selection_start = None;
        self.search_active = false;
        self.search_query.clear();
        self.search_results.clear();
        self.search_current = 0;
        self.undo_stack.clear();
        self.redo_stack.clear();
        self.clipboard.clear();
        self.last_undo_code = DEFAULT_CODE.to_string();
        self.current_sfx = 0;
        self.sfx_data = [Sfx::default(); NUM_SFX];
        self.music_data = [MusicPattern::default(); NUM_MUSIC];
        self.current_music = 0;
        self.music_scroll = 0;
        self.show_file_picker = false;
        self.file_list.clear();
        self.file_picker_scroll = 0;
        self.pending_load = None;
    }
}

// ---------------------------------------------------------------------------
// Unit tests
// ---------------------------------------------------------------------------

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
        assert_eq!(
            zoom_to_sprite_pixel(SPR_ZOOM_X, SPR_ZOOM_Y),
            Some((0, 0))
        );
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
            sheet_click_to_sprite(
                SHEET_X + 15 * 8,
                SHEET_Y + 3 * 8,
                12
            ),
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
        assert_eq!(palette_click_to_color(7 * PAL_SWATCH + 3, PAL_Y + 2), Some(7));
    }

    #[test]
    fn palette_click_to_color_within_swatch() {
        // Inside the last pixel of swatch 0
        assert_eq!(palette_click_to_color(PAL_SWATCH - 1, PAL_Y + PAL_SWATCH - 1), Some(0));
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

    // ===================================================================
    // map_view_to_tile
    // ===================================================================

    #[test]
    fn map_view_to_tile_top_left_no_scroll() {
        // Click at the top-left of the map view with no scroll
        assert_eq!(
            map_view_to_tile(MAP_VIEW_X, MAP_VIEW_Y, 0, 0),
            Some((0, 0))
        );
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

    // ===================================================================
    // Undo/Redo stack operations
    // ===================================================================

    /// Helper: create an Editor with specific code for testing.
    fn editor_with_code(code: &str) -> Editor {
        let mut ed = Editor::new();
        ed.code = code.to_string();
        ed.last_undo_code = code.to_string();
        ed.cursor_pos = 0;
        ed
    }

    #[test]
    fn undo_push_and_pop() {
        let mut ed = editor_with_code("hello");
        // Simulate an edit: change the code, then push undo
        let old_code = ed.code.clone();
        ed.code = "hello world".to_string();
        ed.cursor_pos = 11;
        ed.push_undo_if_changed();
        // Undo stack should have 1 entry (the old state)
        assert_eq!(ed.undo_stack.len(), 1);
        assert_eq!(ed.undo_stack[0].0, old_code);

        // Now perform undo
        ed.perform_undo();
        assert_eq!(ed.code, "hello");
        assert_eq!(ed.undo_stack.len(), 0);
        assert_eq!(ed.redo_stack.len(), 1);
    }

    #[test]
    fn redo_after_undo() {
        let mut ed = editor_with_code("aaa");
        ed.code = "bbb".to_string();
        ed.cursor_pos = 3;
        ed.push_undo_if_changed();
        // undo
        ed.perform_undo();
        assert_eq!(ed.code, "aaa");
        // redo
        ed.perform_redo();
        assert_eq!(ed.code, "bbb");
        assert_eq!(ed.undo_stack.len(), 1);
        assert_eq!(ed.redo_stack.len(), 0);
    }

    #[test]
    fn undo_on_empty_stack() {
        let mut ed = editor_with_code("test");
        // Undo with nothing on stack should be a no-op
        ed.perform_undo();
        assert_eq!(ed.code, "test");
        assert_eq!(ed.redo_stack.len(), 0);
    }

    #[test]
    fn redo_on_empty_stack() {
        let mut ed = editor_with_code("test");
        // Redo with nothing on stack should be a no-op
        ed.perform_redo();
        assert_eq!(ed.code, "test");
        assert_eq!(ed.undo_stack.len(), 0);
    }

    #[test]
    fn new_edit_clears_redo_stack() {
        let mut ed = editor_with_code("aaa");
        ed.code = "bbb".to_string();
        ed.push_undo_if_changed();
        // undo to get "aaa" back, "bbb" goes to redo
        ed.perform_undo();
        assert_eq!(ed.redo_stack.len(), 1);
        // Now make a new edit
        ed.code = "ccc".to_string();
        ed.push_undo_if_changed();
        // Redo stack should be cleared
        assert_eq!(ed.redo_stack.len(), 0);
    }

    #[test]
    fn undo_stack_size_limit() {
        let mut ed = editor_with_code("start");
        // Push more than UNDO_MAX entries
        for i in 0..UNDO_MAX + 20 {
            ed.code = format!("version_{}", i);
            ed.push_undo_if_changed();
        }
        assert!(ed.undo_stack.len() <= UNDO_MAX);
    }

    #[test]
    fn push_undo_no_duplicate() {
        let mut ed = editor_with_code("hello");
        // Push without changing code -- should not add to stack
        ed.push_undo_if_changed();
        assert_eq!(ed.undo_stack.len(), 0);
    }

    // ===================================================================
    // Clipboard copy/paste of lines
    // ===================================================================

    #[test]
    fn clipboard_copy_line_basic() {
        let mut ed = editor_with_code("line1\nline2\nline3");
        ed.cursor_pos = 6; // start of "line2"
        ed.clipboard_copy_line();
        assert_eq!(ed.clipboard, "line2");
    }

    #[test]
    fn clipboard_copy_first_line() {
        let mut ed = editor_with_code("first\nsecond");
        ed.cursor_pos = 0;
        ed.clipboard_copy_line();
        assert_eq!(ed.clipboard, "first");
    }

    #[test]
    fn clipboard_copy_last_line() {
        let mut ed = editor_with_code("a\nb\nc");
        ed.cursor_pos = 4; // at "c"
        ed.clipboard_copy_line();
        assert_eq!(ed.clipboard, "c");
    }

    #[test]
    fn clipboard_cut_line_removes_line() {
        let mut ed = editor_with_code("line1\nline2\nline3");
        ed.cursor_pos = 6; // start of "line2"
        ed.clipboard_cut_line();
        assert_eq!(ed.clipboard, "line2");
        // "line2\n" should be removed
        assert_eq!(ed.code, "line1\nline3");
    }

    #[test]
    fn clipboard_cut_last_line() {
        let mut ed = editor_with_code("aaa\nbbb");
        ed.cursor_pos = 4; // at "bbb"
        ed.clipboard_cut_line();
        assert_eq!(ed.clipboard, "bbb");
        assert_eq!(ed.code, "aaa");
    }

    #[test]
    fn clipboard_paste_inserts_line() {
        let mut ed = editor_with_code("line1\nline3");
        ed.clipboard = "line2".to_string();
        ed.cursor_pos = 0; // on "line1"
        ed.clipboard_paste();
        // Should insert "line2\n" after line1
        assert_eq!(ed.code, "line1\nline2\nline3");
    }

    #[test]
    fn clipboard_paste_empty_clipboard() {
        let mut ed = editor_with_code("hello");
        ed.clipboard = String::new();
        let original = ed.code.clone();
        ed.clipboard_paste();
        // Nothing should change
        assert_eq!(ed.code, original);
    }

    #[test]
    fn clipboard_cut_pushes_undo() {
        let mut ed = editor_with_code("aaa\nbbb");
        ed.cursor_pos = 4;
        ed.clipboard_cut_line();
        // Should have pushed an undo entry
        assert_eq!(ed.undo_stack.len(), 1);
        assert_eq!(ed.undo_stack[0].0, "aaa\nbbb");
    }

    #[test]
    fn clipboard_paste_pushes_undo() {
        let mut ed = editor_with_code("hello");
        ed.clipboard = "world".to_string();
        ed.cursor_pos = 0;
        ed.clipboard_paste();
        assert_eq!(ed.undo_stack.len(), 1);
        assert_eq!(ed.undo_stack[0].0, "hello");
    }

    // ===================================================================
    // Syntax highlighting tokenizer
    // ===================================================================

    #[test]
    fn tokenize_comment() {
        let tokens = tokenize_line("-- this is a comment");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Comment);
        assert_eq!(tokens[0].start, 0);
        assert_eq!(tokens[0].end, 20);
    }

    #[test]
    fn tokenize_inline_comment() {
        let tokens = tokenize_line("x=1 -- comment");
        // Should have tokens for: x, =, 1, space, comment
        let comment = tokens.iter().find(|t| t.kind == TokenKind::Comment);
        assert!(comment.is_some());
        let c = comment.unwrap();
        assert_eq!(&"x=1 -- comment"[c.start..c.end], "-- comment");
    }

    #[test]
    fn tokenize_string_double_quotes() {
        let tokens = tokenize_line("\"hello world\"");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::StringLit);
    }

    #[test]
    fn tokenize_string_single_quotes() {
        let tokens = tokenize_line("'test'");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::StringLit);
    }

    #[test]
    fn tokenize_string_with_escape() {
        let tokens = tokenize_line("\"he\\\"llo\"");
        // Should be a single string token
        let strings: Vec<_> = tokens.iter().filter(|t| t.kind == TokenKind::StringLit).collect();
        assert_eq!(strings.len(), 1);
    }

    #[test]
    fn tokenize_number_decimal() {
        let tokens = tokenize_line("42");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Number);
    }

    #[test]
    fn tokenize_number_hex() {
        let tokens = tokenize_line("0xFF");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Number);
    }

    #[test]
    fn tokenize_number_with_decimal_point() {
        let tokens = tokenize_line("3.14");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Number);
    }

    #[test]
    fn tokenize_keyword_if() {
        let tokens = tokenize_line("if x then");
        let kw: Vec<_> = tokens.iter().filter(|t| t.kind == TokenKind::Keyword).collect();
        assert_eq!(kw.len(), 2); // "if" and "then"
    }

    #[test]
    fn tokenize_keyword_function() {
        let tokens = tokenize_line("function foo()");
        let kw: Vec<_> = tokens.iter().filter(|t| t.kind == TokenKind::Keyword).collect();
        assert_eq!(kw.len(), 1);
        assert_eq!(&"function foo()"[kw[0].start..kw[0].end], "function");
    }

    #[test]
    fn tokenize_all_keyword_types() {
        for kw in LUA_KEYWORDS {
            let tokens = tokenize_line(kw);
            assert_eq!(
                tokens[0].kind,
                TokenKind::Keyword,
                "Expected keyword for: {}",
                kw
            );
        }
    }

    #[test]
    fn tokenize_api_function() {
        let tokens = tokenize_line("cls(1)");
        let api: Vec<_> = tokens.iter().filter(|t| t.kind == TokenKind::ApiFunc).collect();
        assert_eq!(api.len(), 1);
        assert_eq!(&"cls(1)"[api[0].start..api[0].end], "cls");
    }

    #[test]
    fn tokenize_api_pset() {
        let tokens = tokenize_line("pset(10,20,7)");
        let api: Vec<_> = tokens.iter().filter(|t| t.kind == TokenKind::ApiFunc).collect();
        assert_eq!(api.len(), 1);
        assert_eq!(&"pset(10,20,7)"[api[0].start..api[0].end], "pset");
    }

    #[test]
    fn tokenize_identifier_not_keyword() {
        let tokens = tokenize_line("foobar");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Default);
    }

    #[test]
    fn tokenize_empty_line() {
        let tokens = tokenize_line("");
        assert_eq!(tokens.len(), 0);
    }

    #[test]
    fn tokenize_mixed_line() {
        // "local x = 42 -- init"
        let line = "local x = 42 -- init";
        let tokens = tokenize_line(line);
        let kinds: Vec<TokenKind> = tokens.iter().map(|t| t.kind).collect();
        // Expected: Keyword("local"), Default(" "), Default("x"), Default(" = "),
        //           Number("42"), Default(" "), Comment("-- init")
        assert!(kinds.contains(&TokenKind::Keyword));
        assert!(kinds.contains(&TokenKind::Number));
        assert!(kinds.contains(&TokenKind::Comment));
    }

    #[test]
    fn tokenize_contiguous_coverage() {
        // Ensure tokens cover the entire line without gaps
        let line = "if x>0 then cls(1) end -- ok";
        let tokens = tokenize_line(line);
        assert!(!tokens.is_empty());
        assert_eq!(tokens[0].start, 0);
        for i in 1..tokens.len() {
            assert_eq!(
                tokens[i].start, tokens[i - 1].end,
                "Gap between token {} and {} at positions {}-{}",
                i - 1, i, tokens[i - 1].end, tokens[i].start
            );
        }
        assert_eq!(tokens.last().unwrap().end, line.len());
    }

    #[test]
    fn token_color_mapping() {
        assert_eq!(token_color(TokenKind::Default), 6);
        assert_eq!(token_color(TokenKind::Comment), 13);
        assert_eq!(token_color(TokenKind::StringLit), 4);
        assert_eq!(token_color(TokenKind::Number), 14);
        assert_eq!(token_color(TokenKind::Keyword), 12);
        assert_eq!(token_color(TokenKind::ApiFunc), 9);
    }

    // ===================================================================
    // SaveCart action
    // ===================================================================

    #[test]
    fn editor_action_save_cart_variant_exists() {
        // Verify the SaveCart variant can be constructed and matched
        let action = EditorAction::SaveCart;
        match action {
            EditorAction::SaveCart => {} // expected
            _ => panic!("Expected SaveCart variant"),
        }
    }

    #[test]
    fn editor_action_enum_variants() {
        // Ensure all variants can be constructed
        let _none = EditorAction::None;
        let _run = EditorAction::RunGame;
        let _save = EditorAction::SaveCart;
        let _save_as = EditorAction::SaveCartAs;
        let _new = EditorAction::NewCart;
        let _load = EditorAction::LoadCart;
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
    // Selection clearing on non-shift cursor movement
    // ===================================================================

    #[test]
    fn selection_cleared_on_construction() {
        let ed = Editor::new();
        assert_eq!(ed.selection_start, None);
    }

    #[test]
    fn selection_start_field_set_and_cleared() {
        let mut ed = editor_with_code("hello world");
        // Simulate setting a selection
        ed.selection_start = Some(2);
        ed.cursor_pos = 7;
        assert_eq!(ed.selection_start, Some(2));
        // Simulate clearing selection (as arrow key without shift would)
        ed.selection_start = None;
        assert_eq!(ed.selection_start, None);
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
    // SFX editor: pitch-to-y conversion
    // ===================================================================

    #[test]
    fn sfx_pitch_to_y_zero_at_bottom() {
        let y = sfx_pitch_to_y(0);
        // Pitch 0 should be near the bottom of the roll
        assert_eq!(y, SFX_ROLL_Y + SFX_ROLL_H - 1);
    }

    #[test]
    fn sfx_pitch_to_y_max_at_top() {
        let y = sfx_pitch_to_y(63);
        // Pitch 63 should be near the top of the roll
        assert!(y >= SFX_ROLL_Y);
        assert!(y < SFX_ROLL_Y + SFX_ROLL_H);
    }

    #[test]
    fn sfx_pitch_to_y_monotonic() {
        // Higher pitches should yield lower y values (closer to top)
        for p in 1..64_u8 {
            let y_lower = sfx_pitch_to_y(p - 1);
            let y_higher = sfx_pitch_to_y(p);
            assert!(
                y_higher <= y_lower,
                "Pitch {} (y={}) should be <= pitch {} (y={})",
                p, y_higher, p - 1, y_lower
            );
        }
    }

    #[test]
    fn sfx_pitch_to_y_within_bounds() {
        for p in 0..64_u8 {
            let y = sfx_pitch_to_y(p);
            assert!(y >= SFX_ROLL_Y, "pitch {} y={} below roll top", p, y);
            assert!(y < SFX_ROLL_Y + SFX_ROLL_H, "pitch {} y={} above roll bottom", p, y);
        }
    }

    // ===================================================================
    // SFX editor: click-to-note mapping
    // ===================================================================

    #[test]
    fn sfx_click_to_note_top_left() {
        let result = sfx_click_to_note(SFX_ROLL_X, SFX_ROLL_Y);
        assert!(result.is_some());
        let (note_idx, pitch) = result.unwrap();
        assert_eq!(note_idx, 0);
        // Top of roll = highest pitch
        assert!(pitch > 50, "Expected high pitch at top, got {}", pitch);
    }

    #[test]
    fn sfx_click_to_note_bottom_right() {
        let result = sfx_click_to_note(
            SFX_ROLL_X + SFX_ROLL_W - 1,
            SFX_ROLL_Y + SFX_ROLL_H - 1,
        );
        assert!(result.is_some());
        let (note_idx, pitch) = result.unwrap();
        assert_eq!(note_idx, 31); // last note column
        assert_eq!(pitch, 0); // bottom = lowest pitch
    }

    #[test]
    fn sfx_click_to_note_outside_left() {
        assert_eq!(sfx_click_to_note(SFX_ROLL_X - 1, SFX_ROLL_Y), None);
    }

    #[test]
    fn sfx_click_to_note_outside_right() {
        assert_eq!(sfx_click_to_note(SFX_ROLL_X + SFX_ROLL_W, SFX_ROLL_Y), None);
    }

    #[test]
    fn sfx_click_to_note_outside_above() {
        assert_eq!(sfx_click_to_note(SFX_ROLL_X, SFX_ROLL_Y - 1), None);
    }

    #[test]
    fn sfx_click_to_note_outside_below() {
        assert_eq!(sfx_click_to_note(SFX_ROLL_X, SFX_ROLL_Y + SFX_ROLL_H), None);
    }

    #[test]
    fn sfx_click_to_note_column_boundaries() {
        // First pixel of note 1 (column 1)
        let result = sfx_click_to_note(SFX_ROLL_X + SFX_NOTE_W, SFX_ROLL_Y + SFX_ROLL_H / 2);
        assert!(result.is_some());
        assert_eq!(result.unwrap().0, 1);

        // Last pixel of note 0 (column 0)
        let result = sfx_click_to_note(SFX_ROLL_X + SFX_NOTE_W - 1, SFX_ROLL_Y + SFX_ROLL_H / 2);
        assert!(result.is_some());
        assert_eq!(result.unwrap().0, 0);
    }

    // ===================================================================
    // Waveform color mapping
    // ===================================================================

    #[test]
    fn waveform_color_all_valid() {
        // Ensure all 8 waveforms return a valid PICO-8 color (0-15)
        for w in 0..8_u8 {
            let c = waveform_color(w);
            assert!(c <= 15, "Waveform {} returned color {}", w, c);
        }
    }

    #[test]
    fn waveform_color_wraps() {
        // Values > 7 should wrap (masked by & 0x07)
        assert_eq!(waveform_color(8), waveform_color(0));
        assert_eq!(waveform_color(11), waveform_color(3));
    }

    // ===================================================================
    // EditorTab::Sfx variant
    // ===================================================================

    #[test]
    fn editor_tab_sfx_exists() {
        let tab = EditorTab::Sfx;
        assert_eq!(tab, EditorTab::Sfx);
    }

    #[test]
    fn editor_tab_music_exists() {
        let tab = EditorTab::Music;
        assert_eq!(tab, EditorTab::Music);
    }

    #[test]
    fn editor_tab_equality() {
        assert_ne!(EditorTab::Code, EditorTab::Sfx);
        assert_ne!(EditorTab::Sprite, EditorTab::Sfx);
        assert_ne!(EditorTab::Map, EditorTab::Sfx);
        assert_ne!(EditorTab::Music, EditorTab::Sfx);
        assert_ne!(EditorTab::Music, EditorTab::Code);
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
    // Search: next/previous wrapping
    // ===================================================================

    #[test]
    fn search_next_wraps_around() {
        let mut ed = editor_with_code("abc abc abc");
        ed.search_active = true;
        ed.search_query = "abc".to_string();
        ed.update_search_results();
        assert_eq!(ed.search_results.len(), 3);

        // Initial current = 0
        assert_eq!(ed.search_current, 0);

        // Advance through all matches
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
        let mut ed = editor_with_code("abc abc abc");
        ed.search_active = true;
        ed.search_query = "abc".to_string();
        ed.update_search_results();
        assert_eq!(ed.search_results.len(), 3);

        // Start at 0, go previous => wraps to last
        ed.search_current = if ed.search_current == 0 {
            ed.search_results.len() - 1
        } else {
            ed.search_current - 1
        };
        assert_eq!(ed.search_current, 2);

        // Go previous again
        ed.search_current = if ed.search_current == 0 {
            ed.search_results.len() - 1
        } else {
            ed.search_current - 1
        };
        assert_eq!(ed.search_current, 1);
    }

    #[test]
    fn search_cursor_jumps_to_match() {
        let mut ed = editor_with_code("hello world hello");
        ed.search_active = true;
        ed.search_query = "hello".to_string();
        ed.update_search_results();
        assert_eq!(ed.search_results.len(), 2);
        // cursor should be at first match
        assert_eq!(ed.cursor_pos, 0);

        // Advance to next match
        ed.search_current = (ed.search_current + 1) % ed.search_results.len();
        ed.cursor_pos = ed.search_results[ed.search_current];
        assert_eq!(ed.cursor_pos, 12);
    }

    // ===================================================================
    // Token count
    // ===================================================================

    #[test]
    fn token_count_simple_code() {
        // "x = 1" => x(1) =(1) 1(1) = 3 tokens
        let count = count_tokens("x = 1");
        assert_eq!(count, 3);
    }

    #[test]
    fn token_count_with_comment() {
        // Comments should not count as tokens
        let count = count_tokens("x = 1 -- set x");
        assert_eq!(count, 3); // same as without comment
    }

    #[test]
    fn token_count_with_keywords() {
        // "if x then" => if(1) x(1) then(1) = 3 tokens
        let count = count_tokens("if x then");
        assert_eq!(count, 3);
    }

    #[test]
    fn token_count_with_string() {
        // '"hello"' => 1 string token
        let count = count_tokens("\"hello\"");
        assert_eq!(count, 1);
    }

    #[test]
    fn token_count_free_punctuation() {
        // "f(x,y)" => f(1) ( free ) x(1) , free  y(1) ) free = 3 tokens
        let count = count_tokens("f(x,y)");
        assert_eq!(count, 3);
    }

    #[test]
    fn token_count_operators() {
        // "a + b * c" => a(1) +(1) b(1) *(1) c(1) = 5 tokens
        let count = count_tokens("a + b * c");
        assert_eq!(count, 5);
    }

    #[test]
    fn token_count_empty_code() {
        let count = count_tokens("");
        assert_eq!(count, 0);
    }

    #[test]
    fn token_count_only_comments() {
        let count = count_tokens("-- just a comment\n-- another");
        assert_eq!(count, 0);
    }

    #[test]
    fn token_count_multiline() {
        let code = "local x = 1\nlocal y = 2";
        let count = count_tokens(code);
        // local(1) x(1) =(1) 1(1) = 4 per line, 8 total
        assert_eq!(count, 8);
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
    // Auto-indent: integration via editor
    // ===================================================================

    #[test]
    fn auto_indent_matches_previous_line() {
        let mut ed = editor_with_code("  hello");
        // Position cursor at end of line
        ed.cursor_pos = 7; // end of "  hello"
        ed.push_undo_if_changed(); // ensure undo state is fresh

        // Simulate pressing Enter: insert newline with auto-indent
        let lines = split_lines(&ed.code);
        let (cur_line, _) = cursor_to_line_col(ed.cursor_pos, &lines);
        let current_line_text = lines.get(cur_line).copied().unwrap_or("");
        let base_indent = leading_whitespace(current_line_text).to_string();
        let extra_indent = if ends_with_indent_keyword(current_line_text) {
            "  "
        } else {
            ""
        };
        let indent = format!("{}{}", base_indent, extra_indent);
        let insert_str = format!("\n{}", indent);
        ed.code.insert_str(ed.cursor_pos, &insert_str);
        ed.cursor_pos += insert_str.len();

        assert_eq!(ed.code, "  hello\n  ");
        // Cursor should be at position 10 (after the indent)
        assert_eq!(ed.cursor_pos, 10);
    }

    #[test]
    fn auto_indent_extra_after_then() {
        let mut ed = editor_with_code("  if x then");
        ed.cursor_pos = 11; // end of "  if x then"

        let lines = split_lines(&ed.code);
        let (cur_line, _) = cursor_to_line_col(ed.cursor_pos, &lines);
        let current_line_text = lines.get(cur_line).copied().unwrap_or("");
        let base_indent = leading_whitespace(current_line_text).to_string();
        let extra_indent = if ends_with_indent_keyword(current_line_text) {
            "  "
        } else {
            ""
        };
        let indent = format!("{}{}", base_indent, extra_indent);
        let insert_str = format!("\n{}", indent);
        ed.code.insert_str(ed.cursor_pos, &insert_str);
        ed.cursor_pos += insert_str.len();

        assert_eq!(ed.code, "  if x then\n    ");
        assert_eq!(ed.cursor_pos, 16);
    }

    #[test]
    fn auto_indent_extra_after_do() {
        let mut ed = editor_with_code("for i=1,10 do");
        ed.cursor_pos = ed.code.len();

        let lines = split_lines(&ed.code);
        let (cur_line, _) = cursor_to_line_col(ed.cursor_pos, &lines);
        let current_line_text = lines.get(cur_line).copied().unwrap_or("");
        let base_indent = leading_whitespace(current_line_text).to_string();
        let extra_indent = if ends_with_indent_keyword(current_line_text) {
            "  "
        } else {
            ""
        };
        let indent = format!("{}{}", base_indent, extra_indent);
        let insert_str = format!("\n{}", indent);
        ed.code.insert_str(ed.cursor_pos, &insert_str);
        ed.cursor_pos += insert_str.len();

        assert_eq!(ed.code, "for i=1,10 do\n  ");
    }

    #[test]
    fn auto_indent_extra_after_function() {
        let mut ed = editor_with_code("function _init()");
        ed.cursor_pos = ed.code.len();

        let lines = split_lines(&ed.code);
        let (cur_line, _) = cursor_to_line_col(ed.cursor_pos, &lines);
        let current_line_text = lines.get(cur_line).copied().unwrap_or("");
        let base_indent = leading_whitespace(current_line_text).to_string();
        let extra_indent = if ends_with_indent_keyword(current_line_text) {
            "  "
        } else {
            ""
        };
        let indent = format!("{}{}", base_indent, extra_indent);
        let insert_str = format!("\n{}", indent);
        ed.code.insert_str(ed.cursor_pos, &insert_str);
        ed.cursor_pos += insert_str.len();

        assert_eq!(ed.code, "function _init()\n  ");
    }

    // ===================================================================
    // Search state in Editor
    // ===================================================================

    #[test]
    fn search_fields_default() {
        let ed = Editor::new();
        assert!(!ed.search_active);
        assert!(ed.search_query.is_empty());
        assert!(ed.search_results.is_empty());
        assert_eq!(ed.search_current, 0);
    }

    #[test]
    fn search_update_results_populates() {
        let mut ed = editor_with_code("foo bar foo baz foo");
        ed.search_query = "foo".to_string();
        ed.update_search_results();
        assert_eq!(ed.search_results.len(), 3);
        assert_eq!(ed.search_results[0], 0);
        assert_eq!(ed.search_results[1], 8);
        assert_eq!(ed.search_results[2], 16);
    }

    #[test]
    fn search_update_results_empty_query() {
        let mut ed = editor_with_code("foo bar");
        ed.search_query = String::new();
        ed.update_search_results();
        assert!(ed.search_results.is_empty());
    }

    #[test]
    fn reset_clears_search_state() {
        let mut ed = editor_with_code("test code");
        ed.search_active = true;
        ed.search_query = "test".to_string();
        ed.search_results = vec![0];
        ed.search_current = 0;

        ed.reset_to_defaults();

        assert!(!ed.search_active);
        assert!(ed.search_query.is_empty());
        assert!(ed.search_results.is_empty());
        assert_eq!(ed.search_current, 0);
    }
}
