//! Built-in cart editor: five tabs (code, sprite, map, sfx, music) plus a
//! file-picker overlay. `Editor` owns the per-tab state and dispatches the
//! active tab each frame; the host drives it via [`Editor::update`].

mod code;
mod file_picker;
mod highlight;
mod map;
mod music;
mod sfx;
mod sprite;
mod widgets;

use macroquad::prelude::*;
use spark_core::audio::Audio;
use spark_core::console::Console;

pub use code::CodeEditor;
pub use file_picker::FilePicker;
pub use map::MapEditor;
pub use music::MusicEditor;
pub use sfx::SfxEditor;
pub use sprite::SpriteEditor;

use widgets::{alt_down, ctrl_down, shift_down, Mouse, Rect};

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

impl EditorTab {
    const ALL: [EditorTab; 5] = [
        EditorTab::Code,
        EditorTab::Sprite,
        EditorTab::Map,
        EditorTab::Sfx,
        EditorTab::Music,
    ];

    fn label(self) -> &'static str {
        match self {
            EditorTab::Code => "CODE",
            EditorTab::Sprite => "SPR",
            EditorTab::Map => "MAP",
            EditorTab::Sfx => "SFX",
            EditorTab::Music => "MUS",
        }
    }

    /// Neighbouring tab in the given direction, wrapping at both ends.
    fn cycle(self, delta: i32) -> EditorTab {
        let i = Self::ALL.iter().position(|&t| t == self).unwrap_or(0);
        Self::ALL[widgets::wrap_step(i, Self::ALL.len(), delta)]
    }
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
// Shared layout constants
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
// Editor
// ---------------------------------------------------------------------------

pub struct Editor {
    pub tab: EditorTab,
    /// The cart's Lua source. Read/replaced directly by the host.
    pub code: String,

    pub code_editor: CodeEditor,
    pub sprite: SpriteEditor,
    pub map: MapEditor,
    pub sfx: SfxEditor,
    pub music: MusicEditor,
    pub file_picker: FilePicker,

    /// Mouse state in 128x128 coordinates (sampled each `update`).
    pub mouse: Mouse,

    // Screen scale info (set by main loop)
    pub screen_offset_x: f32,
    pub screen_offset_y: f32,
    pub screen_scale: f32,

    /// When a file is selected from the picker, its path is stored here
    /// for main.rs to consume via `.take()`.
    pub pending_load: Option<String>,

    frame: u32,
}

impl Default for Editor {
    fn default() -> Self {
        Self::new()
    }
}

impl Editor {
    pub fn new() -> Self {
        Self {
            tab: EditorTab::Code,
            code: DEFAULT_CODE.to_string(),
            code_editor: CodeEditor::new(DEFAULT_CODE),
            sprite: SpriteEditor::default(),
            map: MapEditor::default(),
            sfx: SfxEditor::default(),
            music: MusicEditor::default(),
            file_picker: FilePicker::default(),
            mouse: Mouse::default(),
            screen_offset_x: 0.0,
            screen_offset_y: 0.0,
            screen_scale: 1.0,
            pending_load: None,
            frame: 0,
        }
    }

    /// Update editor state and draw to `console`. SFX/music edits are made
    /// directly on `audio`. Returns an action for the host.
    pub fn update(&mut self, console: &mut Console, audio: &mut Audio) -> EditorAction {
        self.frame = self.frame.wrapping_add(1);
        self.mouse.poll(
            self.screen_offset_x,
            self.screen_offset_y,
            self.screen_scale,
        );

        console.reset_draw_state();
        console.camera(0, 0);
        console.clip_rect(0, 0, 128, 128, false);

        if let Some(action) = self.handle_global_shortcuts() {
            return action;
        }

        console.cls(Some(0));

        if let EditorAction::RunGame = self.draw_top_bar(console) {
            return EditorAction::RunGame;
        }

        match self.tab {
            EditorTab::Code => {
                self.code_editor
                    .update(console, &mut self.code, &self.mouse, self.frame)
            }
            EditorTab::Sprite => self.sprite.update(console, &self.mouse),
            EditorTab::Map => self.map.update(console, &self.mouse),
            EditorTab::Sfx => self.sfx.update(console, &self.mouse, &mut audio.sfx_data),
            EditorTab::Music => self
                .music
                .update(console, &self.mouse, &mut audio.music_data),
        }

        if self.file_picker.visible {
            if let Some(path) = self.file_picker.update(console, &self.mouse) {
                self.pending_load = Some(path);
                return EditorAction::LoadCart;
            }
        }

        EditorAction::None
    }

    /// Ctrl+R/S/Shift+S/N/O and Alt+Left/Right. Returns an action that must
    /// be handed straight back to the host.
    fn handle_global_shortcuts(&mut self) -> Option<EditorAction> {
        let ctrl = ctrl_down();
        let shift = shift_down();
        if ctrl && is_key_pressed(KeyCode::R) {
            return Some(EditorAction::RunGame);
        }
        if ctrl && shift && is_key_pressed(KeyCode::S) {
            return Some(EditorAction::SaveCartAs);
        }
        if ctrl && !shift && is_key_pressed(KeyCode::S) {
            return Some(EditorAction::SaveCart);
        }
        if ctrl && is_key_pressed(KeyCode::N) {
            return Some(EditorAction::NewCart);
        }
        if ctrl && is_key_pressed(KeyCode::O) {
            self.file_picker.open();
        }

        if alt_down() {
            if is_key_pressed(KeyCode::Left) {
                self.tab = self.tab.cycle(-1);
            }
            if is_key_pressed(KeyCode::Right) {
                self.tab = self.tab.cycle(1);
            }
        }
        None
    }

    /// Tab buttons plus the green "run" triangle on the right.
    fn draw_top_bar(&mut self, console: &mut Console) -> EditorAction {
        console.rectfill(0, 0, 127, TOP_BAR_H - 1, Some(8));

        let mut tx: i32 = 1;
        for tab_id in EditorTab::ALL {
            let label = tab_id.label();
            let w = label.len() as i32 * CHAR_W + 4;
            let selected = self.tab == tab_id;
            let bg: u8 = if selected { 7 } else { 2 };
            let fg: u8 = if selected { 0 } else { 7 };
            console.rectfill(tx, 0, tx + w - 1, TOP_BAR_H - 2, Some(bg));
            console.print(label, Some(tx + 2), Some(1), Some(fg));

            if self.mouse.clicked(Rect::new(tx, 0, w, TOP_BAR_H)) {
                self.tab = tab_id;
            }

            tx += w + 2;
        }

        let run_x = SW - 12;
        let run_y: i32 = 1;
        console.rectfill(run_x - 1, run_y - 1, run_x + 6, run_y + 5, Some(0));
        for i in 0..5_i32 {
            let half = if i <= 2 { i } else { 4 - i };
            for x in run_x..=(run_x + half * 2) {
                console.raw_pset(x, run_y + i, 11);
            }
        }

        if self.mouse.clicked(Rect::new(run_x - 1, 0, 8, TOP_BAR_H)) {
            return EditorAction::RunGame;
        }

        EditorAction::None
    }

    /// Reset code, sfx/music selection and the file picker for a new cart.
    /// (Sprite/map tab positions are kept, as before.)
    pub fn reset_to_defaults(&mut self) {
        self.code = DEFAULT_CODE.to_string();
        self.code_editor = CodeEditor::new(DEFAULT_CODE);
        self.sfx = SfxEditor::default();
        self.music = MusicEditor::default();
        self.file_picker = FilePicker::default();
        self.pending_load = None;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn editor_action_save_cart_variant_exists() {
        let action = EditorAction::SaveCart;
        match action {
            EditorAction::SaveCart => {}
            _ => panic!("Expected SaveCart variant"),
        }
    }

    #[test]
    fn editor_action_enum_variants() {
        let _none = EditorAction::None;
        let _run = EditorAction::RunGame;
        let _save = EditorAction::SaveCart;
        let _save_as = EditorAction::SaveCartAs;
        let _new = EditorAction::NewCart;
        let _load = EditorAction::LoadCart;
    }

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
        assert_eq!(EditorTab::Code, EditorTab::Code);
        assert_ne!(EditorTab::Code, EditorTab::Sprite);
        assert_ne!(EditorTab::Sfx, EditorTab::Music);
        assert_ne!(EditorTab::Music, EditorTab::Code);
    }

    #[test]
    fn editor_tab_cycle_wraps() {
        assert_eq!(EditorTab::Code.cycle(-1), EditorTab::Music);
        assert_eq!(EditorTab::Music.cycle(1), EditorTab::Code);
        assert_eq!(EditorTab::Sprite.cycle(1), EditorTab::Map);
        assert_eq!(EditorTab::Map.cycle(-1), EditorTab::Sprite);
    }

    #[test]
    fn editor_new_defaults() {
        let ed = Editor::new();
        assert_eq!(ed.tab, EditorTab::Code);
        assert_eq!(ed.code, DEFAULT_CODE);
        assert_eq!(ed.code_editor.cursor_pos, 0);
        assert_eq!(ed.sprite.current_sprite, 1);
        assert_eq!(ed.sprite.sprite_color, 7);
        assert_eq!(ed.map.current_tile, 1);
        assert_eq!(ed.sfx.current_sfx, 0);
        assert_eq!(ed.music.current_music, 0);
        assert!(!ed.file_picker.visible);
        assert!(ed.pending_load.is_none());
    }

    #[test]
    fn reset_restores_defaults_but_keeps_sprite_and_map_position() {
        let mut ed = Editor::new();
        ed.code = "print(1)".to_string();
        ed.code_editor.cursor_pos = 5;
        ed.sfx.current_sfx = 7;
        ed.music.current_music = 9;
        ed.sprite.current_sprite = 42;
        ed.map.current_tile = 3;
        ed.pending_load = Some("x.p8".to_string());

        ed.reset_to_defaults();

        assert_eq!(ed.code, DEFAULT_CODE);
        assert_eq!(ed.code_editor.cursor_pos, 0);
        assert_eq!(ed.sfx.current_sfx, 0);
        assert_eq!(ed.music.current_music, 0);
        assert!(ed.pending_load.is_none());
        assert_eq!(ed.sprite.current_sprite, 42);
        assert_eq!(ed.map.current_tile, 3);
    }
}
