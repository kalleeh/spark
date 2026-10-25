//! SFX editor tab: piano-roll note editor with speed and waveform controls.

use macroquad::prelude::*;
use spark_core::audio::{Sfx, NOTES_PER_SFX, NUM_SFX};
use spark_core::console::Console;

use super::widgets::{
    alt_down, arrow_selector, plus_minus_key_delta, step_buttons, wrap_step, Mouse, Rect,
};
use super::{CHAR_W, MAIN_Y};

// ---------------------------------------------------------------------------
// Layout constants
// ---------------------------------------------------------------------------

const SFX_ROLL_X: i32 = 0;
const SFX_ROLL_Y: i32 = 18;
const SFX_ROLL_W: i32 = 128;
const SFX_ROLL_H: i32 = 64;
const SFX_NOTE_W: i32 = 4; // 32 notes * 4 = 128 pixels wide

const WAVEFORM_NAMES: [&str; 8] = ["tri", "tsaw", "saw", "sq", "pls", "org", "noi", "pha"];

// ---------------------------------------------------------------------------
// Pure helpers
// ---------------------------------------------------------------------------

/// Convert a pitch value (0-63) to a Y pixel coordinate within the piano roll area.
/// Pitch 0 is at the bottom, pitch 63 is at the top.
fn sfx_pitch_to_y(pitch: u8) -> i32 {
    let clamped = (pitch as i32).clamp(0, 63);
    SFX_ROLL_Y + SFX_ROLL_H - 1 - (clamped * SFX_ROLL_H / 64)
}

/// Convert a click in the SFX piano roll area to a (note_index, pitch) pair.
/// Returns None if the click is outside the piano roll area.
fn sfx_click_to_note(mouse_x: i32, mouse_y: i32) -> Option<(usize, u8)> {
    if !(SFX_ROLL_X..SFX_ROLL_X + SFX_ROLL_W).contains(&mouse_x)
        || !(SFX_ROLL_Y..SFX_ROLL_Y + SFX_ROLL_H).contains(&mouse_y)
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
// SFX editor
// ---------------------------------------------------------------------------

#[derive(Default)]
pub struct SfxEditor {
    pub current_sfx: usize,
}

impl SfxEditor {
    /// Edits `sfx_data[self.current_sfx]` in place.
    pub fn update(&mut self, console: &mut Console, mouse: &Mouse, sfx_data: &mut [Sfx; NUM_SFX]) {
        console.rectfill(0, MAIN_Y, 127, 127, Some(0));

        self.update_selector(console, mouse);
        let sfx = &mut sfx_data[self.current_sfx];
        Self::update_piano_roll(console, mouse, sfx);
        Self::update_properties(console, mouse, sfx);
    }

    /// `< SFX nn >` header; arrows and `-`/`+` keys wrap over all 64 sounds.
    fn update_selector(&mut self, console: &mut Console, mouse: &Mouse) {
        let sel_y = MAIN_Y + 1;
        let sfx_label = format!("SFX {:02}", self.current_sfx);
        console.print(&sfx_label, Some(10), Some(sel_y), Some(7));
        if let Some(idx) = arrow_selector(console, mouse, 1, 50, sel_y, self.current_sfx, NUM_SFX) {
            self.current_sfx = idx;
        }

        if !alt_down() {
            let delta = plus_minus_key_delta();
            if delta != 0 {
                self.current_sfx = wrap_step(self.current_sfx, NUM_SFX, delta);
            }
        }
    }

    /// Grid + note bars; left-drag sets pitch (enabling silent notes),
    /// right-drag silences a note.
    fn update_piano_roll(console: &mut Console, mouse: &Mouse, sfx: &mut Sfx) {
        console.rectfill(
            SFX_ROLL_X,
            SFX_ROLL_Y,
            SFX_ROLL_X + SFX_ROLL_W - 1,
            SFX_ROLL_Y + SFX_ROLL_H - 1,
            Some(1),
        );

        // Vertical grid lines every 4 notes
        for n in (0..NOTES_PER_SFX).step_by(4) {
            let x = SFX_ROLL_X + n as i32 * SFX_NOTE_W;
            for y in SFX_ROLL_Y..SFX_ROLL_Y + SFX_ROLL_H {
                console.raw_pset(x, y, 2);
            }
        }

        // Horizontal grid lines at each octave (every 12 semitones)
        for octave_pitch in (0..64).step_by(12) {
            let y = sfx_pitch_to_y(octave_pitch);
            if (SFX_ROLL_Y..SFX_ROLL_Y + SFX_ROLL_H).contains(&y) {
                for x in SFX_ROLL_X..SFX_ROLL_X + SFX_ROLL_W {
                    console.raw_pset(x, y, 2);
                }
            }
        }

        // Notes: a bar whose height is proportional to volume (1-7)
        for (n, note) in sfx.notes.iter().enumerate() {
            if note.volume == 0 {
                continue;
            }
            let x = SFX_ROLL_X + n as i32 * SFX_NOTE_W;
            let y = sfx_pitch_to_y(note.pitch);
            let color = waveform_color(note.waveform);
            let bar_h = (note.volume as i32).max(1);
            let y_top = (y - bar_h + 1).max(SFX_ROLL_Y);
            let y_bot = y.min(SFX_ROLL_Y + SFX_ROLL_H - 1);
            console.rectfill(x + 1, y_top, x + SFX_NOTE_W - 1, y_bot, Some(color));
        }

        if mouse.down {
            if let Some((note_idx, pitch)) = sfx_click_to_note(mouse.x, mouse.y) {
                let note = &mut sfx.notes[note_idx];
                note.pitch = pitch;
                if note.volume == 0 {
                    note.volume = 5; // Default volume
                    note.waveform = 0; // Default triangle
                }
            }
        }

        if is_mouse_button_down(MouseButton::Right) {
            if let Some((note_idx, _)) = sfx_click_to_note(mouse.x, mouse.y) {
                sfx.notes[note_idx].volume = 0;
            }
        }
    }

    /// Speed / loop readout, waveform legend, speed `-`/`+` and waveform buttons.
    fn update_properties(console: &mut Console, mouse: &Mouse, sfx: &mut Sfx) {
        let prop_y = SFX_ROLL_Y + SFX_ROLL_H + 2;
        let speed_str = format!("spd:{}", sfx.speed);
        console.print(&speed_str, Some(1), Some(prop_y), Some(6));

        let loop_str = format!("loop:{}-{}", sfx.loop_start, sfx.loop_end);
        console.print(&loop_str, Some(48), Some(prop_y), Some(6));

        // Waveform legend
        let legend_y = prop_y + 8;
        let mut lx: i32 = 1;
        for (i, name) in WAVEFORM_NAMES.iter().enumerate() {
            let c = waveform_color(i as u8);
            console.print(name, Some(lx), Some(legend_y), Some(c));
            lx += name.len() as i32 * CHAR_W + 2;
        }

        // Speed +/- buttons (speed never drops below 1)
        let spd_btn_y = prop_y + 16;
        console.print("spd", Some(1), Some(spd_btn_y), Some(6));
        match step_buttons(console, mouse, 20, 28, spd_btn_y, ("-", "+"), 7) {
            -1 => sfx.speed = sfx.speed.saturating_sub(1).max(1),
            1 => sfx.speed = sfx.speed.saturating_add(1),
            _ => {}
        }

        // Waveform selector buttons: apply to every audible note
        let wav_btn_y = spd_btn_y + 8;
        console.print("wav", Some(1), Some(wav_btn_y), Some(6));
        for i in 0..8_u8 {
            let bx = 20 + i as i32 * 8;
            let c = waveform_color(i);
            console.rectfill(bx, wav_btn_y, bx + 6, wav_btn_y + 5, Some(c));
            console.print(&format!("{}", i), Some(bx + 1), Some(wav_btn_y), Some(0));

            if mouse.clicked(Rect::new(bx, wav_btn_y, 7, 6)) {
                for note in sfx.notes.iter_mut() {
                    if note.volume > 0 {
                        note.waveform = i;
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
                p,
                y_higher,
                p - 1,
                y_lower
            );
        }
    }

    #[test]
    fn sfx_pitch_to_y_within_bounds() {
        for p in 0..64_u8 {
            let y = sfx_pitch_to_y(p);
            assert!(y >= SFX_ROLL_Y, "pitch {} y={} below roll top", p, y);
            assert!(
                y < SFX_ROLL_Y + SFX_ROLL_H,
                "pitch {} y={} above roll bottom",
                p,
                y
            );
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
        let result = sfx_click_to_note(SFX_ROLL_X + SFX_ROLL_W - 1, SFX_ROLL_Y + SFX_ROLL_H - 1);
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
}
