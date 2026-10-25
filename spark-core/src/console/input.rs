//! Button state and `btnp` auto-repeat.

use super::*;

// ---------------------------------------------------------------------------
// Input
// ---------------------------------------------------------------------------

impl Console {
    /// Is button `i` currently held for the given `player`?
    /// `i` is the button index (0-5: left, right, up, down, O, X).
    /// `player` selects the player (0 = player 1, 1 = player 2).
    /// Out-of-bounds combinations return `false`.
    pub fn btn(&self, i: u8, player: u8) -> bool {
        let idx = i as usize + (player as usize) * 6;
        if idx >= 12 {
            return false;
        }
        self.btn_state[idx]
    }

    /// Was button `i` *just* pressed this frame for the given `player`?
    /// `i` is the button index (0-5: left, right, up, down, O, X).
    /// `player` selects the player (0 = player 1, 1 = player 2).
    /// Out-of-bounds combinations return `false`.
    pub fn btnp(&self, i: u8, player: u8) -> bool {
        let idx = i as usize + (player as usize) * 6;
        if idx >= 12 {
            return false;
        }
        self.btnp_from_frames(self.btn_frames_held[idx])
    }

    /// PICO-8 `btnp` auto-repeat rule: true on the first held frame, then again
    /// once the button has been held `delay` frames, then every `interval`
    /// frames after that. `frames` is the count of consecutive frames the
    /// button has been held (1 on the first frame).
    ///
    /// The delay/interval come from the `0x5F5C`/`0x5F5D` registers: 0 selects
    /// the defaults (15 / 4); a delay of 255 disables repeat (only the initial
    /// press fires), matching the reference.
    fn btnp_from_frames(&self, frames: u16) -> bool {
        if frames == 0 {
            return false;
        }
        // First press fires immediately.
        if frames == 1 {
            return true;
        }
        let delay = match self.btnp_repeat_delay {
            0 => 15u16,
            255 => return false, // repeat disabled
            d => d as u16,
        };
        let interval = match self.btnp_repeat_interval {
            0 => 4u16,
            i => i as u16,
        };
        // After the delay, fire on the delay frame and every interval after.
        frames >= delay && (frames - delay).is_multiple_of(interval)
    }

    /// Called once per frame before running the game's `_update`.
    /// Saves the current state as *previous* and installs `new_state`, and
    /// updates the per-button held-frame counters that drive `btnp` repeat.
    /// Indices 0-5: player 1 (left, right, up, down, O, X)
    /// Indices 6-11: player 2 (left, right, up, down, O, X)
    pub fn update_input(&mut self, new_state: [bool; 12]) {
        self.btn_prev = self.btn_state;
        self.btn_state = new_state;
        for (held, &down) in self.btn_frames_held.iter_mut().zip(new_state.iter()) {
            *held = if down { held.saturating_add(1) } else { 0 };
        }
    }
}
