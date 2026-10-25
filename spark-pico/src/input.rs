// Button debouncing for the firmware.
//
// The board layer (`board::InputSource::read`) delivers the six PICO-8
// buttons per frame, already inverted so `true` means "pressed":
//   0 = Left  (D-pad)
//   1 = Right (D-pad)
//   2 = Up    (D-pad)
//   3 = Down  (D-pad)
//   4 = O     (action 1)
//   5 = X     (action 2)
//
// Debouncing
// ----------
// Mechanical switches can bounce for 5-20 ms after being pressed or released,
// producing rapid on/off transitions that look like multiple presses. This
// module implements a 2-frame integration debounce: a button's state only
// changes when the raw GPIO reading matches for 2 consecutive frames. At
// 30 fps each frame is ~33 ms, so the debounce window is 33-66 ms (half that
// at 60 fps). This is enough to filter typical bounce without adding
// perceptible input lag.
//
// Edge detection (btnp) is done by `spark_core::console::Console` from the
// per-frame state this module hands it, so `Input` only tracks the debounced
// level.

/// Number of buttons in the PICO-8 button layout (one player).
pub const NUM_BUTTONS: usize = 6;

/// Tracks the debounced pressed state of all 6 buttons across frames.
///
/// # Debounce algorithm
///
/// Each button has a `raw_prev` latch that records the previous frame's raw
/// GPIO reading. The debounced `current` state only changes when the raw
/// reading matches `raw_prev` -- i.e., the signal has been stable for two
/// consecutive frames. This filters mechanical bounce (typically 5-20 ms)
/// with zero additional memory or computation.
pub struct Input {
    /// Debounced button state reported to the game (true = pressed).
    current: [bool; NUM_BUTTONS],
    /// Previous frame's raw GPIO reading (for debounce integration).
    raw_prev: [bool; NUM_BUTTONS],
}

impl Input {
    /// Create a new `Input` with all buttons released.
    pub fn new() -> Self {
        Self {
            current: [false; NUM_BUTTONS],
            raw_prev: [false; NUM_BUTTONS],
        }
    }

    /// Install new raw GPIO readings with 2-frame debounce.
    ///
    /// Call this exactly once per frame, before running the game's `_update`
    /// callback.  Each element of `raw` should be `true` when the
    /// corresponding button is pressed (`board::InputSource::read` already
    /// inverts active-low wiring).
    pub fn update(&mut self, raw: [bool; NUM_BUTTONS]) {
        // 2-frame integration debounce: only accept a state change when
        // the raw reading matches the previous raw reading.
        for i in 0..NUM_BUTTONS {
            if raw[i] == self.raw_prev[i] {
                self.current[i] = raw[i];
            }
            // else: readings disagree, keep current debounced state unchanged
        }
        self.raw_prev = raw;
    }

    /// Return the current debounced button state array.
    ///
    /// This is the value to pass (padded to 12 entries) to
    /// `Console::update_input`.
    #[inline]
    pub fn state(&self) -> [bool; NUM_BUTTONS] {
        self.current
    }
}
