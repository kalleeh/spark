// GPIO button reading for RP2350
//
// Reads 8 GPIO buttons mapped to the PICO-8 button layout:
//   0 = Left  (D-pad)
//   1 = Right (D-pad)
//   2 = Up    (D-pad)
//   3 = Down  (D-pad)
//   4 = O     (action 1)
//   5 = X     (action 2)
//   6 = Start (unused by PICO-8)
//   7 = Select (unused by PICO-8)
//
// Buttons are active-low: the GPIO pin is pulled high by an internal pull-up
// resistor and driven low when the button is pressed. `read_pin_active_low`
// inverts the reading so that `true` means "pressed".
//
// Debouncing
// ----------
// Mechanical switches can bounce for 5-20 ms after being pressed or released,
// producing rapid on/off transitions that look like multiple presses. This
// module implements a 2-frame integration debounce: a button's state only
// changes when the raw GPIO reading matches for 2 consecutive frames. At
// 30 fps each frame is ~33 ms, so the debounce window is 33-66 ms. This
// is enough to filter typical bounce without adding perceptible input lag.
//
// The `Input` struct tracks current and previous frame state so it can
// answer both `btn` (held) and `btnp` (just pressed) queries. The main
// loop should call `Input::update` once per frame with the raw GPIO
// readings, then pass the resulting state to `Console::update_input`.

/// Number of buttons in the PICO-8 button layout.
pub const NUM_BUTTONS: usize = 8;

/// Button indices matching the PICO-8 convention.
#[allow(dead_code)]
pub const BTN_LEFT: u8 = 0;
#[allow(dead_code)]
pub const BTN_RIGHT: u8 = 1;
#[allow(dead_code)]
pub const BTN_UP: u8 = 2;
#[allow(dead_code)]
pub const BTN_DOWN: u8 = 3;
#[allow(dead_code)]
pub const BTN_O: u8 = 4;
#[allow(dead_code)]
pub const BTN_X: u8 = 5;
#[allow(dead_code)]
pub const BTN_START: u8 = 6;
#[allow(dead_code)]
pub const BTN_SELECT: u8 = 7;

/// Read an active-low GPIO pin and return `true` when the button is pressed.
///
/// Buttons wired with an internal pull-up read high when released and low
/// when pressed.  This function inverts the logic so callers get `true` for
/// "pressed".
///
/// The `embedded_hal::digital::InputPin` trait is used so this works with
/// any HAL pin type (including `rp235x_hal` GPIO pins).
pub fn read_pin_active_low<P: embedded_hal::digital::InputPin>(pin: &mut P) -> bool {
    // is_low() returns Result<bool, _>.  On infallible pins the error type
    // is `core::convert::Infallible` so unwrap is always safe.  For any
    // fallible pin we treat an error as "not pressed".
    pin.is_low().unwrap_or(false)
}

/// Tracks the pressed / just-pressed state of all 8 buttons across frames,
/// with 2-frame integration debouncing.
///
/// # Debounce algorithm
///
/// Each button has a `raw_prev` latch that records the previous frame's raw
/// GPIO reading. The debounced `current` state only changes when the raw
/// reading matches `raw_prev` -- i.e., the signal has been stable for two
/// consecutive frames (~66 ms at 30 fps). This filters mechanical bounce
/// (typically 5-20 ms) with zero additional memory or computation.
///
/// # Usage
///
/// ```ignore
/// let mut input = Input::new();
///
/// // Once per frame, before running _update:
/// let raw = [
///     read_pin_active_low(&pin_left),
///     read_pin_active_low(&pin_right),
///     read_pin_active_low(&pin_up),
///     read_pin_active_low(&pin_down),
///     read_pin_active_low(&pin_o),
///     read_pin_active_low(&pin_x),
///     read_pin_active_low(&pin_start),
///     read_pin_active_low(&pin_select),
/// ];
/// input.update(raw);
///
/// // Feed into the spark-core Console:
/// console.update_input(input.state());
/// ```
pub struct Input {
    /// Debounced button state reported to the game (true = pressed).
    current: [bool; NUM_BUTTONS],
    /// Previous frame's debounced state (for edge detection in btnp/btnr).
    previous: [bool; NUM_BUTTONS],
    /// Previous frame's raw GPIO reading (for debounce integration).
    raw_prev: [bool; NUM_BUTTONS],
}

impl Input {
    /// Create a new `Input` with all buttons released.
    pub fn new() -> Self {
        Self {
            current: [false; NUM_BUTTONS],
            previous: [false; NUM_BUTTONS],
            raw_prev: [false; NUM_BUTTONS],
        }
    }

    /// Latch the previous state and install new raw GPIO readings with
    /// 2-frame debounce.
    ///
    /// Call this exactly once per frame, before running the game's `_update`
    /// callback.  Each element of `raw` should be `true` when the
    /// corresponding button is pressed (i.e. already inverted from the
    /// active-low GPIO level -- use `read_pin_active_low`).
    ///
    /// A button's debounced state only changes when the raw reading has been
    /// stable for 2 consecutive frames, filtering mechanical bounce.
    pub fn update(&mut self, raw: [bool; NUM_BUTTONS]) {
        self.previous = self.current;

        // 2-frame integration debounce: only accept a state change when
        // the raw reading matches the previous raw reading.
        let mut i = 0;
        while i < NUM_BUTTONS {
            if raw[i] == self.raw_prev[i] {
                self.current[i] = raw[i];
            }
            // else: readings disagree, keep current debounced state unchanged
            i += 1;
        }

        self.raw_prev = raw;
    }

    /// Return the current debounced button state array.
    ///
    /// This is the value you should pass to `Console::update_input`.
    #[inline]
    pub fn state(&self) -> [bool; NUM_BUTTONS] {
        self.current
    }

    /// Is button `i` currently held down?
    #[inline]
    pub fn btn(&self, i: u8) -> bool {
        let idx = i as usize;
        if idx < NUM_BUTTONS {
            self.current[idx]
        } else {
            false
        }
    }

    /// Was button `i` just pressed this frame (pressed now, released last frame)?
    #[inline]
    pub fn btnp(&self, i: u8) -> bool {
        let idx = i as usize;
        if idx < NUM_BUTTONS {
            self.current[idx] && !self.previous[idx]
        } else {
            false
        }
    }

    /// Was button `i` just released this frame (released now, pressed last frame)?
    #[inline]
    pub fn btnr(&self, i: u8) -> bool {
        let idx = i as usize;
        if idx < NUM_BUTTONS {
            !self.current[idx] && self.previous[idx]
        } else {
            false
        }
    }

    /// Return a bitmask of all currently held buttons (bit 0 = button 0, etc.).
    ///
    /// Useful for compact serialisation or debug display.
    #[inline]
    pub fn held_mask(&self) -> u8 {
        let mut mask: u8 = 0;
        let mut i = 0;
        while i < NUM_BUTTONS {
            if self.current[i] {
                mask |= 1 << i;
            }
            i += 1;
        }
        mask
    }

    /// Return a bitmask of all buttons that were just pressed this frame.
    #[inline]
    pub fn pressed_mask(&self) -> u8 {
        let mut mask: u8 = 0;
        let mut i = 0;
        while i < NUM_BUTTONS {
            if self.current[i] && !self.previous[i] {
                mask |= 1 << i;
            }
            i += 1;
        }
        mask
    }
}
