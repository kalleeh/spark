//! Tufty 2350 buttons -> PICO-8 controls.
//!
//! Six tactile switches, all pulled up internally and **active-low**
//! (`board/pimoroni_tufty2350.h:38-42`, `modules/c/powman/powman.c:303-318`):
//!
//! | Button | GPIO | PICO-8 |
//! |--------|------|--------|
//! | A      | 7    | left   |
//! | C      | 10   | right  |
//! | Up     | 11   | up     |
//! | Down   | 6    | down   |
//! | B      | 9    | O      |
//! | Home   | 22   | X      |
//!
//! A/B/C sit in a row under the screen (A left, C right), Up/Down on the
//! side, Home (also BOOTSEL) by itself -- hence A/C as the horizontal d-pad,
//! the middle button as O and Home as X. The reset switch (GPIO 14) is not a
//! game button.

use embedded_hal::digital::InputPin;
use rp235x_hal::gpio::bank0::{Gpio10, Gpio11, Gpio22, Gpio6, Gpio7, Gpio9};
use rp235x_hal::gpio::{FunctionNull, FunctionSioInput, Pin, PullDown, PullUp};

use crate::board::InputSource;

type Raw<I> = Pin<I, FunctionNull, PullDown>;
type Btn<I> = Pin<I, FunctionSioInput, PullUp>;

/// The six buttons, configured as pull-up inputs.
pub struct Buttons {
    up: Btn<Gpio11>,
    down: Btn<Gpio6>,
    a: Btn<Gpio7>,
    b: Btn<Gpio9>,
    c: Btn<Gpio10>,
    home: Btn<Gpio22>,
}

impl Buttons {
    /// Take the raw pins and enable their pull-ups.
    pub fn new(
        up: Raw<Gpio11>,
        down: Raw<Gpio6>,
        a: Raw<Gpio7>,
        b: Raw<Gpio9>,
        c: Raw<Gpio10>,
        home: Raw<Gpio22>,
    ) -> Self {
        Self {
            up: up.into_pull_up_input(),
            down: down.into_pull_up_input(),
            a: a.into_pull_up_input(),
            b: b.into_pull_up_input(),
            c: c.into_pull_up_input(),
            home: home.into_pull_up_input(),
        }
    }
}

/// Read an active-low pin: `true` when pressed. Infallible on rp235x-hal
/// pins; any error would read as "not pressed".
fn pressed<P: InputPin>(pin: &mut P) -> bool {
    pin.is_low().unwrap_or(false)
}

impl InputSource for Buttons {
    fn read(&mut self) -> [bool; 6] {
        [
            pressed(&mut self.a),    // left
            pressed(&mut self.c),    // right
            pressed(&mut self.up),   // up
            pressed(&mut self.down), // down
            pressed(&mut self.b),    // O
            pressed(&mut self.home), // X
        ]
    }
}
