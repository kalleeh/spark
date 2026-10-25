//! Board abstraction: everything that knows a pin number lives below here.
//!
//! Exactly one board is compiled in, selected by a Cargo feature:
//! * `board-pico2` (default) -- Raspberry Pi Pico 2 with an SPI ST7789 LCD,
//!   8 GPIO buttons and PWM audio (`pico2`).
//! * `board-tufty2350` -- Pimoroni Tufty 2350: parallel LCD over PIO/DMA,
//!   6 buttons, 8 MB PSRAM, no audio (`tufty2350`; see
//!   `docs/tufty2350-port.md`).
//!
//! `init` brings up clocks and peripherals, registers the LCD with
//! `display::install` and hands `main` a [`Board`] with everything the
//! board-independent frame loop needs. `main.rs` must not touch any pin or
//! peripheral directly.

#[cfg(all(feature = "board-pico2", feature = "board-tufty2350"))]
compile_error!("enable exactly one board feature: `board-pico2` or `board-tufty2350`, not both");
#[cfg(not(any(feature = "board-pico2", feature = "board-tufty2350")))]
compile_error!("enable exactly one board feature: `board-pico2` (default) or `board-tufty2350`");

#[cfg(feature = "board-pico2")]
pub mod pico2;
#[cfg(feature = "board-tufty2350")]
pub mod tufty2350;

#[cfg(feature = "board-pico2")]
pub use pico2::init;
#[cfg(feature = "board-tufty2350")]
pub use tufty2350::init;

/// The system timer (TIMER0, 1 MHz), started by `init`.
pub type Timer = rp235x_hal::Timer<rp235x_hal::timer::CopyableTimer0>;

// GPIO masks `fatal.rs` toggles through `SIO.GPIO_OUT_XOR` when the firmware
// dies, without owning the pins. Each board's `init` configures the LED pin
// as an SIO output. Both must be GPIO < 32: GPIO 32-47 live in the separate
// `GPIO_HI_OUT_*` SIO registers, which `fatal.rs` does not touch.
//
// Pico 2: on-board LED GPIO 25, LCD backlight GPIO 22 (plain SIO output, so
// the blink is visible on the panel too).
#[cfg(feature = "board-pico2")]
pub const FATAL_LED_MASK: u32 = 1 << 25;
#[cfg(feature = "board-pico2")]
pub const FATAL_BACKLIGHT_MASK: u32 = 1 << 22;
// Tufty 2350: no plain LED -- the first case LED (GPIO 0) is used instead.
// The backlight (GPIO 26) is a PWM pad on this board, so XOR-ing its SIO bit
// has no visible effect (the panel simply stays lit, which keeps the error
// text readable); the mask is kept for the shared blink code path.
#[cfg(feature = "board-tufty2350")]
pub const FATAL_LED_MASK: u32 = 1 << 0;
#[cfg(feature = "board-tufty2350")]
pub const FATAL_BACKLIGHT_MASK: u32 = 1 << 26;

/// Per-frame raw button levels, `true` = pressed (already inverted from any
/// active-low wiring). Debouncing is done afterwards by `input::Input`.
pub trait InputSource {
    /// PICO-8 button order: left, right, up, down, O, X.
    fn read(&mut self) -> [bool; 6];
}

/// An external RAM window mapped into the address space (Tufty 2350 PSRAM on
/// QMI chip-select 1). Used as a second heap; never the global allocator.
#[derive(Clone, Copy)]
pub struct PsramRegion {
    pub base: usize,
    pub len: usize,
}

/// The PWM slices `audio_out::start` needs: `carrier.channel_a` must already
/// be routed to the audio pin; `tick` has no pin and only supplies the
/// sample-rate wrap interrupt.
///
/// Only `audio_out::start` (pico2) reads the fields; boards without audio
/// never construct one.
#[cfg_attr(not(feature = "board-pico2"), allow(dead_code))]
pub struct AudioBackend {
    pub carrier: rp235x_hal::pwm::Slice<rp235x_hal::pwm::Pwm4, rp235x_hal::pwm::FreeRunning>,
    pub tick: rp235x_hal::pwm::Slice<rp235x_hal::pwm::Pwm5, rp235x_hal::pwm::FreeRunning>,
}

/// What `board::init` hands to `main`.
pub struct Board {
    /// System clock in Hz.
    pub sys_hz: u32,
    /// TIMER0, already running; used for frame pacing and `stat(8)`.
    pub timer: Timer,
    /// Button source polled once per frame.
    pub input: &'static mut dyn InputSource,
    /// PWM audio hardware, if the board has any. `None` means the synth still
    /// runs (so music/sfx state advances) but nothing is output.
    pub audio: Option<AudioBackend>,
    /// External RAM, if present and successfully brought up.
    pub psram: Option<PsramRegion>,
}
