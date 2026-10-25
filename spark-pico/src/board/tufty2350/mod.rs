//! Pimoroni Tufty 2350 (Badgeware). See `docs/tufty2350-port.md`.
//!
//! * `clock.rs`   -- 200 MHz PLL setup (250 MHz behind `OVERCLOCK_250`)
//! * `lcd.rs`     -- PIO1 + DMA parallel ST7789 driver
//! * `buttons.rs` -- A/B/C/Up/Down/Home -> PICO-8 mapping
//! * `psram.rs`   -- QMI CS1 PSRAM bring-up (T4)
//!
//! Pins touched here (everything else is left at its reset state):
//! * LCD: 21, 26, 27, 28, 30, 31, 32-39 (`lcd::LcdPins`)
//! * Buttons: 6, 7, 9, 10, 11, 22 (`buttons::Buttons`)
//! * GPIO 0: first case LED, SIO output, blinked by `fatal.rs`
//!   (`board::FATAL_LED_MASK`)
//! * GPIO 23 (`WL_ON`) and 41 (`POWER_EN`, RTC/LED rail): inputs with
//!   pull-down, never driven -- nothing needs those rails
//!   (`modules/c/powman/powman.c:59-69,344`)
//!
//! No audio hardware: `Board::audio` is `None`.

pub mod buttons;
pub mod clock;
pub mod lcd;
pub mod psram;

use alloc::boxed::Box;
use embedded_hal::digital::OutputPin;
use rp235x_hal::dma::DMAExt;
use rp235x_hal::Clock;

use super::Board;
use crate::display;

/// Bring up clocks, the LCD, buttons and PSRAM.
///
/// Must be called after the heap is initialised (the LCD and button objects
/// are leaked onto it).
pub fn init(mut pac: rp235x_hal::pac::Peripherals) -> Board {
    // ------------------------------------------------------------------
    // Clocks: 200 MHz
    // ------------------------------------------------------------------
    let mut watchdog = rp235x_hal::Watchdog::new(pac.WATCHDOG);
    let clocks = clock::init(
        pac.XOSC,
        pac.CLOCKS,
        pac.PLL_SYS,
        pac.PLL_USB,
        &pac.POWMAN,
        &mut pac.RESETS,
        &mut watchdog,
    );
    let sys_hz = clocks.system_clock.freq().to_Hz();

    let mut timer = rp235x_hal::Timer::new_timer0(pac.TIMER0, &mut pac.RESETS, &clocks);

    // ------------------------------------------------------------------
    // SIO & GPIO pins
    // ------------------------------------------------------------------
    let sio = rp235x_hal::Sio::new(pac.SIO);
    let pins = rp235x_hal::gpio::Pins::new(
        pac.IO_BANK0,
        pac.PADS_BANK0,
        sio.gpio_bank0,
        &mut pac.RESETS,
    );

    // Case LED 0 as the fatal-error blink indicator (there is no plain
    // on-board LED). SIO output so `fatal.rs` can toggle it via GPIO_OUT_XOR
    // without owning the pin.
    let mut led = pins.gpio0.into_push_pull_output();
    let _ = led.set_low();

    // Power-domain controls: leave low, never drive (module docs).
    let _wl_on = pins.gpio23.into_pull_down_input();
    let _power_en = pins.gpio41.into_pull_down_input();

    // ------------------------------------------------------------------
    // LCD: PIO1 SM0, DMA channel 0, PWM slice 5A (GPIO 26) for the backlight
    // ------------------------------------------------------------------
    let dma = pac.DMA.split(&mut pac.RESETS);
    let pwm_slices = rp235x_hal::pwm::Slices::new(pac.PWM, &mut pac.RESETS);
    let lcd_pins = lcd::LcdPins {
        cs: pins.gpio27,
        dc: pins.gpio28,
        wr: pins.gpio30,
        rd: pins.gpio31,
        db0: pins.gpio32,
        db1: pins.gpio33,
        db2: pins.gpio34,
        db3: pins.gpio35,
        db4: pins.gpio36,
        db5: pins.gpio37,
        db6: pins.gpio38,
        db7: pins.gpio39,
        backlight: pins.gpio26,
        te: pins.gpio21,
    };
    let lcd = lcd::init(
        pac.PIO1,
        dma.ch0,
        pwm_slices.pwm5,
        lcd_pins,
        sys_hz,
        &mut pac.RESETS,
        &mut timer,
    );
    display::install(lcd);

    // ------------------------------------------------------------------
    // Buttons
    // ------------------------------------------------------------------
    let buttons = buttons::Buttons::new(
        pins.gpio11,
        pins.gpio6,
        pins.gpio7,
        pins.gpio9,
        pins.gpio10,
        pins.gpio22,
    );

    Board {
        sys_hz,
        timer,
        input: Box::leak(Box::new(buttons)),
        audio: None,
        psram: psram::init(sys_hz),
    }
}
