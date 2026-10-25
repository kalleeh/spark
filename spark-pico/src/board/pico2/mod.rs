//! Raspberry Pi Pico 2 with a hand-wired SPI ST7789 LCD.
//!
//! Pins:
//! * SPI0 ST7789 240x240: SCK 18, MOSI 19, CS 17, DC 20, RST 21, backlight 22
//! * Buttons (active-low, pull-up): GPIO 0-7 = left, right, up, down, O, X,
//!   start, select. Only the first six are PICO-8 buttons; 6/7 are still
//!   configured as pull-ups but not read.
//! * PWM audio: GPIO 8 (slice 4 channel A carrier; slice 5 is the sample clock)
//! * On-board LED: GPIO 25 (blinked by `fatal.rs` via SIO)

pub mod lcd;

use alloc::boxed::Box;
use embedded_hal::digital::{InputPin, OutputPin};
use fugit::RateExtU32;
use rp235x_hal::gpio::bank0::{Gpio0, Gpio1, Gpio2, Gpio3, Gpio4, Gpio5};
use rp235x_hal::gpio::{FunctionSio, Pin, PullUp, SioInput};
use rp235x_hal::Clock;

use super::{AudioBackend, Board, InputSource};
use crate::display;

/// Read an active-low GPIO pin and return `true` when the button is pressed.
fn read_pin_active_low<P: InputPin>(pin: &mut P) -> bool {
    // Infallible on rp235x-hal pins; any error reads as "not pressed".
    pin.is_low().unwrap_or(false)
}

type Btn<I> = Pin<I, FunctionSio<SioInput>, PullUp>;

/// The six PICO-8 buttons on GPIO 0-5.
pub struct Buttons {
    left: Btn<Gpio0>,
    right: Btn<Gpio1>,
    up: Btn<Gpio2>,
    down: Btn<Gpio3>,
    o: Btn<Gpio4>,
    x: Btn<Gpio5>,
}

impl InputSource for Buttons {
    fn read(&mut self) -> [bool; 6] {
        [
            read_pin_active_low(&mut self.left),
            read_pin_active_low(&mut self.right),
            read_pin_active_low(&mut self.up),
            read_pin_active_low(&mut self.down),
            read_pin_active_low(&mut self.o),
            read_pin_active_low(&mut self.x),
        ]
    }
}

/// Bring up clocks (150 MHz), the LCD, buttons and audio PWM.
///
/// Must be called after the heap is initialised (the LCD and button objects
/// are leaked onto it, a few dozen bytes for the program's lifetime).
pub fn init(mut pac: rp235x_hal::pac::Peripherals) -> Board {
    // ------------------------------------------------------------------
    // Clocks
    // ------------------------------------------------------------------
    let mut watchdog = rp235x_hal::Watchdog::new(pac.WATCHDOG);

    let clocks = rp235x_hal::clocks::init_clocks_and_plls(
        12_000_000u32, // XOSC crystal frequency
        pac.XOSC,
        pac.CLOCKS,
        pac.PLL_SYS,
        pac.PLL_USB,
        &mut pac.RESETS,
        &mut watchdog,
    )
    .unwrap();
    let sys_hz = clocks.system_clock.freq().to_Hz();

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

    // On-board LED (Pico 2: GPIO 25). Configured as an SIO output so the
    // panic handler can blink it through the SIO XOR register; the pin
    // object itself is not needed after this (the pad keeps its function).
    let mut led = pins.gpio25.into_push_pull_output();
    let _ = led.set_low();

    // ------------------------------------------------------------------
    // SPI0 for ST7789 display
    // ------------------------------------------------------------------
    let sck_pin = pins.gpio18.into_function::<rp235x_hal::gpio::FunctionSpi>();
    let mosi_pin = pins.gpio19.into_function::<rp235x_hal::gpio::FunctionSpi>();
    let cs_pin = pins.gpio17.into_push_pull_output();
    let dc_pin = pins.gpio20.into_push_pull_output();
    let rst_pin = pins.gpio21.into_push_pull_output();
    let mut bl_pin = pins.gpio22.into_push_pull_output();

    // Turn on backlight
    let _ = bl_pin.set_high();

    // rp235x-hal SPI::new takes (TX/MOSI, SCK) order
    let spi_device = rp235x_hal::spi::Spi::<_, _, _, 8>::new(pac.SPI0, (mosi_pin, sck_pin));

    let spi = spi_device.init(
        &mut pac.RESETS,
        clocks.peripheral_clock.freq(),
        62_500_000u32.Hz(),
        embedded_hal::spi::MODE_3,
    );

    let spi_bus = embedded_hal_bus::spi::ExclusiveDevice::new_no_delay(spi, cs_pin).unwrap();

    // ------------------------------------------------------------------
    // Button inputs (active-low with pull-up)
    // ------------------------------------------------------------------
    let buttons = Buttons {
        left: pins.gpio0.into_pull_up_input(),
        right: pins.gpio1.into_pull_up_input(),
        up: pins.gpio2.into_pull_up_input(),
        down: pins.gpio3.into_pull_up_input(),
        o: pins.gpio4.into_pull_up_input(),
        x: pins.gpio5.into_pull_up_input(),
    };
    // Start / Select: wired but not PICO-8 buttons. Keep the pads configured
    // as before; the levels are never read.
    let _start = pins.gpio6.into_pull_up_input();
    let _select = pins.gpio7.into_pull_up_input();

    // ------------------------------------------------------------------
    // PWM audio: slice 4 channel A on GPIO 8 (carrier), slice 5 (sample clock)
    // ------------------------------------------------------------------
    // GPIO 8 is the first free pin after the 8 button inputs (GPIO 0-7).
    // Connect GPIO 8 through a low-pass RC filter (1 kOhm + 100 nF) to
    // a speaker or small amplifier. See audio_out.rs for the design.
    let pwm_slices = rp235x_hal::pwm::Slices::new(pac.PWM, &mut pac.RESETS);
    let mut pwm4 = pwm_slices.pwm4;
    let _audio_pin = pwm4.channel_a.output_to(pins.gpio8);

    // ------------------------------------------------------------------
    // ST7789 display init
    // ------------------------------------------------------------------
    let mut timer = rp235x_hal::Timer::new_timer0(pac.TIMER0, &mut pac.RESETS, &clocks);
    let lcd = lcd::Display::new(spi_bus, dc_pin, rst_pin, &mut timer).unwrap();
    // Type-erase and register the one display so the main loop and the
    // fatal/panic path reach it the same way (see display::sink). A few
    // dozen bytes of heap, leaked on purpose for the program's lifetime.
    display::install(Box::leak(Box::new(lcd)));

    Board {
        sys_hz,
        timer,
        input: Box::leak(Box::new(buttons)),
        audio: Some(AudioBackend {
            carrier: pwm4,
            tick: pwm_slices.pwm5,
        }),
        psram: None,
    }
}
