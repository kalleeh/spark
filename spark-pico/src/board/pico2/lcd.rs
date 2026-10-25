// ST7789 SPI LCD driver for RP2350
//
// Drives a 240x240 ST7789 SPI LCD. The 128x128 PICO-8 screen is rendered
// at 1:1 and centered on the display at offset (56, 56) with black borders.
//
// Performance
// -----------
// Each 4-bit screen index is first remapped through the console's *screen
// palette* (`pal(c0, c1, 1)` -- the second palette in PICO-8) and then looked
// up in a `const` RGB565 table, mirroring `Console::screen_to_rgba_buf` in
// spark-core. The `set_pixels` API sets the address window once, then streams
// all 128*128*2 = 32768 bytes over SPI.
//
// At a 62.5 MHz SPI clock the raw transfer is 32768 * 8 / 62.5e6 = 4.2 ms;
// mipidsi's per-chunk overhead (command bytes, CS toggling) pushes this to
// roughly 5 ms in practice -- about 15% of a 33 ms frame, 30% of a 16.7 ms one.
// DMA would let this overlap with game logic but requires integrating with
// mipidsi's SPI interface; not worth it at this frame budget.

use embedded_hal::delay::DelayNs;
use embedded_hal::digital::OutputPin;
use embedded_hal::spi::SpiDevice;

use display_interface_spi::SPIInterface;
use embedded_graphics_core::pixelcolor::raw::RawU16;
use embedded_graphics_core::pixelcolor::Rgb565;
use mipidsi::models::ST7789;
use mipidsi::options::{ColorInversion, ColorOrder, Orientation};
use mipidsi::Builder;
use spark_core::rgb565::PICO8_RGB565;

use crate::display::{FrameSink, SCREEN_SIZE};

/// Screen dimensions for the PICO-8 virtual console.
const SCREEN_W: u16 = 128;
const SCREEN_H: u16 = 128;

/// Physical LCD dimensions.
const LCD_W: u16 = 240;
const LCD_H: u16 = 240;

/// Offset to center the 128x128 screen on the 240x240 LCD.
const OFFSET_X: u16 = (LCD_W - SCREEN_W) / 2;
const OFFSET_Y: u16 = (LCD_H - SCREEN_H) / 2;

/// ST7789 display driver wrapper.
pub struct Display<SPI, DC, RST>
where
    SPI: SpiDevice,
    DC: OutputPin,
    RST: OutputPin,
{
    driver: mipidsi::Display<SPIInterface<SPI, DC>, ST7789, RST>,
}

impl<SPI, DC, RST> Display<SPI, DC, RST>
where
    SPI: SpiDevice,
    DC: OutputPin,
    RST: OutputPin,
{
    /// Initialize the ST7789 display.
    pub fn new(
        spi: SPI,
        dc: DC,
        rst: RST,
        delay: &mut impl DelayNs,
    ) -> Result<Self, mipidsi::error::InitError<RST::Error>> {
        let di = SPIInterface::new(spi, dc);

        let driver = Builder::new(ST7789, di)
            .display_size(LCD_W, LCD_H)
            .color_order(ColorOrder::Rgb)
            .invert_colors(ColorInversion::Normal)
            .orientation(Orientation::new())
            .reset_pin(rst)
            .init(delay)?;

        Ok(Self { driver })
    }
}

impl<SPI, DC, RST> FrameSink for Display<SPI, DC, RST>
where
    SPI: SpiDevice,
    DC: OutputPin,
    RST: OutputPin,
{
    /// Push the 128x128 indexed-color screen buffer to the display.
    fn show(&mut self, screen: &[u8; SCREEN_SIZE], screen_pal: &[u8; 16]) {
        let colors = screen.iter().map(|&idx| {
            let mapped = screen_pal[(idx & 0x0F) as usize] & 0x0F;
            Rgb565::from(RawU16::new(PICO8_RGB565[mapped as usize]))
        });

        let _ = self.driver.set_pixels(
            OFFSET_X,
            OFFSET_Y,
            OFFSET_X + SCREEN_W - 1,
            OFFSET_Y + SCREEN_H - 1,
            colors,
        );
    }
}
