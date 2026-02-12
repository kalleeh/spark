// ST7789 SPI LCD driver for RP2350
//
// Drives a 240x240 ST7789 SPI LCD. The 128x128 PICO-8 screen is rendered
// at 1:1 and centered on the display at offset (56, 56) with black borders.
//
// Performance
// -----------
// The palette conversion uses a `const` RGB565 lookup table -- each 4-bit
// PICO-8 color index maps directly to a pre-computed 16-bit RGB565 value
// with zero runtime arithmetic (just a table load). The `set_pixels` API
// sets the address window once, then streams all 128*128*2 = 32768 bytes
// over SPI.
//
// At 62.5 MHz SPI clock, the raw transfer takes 32768 * 8 / 62.5e6 ~ 4.2 ms
// in theory, though mipidsi overhead (command bytes, CS toggling) adds some
// margin. In practice the display update completes in under 5 ms.
//
// Future optimization: DMA could free the CPU during the SPI transfer,
// allowing audio generation to overlap with display update. However, this
// requires careful integration with mipidsi's SPI interface and is not
// worth the complexity given that the current ~5 ms transfer leaves plenty
// of frame budget.

use embedded_hal::delay::DelayNs;
use embedded_hal::digital::OutputPin;
use embedded_hal::spi::SpiDevice;

use display_interface_spi::SPIInterface;
use embedded_graphics_core::pixelcolor::raw::RawU16;
use embedded_graphics_core::pixelcolor::Rgb565;
use embedded_graphics_core::prelude::*;
use mipidsi::models::ST7789;
use mipidsi::options::{ColorInversion, ColorOrder, Orientation};
use mipidsi::Builder;

/// Screen dimensions for the PICO-8 virtual console.
const SCREEN_W: u16 = 128;
const SCREEN_H: u16 = 128;

/// Physical LCD dimensions.
const LCD_W: u16 = 240;
const LCD_H: u16 = 240;

/// Offset to center the 128x128 screen on the 240x240 LCD.
const OFFSET_X: u16 = (LCD_W - SCREEN_W) / 2;
const OFFSET_Y: u16 = (LCD_H - SCREEN_H) / 2;

/// PICO-8 16-color palette in RGB565 format.
const PALETTE_RGB565: [u16; 16] = [
    rgb888_to_rgb565(0, 0, 0),         // 0 black
    rgb888_to_rgb565(29, 43, 83),      // 1 dark_blue
    rgb888_to_rgb565(126, 37, 83),     // 2 dark_purple
    rgb888_to_rgb565(0, 135, 81),      // 3 dark_green
    rgb888_to_rgb565(171, 82, 54),     // 4 brown
    rgb888_to_rgb565(95, 87, 79),      // 5 dark_grey
    rgb888_to_rgb565(194, 195, 199),   // 6 light_grey
    rgb888_to_rgb565(255, 241, 232),   // 7 white
    rgb888_to_rgb565(255, 0, 77),      // 8 red
    rgb888_to_rgb565(255, 163, 0),     // 9 orange
    rgb888_to_rgb565(255, 236, 39),    // 10 yellow
    rgb888_to_rgb565(0, 228, 54),      // 11 green
    rgb888_to_rgb565(41, 173, 255),    // 12 blue
    rgb888_to_rgb565(131, 118, 156),   // 13 indigo/lavender
    rgb888_to_rgb565(255, 119, 168),   // 14 pink
    rgb888_to_rgb565(255, 204, 170),   // 15 peach
];

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

    /// Push the 128x128 indexed-color screen buffer to the display.
    ///
    /// Uses mipidsi's set_pixels to stream all pixel data after setting
    /// the address window once, which is faster than per-pixel draw_iter.
    pub fn update(&mut self, screen: &[u8; 128 * 128]) {
        let colors = screen.iter().map(|&idx| {
            let raw = PALETTE_RGB565[(idx & 0x0F) as usize];
            Rgb565::from(RawU16::new(raw))
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

/// Convert an RGB888 triplet to an RGB565 u16 value at compile time.
pub const fn rgb888_to_rgb565(r: u8, g: u8, b: u8) -> u16 {
    ((r as u16 >> 3) << 11) | ((g as u16 >> 2) << 5) | (b as u16 >> 3)
}
