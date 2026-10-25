//! The frame-sink contract and the one registered display.
//!
//! The concrete LCD driver is board-specific (`board::pico2::lcd` drives an
//! SPI ST7789, `board::tufty2350::lcd` a parallel panel over PIO/DMA). Both
//! implement [`FrameSink`]; `board::init` registers the one instance via
//! [`install`] and the main loop and the fatal-error / panic path reach it
//! through [`sink`].

pub use spark_core::console::SCREEN_SIZE;

/// Anything that can present a console frame.
pub trait FrameSink {
    /// Push a 128x128 indexed-color frame, applying the screen palette.
    ///
    /// `screen_pal` is `Console::screen_pal`: the display-time remap set by
    /// `pal(c0, c1, 1)`. Each index goes through it before the colour lookup,
    /// exactly like `Console::screen_to_rgba_buf` does on the desktop.
    fn show(&mut self, screen: &[u8; SCREEN_SIZE], screen_pal: &[u8; 16]);
}

/// The registered frame sink. Written once by [`install`] at boot.
///
/// Invariant: single core, and no interrupt handler touches this. Every
/// access goes through [`sink`], which hands out a fresh reference; callers
/// must not keep one alive across another call to `sink()`.
static mut SINK: Option<&'static mut dyn FrameSink> = None;

/// Register the display. Call once, after the display is initialised.
pub fn install(display: &'static mut dyn FrameSink) {
    // Safety: boot-time, single core, before any other access.
    unsafe {
        *core::ptr::addr_of_mut!(SINK) = Some(display);
    }
}

/// Borrow the registered display, if any.
///
/// # Safety
/// The returned reference aliases the static; the caller must not hold two
/// of them at once and must not call this from an interrupt handler.
pub unsafe fn sink() -> Option<&'static mut dyn FrameSink> {
    match &mut *core::ptr::addr_of_mut!(SINK) {
        Some(s) => Some(&mut **s),
        None => None,
    }
}
