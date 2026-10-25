//! Fatal-error reporting: the Lua error screen and the panic handler.
//!
//! Both paths end in the same place: the message is printed in red on a black
//! console screen and pushed to the LCD (mirroring the desktop's behaviour on
//! a runtime error), then the core parks. There is no debugger attached to a
//! Pico in a case, so making failures *visible* is the whole point.
//!
//! Everything here must work without the heap (a panic may originate inside
//! the allocator) and must never panic itself: the message is formatted into
//! a fixed buffer, only ASCII is kept, and the panic handler is re-entrancy
//! guarded so a second panic degrades to the blink loop.

use core::fmt::{self, Write};
use core::panic::PanicInfo;
use core::sync::atomic::{AtomicBool, Ordering};

use crate::board::{FATAL_BACKLIGHT_MASK, FATAL_LED_MASK};
use crate::{display, lua_ffi};

/// Glyphs per row: 128 px / 4 px per character.
const COLS: usize = 32;
/// Rows per screen: 128 px / 6 px per line.
const ROWS: usize = 21;
/// Room for a full screen of text plus one newline per row.
const CAP: usize = COLS * ROWS + ROWS;

/// A `fmt::Write` sink that word-wraps at [`COLS`] by inserting newlines,
/// keeps only ASCII (so the buffer is always valid UTF-8) and truncates
/// silently at capacity.
struct ScreenText {
    buf: [u8; CAP],
    len: usize,
    col: usize,
}

impl ScreenText {
    const fn new() -> Self {
        Self {
            buf: [0; CAP],
            len: 0,
            col: 0,
        }
    }

    fn push(&mut self, b: u8) -> bool {
        if self.len >= CAP {
            return false;
        }
        self.buf[self.len] = b;
        self.len += 1;
        true
    }

    fn as_str(&self) -> &str {
        // Safety: `write_str` only ever stores ASCII bytes.
        unsafe { core::str::from_utf8_unchecked(&self.buf[..self.len]) }
    }
}

impl Write for ScreenText {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for b in s.bytes() {
            let ok = if b == b'\n' {
                self.col = 0;
                self.push(b'\n')
            } else {
                if self.col == COLS {
                    if !self.push(b'\n') {
                        return Err(fmt::Error);
                    }
                    self.col = 0;
                }
                self.col += 1;
                self.push(if b.is_ascii() { b } else { b'?' })
            };
            if !ok {
                return Err(fmt::Error);
            }
        }
        Ok(())
    }
}

/// Render `args` in red on a cleared console screen and push it to the LCD.
///
/// Returns `false` if the console or display are not initialised yet (boot
/// failure before the engine came up), so the caller can fall back to a
/// purely visual signal.
fn show(args: fmt::Arguments<'_>) -> bool {
    let mut text = ScreenText::new();
    // Truncation is reported as an error by the sink; the prefix is still
    // what we want to show, so the result is intentionally ignored.
    let _ = text.write_fmt(args);

    // Safety: single core; the main loop is not running concurrently with a
    // fatal error (we never return to it), so re-deriving the console and
    // display references here does not race with anything. If the fault
    // happened *inside* the display driver, the SPI transaction may be left
    // half-done and the message may not render -- the blink still will.
    unsafe {
        if !lua_ffi::state_ready() {
            return false;
        }
        let Some(sink) = display::sink() else {
            return false;
        };
        let con = lua_ffi::console();
        con.cls(Some(0));
        con.print(text.as_str(), Some(0), Some(0), Some(8));
        sink.show(&con.screen, &con.screen_pal);
    }
    true
}

/// Toggle GPIOs through SIO without owning the pins.
fn toggle(mask: u32) {
    // Safety: GPIO_OUT_XOR is a write-only atomic-toggle register; writing it
    // cannot disturb any other pin, and we are past the point of no return.
    unsafe {
        (*rp235x_hal::pac::SIO::ptr())
            .gpio_out_xor()
            .write(|w| w.bits(mask));
    }
}

/// Park forever, blinking `mask` at ~2 Hz.
fn blink_forever(mask: u32) -> ! {
    loop {
        toggle(mask);
        // ~0.25 s at 150 MHz (delay() is >= n cycles).
        cortex_m::asm::delay(37_500_000);
    }
}

/// A Lua error the cart cannot recover from (load/parse error, error inside
/// `_init`/`_update`/`_draw`, prelude failure). Shows `msg` and halts with the
/// screen left readable, like PICO-8's own error screen.
pub fn halt(msg: &str) -> ! {
    // Stop the audio tick so the speaker goes quiet (holds mid-rail).
    cortex_m::interrupt::disable();
    if show(format_args!("runtime error\n\n{msg}")) {
        loop {
            cortex_m::asm::wfi();
        }
    }
    blink_forever(FATAL_BACKLIGHT_MASK | FATAL_LED_MASK)
}

static PANICKING: AtomicBool = AtomicBool::new(false);

/// Rust panic: print location + message on the LCD (best effort) and blink.
///
/// If the message could be shown, only the LED blinks so the text stays
/// readable; otherwise the backlight blinks too so a bare board still shows
/// *something*.
#[panic_handler]
fn panic(info: &PanicInfo<'_>) -> ! {
    cortex_m::interrupt::disable();
    // A panic while rendering the previous panic must not recurse.
    if PANICKING.swap(true, Ordering::SeqCst) {
        blink_forever(FATAL_BACKLIGHT_MASK | FATAL_LED_MASK);
    }
    if show(format_args!("panic\n\n{info}")) {
        blink_forever(FATAL_LED_MASK);
    }
    blink_forever(FATAL_BACKLIGHT_MASK | FATAL_LED_MASK)
}
