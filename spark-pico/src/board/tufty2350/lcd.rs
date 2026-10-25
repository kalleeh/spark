//! Tufty 2350 LCD: ST7789 320x240 over an 8-bit 8080 parallel bus, driven by
//! PIO1 + one DMA channel. A port of Pimoroni's `modules/c/st7789/st7789.cpp`
//! (the Badgeware reference driver); every register value below is theirs.
//!
//! Wiring (`st7789.hpp:35-41`, `docs/tufty2350-port.md`):
//! * DB0-7 = GPIO 32-39 (PIO `out pins`), WR = GPIO 30 (PIO side-set),
//!   RD = GPIO 31 (SIO output, held high: we never read the panel),
//!   CS = GPIO 27, DC/RS = GPIO 28 (SIO outputs), backlight = GPIO 26 (PWM),
//!   TE/vsync = GPIO 21 (input, unused). No reset pin -- SWRESET only.
//!
//! # PIO GPIOBASE (read this before touching pin numbers)
//!
//! A PIO block can only address 32 consecutive GPIOs. The data pins live at
//! GPIO 32-39, so `PIO1.GPIOBASE` is set to **16** and *every pin number the
//! state machine sees is `gpio - 16`*: DB0 is pin 16, WR is pin 14, and
//! `set_pindirs` takes the same shifted numbers. The pad function mux
//! (`into_function::<FunctionPio1>()`) still uses real GPIO numbers. The HAL
//! has no wrapper for GPIOBASE and its `split()` resets the block (clearing
//! it), so the register is written through the PAC right after `split`,
//! before the program is installed. Datasheet §11.7 "GPIOBASE"; Pimoroni:
//! `pio_set_gpio_base(parallel_pio, d0 + 8 >= 32 ? 16 : 0)` (`st7789.hpp:60`).
//!
//! # Bus protocol
//!
//! The PIO program is two instructions, 2 cycles per byte (`st7789_parallel.pio`):
//! ```text
//! .side_set 1
//! .wrap_target
//!     out pins, 8  side 0      ; data on DB0-7, WR low
//!     nop          side 1      ; WR high: panel latches on the rising edge
//! .wrap
//! ```
//! Out-shift left, autopull at 8 bits, TX FIFO joined (8 entries). The DMA
//! feeds the TX FIFO one *byte* per transfer; the RP2350's narrow-write
//! replication fills all four bytes of the FIFO word, and the left shift
//! emits the top byte, so each 8-bit DMA transfer is exactly one bus byte.
//! Clock divider `ceil(2 * max(1, sys_hz / 44 MHz)) / 2` keeps the bus at or
//! under Pimoroni's flicker-free 44 MHz PIO clock (22 MB/s): 5.0 at 200 MHz.
//!
//! `wait_for_dma` = DMA channel idle **and** the state machine reports
//! `FDEBUG.TXSTALL` (it drained the FIFO and is waiting for data). Only the
//! second condition proves the last byte is on the bus, so CS/DC may change
//! (`st7789.cpp` `pio_sm_block_until_stalled`). The flag is cleared first,
//! then waited on, exactly like Pimoroni: a stale flag from before the
//! transfer must not satisfy the wait.
//!
//! # Frame layout and orientation
//!
//! MADCTL is `0x90` = ROW_ORDER (MY) | SCAN_ORDER (ML), no SWAP_XY: the panel
//! is addressed **portrait**, 240 columns x 320 rows, so a frame after RAMWR
//! is 320 rows of 240 pixels (480 bytes, RGB565 high byte first). Pimoroni's
//! landscape framebuffer is rotated in software: portrait row `r` receives
//! framebuffer *column* `x = r`, and position `p` within the row receives
//! framebuffer row `y = p` (`ST7789::update`). That is the orientation their
//! badge UI is known to display correctly, so this driver copies it: on the
//! device, portrait rows run left-to-right and positions within a row run
//! top-to-bottom.
//!
//! The 128x128 PICO-8 screen is stretched to 240x240 with nearest-neighbour
//! LUTs and centered: rows 0..40 and 280..320 are black, row `40 + i` shows
//! image column `LUT[i]` and position `p` in it shows image row `LUT[p]`.
//! If the first boot photo shows the picture mirrored, flip [`FLIP_X`]
//! (left/right, reverses the row walk) or [`FLIP_Y`] (top/bottom, reverses
//! the in-row LUT). Both are compile-time constants; nothing else changes.

use alloc::boxed::Box;
use core::sync::atomic::{compiler_fence, Ordering};

use embedded_hal::delay::DelayNs;
use embedded_hal::digital::OutputPin;
use embedded_hal::pwm::SetDutyCycle;
use rp235x_hal::dma::{Channel, SingleChannel, CH0};
use rp235x_hal::gpio::bank0::{
    Gpio21, Gpio26, Gpio27, Gpio28, Gpio30, Gpio31, Gpio32, Gpio33, Gpio34, Gpio35, Gpio36, Gpio37,
    Gpio38, Gpio39,
};
use rp235x_hal::gpio::{FunctionNull, FunctionPio1, FunctionSioOutput, Pin, PinState, PullDown};
use rp235x_hal::pac::{PIO1, RESETS};
use rp235x_hal::pio::{
    Buffers, PIOBuilder, PIOExt, PinDir, Running, ShiftDirection, StateMachine, Tx, PIO, SM0,
};
use rp235x_hal::pwm::{FreeRunning, Pwm5, Slice};
use spark_core::rgb565::{column_to_rgb565_be, nearest_lut, palette_rgb565_be};

use crate::display::{FrameSink, SCREEN_SIZE};

// ---------------------------------------------------------------------------
// Orientation
// ---------------------------------------------------------------------------

/// Mirror left/right (walk the image columns 127..0 instead of 0..127).
/// Decided by the first boot photo; see the module docs.
pub const FLIP_X: bool = false;
/// Mirror top/bottom (walk the image rows 127..0 within each portrait row).
/// Decided by the first boot photo; see the module docs.
pub const FLIP_Y: bool = false;

// ---------------------------------------------------------------------------
// Pins
// ---------------------------------------------------------------------------

/// Real GPIO numbers (`st7789.hpp:35-41`).
const GPIO_DB0: u8 = 32;
const GPIO_WR: u8 = 30;
/// PIO1 sees GPIO `n` as pin `n - PIO_GPIOBASE` (module docs).
const PIO_GPIOBASE: u8 = 16;

/// Bank-0 pin in its reset state, as handed out by `rp235x_hal::gpio::Pins`.
type Raw<I> = Pin<I, FunctionNull, PullDown>;

/// The LCD's pins, taken from `Pins` by `tufty2350::init`.
pub struct LcdPins {
    pub cs: Raw<Gpio27>,
    pub dc: Raw<Gpio28>,
    pub wr: Raw<Gpio30>,
    pub rd: Raw<Gpio31>,
    pub db0: Raw<Gpio32>,
    pub db1: Raw<Gpio33>,
    pub db2: Raw<Gpio34>,
    pub db3: Raw<Gpio35>,
    pub db4: Raw<Gpio36>,
    pub db5: Raw<Gpio37>,
    pub db6: Raw<Gpio38>,
    pub db7: Raw<Gpio39>,
    pub backlight: Raw<Gpio26>,
    pub te: Raw<Gpio21>,
}

// ---------------------------------------------------------------------------
// Panel geometry
// ---------------------------------------------------------------------------

/// Portrait panel: 240 pixels per row, 320 rows.
const PANEL_W: usize = 240;
const PANEL_H: usize = 320;
/// Bytes per portrait row (RGB565).
const ROW_BYTES: usize = PANEL_W * 2;
/// Black rows above and below the centered 240x240 image.
const BORDER_ROWS: usize = (PANEL_H - PANEL_W) / 2;

/// 128 -> 240 nearest-neighbour index table (host-tested in spark-core).
const SCALE_LUT: [u8; PANEL_W] = nearest_lut::<PANEL_W>(128);

const fn reversed(t: [u8; PANEL_W]) -> [u8; PANEL_W] {
    let mut out = [0u8; PANEL_W];
    let mut i = 0;
    while i < PANEL_W {
        out[i] = t[PANEL_W - 1 - i];
        i += 1;
    }
    out
}

/// Image column shown by portrait row `BORDER_ROWS + i`.
const COL_WALK: [u8; PANEL_W] = if FLIP_X {
    reversed(SCALE_LUT)
} else {
    SCALE_LUT
};
/// Image row shown at position `p` within a portrait row.
const ROW_LUT: [u8; PANEL_W] = if FLIP_Y {
    reversed(SCALE_LUT)
} else {
    SCALE_LUT
};

/// One black portrait row, DMA'd for the borders (read from flash; the DMA
/// can source XIP directly).
static ZERO_ROW: [u8; ROW_BYTES] = [0; ROW_BYTES];

/// Two row buffers: convert row N+1 into one while row N streams out of the
/// other. Only `Lcd::show` touches this (single core, no interrupts involved).
static mut ROWBUF: [[u8; ROW_BYTES]; 2] = [[0; ROW_BYTES]; 2];

// ---------------------------------------------------------------------------
// ST7789 commands used (`st7789.cpp` `enum reg`)
// ---------------------------------------------------------------------------

const SWRESET: u8 = 0x01;
const SLPOUT: u8 = 0x11;
const INVON: u8 = 0x21;
const DISPON: u8 = 0x29;
const CASET: u8 = 0x2A;
const RASET: u8 = 0x2B;
const RAMWR: u8 = 0x2C;
const TEON: u8 = 0x35;
const MADCTL: u8 = 0x36;
const COLMOD: u8 = 0x3A;
const STE: u8 = 0x44;
const RAMCTRL: u8 = 0xB0;
const PORCTRL: u8 = 0xB2;
const GCTRL: u8 = 0xB7;
const VCOMS: u8 = 0xBB;
const LCMCTRL: u8 = 0xC0;
const VDVVRHEN: u8 = 0xC2;
const VRHS: u8 = 0xC3;
const VDVS: u8 = 0xC4;
const FRCTRL2: u8 = 0xC6;
const PWCTRL1: u8 = 0xD0;
const GMCTRP1: u8 = 0xE0;
const GMCTRN1: u8 = 0xE1;

/// Backlight PWM level after init. Pimoroni's `set_backlight(230)` maps
/// brightness through gamma 2.8: `(230/255)^2.8 * 65535 = 49092` (~75 %
/// duty). Their comment: "180 = about half perceptual brightness". Below
/// roughly 11 % duty (~7200) the panel's backlight driver does not light at
/// all, so never go near zero for a "dim" mode.
const BACKLIGHT_ON: u16 = 49_092;

// ---------------------------------------------------------------------------
// Driver
// ---------------------------------------------------------------------------

/// The parallel ST7789 driver. One instance, leaked by [`init`].
pub struct Lcd {
    cs: Pin<Gpio27, FunctionSioOutput, PullDown>,
    dc: Pin<Gpio28, FunctionSioOutput, PullDown>,
    /// TX FIFO handle of the state machine: DREQ number, FIFO address and
    /// the TXSTALL flag.
    tx: Tx<(PIO1, SM0)>,
    /// The one DMA channel; all bus traffic goes through it.
    dma: Channel<CH0>,
    /// Backlight PWM (slice 5 channel A = GPIO 26).
    backlight: Slice<Pwm5, FreeRunning>,
    _sm: StateMachine<(PIO1, SM0), Running>,
    _pio: PIO<PIO1>,
}

/// PIO clock divider as (integer, 1/256ths):
/// `ceil(2 * max(1, sys_hz / 44 MHz)) / 2` (`st7789.hpp:88`). 200 MHz -> 5.0,
/// 250 MHz -> 6.0, 150 MHz -> 3.5.
fn pio_clkdiv(sys_hz: u32) -> (u16, u8) {
    const MAX_PIO_HZ: u64 = 44_000_000;
    let twice = (2 * u64::from(sys_hz)).div_ceil(MAX_PIO_HZ).max(2);
    ((twice / 2) as u16, ((twice % 2) * 128) as u8)
}

impl Lcd {
    /// Program the DMA channel for byte transfers memory -> PIO1 SM0 TX FIFO,
    /// paced by the FIFO's DREQ (`ST7789::configure_dma`). `incr_read = false`
    /// streams the same byte repeatedly (used once, to clear the panel).
    fn configure_dma(&mut self, incr_read: bool) {
        let ch = self.dma.ch();
        ch.ch_al1_ctrl().write(|w| unsafe {
            w.data_size().size_byte();
            w.incr_read().bit(incr_read);
            w.incr_write().clear_bit();
            w.treq_sel().bits(self.tx.dreq_value());
            // Chain to self = no chaining.
            w.chain_to().bits(self.dma.id());
            w.bswap().clear_bit();
            w.en().set_bit()
        });
        ch.ch_write_addr()
            .write(|w| unsafe { w.bits(self.tx.fifo_address() as u32) });
    }

    /// Kick off a transfer of `len` bytes from `src`; returns immediately.
    fn start_dma(&mut self, src: *const u8, len: usize) {
        // The bytes at `src` were written by the CPU just now; make sure they
        // are in memory before the DMA is triggered.
        compiler_fence(Ordering::SeqCst);
        cortex_m::asm::dsb();
        let ch = self.dma.ch();
        ch.ch_trans_count()
            .write(|w| unsafe { w.count().bits(len as u32).mode().normal() });
        // READ_ADDR trigger alias: writing it starts the channel.
        ch.ch_al3_read_addr_trig()
            .write(|w| unsafe { w.bits(src as u32) });
    }

    /// Block until the previous transfer has fully left the panel bus
    /// (module docs, "Bus protocol").
    fn wait_for_dma(&mut self) {
        while self.dma.ch().ch_ctrl_trig().read().busy().bit_is_set() {}
        self.tx.clear_stalled_flag();
        while !self.tx.has_stalled() {}
    }

    fn write_blocking(&mut self, data: &[u8]) {
        self.start_dma(data.as_ptr(), data.len());
        self.wait_for_dma();
    }

    /// Send a command byte and leave the bus in data mode with CS asserted
    /// (`ST7789::update`'s RAMWR prologue).
    fn begin_command(&mut self, cmd: u8) {
        self.wait_for_dma();
        let _ = self.dc.set_low();
        let _ = self.cs.set_low();
        let byte = [cmd];
        self.write_blocking(&byte);
        let _ = self.dc.set_high();
    }

    /// `ST7789::command`: CS low, DC low, command byte, DC high, data, CS high.
    fn command(&mut self, cmd: u8, data: &[u8]) {
        self.begin_command(cmd);
        if !data.is_empty() {
            self.write_blocking(data);
        }
        let _ = self.cs.set_high();
    }

    /// Panel bring-up: `ST7789::init` (`st7789.cpp:58-141`), verbatim.
    fn init_panel(&mut self, delay: &mut impl DelayNs) {
        // No reset line on the Tufty: software reset only.
        self.command(SWRESET, &[]);
        delay.delay_ms(150);

        self.command(COLMOD, &[0x05]); // 16 bits per pixel (RGB565)
        self.command(PORCTRL, &[0x0C, 0x0C, 0x00, 0x33, 0x33]);
        self.command(LCMCTRL, &[0x2C]);
        self.command(VDVVRHEN, &[0x01]);
        self.command(VRHS, &[0x0F]);
        self.command(VDVS, &[0x20]);
        self.command(PWCTRL1, &[0xA4, 0xA1]);
        self.command(FRCTRL2, &[0x0F]);
        // "required to avoid a weird light grey banding issue with low
        // brightness green" -- pimoroni-pico issue #1040.
        self.command(RAMCTRL, &[0x00, 0xC0]);
        // 320x240 panel tuning + gamma curves.
        self.command(GCTRL, &[0x35]);
        self.command(VCOMS, &[0x1B]);
        self.command(
            GMCTRP1,
            &[
                0xF0, 0x00, 0x06, 0x04, 0x05, 0x05, 0x31, 0x44, 0x48, 0x36, 0x12, 0x12, 0x2B, 0x34,
            ],
        );
        self.command(
            GMCTRN1,
            &[
                0xF0, 0x0B, 0x0F, 0x0F, 0x0D, 0x26, 0x31, 0x43, 0x47, 0x38, 0x14, 0x14, 0x2C, 0x32,
            ],
        );
        self.command(INVON, &[]); // this panel needs inversion for true colours
        self.command(SLPOUT, &[]);
        delay.delay_ms(100);

        // Full window, portrait addressing (module docs, "Frame layout").
        self.command(CASET, &[0x00, 0x00, 0x00, 0xEF]); // columns 0..=239
        self.command(RASET, &[0x00, 0x00, 0x01, 0x3F]); // rows 0..=319
        self.command(MADCTL, &[0x90]); // ROW_ORDER | SCAN_ORDER

        // Clear GRAM to black: RAMWR + 320*240*2 zero bytes streamed from a
        // single address with the read increment off (Pimoroni's trick to
        // avoid a 150 KB memset).
        self.begin_command(RAMWR);
        self.configure_dma(false);
        self.start_dma(ZERO_ROW.as_ptr(), PANEL_W * PANEL_H * 2);
        self.wait_for_dma();
        self.configure_dma(true);

        self.command(TEON, &[0x00]); // tearing-effect line on (GPIO 21; unused)
        self.command(STE, &[0x00, 0x00]);
        self.command(DISPON, &[]);
    }
}

impl FrameSink for Lcd {
    /// Stream one frame: RAMWR, 40 black rows, 240 image rows, 40 black rows.
    /// Returns with the last row still in flight (the next `begin_command`
    /// waits for it); CS stays asserted between frames, as in Pimoroni's
    /// `update()`.
    fn show(&mut self, screen: &[u8; SCREEN_SIZE], screen_pal: &[u8; 16]) {
        let pal_be = palette_rgb565_be(screen_pal);

        self.begin_command(RAMWR);
        for _ in 0..BORDER_ROWS {
            self.wait_for_dma();
            self.start_dma(ZERO_ROW.as_ptr(), ROW_BYTES);
        }

        let bufs = core::ptr::addr_of_mut!(ROWBUF) as *mut [u8; ROW_BYTES];
        for (i, &sx) in COL_WALK.iter().enumerate() {
            // Safety: buffer `i & 1` was last handed to the DMA two rows ago,
            // and the `wait_for_dma` in the previous iteration completed that
            // transfer, so nothing reads it now; the other buffer (which the
            // DMA may be reading) is not referenced.
            let buf = unsafe { &mut *bufs.add(i & 1) };
            column_to_rgb565_be(screen, &pal_be, sx as usize, &ROW_LUT, buf);
            self.wait_for_dma();
            self.start_dma(buf.as_ptr(), ROW_BYTES);
        }

        for _ in 0..BORDER_ROWS {
            self.wait_for_dma();
            self.start_dma(ZERO_ROW.as_ptr(), ROW_BYTES);
        }
    }
}

/// Bring up the pins, PIO1 SM0, the DMA channel and the panel, and return the
/// one driver instance (leaked for the program's lifetime, like the pico2
/// driver). Must run after the heap is initialised.
pub fn init(
    pio1: PIO1,
    dma: Channel<CH0>,
    mut backlight: Slice<Pwm5, FreeRunning>,
    pins: LcdPins,
    sys_hz: u32,
    resets: &mut RESETS,
    delay: &mut impl DelayNs,
) -> &'static mut Lcd {
    // --- Plain GPIO --------------------------------------------------------
    // CS/DC idle high (SIO outputs). RD is never pulsed: hold it high so the
    // panel's output drivers stay off (`st7789.hpp:68-70,96`).
    let cs = pins.cs.into_push_pull_output_in_state(PinState::High);
    let dc = pins.dc.into_push_pull_output_in_state(PinState::High);
    let _rd = pins.rd.into_push_pull_output_in_state(PinState::High);
    // Tearing-effect output from the panel; configured as an input and left
    // alone (we do not vsync yet).
    let _te = pins.te.into_pull_down_input();

    // --- Backlight PWM: wrap 65535 at sys clock (~3 kHz), off during init
    // "to avoid nasty surprises" (`st7789.cpp:70-77`). ------------------------
    backlight.default_config();
    backlight.set_top(65535);
    backlight.enable();
    let _bl_pin = backlight.channel_a.output_to(pins.backlight);
    let _ = backlight.channel_a.set_duty_cycle(0);

    // --- PIO1 ---------------------------------------------------------------
    // Pad mux first: DB0-7 and WR belong to PIO1 (`pio_gpio_init`).
    let _wr = pins.wr.into_function::<FunctionPio1>();
    let _db = (
        pins.db0.into_function::<FunctionPio1>(),
        pins.db1.into_function::<FunctionPio1>(),
        pins.db2.into_function::<FunctionPio1>(),
        pins.db3.into_function::<FunctionPio1>(),
        pins.db4.into_function::<FunctionPio1>(),
        pins.db5.into_function::<FunctionPio1>(),
        pins.db6.into_function::<FunctionPio1>(),
        pins.db7.into_function::<FunctionPio1>(),
    );

    let (mut pio, sm0, _, _, _) = pio1.split(resets);
    // GPIOBASE = 16 so the SM can reach GPIO 32-39. Must follow `split` (which
    // resets the block) and precede any SM configuration. See module docs.
    // Safety: PIO1 is owned by `pio`; this is the only GPIOBASE write.
    unsafe {
        (*PIO1::ptr())
            .gpiobase()
            .write(|w| w.bits(u32::from(PIO_GPIOBASE)));
    }

    let program = pio::pio_asm!(
        ".side_set 1",
        ".wrap_target",
        "out pins, 8 side 0",
        "nop side 1",
        ".wrap",
    )
    .program;
    let installed = pio.install(&program).unwrap();

    let (div_int, div_frac) = pio_clkdiv(sys_hz);
    let (mut sm, _rx, tx) = PIOBuilder::from_installed_program(installed)
        // `sm_config_set_out_pins(&c, d0, 8)` / `sm_config_set_sideset_pins(&c, wr_sck)`,
        // both relative to GPIOBASE.
        .out_pins(GPIO_DB0 - PIO_GPIOBASE, 8)
        .side_set_pin_base(GPIO_WR - PIO_GPIOBASE)
        // `PIO_FIFO_JOIN_TX`: 8-deep TX FIFO, no RX.
        .buffers(Buffers::OnlyTx)
        // `sm_config_set_out_shift(&c, false /*shift left*/, true /*autopull*/, 8)`.
        .out_shift_direction(ShiftDirection::Left)
        .autopull(true)
        .pull_threshold(8)
        .clock_divisor_fixed_point(div_int, div_frac)
        .build(sm0);
    // `pio_sm_set_consecutive_pindirs(d0, 8, out)` + `(wr_sck, 1, out)`.
    sm.set_pindirs(
        (0..8u8)
            .map(|i| (GPIO_DB0 - PIO_GPIOBASE + i, PinDir::Output))
            .chain(core::iter::once((GPIO_WR - PIO_GPIOBASE, PinDir::Output))),
    );
    let sm = sm.start();

    let mut lcd = Lcd {
        cs,
        dc,
        tx,
        dma,
        backlight,
        _sm: sm,
        _pio: pio,
    };
    lcd.configure_dma(true);
    lcd.init_panel(delay);

    // "Turn backlight on now surprises have passed."
    let _ = lcd.backlight.channel_a.set_duty_cycle(BACKLIGHT_ON);

    Box::leak(Box::new(lcd))
}
