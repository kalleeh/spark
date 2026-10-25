//! Tufty 2350 PSRAM (APS6404L-class, 8 MB, on QMI chip-select 1 / GPIO 8).
//!
//! A port of `spark-pico/reference/sfe_psram.c` (SparkFun, MIT) to the PAC.
//! Every register write cites the reference line it mirrors (`sfe:NNN`).
//!
//! Sequence (`sfe:setup_psram`):
//! 1. GPIO 8 -> `XIP_CS1n` (`gpio_set_function`, sfe:213).
//! 2. Direct mode on CS1: leave QPI (`0xF5`), read ID (`0x9F`) -> size
//!    (`sfe:get_psram_size`). No/unknown chip -> `None`.
//! 3. Direct mode again: RSTEN `0x66`, RST `0x99`, quad enable `0x35`
//!    (sfe:222-249).
//! 4. `M1_TIMING` from the system clock (`sfe:set_psram_timing`).
//! 5. `M1_RFMT`/`M1_RCMD` quad read `0xEB`, `M1_WFMT`/`M1_WCMD` quad write
//!    `0x38`, `XIP_CTRL.WRITABLE_M1` (sfe:256-284).
//! 6. Self-test three words through the *uncached* alias; on any mismatch
//!    report `None` so the firmware falls back to the SRAM heap.
//!
//! **Direct mode stalls XIP.** While `DIRECT_CSR.EN` is set the QMI serves
//! no flash fetches, so the two direct-mode phases run from RAM
//! (`#[link_section = ".data..."]`, the Rust equivalent of the reference's
//! `__no_inline_not_in_flash_func`) and must not call anything in flash:
//! no `critical_section` entry (done by the caller), no `u64` division
//! (timing values are computed before), only inlined PAC accessors and
//! immediates. Interrupts are off for the whole of each phase.

use rp235x_hal::pac;

use crate::board::PsramRegion;

/// PSRAM chip select: QMI CS1 is GPIO 8 on the Tufty 2350 (board facts).
const PSRAM_CS_GPIO: usize = 8;
/// XIP window for chip-select 1: cached alias (heap lives here) and the
/// no-cache/no-allocate alias used by the self-test.
const CS1_CACHED_BASE: usize = 0x1100_0000;
const CS1_UNCACHED_BASE: usize = 0x1500_0000;

// --- Datasheet constants (sfe:56-68), femtosecond integer math ---------------
/// 1 s in femtoseconds (sfe:56).
const SEC_TO_FS: u64 = 1_000_000_000_000_000;
/// Max CS low time 8 us, pre-divided by the register's 64-cycle unit:
/// 8000 ns * 1e6 fs / 64 (sfe:61).
const MAX_SELECT_FS64: u64 = 125_000_000;
/// Min CS high time 50 ns (sfe:64).
const MIN_DESELECT_FS: u64 = 50_000_000;
/// Max SCK at 3.3 V (sfe:68).
const MAX_SCK_HZ: u32 = 109_000_000;

// --- Command bytes (sfe:71-78) -------------------------------------------------
const CMD_QUAD_END: u16 = 0xF5;
const CMD_QUAD_ENABLE: u16 = 0x35;
const CMD_READ_ID: u16 = 0x9F;
const CMD_RSTEN: u16 = 0x66;
const CMD_RST: u16 = 0x99;
const CMD_QUAD_READ: u8 = 0xEB;
const CMD_QUAD_WRITE: u8 = 0x38;
const CMD_NOOP: u16 = 0xFF;
/// "Known good die" byte of the read-ID reply (sfe:80).
const PSRAM_KGD: u8 = 0x5D;

/// Direct-mode clock divider (sfe:93, `30 << CLKDIV_LSB`): slow SPI for
/// the command phase, independent of the XIP timing programmed later.
const DIRECT_CLKDIV: u8 = 30;

/// `M1_TIMING` field values derived from the system clock
/// (`sfe:set_psram_timing`, 160-196). Computed outside the RAM functions
/// because the `u64` divisions call into flash.
struct Timing {
    clkdiv: u8,
    max_select: u8,
    min_deselect: u8,
}

impl Timing {
    fn from_sys_hz(sys_hz: u32) -> Self {
        // sfe:167: clock divider so SCK <= MAX_SCK_HZ (ceil).
        let clkdiv = sys_hz.div_ceil(MAX_SCK_HZ);
        // sfe:173: femtoseconds per system clock cycle.
        let fs_per_cycle = SEC_TO_FS / u64::from(sys_hz);
        // sfe:177: MAX_SELECT is in units of 64 cycles; the constant is
        // pre-divided by 64, so this is 8 us / (64 * cycle).
        let max_select = MAX_SELECT_FS64 / fs_per_cycle;
        // sfe:183: MIN_DESELECT in cycles, rounded up so it is >= 50 ns.
        let min_deselect = MIN_DESELECT_FS.div_ceil(fs_per_cycle);
        // Clamp to the field widths (CLKDIV 8, MAX_SELECT 6, MIN_DESELECT 5
        // bits); the reference relies on `uint8_t` truncation instead.
        Self {
            clkdiv: clkdiv.min(0xFF) as u8,
            max_select: max_select.min(0x3F) as u8,
            min_deselect: min_deselect.min(0x1F) as u8,
        }
    }
}

/// Bring up the PSRAM and return its cached window, or `None` if no chip
/// answers the ID query or the self-test fails.
///
/// Interrupts are disabled around each direct-mode phase (the reference's
/// `save_and_disable_interrupts`); `critical_section::with` is that.
pub fn init(sys_hz: u32) -> Option<PsramRegion> {
    let qmi = unsafe { &*pac::QMI::ptr() };
    let xip = unsafe { &*pac::XIP_CTRL::ptr() };

    // sfe:213: gpio_set_function(psram_cs_pin, GPIO_FUNC_XIP_CS1).
    set_cs_pin_function();

    // sfe:216: probe the chip; zero means nothing recognisable answered.
    let len = critical_section::with(|_| detect_size(qmi));
    if len == 0 {
        return None;
    }

    // sfe:222-249: reset the chip and switch it to quad (QPI) mode.
    critical_section::with(|_| reset_and_quad_enable(qmi));

    // sfe:160-196 (set_psram_timing) and sfe:256-284: XIP timing, read /
    // write formats, then allow writes through the CS1 window.
    let timing = Timing::from_sys_hz(sys_hz);
    critical_section::with(|_| {
        program_xip_mode(qmi, &timing);
        // sfe:283: xip_ctrl_hw->ctrl |= XIP_CTRL_WRITABLE_M1_BITS.
        xip.ctrl().modify(|_, w| w.writable_m1().set_bit());
    });

    let region = PsramRegion {
        base: CS1_CACHED_BASE,
        len,
    };
    if !self_test(len) {
        return None;
    }
    Some(region)
}

/// sfe:213 `gpio_set_function(pin, GPIO_FUNC_XIP_CS1)`: the SDK sets the
/// pad's input-enable, clears output-disable, selects the function and
/// finally lifts the RP2350 pad isolation. FUNCSEL 9 is `XIP_SS_N[1]` on
/// GPIO 8 (the PAC names value 9 `GPCK`; `rp235x_hal::gpio::set_function`
/// maps `XipCs1` to the same value).
fn set_cs_pin_function() {
    let io = unsafe { &*pac::IO_BANK0::ptr() };
    let pads = unsafe { &*pac::PADS_BANK0::ptr() };
    pads.gpio(PSRAM_CS_GPIO).modify(|_, w| {
        w.ie().set_bit();
        w.od().clear_bit();
        w
    });
    io.gpio(PSRAM_CS_GPIO)
        .gpio_ctrl()
        .write(|w| w.funcsel().gpck());
    pads.gpio(PSRAM_CS_GPIO).modify(|_, w| w.iso().clear_bit());
}

/// Wait for the direct-mode engine to go idle (sfe:98, 108, 126).
#[inline(always)]
fn wait_not_busy(qmi: &pac::qmi::RegisterBlock) {
    while qmi.direct_csr().read().busy().bit_is_set() {}
}

/// sfe:get_psram_size (86-158): enter direct mode, drop the chip out of QPI
/// with `0xF5`, then clock out READ_ID (`0x9F`) + 3 address bytes and read
/// back MF-ID / KGD / EID. Returns the density in bytes, or 0.
///
/// Runs from RAM with interrupts off; must not touch flash (see module doc).
#[inline(never)]
#[link_section = ".data.psram_detect_size"]
fn detect_size(qmi: &pac::qmi::RegisterBlock) -> usize {
    // sfe:93: direct_csr = 30 << CLKDIV_LSB | EN.
    qmi.direct_csr().write(|w| {
        unsafe { w.clkdiv().bits(DIRECT_CLKDIV) };
        w.en().set_bit()
    });
    // sfe:98: wait for the cooldown of the last XIP transfer.
    wait_not_busy(qmi);

    // sfe:103-115: assert CS1, send QUAD_END with OE and quad width so a
    // chip already in QPI mode (warm boot) understands it, then release.
    qmi.direct_csr().modify(|_, w| w.assert_cs1n().set_bit());
    qmi.direct_tx().write(|w| {
        unsafe { w.data().bits(CMD_QUAD_END) };
        w.oe().set_bit();
        w.iwidth().q()
    });
    wait_not_busy(qmi);
    let _ = qmi.direct_rx().read().bits();
    qmi.direct_csr().modify(|_, w| w.assert_cs1n().clear_bit());

    // sfe:118-139: READ_ID: byte 0 command, 1-3 address (don't care),
    // 4 MF-ID, 5 KGD, 6 EID. Everything else is read and discarded.
    qmi.direct_csr().modify(|_, w| w.assert_cs1n().set_bit());
    let mut kgd: u8 = 0;
    let mut eid: u8 = 0;
    for i in 0..7u32 {
        let cmd = if i == 0 { CMD_READ_ID } else { CMD_NOOP };
        qmi.direct_tx().write(|w| unsafe { w.data().bits(cmd) });
        // sfe:126-131: wait until the byte has left the TX FIFO and the
        // transfer is complete before reading the RX FIFO.
        while qmi.direct_csr().read().txempty().bit_is_clear() {}
        wait_not_busy(qmi);
        let rx = qmi.direct_rx().read().bits() as u8;
        if i == 5 {
            kgd = rx;
        } else if i == 6 {
            eid = rx;
        }
    }

    // sfe:142: release CS1 and leave direct mode (XIP resumes).
    qmi.direct_csr()
        .modify(|_, w| w.assert_cs1n().clear_bit().en().clear_bit());

    // sfe:145-156: density from the EID's top three bits.
    if kgd != PSRAM_KGD {
        return 0;
    }
    let mib: usize = 1024 * 1024;
    let size_id = eid >> 5;
    if eid == 0x26 || size_id == 2 {
        8 * mib
    } else if size_id == 0 {
        2 * mib
    } else if size_id == 1 {
        4 * mib
    } else {
        mib
    }
}

/// sfe:220-249: RSTEN, RST, QUAD_ENABLE in single-SPI direct mode, each
/// framed by CS1 with a short deselect gap.
///
/// Runs from RAM with interrupts off; must not touch flash (see module doc).
#[inline(never)]
#[link_section = ".data.psram_reset_and_quad_enable"]
fn reset_and_quad_enable(qmi: &pac::qmi::RegisterBlock) {
    // sfe:222: direct_csr = 30 << CLKDIV_LSB | EN.
    qmi.direct_csr().write(|w| {
        unsafe { w.clkdiv().bits(DIRECT_CLKDIV) };
        w.en().set_bit()
    });
    // sfe:227: cooldown of the last XIP transfer.
    wait_not_busy(qmi);

    for i in 0..3u32 {
        // sfe:234: assert CS1.
        qmi.direct_csr().modify(|_, w| w.assert_cs1n().set_bit());
        let cmd = match i {
            0 => CMD_RSTEN,       // sfe:236
            1 => CMD_RST,         // sfe:238
            _ => CMD_QUAD_ENABLE, // sfe:240
        };
        qmi.direct_tx().write(|w| unsafe { w.data().bits(cmd) });
        wait_not_busy(qmi); // sfe:242
        qmi.direct_csr().modify(|_, w| w.assert_cs1n().clear_bit()); // sfe:245
                                                                     // sfe:246: 20 nops of deselect time between commands. Inline asm,
                                                                     // not `cortex_m::asm::nop()`: that one is an out-of-line `bl __nop`
                                                                     // into flash (seen in the disassembly), which must not execute here.
        for _ in 0..20 {
            // Safety: a bare `nop` has no effect on memory or registers.
            unsafe { core::arch::asm!("nop", options(nomem, nostack, preserves_flags)) };
        }
        let _ = qmi.direct_rx().read().bits(); // sfe:249
    }

    // sfe:253: release CS1 and leave direct mode (XIP resumes).
    qmi.direct_csr()
        .modify(|_, w| w.assert_cs1n().clear_bit().en().clear_bit());
}

/// sfe:189-196 + 262-281: XIP timing and the quad read/write transfer
/// formats for CS1. Plain register writes; XIP on CS0 keeps running.
fn program_xip_mode(qmi: &pac::qmi::RegisterBlock, t: &Timing) {
    // sfe:189-196: m[1].timing =
    //   PAGEBREAK_1024 | SELECT_HOLD 3 | COOLDOWN 1 | RXDELAY 1 |
    //   MAX_SELECT | MIN_DESELECT | CLKDIV.
    qmi.m1_timing().write(|w| {
        w.pagebreak()._1024();
        unsafe {
            w.select_hold().bits(3);
            w.cooldown().bits(1);
            w.rxdelay().bits(1);
            w.max_select().bits(t.max_select);
            w.min_deselect().bits(t.min_deselect);
            w.clkdiv().bits(t.clkdiv)
        }
    });

    // sfe:262-269: m[1].rfmt = all widths Q, dummy 24 bits, prefix 8 bits,
    // no suffix (QPI fast read 0xEB: 8 command + 24 address + 6 dummy
    // clocks = 24 dummy bits at quad width).
    qmi.m1_rfmt().write(|w| {
        w.prefix_width().q();
        w.addr_width().q();
        w.suffix_width().q();
        w.dummy_width().q();
        w.dummy_len()._24();
        w.data_width().q();
        w.prefix_len()._8();
        w.suffix_len().none()
    });
    // sfe:271: m[1].rcmd = QUAD_READ prefix, suffix 0.
    qmi.m1_rcmd().write(|w| unsafe {
        w.prefix().bits(CMD_QUAD_READ);
        w.suffix().bits(0)
    });

    // sfe:273-280: m[1].wfmt = all widths Q, no dummy, prefix 8 bits, no
    // suffix (QPI quad write 0x38).
    qmi.m1_wfmt().write(|w| {
        w.prefix_width().q();
        w.addr_width().q();
        w.suffix_width().q();
        w.dummy_width().q();
        w.dummy_len().none();
        w.data_width().q();
        w.prefix_len()._8();
        w.suffix_len().none()
    });
    // sfe:281: m[1].wcmd = QUAD_WRITE prefix, suffix 0.
    qmi.m1_wcmd().write(|w| unsafe {
        w.prefix().bits(CMD_QUAD_WRITE);
        w.suffix().bits(0)
    });
}

/// Write/read-back check of three words (start, middle, last) through the
/// uncached alias, so the XIP cache cannot answer for the chip. Distinct
/// values are written first and verified afterwards, which also catches a
/// smaller die aliasing into a larger claimed window; a second pass with the
/// complements catches stuck bits.
fn self_test(len: usize) -> bool {
    let offsets = [0usize, (len / 2) & !3, len - 4];
    let patterns = [0xA5C3_3C5Au32, 0x0F1E_2D3Cu32, 0x8421_1248u32];
    for pass in 0..2u32 {
        let flip = if pass == 0 { 0 } else { !0u32 };
        for (off, pat) in offsets.iter().zip(patterns) {
            let p = (CS1_UNCACHED_BASE + off) as *mut u32;
            // Safety: inside the window the QMI just mapped and made writable.
            unsafe { core::ptr::write_volatile(p, pat ^ flip) };
        }
        for (off, pat) in offsets.iter().zip(patterns) {
            let p = (CS1_UNCACHED_BASE + off) as *const u32;
            // Safety: as above.
            if unsafe { core::ptr::read_volatile(p) } != pat ^ flip {
                return false;
            }
        }
    }
    true
}
