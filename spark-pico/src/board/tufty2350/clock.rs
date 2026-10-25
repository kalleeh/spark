//! Tufty 2350 clock setup: 200 MHz system clock (Pimoroni's tested config).
//!
//! Pimoroni's board header (`board/pimoroni_tufty2350.h:63-74`) documents two
//! PLL_SYS configurations produced by the SDK's `vcocalc.py`:
//!
//! * **200 MHz** -- VCO 1200 MHz, postdiv 6/1, default core voltage (1.10 V).
//!   "Most of our testing was done at 200MHz." This is what we run.
//! * **250 MHz** -- VCO 1500 MHz, postdiv 6/1. "Working, but not tested on
//!   enough boards to fully verify." Needs the core regulator raised to
//!   1.20 V *before* the PLL is switched in. Available behind
//!   [`OVERCLOCK_250`], off by default. 266 MHz is the PSRAM ceiling
//!   (`docs/tufty2350-port.md`), so nothing faster is offered.
//!
//! The rest of the tree (USB PLL 48 MHz, clk_ref = XOSC, clk_peri = clk_sys,
//! clk_usb/clk_adc = PLL_USB, clk_hstx = PLL_SYS) is the HAL's
//! `init_clocks_and_plls` default, reproduced here step by step because that
//! helper hard-codes the 150 MHz PLL_SYS config.

use fugit::{HertzU32, RateExtU32};
use rp235x_hal::clocks::ClocksManager;
use rp235x_hal::pac::{CLOCKS, PLL_SYS, PLL_USB, POWMAN, RESETS, XOSC};
use rp235x_hal::pll::{common_configs::PLL_USB_48MHZ, setup_pll_blocking, PLLConfig};
use rp235x_hal::xosc::setup_xosc_blocking;
use rp235x_hal::Watchdog;

/// Crystal frequency on the Tufty 2350 (same 12 MHz as every RP2350 board).
const XOSC_HZ: u32 = 12_000_000;

/// Run the core at 250 MHz instead of 200 MHz. Requires raising VREG to
/// 1.20 V first (done automatically below). Off by default: Pimoroni's own
/// verdict is "not tested on enough boards to fully verify", and 200 MHz is
/// already ~5x what a PICO-8 frame needs.
pub const OVERCLOCK_250: bool = false;

/// PLL_SYS for 200 MHz: 12 MHz / 1 * 100 = 1200 MHz VCO, / 6 / 1 = 200 MHz.
/// Source: `pimoroni_tufty2350.h:70-74` (`vcocalc.py --cmake 200`).
const PLL_SYS_200MHZ: PLLConfig = PLLConfig {
    vco_freq: HertzU32::MHz(1200),
    refdiv: 1,
    post_div1: 6,
    post_div2: 1,
};

/// PLL_SYS for 250 MHz: 12 MHz / 1 * 125 = 1500 MHz VCO, / 6 / 1 = 250 MHz.
/// Source: `pimoroni_tufty2350.h:61-66` (`vcocalc.py --cmake 250`).
const PLL_SYS_250MHZ: PLLConfig = PLLConfig {
    vco_freq: HertzU32::MHz(1500),
    refdiv: 1,
    post_div1: 6,
    post_div2: 1,
};

/// Every write to a POWMAN register must carry this password in bits 31:16
/// or it is ignored (and `POWMAN.BADPASSWD` is set). RP2350 datasheet
/// §6.4 "Power Management (POWMAN)", "Register protection"; the HAL's own
/// `powman.rs` uses the same `0x5AFE_0000` key.
const POWMAN_PASSWORD: u32 = 0x5AFE_0000;

/// `VREG_CTRL.UNLOCK` (bit 13): the regulator interface is locked at power-up
/// and must be unlocked once before `VREG` accepts writes. Pimoroni does this
/// in `modules/c/powman/powman.c:161-162`; the SDK's `vreg_set_voltage` does
/// the same.
const VREG_CTRL_UNLOCK: u32 = 1 << 13;

/// `VREG.VSEL` (bits 8:4) encoding for 1.20 V -- one step above the 1.10 V
/// default (`0b01011`). Below the 1.30 V limit, so `DISABLE_VOLTAGE_LIMIT`
/// is not needed. Encoding table: PAC `powman::vreg::VSEL` field docs /
/// datasheet §6.4 "VREG" register.
const VREG_VSEL_1V20: u32 = 0b01101 << 4;
const VREG_VSEL_MASK: u32 = 0b11111 << 4;
/// `VREG.UPDATE_IN_PROGRESS` (bit 15): writes to `VREG` are ignored while set.
const VREG_UPDATE_IN_PROGRESS: u32 = 1 << 15;

/// Raise the core regulator to 1.20 V for the 250 MHz configuration.
///
/// Sequence per the SDK's `vreg_set_voltage` (RP2350 variant): unlock the
/// interface, wait for any in-flight update, write VSEL, wait until the
/// regulator reports the new setting applied. Must run while the core is
/// still on the boot clock (ROSC/clk_ref), i.e. before the PLL is selected.
fn raise_vreg_1v20(powman: &POWMAN) {
    // Unlock (sticky: "cannot be relocked when it is unlocked"). Read-modify-
    // write so HT_TH/RST_N keep their reset values; the read returns the low
    // 16 bits only, the password lives in the write-only upper half.
    powman
        .vreg_ctrl()
        .modify(|r, w| unsafe { w.bits(POWMAN_PASSWORD | (r.bits() & 0xFFFF) | VREG_CTRL_UNLOCK) });

    while powman.vreg().read().bits() & VREG_UPDATE_IN_PROGRESS != 0 {}
    powman.vreg().modify(|r, w| unsafe {
        w.bits(POWMAN_PASSWORD | ((r.bits() & 0xFFFF) & !VREG_VSEL_MASK) | VREG_VSEL_1V20)
    });
    while powman.vreg().read().bits() & VREG_UPDATE_IN_PROGRESS != 0 {}

    // The regulator output ramps after UPDATE_IN_PROGRESS clears; give it a
    // moment before asking for a faster clock. ~1 ms even on the slow boot
    // clock (delay() is >= n cycles).
    cortex_m::asm::delay(12_000);
}

/// Bring up XOSC + PLLs and return the running clock tree.
///
/// Panics (into the fatal blink loop) if the XOSC or a PLL fails to lock --
/// a hardware fault; nothing sensible can run without a system clock.
pub fn init(
    xosc: XOSC,
    clocks: CLOCKS,
    pll_sys: PLL_SYS,
    pll_usb: PLL_USB,
    powman: &POWMAN,
    resets: &mut RESETS,
    watchdog: &mut Watchdog,
) -> ClocksManager {
    if OVERCLOCK_250 {
        raise_vreg_1v20(powman);
    }
    let pll_sys_cfg = if OVERCLOCK_250 {
        PLL_SYS_250MHZ
    } else {
        PLL_SYS_200MHZ
    };

    // From here on this mirrors `rp235x_hal::clocks::init_clocks_and_plls`.
    let xosc = setup_xosc_blocking(xosc, XOSC_HZ.Hz()).unwrap();

    // Watchdog tick = 1 us, which is what the 1 MHz TIMER0 counts.
    watchdog.enable_tick_generation((XOSC_HZ / 1_000_000) as u16);

    let mut clocks = ClocksManager::new(clocks);

    let pll_sys = setup_pll_blocking(
        pll_sys,
        xosc.operating_frequency(),
        pll_sys_cfg,
        &mut clocks,
        resets,
    )
    .unwrap();
    let pll_usb = setup_pll_blocking(
        pll_usb,
        xosc.operating_frequency(),
        PLL_USB_48MHZ,
        &mut clocks,
        resets,
    )
    .unwrap();

    // clk_ref = XOSC, clk_sys = PLL_SYS, clk_usb/clk_adc = PLL_USB,
    // clk_hstx = PLL_SYS, clk_peri = clk_sys (no UART/SPI on this board, so
    // a 200 MHz peripheral clock is harmless).
    clocks.init_default(&xosc, &pll_sys, &pll_usb).unwrap();
    clocks
}
