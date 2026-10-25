# Spark on the Pimoroni Badgeware Tufty (Tufty 2350) — port spec

Status: implemented (2026-09-12) behind the `board-tufty2350` feature; builds and links, **never run on hardware yet** — see the first-boot checklist at the end. Source of truth for hardware facts is the
Pimoroni firmware checkout at `~/Projects.local/tufty2350` (files cited below)
and `spark-pico/reference/sfe_psram.c` (SparkFun, MIT) for PSRAM bring-up.

## Board facts

| Item | Value | Source |
|---|---|---|
| MCU | RP2350B (QFN-80, GPIO 0-47), dual Cortex-M33 | board/pimoroni_tufty2350.h |
| SRAM / PSRAM / flash | 520 KB / 8 MB (QMI CS1 on **GPIO 8**, mapped at `0x11000000` cached, `0x15000000` uncached) / 16 MB | h:35, st7789.hpp:21 |
| Sys clock | 200 MHz @ default 1.10 V (Pimoroni's tested config); 250 MHz needs VREG 1.20 V (POWMAN `VREG` VSEL `0b01101`) **before** PLL setup; 266 MHz is the PSRAM ceiling | h:70-74, ci/pico-sdk-crt0-startup-rosc.patch |
| LCD | ST7789, 320x240, **8-bit 8080 parallel**: DB0-7 = GPIO 32-39, WR = 30 (PIO side-set), RD = 31 (SIO, held high), RS/DC = 28, CS = 27, backlight PWM = 26, TE/vsync = 21. **No reset pin** (SWRESET only) | st7789.hpp:35-41 |
| Buttons | Up 11, Down 6, A 7, B 9, C 10, Home 22 (= BOOTSEL). All **internal pull-up, active-low**. Reset button on 14 (pull-up, low = held) | h:38-42, powman.c:303-318 |
| Audio | none | pins.csv |
| Power | Nothing to hold. GPIO 41 (`POWER_EN`) only switches the RTC/LED rail — leave low. Wi-Fi (`WL_ON` GPIO 23) — leave low. | powman.c:59-69,344 |
| Other | RTC PCF85063A on I2C0 (SDA 4 / SCL 5), VBUS detect GPIO 12 (active-high), light sensor ADC GPIO 43, case LEDs GPIO 0-3, SWD header | pins.csv |

### LCD protocol (copy exactly)

PIO program (`modules/c/st7789/st7789_parallel.pio`), 2 cycles/byte:
```
.program st7789_parallel
.side_set 1
.wrap_target
    out pins, 8  side 0
    nop          side 1
.wrap
```
State machine (`st7789.hpp:58-98`): PIO1, **`GPIOBASE = 16`** (PAC `PIO1.gpiobase`,
required because data pins ≥ 32; all pin numbers given to the SM are then
`gpio - 16`), out pins = 8 from DB0, side-set = WR, TX FIFO join, out-shift
left with autopull at 8 bits, clock divider
`ceil(2 * max(1, sys_hz / 44 MHz)) / 2` (44 MHz is Pimoroni's flicker-free
ceiling; 200 MHz → 5.0, 250 MHz → 6.0). DMA: 8-bit transfers, DREQ = PIO1 TX
of that SM, write to `PIO1.txf[sm]`. `command()` = CS low, DC low, 1-byte DMA,
wait (DMA done **and** PIO `FDEBUG.TXSTALL`), DC high, data DMA. RD is a plain
output held high.

Init sequence (`st7789.cpp:58-141`), after backlight PWM off:
```
SWRESET(0x01); delay 150 ms
COLMOD  0x3A: 05
PORCTRL 0xB2: 0C 0C 00 33 33
LCMCTRL 0xC0: 2C
VDVVRHEN 0xC2: 01
VRHS    0xC3: 0F
VDVS    0xC4: 20
PWCTRL1 0xD0: A4 A1
FRCTRL2 0xC6: 0F
RAMCTRL 0xB0: 00 C0
GCTRL   0xB7: 35
VCOMS   0xBB: 1B
GMCTRP1 0xE0: F0 00 06 04 05 05 31 44 48 36 12 12 2B 34
GMCTRN1 0xE1: F0 0B 0F 0F 0D 26 31 43 47 38 14 14 2C 32
INVON   0x21
SLPOUT  0x11; delay 100 ms
CASET   0x2A: 00 00 00 EF      (0..239)
RASET   0x2B: 00 00 01 3F      (0..319)
MADCTL  0x36: 90               (ROW_ORDER|SCAN_ORDER; portrait 240 wide x 320 tall)
RAMWR   0x2C + 153600 zero bytes (clear)
TEON    0x35: 00
STE     0x44: 00 00
DISPON  0x29
backlight on (~230/255, gamma 2.8; duty floor ~11 % or the panel stays dark)
```
Pixels are RGB565, **high byte first** on the bus. A frame is 320 portrait rows
of 240 pixels (480 bytes each) after `RAMWR`; Pimoroni streams one row per
DMA with a two-row ping-pong buffer, never waiting on the last one. Backlight:
PWM wrap 65535 at sys clock (≈3 kHz).

## Design

### Board abstraction (spark-pico)
Cargo features `board-pico2` (default; today's SPI ST7789 + PWM audio) and
`board-tufty2350`, mutually exclusive, compile-time checked. New module
`src/board/{mod.rs, pico2.rs, tufty2350.rs}`. `board::init(pac, ...)` owns all
pin/peripheral setup and returns:
- a `&'static mut dyn FrameSink` (existing trait in `display.rs`), installed
  via `display::install`;
- an `InputSource` that yields `[bool; 6]` in PICO-8 order
  (left, right, up, down, O, X) each frame;
- `Option<AudioBackend>` (Tufty: `None` — the synth still runs so `stat()`
  and music position advance, but no PWM/IRQ is started);
- the system clock in Hz.
`main.rs` loses every direct pin reference. `cart_baked`, `lua_ffi`, `fatal`,
`cmath`, `audio_out` (behind the pico2 feature) are unchanged.

### Tufty display (`board/tufty2350/lcd.rs`)
- PIO via `rp235x_hal::pio` + `pio_proc::pio_asm!` for the program; set
  `GPIOBASE` through the PAC (the HAL has no wrapper).
- DMA via `rp235x_hal::dma::single_buffer` (or double_buffer) to the SM `Tx`,
  8-bit transfers.
- `FrameSink::show`: map 128x128 → **centered 240x240** with nearest-neighbour
  LUTs (`row_lut[240]`, `col_lut[240]`, both `i*128/240`), rows 0-39 and
  280-319 of the portrait frame are black (single static 480-byte zero row,
  DMA'd 80 times), rows 40-279 convert one source column each. Palette →
  RGB565 (`screen_pal` applied first, exactly like `screen_to_rgba_buf`)
  through a 16-entry big-endian LUT. Orientation flags (flip x / flip y /
  transpose) are `const`s so a wrong guess is a one-line fix after the first
  boot photo.
- The 128→240 LUT + RGB565 packing are pure functions: put them in
  `spark-core` (`no_std`) with host unit tests so the only untested code on
  the device is the PIO/DMA glue.
- Budget: 153,600 bytes at ~20 MB/s ≈ 7.5 ms/frame, DMA-driven; the CPU
  conversion is ~58 K LUT lookups. Later: move `show` to core 1.

### Input (`board/tufty2350/buttons.rs`)
Pull-up inputs, active-low. Map: **A → left, C → right, B → O, Home → X,
Up/Down native.** Feed the existing `input::Input` debouncer.

### Clock and power
`init_clocks_and_plls` at **200 MHz** (VCO 1200 MHz, postdiv 6/1) by default.
`const OVERCLOCK_250: bool = false` path: write POWMAN `VREG` VSEL 1.20 V
(unlock + wait `UPDATE_IN_PROGRESS`, per Pimoroni's crt0 patch) before the
PLL, then VCO 1500 MHz 6/1. GPIO 23 and 41 left as inputs (low).

### PSRAM (`board/tufty2350/psram.rs`)
Port `reference/sfe_psram.c` to the PAC `QMI`/`XIP_CTRL` registers:
1. GPIO 8 → function `XIP_CS1`.
2. Direct mode on CS1: RSTEN `0x66`, RST `0x99`, read ID `0x9F` → size
   (expect 8 MB; fall back to "no PSRAM" cleanly), QUAD ENABLE `0x35`.
3. `M1_TIMING`: clkdiv = ceil(sys / 109 MHz), max_select / min_deselect from
   the datasheet constants in the reference, `rxdelay` per reference.
4. `M1_RFMT`/`M1_RCMD` quad read `0xEB`, `M1_WFMT`/`M1_WCMD` quad write `0x38`,
   `XIP_CTRL.WRITABLE_M1 = 1`.
5. Region `0x11000000 .. +size` becomes a second heap (`embedded_alloc::LlffHeap`
   instance, not the global allocator).
Lua gets a custom `lua_Alloc` (switch `luaL_newstate` → `lua_newstate`) that
serves **all Lua allocations from the PSRAM heap**; SRAM keeps the console,
audio, Rust allocations and stack. `HEAP_SIZE` (SRAM) shrinks to ~96 KB,
freeing ~300 KB of SRAM for a larger stack and a later 240x240 line cache.
`stat(0)` reports PSRAM heap use. Follow-up (not now): route small hot Lua
objects to SRAM by size class.

### Flashing
Hold **Home** while tapping **Reset** → RP2350 mass-storage. Convert with
`picotool uf2 convert spark-pico.elf spark-pico.uf2 --family rp2350-arm-s`
(picotool is not installed here; document `brew install picotool`). Copy the
UF2 to the drive. This replaces Badgeware/BadgeOS; restore by copying
Pimoroni's release UF2 the same way. SWD header + `--profile pico` symbols
work with probe-rs.

### Verification (no hardware in this environment)
- `cargo build --profile pico -p spark-pico --target thumbv8m.main-none-eabihf --features board-tufty2350`
  and the default `board-pico2` build both link; sizes reported.
- `cargo test -p spark-core` covers the scaling LUT and RGB565 packing.
- `api_parity`, clippy, fmt as usual.
- First-boot checklist for the user: backlight comes on → init ran; PICO-8
  boot screen visible → PIO/DMA/orientation right (fix flip consts if
  mirrored); buttons move things → mapping right; `stat(0)` via a test cart
  prints heap KB → PSRAM alive.

## Work packages
- **T1 spark-core**: `pub fn scale_lut(dst: usize, src: usize) -> [u8; N]`-style
  LUT builder + big-endian RGB565 palette table + tests.
- **T2 spark-pico board abstraction**: `board/` module, features, `main.rs`
  rewired, pico2 path behaviour-identical (build + size check).
- **T3 spark-pico Tufty LCD + buttons + clock**: PIO/DMA driver, init
  sequence, `FrameSink`, button map, 200 MHz clocks, backlight PWM.
- **T4 spark-pico PSRAM + Lua allocator**: QMI bring-up, PSRAM heap,
  `lua_newstate` with custom allocator, `stat(0)`, SRAM heap resize.
- **Integrator**: CLAUDE.md (board features, build commands), README flashing
  section, memory notes.
