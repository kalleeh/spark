# Spark

A PICO-8-compatible fantasy console written in Rust, targeting both desktop and the RP2350 microcontroller.

## What is this?

Spark is an open-source fantasy console that runs PICO-8 cartridges (`.p8` and `.p8.png` files). It provides a built-in code/sprite/map/SFX/music editor, a Lua runtime with PICO-8-compatible API, and pixel-perfect 128x128 rendering. The same core library compiles to native desktop (via macroquad) and bare-metal RP2350 firmware (Raspberry Pi Pico 2).

## Features

**Runtime**
- 128x128 screen with the standard PICO-8 16-color palette
- Lua 5.4 scripting with full PICO-8 API surface
- `.p8` text cart and `.p8.png` steganographic cart loading
- 30fps and 60fps modes (`_update` / `_update60`)
- 2-player input, mouse support, keyboard input via `stat(30)`/`stat(31)`

**Editor**
- Tabbed editor: Code, Sprite, Map, SFX, Music
- Syntax-aware code editor with undo/redo, find, copy/paste
- Sprite editor with zoom, color palette, sheet preview
- Map editor with tile placement
- SFX piano-roll editor and music pattern sequencer
- Token and character count display (PICO-8 limits)

**PICO-8 API Coverage**

| Category | Functions |
|----------|-----------|
| Graphics | `cls` `pset` `pget` `line` `rect` `rectfill` `circ` `circfill` `oval` `ovalfill` `spr` `sspr` `sget` `sset` `print` `tline` |
| Drawing state | `camera` `clip` `pal` `palt` `color` `cursor` `fillp` |
| Map | `map` `mget` `mset` `fget` `fset` |
| Input | `btn` `btnp` `stat` |
| Math | `sin` `cos` `atan2` `sqrt` `abs` `flr` `ceil` `min` `max` `mid` `sgn` `rnd` |
| Bitwise | `band` `bor` `bxor` `bnot` `shl` `shr` `lshr` `rotl` `rotr` |
| Memory | `peek` `poke` `peek2` `peek4` `poke2` `poke4` `memcpy` `memset` `cstore` `reload` |
| Table | `add` `del` `deli` `count` `foreach` `all` |
| String | `sub` `tostr` `tonum` `chr` `ord` `split` |
| Audio | `sfx` `music` |
| System | `time`/`t` `flip` `load` `run` `stop` `dget` `dset` `cartdata` `printh` `extcmd` `menuitem` |

**Preprocessor**
- Compound assignment (`+=`, `-=`, `*=`, `/=`, `%=`, `..=`)
- Not-equal shorthand (`!=` to `~=`)
- Print shorthand (`?expr`)
- Single-line `if (cond) stmt` / `if (cond) stmt else stmt`
- Backslash line continuation
- `#include` directive support

## Architecture

```
+-----------------------------------------------------------+
|                      Workspace                            |
|                                                           |
|  +---------------------------------------------------+   |
|  |  spark-core  (no_std library)                      |   |
|  |  Console, Audio, Cart parser, Lua preprocessor,    |   |
|  |  Font rendering, Game state                        |   |
|  +---------------------------+-----------------------+   |
|           |                           |                   |
|           v                           v                   |
|  +-----------------+     +------------------------+       |
|  | spark-desktop   |     | spark-pico             |       |
|  | macroquad +     |     | RP2350 bare-metal      |       |
|  | mlua (Lua 5.4)  |     | Lua 5.4 via C (cc)     |       |
|  | Built-in editor |     | ST7789 SPI display     |       |
|  | Audio output    |     | PWM audio, GPIO input  |       |
|  | .p8.png support |     | Baked-in cart ROM      |       |
|  +-----------------+     +------------------------+       |
+-----------------------------------------------------------+
```

`spark-core` is a `#![no_std]` crate with an optional `std` feature. The desktop target enables `std`; the RP2350 target uses `default-features = false` for bare-metal compatibility. Both targets share identical console rendering, audio synthesis, cart parsing, and Lua preprocessing logic.

## Getting Started (Desktop)

**Prerequisites:** Rust toolchain (stable)

```sh
# Build
cargo build --release

# Run the editor (opens with a blank cart)
cargo run --release

# Open a cart file
cargo run --release -- carts/hello.p8

# Open and immediately run a cart
cargo run --release -- carts/hello.p8 --run
```

The binary is produced at `target/release/spark`.

## Getting Started (RP2350 / Pico 2)

**Prerequisites:** Rust toolchain with `thumbv8m.main-none-eabihf` target, `elf2uf2-rs` or `picotool`

```sh
# Install the target (one-time)
rustup target add thumbv8m.main-none-eabihf

# Build the firmware
cargo build --release -p spark-pico

# Convert to UF2 and flash (hold BOOTSEL, plug in Pico 2)
elf2uf2-rs target/thumbv8m.main-none-eabihf/release/spark-pico

# Or copy the UF2 manually to the mounted drive
```

**Hardware wiring (SPI0 to ST7789 display):**

| Signal | GPIO Pin |
|--------|----------|
| SCK    | GP18     |
| MOSI   | GP19     |
| CS     | GP17     |
| DC     | GP20     |
| RST    | GP21     |
| BL     | GP22     |

**Memory budget (520 KB SRAM):**

| Region | Size |
|--------|------|
| Console (static) | ~65 KB |
| Audio (static) | ~9 KB |
| Heap (Lua + alloc) | 200 KB |
| Stack | ~8 KB |

## Editor Keyboard Shortcuts

All shortcuts use Ctrl on Linux/Windows and Cmd on macOS.

**Global**

| Shortcut | Action |
|----------|--------|
| Ctrl+R | Run cart |
| Ctrl+S | Save cart |
| Ctrl+Shift+S | Save cart as |
| Ctrl+N | New cart |
| Ctrl+O | Open file picker |
| Alt+Left/Right | Cycle editor tabs |
| Escape | Stop running cart (return to editor) |

**Code Editor**

| Shortcut | Action |
|----------|--------|
| Ctrl+F | Toggle find/search |
| Ctrl+G / Enter | Jump to next search match |
| Ctrl+A | Select all |
| Ctrl+Z | Undo |
| Ctrl+Y / Ctrl+Shift+Z | Redo |
| Ctrl+C | Copy selection (or current line) |
| Ctrl+X | Cut selection (or current line) |
| Ctrl+V | Paste |
| Shift+Arrows | Extend selection |

**Game Input**

| Key | Action |
|-----|--------|
| Arrow keys | Player 1 D-pad |
| Z / C / N | Player 1 O button |
| X / V / M | Player 1 X button |
| E / S / D / F | Player 2 D-pad (up/left/down/right) |
| LShift / A | Player 2 O / X buttons |

## PICO-8 Compatibility Notes

Spark aims for high compatibility with PICO-8 cartridges. Key differences:

| Area | PICO-8 | Spark |
|------|--------|-------|
| Number type | 16.16 fixed-point | IEEE 754 `f64` (desktop) / `f32` (RP2350) |
| Lua version | Custom PICO-8 Lua | Lua 5.4 with PICO-8 compat shims |
| Integer overflow | Wraps at 32767.9999 | Standard float behavior |
| Bitwise ops | Operate on fixed-point | Emulated on float with floor/cast |
| `peek`/`poke` | Full 32 KB memory map | Implemented for sprite, map, flag, SFX, user data regions |

**What works well:**
- Standard drawing, sprite, map, and input APIs
- PICO-8 Lua syntax extensions (compound assignment, `!=`, `?`, single-line `if`)
- Audio: SFX playback and music pattern sequencing
- Cart loading: `.p8` text carts and `.p8.png` image carts
- Multi-cart support via `load()` with breadcrumb passing
- Memory-mapped access (`peek`/`poke`/`memcpy`/`memset`/`cstore`/`reload`)

**Known limitations:**
- Fixed-point edge cases: carts that depend on exact 16.16 overflow or precision will behave differently
- Some `stat()` fields are stubbed (returns sensible defaults)
- No network or GPIO API

## Tests

Tests are spread across both `spark-core` (200 tests) and `spark-desktop` (245 tests), covering the console, audio engine, cart parser, Lua preprocessor, editor, PNG decoding, and Lua API bindings.

```sh
# Run all tests
cargo test --workspace

# Run only the core library tests (no desktop dependencies)
cargo test -p spark-core

# Run tests with output
cargo test --workspace -- --nocapture
```

Total: **445 tests** across 8 source files.

## Example Cart

```lua
-- hello.p8 (carts/hello.p8)
t=0
function _init()
 cls()
end

function _update()
 t+=1
end

function _draw()
 cls(1)
 for i=0,15 do
  rectfill(i*8,0,i*8+7,7,i)
 end
 print("spark fantasy console",10,40,7)
 circ(64,90,10+sin(t/30)*5,10)
 circfill(64,90,6+sin(t/30)*3,12)
end
```

## License

MIT
