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
- Compound assignment (`+=`, `-=`, `*=`, `/=`, `%=`, `..=`, `^=`, `\=`)
- Bitwise/shift compound assignment (`&=`, `|=`, `^^=`, `<<=`, `>>=`, `>>>=`, `<<>=`, `>><=`) → API calls (`band`/`bor`/`bxor`/`shl`/`shr`/`lshr`/`rotl`/`rotr`)
- Not-equal shorthand (`!=` to `~=`)
- Print shorthand (`?expr`)
- Single-line `if (cond) stmt` / `if (cond) stmt else stmt`
- Backslash line continuation
- `//` single-line comments (PICO-8 treats `//` as a comment, converted to `--`)
- `\` integer division (PICO-8 uses `\`; converted to Lua `//`), incl. `\=`
- Infix bitwise operators with no Lua equivalent (`>>>`, `<<>`, `>><`, `^^`) → API calls, precedence-aware
- Unary peek operators (`@`→`peek`, `%`→`peek2`, `$`→`peek4`), priority-aware
- Binary number literals (`0b1010`, with optional fraction) → decimal
- Custom string escapes (`\*` `\#` `\-` `\|` `\+` `\^` → P8SCII control bytes)
- `#include` directive support

`print()` interprets the P8SCII control codes those escapes produce — char
repeat (`\*`), background color (`\#`), cursor moves (`\-`/`\|`/`\+`), and the
`\^` command family (color `\^c`, home `\^g`/`\^h`, absolute jump `\^j`). Every
`\^` command consumes its correct parameter count, so style/font commands not
yet rendered still never leak bytes as stray glyphs.

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

## Getting Started (RP2350 firmware)

The firmware supports two boards, selected by a Cargo feature:

| Feature | Board | Display | Input | Audio |
|---|---|---|---|---|
| `board-pico2` (default) | Raspberry Pi Pico 2 + SPI ST7789 128x128 | SPI0 | 8 buttons on GP0-7 | PWM on GP8 |
| `board-tufty2350` | Pimoroni Badgeware Tufty (Tufty 2350) | built-in 320x240, 8-bit parallel via PIO + DMA | face buttons | none on board |

**Prerequisites:** Rust toolchain with the `thumbv8m.main-none-eabihf` target and `picotool`
(`brew install picotool`) for the UF2 conversion.

```sh
rustup target add thumbv8m.main-none-eabihf

# Pico 2 (default board). --profile pico = size-optimized + debug symbols kept.
cargo build --profile pico -p spark-pico --target thumbv8m.main-none-eabihf

# Badgeware Tufty
cargo build --profile pico -p spark-pico --target thumbv8m.main-none-eabihf \
    --no-default-features --features board-tufty2350

# Convert to UF2
picotool uf2 convert target/thumbv8m.main-none-eabihf/pico/spark-pico spark-pico.uf2 --family rp2350-arm-s
```

Flashing: put the board in mass-storage mode (Pico 2: hold BOOTSEL while plugging in;
Tufty: hold **Home** and tap **Reset**), then copy `spark-pico.uf2` onto the drive that
appears. On the Tufty this replaces the Badgeware/BadgeOS firmware; restore it by copying
Pimoroni's release UF2 the same way. Both boards expose SWD, and the `pico` profile keeps
DWARF so `probe-rs` can symbolize a fault.

**Pico 2 wiring (SPI0 to ST7789):**

| Signal | GPIO |
|--------|------|
| SCK    | GP18 |
| MOSI   | GP19 |
| CS     | GP17 |
| DC     | GP20 |
| RST    | GP21 |
| BL     | GP22 |
| Buttons L/R/U/D/O/X/Start/Select | GP0-7 (active-low) |
| Audio (PWM) | GP8 |

**Tufty button map:** Up/Down native, **A = left, C = right, B = O, Home = X**.
The 128x128 PICO-8 screen is shown nearest-neighbour scaled to a centered 240x240 area.
Details, pinout and the PSRAM design: `docs/tufty2350-port.md`.

**Memory (both boards, 520 KB SRAM):** Console and Audio are `const`-constructed statics
(~75 KB); the SRAM heap is 392 KB; the stack is ~52 KB and guarded by MSPLIM. On the Tufty
the Lua heap moves to the 8 MB PSRAM and the SRAM heap shrinks accordingly. `stat(0)`
reports live Lua heap use in KB.

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
| Bitwise ops | Operate on fixed-point | Bit-exact: operate on the 16.16 bit pattern |
| `fillp` | 4x4 pattern, MSB-first, two-color | Matches: bit 15 = top-left; shape fills (`rectfill`/`circfill`/`ovalfill`) use both color nibbles (primary + secondary), sprites/lines use the primary |
| `circ`/`circfill`/`oval`/`ovalfill` | Specific PICO-8 Bresenham algorithms | Match PICO-8 pixel-for-pixel (r=1 diamond/plus; no outline bulge; integer-radius ellipse) |
| `sspr` scaling | 16.16 fixed-point source stepping | Matches: source index `(i * ((sw<<16)/dw)) >> 16`, so non-integer scales sample the same source pixels as PICO-8 |
| `tline` | Textured **map** line | Samples the map (`mget(flr mx, flr my)`) for a tile, then that tile's sub-pixel — not the sprite sheet directly. (Tline-map width/height/offset registers default-only.) |
| `line` forms | `line()` reset / `line(c)` / `line(x,y)` continue / `line(x0,y0,x1,y1)` | Matches: stateful polyline — a full segment sets the anchor, `line(x,y)` continues from it; `line()` resets and `line(c)` resets + sets the pen color. A 2/3-arg call with no anchor draws nothing (only updates the pen). |
| Pen color persistence | Any draw call with a color arg sets the pen | Matches: a color passed to `line`/`rect`/`circ`/`spr`-less primitives/`print` etc. updates the persistent pen color (full byte, both `fillp` nibbles), so later argless draws reuse it |
| `clip` | Clamps clip *edges*; returns prior rect; 5th `clip_previous` arg | Matches: `clip(x,y,w,h)` clamps `x` and `x+w` to `[0,128]` (a negative origin or oversized width shrinks the region, never shifts the far edge off-screen), returns the previous `(x,y,w,h)`, and the optional 5th arg intersects with the current clip |
| State-setter return values | `camera`/`color`/`cursor`/`fillp`/`clip` return their prior value | Matches: each returns what it replaced (`camera`→prior `(x,y)`, `color`→prior pen byte, `cursor`→prior `(x,y)`, `fillp`→prior pattern number, `clip`→prior `(x,y,w,h)`), so carts that save-and-restore draw state work |
| `print` arg forms | `print(s)` / `print(s,col)` / `print(s,x,y[,col])` | Matches: the 2-arg form treats arg2 as the **color** (printing at the cursor), not as an x position; returns the new x after the last glyph |
| `pal` arg forms | `pal()` / `pal(tbl[,p])` / `pal(p)` / `pal(c0,c1[,p])` | Matches: table form bulk-remaps from `{[c0]=c1,...}`, a lone number resets palette `p`, the pair form returns the previous mapping for `c0` |
| `chr`/`ord`/`split`/`tostr` | Multi-arg / count / size / hex-flag forms | Matches: `chr(a,b,...)` builds raw bytes; `ord(s,i,n)` returns `n` byte values; `split(s,n)` chunks by size and keeps spaces; `tostr(x,flags)` emits the 16.16 hex forms (`0x1`→`0xWWWW.FFFF`, `0x2`→raw int, `0x3`→`0xWWWWFFFF`) |
| `tonum(s,flags)` | Hex / shift flag forms (inverse of `tostr`) | Matches: `0x1` reads the string as raw hex (non-hex→0), `0x2` parses then shifts the 16.16 bit pattern right 16; `tonum(tostr(x,0x2),0x2)` round-trips |
| `add`/`del`/`count` | Insert-position / return-value / value-count forms | Matches: `add(c,x,i)` inserts at index `i` and returns `x`; `del(c,v)` returns the removed value; `count(c,v)` counts elements equal to `v`, and `count(c)` counts non-nil elements in `[1,#c]` |
| `all`/`foreach` | Safe against deletion during iteration | Matches: a callback may `del()` the current element (re-reads `#c` each step, skips nil holes, doesn't skip the element shifted into the freed slot) — required by carts like Celeste that destroy objects inside `foreach(objects, ...)` |
| `stat` fields | 24/54 music pattern, 28 keyboard bool, 29 = 0 | Matches the reference field map: `stat(28)` returns a boolean (keyboard scancode, unsupported→false), `stat(29)`→0, and `stat(54)` aliases `stat(24)` (was incorrectly returning an SFX id for 26-29) |
| `menuitem` callback | Receives a button bitmask; returning true keeps the menu open | Matches: the pause-menu callback is invoked with the trigger bitmask and, if it returns a truthy value, the pause menu stays open instead of closing |
| `btnp` auto-repeat | Re-fires at 15 frames held, then every 4 frames; configurable via `0x5F5C`/`0x5F5D` | Matches: `btnp` fires on the first frame, again at frame 15, then every 4th frame while held (resets on release). `poke(0x5F5C, delay)` / `poke(0x5F5D, interval)` override the timing; delay 255 disables repeat |
| `peek`/`poke` | Full 32 KB memory map | Implemented for sprite, map, flag, SFX, user data, and the draw-state hardware registers at `0x5F00-0x5F3F` (camera/clip/palette); `peek4`/`poke4` use the exact 16.16 byte layout |
| RNG (`rnd`/`srand`) | Deterministic integer PRNG | Bit-exact: same algorithm, so seeded sequences match |

**What works well:**
- Standard drawing, sprite, map, and input APIs
- PICO-8 Lua syntax extensions (compound assignment, `!=`, `?`, single-line `if`)
- Audio: SFX playback and music pattern sequencing
- Cart loading: `.p8` text carts and real PICO-8 `.p8.png` image carts
  (160x205 steganographic format, with `:c:` and `\0pxa` code decompression)
- Multi-cart support via `load()` with breadcrumb passing
- Memory-mapped access (`peek`/`poke`/`memcpy`/`memset`/`cstore`/`reload`)
- Deterministic RNG: `rnd`/`srand` reproduce PICO-8's exact sequence, so
  seeded procedural-generation carts match
- Bitwise ops operate on the full 16.16 bit pattern (e.g. `shl(0.125, 3) == 1`)

**Known limitations:**
- Number precision: arithmetic uses IEEE floats, not 16.16 fixed-point, so
  carts that depend on exact overflow-wrap (at ±32768), the constant 1/65536
  precision grid, or `x/0` saturating to `±0x7fff.ffff` will diverge. Making
  arithmetic itself bit-exact would require a fixed-point `LUA_NUMBER`, which
  is out of scope (it means forking the C Lua 5.4 number core). The
  cart-breaking cases — RNG, bitwise, and `peek4`/`poke4` layout — are handled
  exactly at the API layer.
- Some `stat()` fields are stubbed (returns sensible defaults)
- No network or GPIO API

## Tests

Tests come in three layers:

- **Engine unit tests** (`spark-core`): exercise the `Console`/`Audio`/cart/preprocessor Rust methods directly (drawing, memory map, bitwise, RNG, parsing).
- **Lua API coverage** (`spark-desktop/tests/lua_api_coverage.rs`): drive **every** registered PICO-8 global *through Lua* and assert on the resulting screen/state. This is where binding-layer bugs surface — wrong return type, Lua truthiness (`0` is truthy!), memory addresses, fixed-point conventions — which engine unit tests miss because the Rust method can be correct while the binding is wrong. A `api_surface_is_fully_covered` test fails if a new global is registered without a test, so coverage can't silently regress.
- **End-to-end** (`spark-desktop/tests/real_cart.rs`): loads a real BBS `.p8.png` and runs frames through the whole pipeline.

```sh
# Run all tests
cargo test --workspace

# Run only the core library tests (no desktop dependencies)
cargo test -p spark-core

# Run only the Lua API coverage suite
cargo test -p spark-desktop --test lua_api_coverage

# Run tests with output
cargo test --workspace -- --nocapture
```

Total: **598 tests** (including 54 Lua-level API coverage tests over all 87 registered globals, plus a parity test that the desktop and Pico binding layers expose the same API surface). Tricky numeric semantics (bitwise, trig, `tonum` hex/binary, `sgn`) live in `spark-core/api_ops.rs` and are shared by both targets so they cannot diverge.

Two fuzz examples harden the engine against inputs the sample carts never
exercise (both deterministic, no external deps; run under debug overflow checks
so an overflow panics and is caught):

- `fuzz_engine` hammers every drawing API and the full memory API
  (`peek`/`poke`/`peek2`/`peek4`/`memcpy`/`memset`/`cstore`/`reload`/`dget`/
  `dset`) with boundary/extreme inputs (`i32::MIN`/`MAX`, huge radii, extreme
  camera/clip, full u16 address space) for arithmetic overflow, out-of-bounds,
  and unbounded-loop bugs. Locked in by `test_drawing_extreme_coords_no_panic`
  and `test_memory_ops_full_address_space_no_panic`.
- `fuzz_loader` throws random / truncated / bit-flipped bytes at the cart-load
  pipeline (`.p8.png` steganography + `\0pxa`/`:c:` decompression, `.p8`
  parsing, the Lua preprocessor) — the untrusted-input path — asserting it
  never panics. Found a `\0pxa` out-of-bounds and a UTF-8 boundary crash, now
  covered by regression tests.
- `fuzz_audio` fills the SFX/music tables with arbitrary bytes, fires random
  `sfx`/`music` calls, and pumps `generate_samples` at extreme/degenerate
  sample rates, asserting every emitted sample is finite. Found a NaN-sample
  bug (zero sample rate → infinite phase); covered by
  `test_generate_samples_always_finite`.

## Headless Cart Diagnostics (`cart_check`)

`cart_check` is a window-free harness that loads a cart, runs it through the
real Lua API for N frames, and reports whether it executes and draws. It is the
primary tool for diagnosing rendering and compatibility bugs without a display —
useful for CI, for batch compatibility sweeps, and for an AI agent investigating
a visual bug, since the PNG output and trace logs can be read back directly.

```sh
# Quick compatibility check across a folder of carts
cargo run -p spark-desktop --example cart_check -- carts/*.p8.png

# Capture the final frame as <cart>.spark.png
cargo run -p spark-desktop --example cart_check -- --png carts/hello.p8

# Drive into gameplay with scripted input, dumping a PNG every 20 frames
cargo run -p spark-desktop --example cart_check -- \
    --frames 120 --press "x@5,right@40-110,o@70" \
    --every 20 --outdir /tmp/frames carts/game.p8.png

# Trace draw-call counts and state-call args per frame (to stderr)
cargo run -p spark-desktop --example cart_check -- --trace --frames 60 carts/game.p8.png

# Inspect cart/engine state after the run WITHOUT writing a throwaway program
cargo run -p spark-desktop --example cart_check -- \
    --frames 40 --press "x@10" \
    --eval "p1.x..','..p1.y" --eval "fget(mget(2,15),0)" carts/game.p8.png
```

| Flag | Effect |
|------|--------|
| `--frames N` | Number of frames to run (default 60) |
| `--png` | Write `<cart>.spark.png` of the final frame |
| `--every N` | Dump `frame_NNNN.png` every N frames into `--outdir` |
| `--outdir DIR` | Output directory for `--every` (default: alongside the cart) |
| `--press SPEC` | Scripted P1 input. Comma-separated `button@frame` or `button@start-end`. Buttons: `left right up down o x`. Frames 0-based. Example: `"x@5,right@40-110"` |
| `--mouse SPEC` | Scripted mouse (`stat(32/33/34)`), `;`-separated. `move@<frames>:x,y` positions the cursor; `left`/`right`/`mid@<frames>` hold a button. Example: `"move@0-200:64,80;left@40-60"`. Needed for mouse-driven carts. |
| `--trace` | Wrap the draw API and print per-frame geometry-call counts (`spr`, `map`, `print`, …) plus the last args to state calls (`camera`, `pal`, `palt`, `fillp`, `clip`, `color`) |
| `--eval EXPR` | After the run, evaluate a Lua expression against the live cart state and print it (to stderr). Repeatable. The supported way to inspect player position, tile reads, flags, etc. — no throwaway program needed. |

`--trace` and `--eval` work by running Lua against the same state the cart uses;
they require no engine changes and do not affect normal execution. A blank
screen (`PIXELS 0`), a `FAIL` with a Lua error line, or an anomalous draw count
(e.g. a fill pattern appearing where the cart never called `fillp`) each point
at a specific layer to investigate. Generated PNGs (`*.spark.png`,
`frame_*.png`, `screenshot_*.png`) are git-ignored.

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
