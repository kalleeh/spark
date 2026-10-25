# Spark — agent working notes

Spark is a PICO-8-compatible fantasy console in Rust. Workspace layout:

- `spark-core` — `#![no_std]` + `alloc` library: console/renderer
  (`src/console/{draw,text,memory,input}.rs`), audio, cart parser, Lua
  preprocessor, font, game state. **Shared by both targets.** The PICO-8 memory
  map lives in `console/memory.rs` and matches fake-08 (`0x1000-0x1FFF` is the
  upper sheet aliased with map rows 32-63; flags at `0x3000`; cartdata at
  `0x5E00`; draw-state registers at `0x5F00`).
- `spark-desktop` — native app (macroquad + mlua/Lua 5.4, `std`). Built-in
  editor (`src/editor/`, one file per tab), audio output, `.p8.png` support,
  the window-free `spark_desktop_lib`. **All cart start/step/teardown goes
  through `runtime::CartRuntime`** (`start` → `tick` → `finish`); main.rs,
  `cart_check`, and the tests share it — never re-implement that sequence.
- `spark-pico` — bare-metal RP2350/Pico 2 firmware (Lua 5.4 via vendored C).

Changes to `spark-core` affect **both** the desktop app and the Pico firmware —
verify both (see Verifying below).

## Diagnosing rendering / compatibility bugs — use `cart_check` first

When a cart renders wrong or misbehaves, **reproduce it headlessly** before
touching the renderer. `cart_check` runs a cart through the real Lua API with no
window and lets you capture frames, script input, and trace draw calls. The PNG
output and trace logs are directly readable by an agent — read them with the
Read tool to *see* the bug instead of speculating.

```sh
# Drive into gameplay, dump frames + draw trace, and inspect state inline
cargo run -p spark-desktop --example cart_check -- \
    --frames 120 --press "x@5,right@40-110" --every 20 --outdir /tmp/f --trace \
    --eval "p1.x..','..p1.y" --eval "fget(mget(2,15),0)" <cart>
```

Then `Read /tmp/f/frame_0040.png` to inspect the rendered frame. See the
"Headless Cart Diagnostics" section of `README.md` for the full flag reference.

Typical signal → cause mapping:
- `PIXELS 0` / "screen is blank" → cart errored early or never drew; check the
  `FAIL` detail line for the Lua error and source line.
- a fill pattern / checkerboard the cart didn't ask for → look at the renderer,
  not the cart. `--trace` shows whether the cart actually called `fillp`/`map`/
  `spr`; if the artifact appears with no matching call, it's our bug.
- physics/logic gone wrong (entity flung, stuck, wrong state) → use `--eval` to
  read the relevant globals/fields frame-over-frame. A classic trap: an API that
  should return a Lua boolean returning a number instead — `0` is **truthy** in
  Lua, so `if api(...)` fires when it shouldn't.

**Do NOT create throwaway `dump_*.rs` / `probe_*.rs` examples to inspect state.**
`--eval EXPR` runs an arbitrary Lua expression against the live post-run state
and prints it — that covers player position, tile reads (`mget`), flags
(`fget`), memory (`peek`), etc. If you ever need something `--eval` genuinely
can't express, add a *named, documented* flag to `cart_check` rather than a
scratch file. `spark-desktop/examples/` holds exactly four committed tools:
`cart_check` plus the `fuzz_engine` / `fuzz_loader` / `fuzz_audio` fuzzers
(all declared as `[[example]]` in `spark-desktop/Cargo.toml`). Nothing else.

Ground truth for PICO-8 behavior: clone references when needed —
fake-08 (`source/graphics.cpp` etc.) and zepto8 are the C++ implementations
already used to validate this codebase. Verify a behavior against them before
changing semantics.

## Verifying changes

```sh
cargo test -p spark-core -p spark-desktop          # unit/integration tests
cargo fmt --all
cargo clippy -p spark-core -p spark-desktop --all-targets   # NOTE: not --workspace
cargo build --profile pico -p spark-pico --target thumbv8m.main-none-eabihf  # Pico 2 (default board); MUST link
cargo build --profile pico -p spark-pico --target thumbv8m.main-none-eabihf --no-default-features --features board-tufty2350  # Tufty 2350; MUST link
```

**Boards.** `spark-pico` compiles for exactly one board, chosen by a Cargo
feature (`board-pico2` default, `board-tufty2350`; both or neither is a
`compile_error!`). All pin/peripheral code lives under `spark-pico/src/board/`
behind `board::init(pac) -> Board`; `main.rs` is board-independent. Build both
boards for any `spark-pico` change outside a single `board/<name>/` directory.
Tufty design, pinout and work packages: `docs/tufty2350-port.md`.

**Use `cargo build` (not just `cargo check`) for the Pico crate.** `check` only
type-checks; it does NOT link, and for a long time the firmware passed `check`
while failing to LINK (undefined `__sidata`/`__bi_entries_start`). The linker
args (`-Tlink.x`, `--nmagic`) are emitted by `spark-pico/build.rs` via
`cargo:rustc-link-arg-bins`, so the build links from **any** directory (a
previous `.cargo/config.toml` rustflags approach only worked with cwd inside
`spark-pico/`). `--profile pico` is `release` + `opt-level="s"` + DWARF kept
(`strip=false`) so probe-rs/gdb can symbolize a HardFault; plain `--release`
also links. A green `check` is NOT evidence the firmware is flashable — only a
successful `build` produces a real ELF. Report `llvm-size`/`arm-none-eabi-size`
on the ELF when touching RAM: static RAM is ~468 KB of 520 KB (`.data` ~75 KB =
`const`-constructed Console + Audio statics; heap 392 KB, essentially all for
Lua), leaving a ~52 KB MSPLIM-guarded stack. Keep `Console::new`/`Audio::new`/
`CartData::new` `const fn` — the firmware relies on it to avoid boot-time stack
temporaries. Lua's GC is tuned in `lua_ffi::tune_gc` (pause 120) so peak heap
stays near live data. The vendored Lua is built `LUA_32BITS` (`lua_Number` =
`f32`, asserted by `_Static_assert` in `lua/compat/luacompat.c`); the FFI in
`lua_ffi.rs` uses `LuaNumber`/`LuaInteger` aliases — never `f64`/`i64` there.

`clippy --workspace` fails: `spark-pico` vendors Lua C with ARM inline asm that
can't compile for the host. Lint the two host crates and `cargo build` the Pico
crate against its embedded target instead.

For any `spark-core` change, run the Pico `cargo build` — a renderer/audio fix
lands on the microcontroller too.

## API tests live at the binding layer

Engine unit tests (`spark-core`) test Rust methods; they pass even when the
*Lua binding* is wrong (e.g. `fget` returned a number `0`, which is truthy in
Lua). The real coverage is `spark-desktop/tests/lua_api_coverage.rs`: it drives
every registered global *through Lua* and asserts on screen/state. **When you
add or change a PICO-8 API function, add/adjust its test there** — the
`api_surface_is_fully_covered` guard fails the build if a new global has no
coverage entry. Assert true PICO-8 semantics (verified vs fake-08), not just
current behavior: boolean vs number returns, 16.16 fixed-point, angle-as-turns,
camera/clip offsets, transparency.

## Two binding layers — keep them in lockstep

The API is wired to Lua twice: `spark-desktop/src/lua_api.rs` (mlua, host) and
`spark-pico/src/lua_ffi.rs` (C FFI, embedded). The Pico layer **cannot run on
the host** (vendored Lua C with ARM asm), so it has no runtime test. Two
safeguards keep them from drifting (both have bitten us — `fget`, `tonum`):
1. **Shared numeric semantics** live in `spark-core/src/api_ops.rs` (bitwise,
   trig, `sgn`, `parse_pico8_number`). Both layers call these — do NOT
   re-implement the math inline in a binding. Unit-tested once on the host.
2. **`spark-desktop/tests/api_parity.rs`** parses both source files and fails if
   they register different API-name sets. Add a function to one layer → add it
   to the other (or record an intentional exception in `desktop_only`/
   `pico_only`).

`cart_check` can drive the **mouse** too (`--mouse "move@0-200:64,80;left@40-60"`)
— needed to test mouse-driven carts (click-to-dash, etc.).
