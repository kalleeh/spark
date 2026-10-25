//! Lua 5.4 C FFI bindings for the Spark fantasy console on RP2350.
//!
//! This module provides:
//! - Raw `extern "C"` bindings to the Lua 5.4 C API (compiled from vendored sources)
//! - A custom Lua allocator backed by `embedded-alloc` / the global allocator
//! - The single owner of the engine statics (`Console`, `Audio`, `GameState`)
//! - All PICO-8 API functions registered into Lua as globals
//! - Lifecycle helpers: `new_lua_state`, `load_code`, `run_top`, `call_global`
//!
//! **Number ABI.** The C library is compiled with `LUA_32BITS` (see
//! `build.rs`), so `lua_Number` is `float` and `lua_Integer` is `int`.
//! [`LuaNumber`] / [`LuaInteger`] are the only types used for those in the
//! `extern` block, and `lua/compat/luacompat.c` `_Static_assert`s the C sizes,
//! so a drift on either side is a build error rather than silent register
//! corruption (on hard-float a `double` travels in `d0`, a `float` in `s0`).
//!
//! **Semantics** mirror `spark-desktop/src/lua_api.rs`; the numeric corner
//! cases (`tonum`, `tostr`, bitwise ops, trig) go through
//! `spark_core::api_ops` so the two targets share one implementation.
//! `spark-desktop/tests/api_parity.rs` parses this file for the
//! `lua_register` calls and the `PRELUDE_DEFS` headers and fails if the two
//! layers register different names.

use core::alloc::{GlobalAlloc, Layout};
use core::ffi::{c_char, c_void, CStr};
use core::mem::MaybeUninit;
use core::sync::atomic::{AtomicBool, Ordering};

use alloc::borrow::Cow;
use alloc::boxed::Box;
use alloc::format;
use alloc::string::String;

use embedded_alloc::LlffHeap as Heap;
use spark_core::api_ops;
use spark_core::audio::Audio;
use spark_core::console::Console;
use spark_core::fixed::{from_bits, to_bits};
use spark_core::game_state::GameState;

// ============================================================================
// Part 1: Raw Lua C API Bindings
// ============================================================================

/// Opaque Lua state handle.
pub type LuaState = c_void;

/// `lua_Number` under `LUA_32BITS`.
pub type LuaNumber = f32;
/// `lua_Integer` under `LUA_32BITS`.
pub type LuaInteger = i32;
/// `lua_KContext` (`intptr_t`).
pub type LuaKContext = isize;

/// Lua C function signature.
pub type LuaCFunction = unsafe extern "C" fn(l: *mut LuaState) -> i32;

/// Lua allocator function signature.
pub type LuaAlloc = unsafe extern "C" fn(
    ud: *mut c_void,
    ptr: *mut c_void,
    osize: usize,
    nsize: usize,
) -> *mut c_void;

// Garbage-collector control (lua.h)
const LUA_GCCOLLECT: i32 = 2;
const LUA_GCINC: i32 = 11;

// Lua type tags (lua.h)
const LUA_TNONE: i32 = -1;
const LUA_TNIL: i32 = 0;
const LUA_TBOOLEAN: i32 = 1;
const LUA_TNUMBER: i32 = 3;
const LUA_TSTRING: i32 = 4;
const LUA_TTABLE: i32 = 5;
const LUA_TFUNCTION: i32 = 6;

/// Thread status: no errors.
const LUA_OK: i32 = 0;

extern "C" {
    // -- State management --
    pub fn lua_newstate(f: LuaAlloc, ud: *mut c_void) -> *mut LuaState;
    /// Returns the previous panic function, NULL for a fresh state.
    pub fn lua_atpanic(l: *mut LuaState, panicf: LuaCFunction) -> Option<LuaCFunction>;
    pub fn luaL_openlibs(l: *mut LuaState);

    // -- Stack operations --
    pub fn lua_gettop(l: *mut LuaState) -> i32;
    pub fn lua_settop(l: *mut LuaState, idx: i32);
    pub fn lua_pushnil(l: *mut LuaState);
    pub fn lua_pushboolean(l: *mut LuaState, b: i32);
    pub fn lua_pushinteger(l: *mut LuaState, n: LuaInteger);
    pub fn lua_pushnumber(l: *mut LuaState, n: LuaNumber);
    pub fn lua_pushlstring(l: *mut LuaState, s: *const c_char, len: usize) -> *const c_char;
    pub fn lua_pushcclosure(l: *mut LuaState, f: LuaCFunction, n: i32);
    pub fn lua_pushvalue(l: *mut LuaState, idx: i32);

    // -- Get from stack --
    pub fn lua_toboolean(l: *mut LuaState, idx: i32) -> i32;
    pub fn lua_tointegerx(l: *mut LuaState, idx: i32, isnum: *mut i32) -> LuaInteger;
    pub fn lua_tonumberx(l: *mut LuaState, idx: i32, isnum: *mut i32) -> LuaNumber;
    pub fn lua_tolstring(l: *mut LuaState, idx: i32, len: *mut usize) -> *const c_char;
    pub fn lua_type(l: *mut LuaState, idx: i32) -> i32;
    pub fn lua_isinteger(l: *mut LuaState, idx: i32) -> i32;

    // -- Table operations --
    pub fn lua_getglobal(l: *mut LuaState, name: *const c_char) -> i32;
    pub fn lua_setglobal(l: *mut LuaState, name: *const c_char);
    pub fn lua_rawseti(l: *mut LuaState, idx: i32, n: LuaInteger);
    pub fn lua_createtable(l: *mut LuaState, narr: i32, nrec: i32);
    pub fn lua_next(l: *mut LuaState, idx: i32) -> i32;

    // -- Execution --
    /// Variadic in C (`lua_gc(L, what, ...)`); the extra `i32`s are the
    /// per-`what` parameters (pause/stepmul/stepsize for `LUA_GCINC`).
    pub fn lua_gc(l: *mut LuaState, what: i32, ...) -> i32;
    pub fn luaL_loadbufferx(
        l: *mut LuaState,
        buff: *const c_char,
        sz: usize,
        name: *const c_char,
        mode: *const c_char,
    ) -> i32;
    pub fn lua_pcallk(
        l: *mut LuaState,
        nargs: i32,
        nresults: i32,
        errfunc: i32,
        ctx: LuaKContext,
        k: Option<unsafe extern "C" fn(*mut LuaState, i32, LuaKContext) -> i32>,
    ) -> i32;
}

// ---------------------------------------------------------------------------
// Macro-equivalent helper functions
// ---------------------------------------------------------------------------

/// Pop `n` values from the Lua stack.  (C macro: `lua_settop(L, -(n)-1)`.)
#[inline]
unsafe fn lua_pop(l: *mut LuaState, n: i32) {
    lua_settop(l, -(n) - 1);
}

/// Read an integer from the stack without an `isnum` out-parameter.
#[inline]
unsafe fn lua_tointeger(l: *mut LuaState, idx: i32) -> LuaInteger {
    lua_tointegerx(l, idx, core::ptr::null_mut())
}

/// Read a number from the stack without an `isnum` out-parameter.
#[inline]
unsafe fn lua_tonumber(l: *mut LuaState, idx: i32) -> LuaNumber {
    lua_tonumberx(l, idx, core::ptr::null_mut())
}

/// Check if the value at `idx` is nil.
#[inline]
unsafe fn lua_isnil(l: *mut LuaState, idx: i32) -> bool {
    lua_type(l, idx) == LUA_TNIL
}

/// Check if the stack slot at `idx` contains no value.
#[inline]
unsafe fn lua_isnone(l: *mut LuaState, idx: i32) -> bool {
    lua_type(l, idx) == LUA_TNONE
}

/// Check if the value at `idx` is nil **or** none (no value).
#[inline]
unsafe fn lua_isnoneornil(l: *mut LuaState, idx: i32) -> bool {
    lua_type(l, idx) <= 0
}

/// Protected call without continuation.
#[inline]
unsafe fn lua_pcall(l: *mut LuaState, nargs: i32, nresults: i32, errfunc: i32) -> i32 {
    lua_pcallk(l, nargs, nresults, errfunc, 0, None)
}

/// Register a C function as a global.
#[inline]
unsafe fn lua_register(l: *mut LuaState, name: *const c_char, f: LuaCFunction) {
    lua_pushcclosure(l, f, 0);
    lua_setglobal(l, name);
}

// ============================================================================
// Part 2: Lua Allocator Bridge
// ============================================================================

/// Alignment of every block handed to Lua; satisfies all Lua-internal
/// structures (the largest is 8-byte `lua_Number`/`double`-sized unions).
const LUA_ALIGN: usize = 8;

/// The `lua_Alloc` contract over any block allocator (`GlobalAlloc` is
/// exactly "alloc(layout) / dealloc(ptr, layout)" plus a default `realloc`
/// built from the two). Kept generic and tiny so it can be reviewed by eye:
/// `spark-pico` cannot run on the host, so this has no runtime test.
///
/// Lua's rules (lua.h, `lua_Alloc`):
/// * `nsize == 0`: free `ptr` (which may be NULL) and return NULL;
/// * `ptr == NULL`: allocate `nsize` bytes -- `osize` is then the *object
///   type tag*, not a size, so it must not be used as one;
/// * otherwise: reallocate `ptr` from `osize` to `nsize` bytes.
///
/// Returning NULL on failure is what makes Lua raise a catchable
/// "not enough memory" error (after an emergency GC) instead of the
/// firmware panicking inside the allocator.
///
/// # Safety
/// `ptr`/`osize` must describe a live block previously returned by this
/// function for the same allocator, per the contract above.
#[inline]
unsafe fn lua_alloc_with<A: GlobalAlloc>(
    a: &A,
    ptr: *mut c_void,
    osize: usize,
    nsize: usize,
) -> *mut c_void {
    if nsize == 0 {
        if !ptr.is_null() {
            a.dealloc(
                ptr as *mut u8,
                Layout::from_size_align_unchecked(osize, LUA_ALIGN),
            );
        }
        core::ptr::null_mut()
    } else if ptr.is_null() {
        a.alloc(Layout::from_size_align_unchecked(nsize, LUA_ALIGN)) as *mut c_void
    } else {
        a.realloc(
            ptr as *mut u8,
            Layout::from_size_align_unchecked(osize, LUA_ALIGN),
            nsize,
        ) as *mut c_void
    }
}

/// The `lua_Alloc` passed to `lua_newstate`. `ud` is the `&'static Heap`
/// chosen by [`new_lua_state`] (PSRAM on the Tufty, the SRAM global heap
/// otherwise), so Lua's whole heap lives in one allocator and `stat(0)`
/// can report it.
unsafe extern "C" fn lua_alloc_fn(
    ud: *mut c_void,
    ptr: *mut c_void,
    osize: usize,
    nsize: usize,
) -> *mut c_void {
    let heap = &*(ud as *const Heap);
    lua_alloc_with(heap, ptr, osize, nsize)
}

/// Panic handler installed by [`new_lua_state`] (what `luaL_newstate` does
/// with its stderr-printing `panic`): an error raised outside any `pcall`
/// (only possible during `luaL_openlibs`/`register_api`, e.g. out of
/// memory) shows the message on the LCD and halts instead of hitting the
/// silent `abort()` breakpoint loop.
unsafe extern "C" fn lua_panic(l: *mut LuaState) -> i32 {
    let msg = if lua_type(l, -1) == LUA_TSTRING {
        pop_error(l)
    } else {
        String::from("(error object is not a string)")
    };
    crate::fatal::halt(&format!("unprotected lua error: {msg}"))
}

// ============================================================================
// Part 3: Engine state -- single owner
// ============================================================================
//
// `Console` (~65 KB) and `Audio` (~17 KB) far exceed the stack. Both have
// `const fn` constructors, so they are plain `static`s: the initializer is
// evaluated at compile time and lands in `.data`, copied from flash by the
// cortex-m-rt reset handler -- zero runtime stack cost. (An earlier design
// wrote `Console::new()` into a `static MaybeUninit`, which built the value
// in a 65 KB stack temporary that overlapped the static on a 45 KB stack.)
// `GameState` is a few dozen bytes and its constructor is not `const`
// (it seeds the RNG), so it is heap-allocated once via [`heap_call`].
//
// This module is the *only* owner; `main.rs` and the Lua callbacks both go
// through `console()` / `audio()` / `game_state()`.
//
// Invariants that make the raw-pointer access sound:
//   * single core, and the only interrupt handler in the firmware
//     (`audio_out::PWM_IRQ_WRAP_0`) never touches these;
//   * `init_state` runs exactly once, before any accessor is used
//     (`state_ready` guards the fatal/panic path, which may run earlier);
//   * every accessor hands out a *fresh* `&'static mut`; a caller must not
//     keep one alive across a call into Lua (which re-derives its own) or
//     across another accessor call for the same object.

static mut CONSOLE_MEM: Console = Console::new();
static mut AUDIO_MEM: Audio = Audio::new();
static mut CONSOLE: *mut Console = core::ptr::null_mut();
static mut AUDIO: *mut Audio = core::ptr::null_mut();
static mut GAME_STATE: *mut GameState = core::ptr::null_mut();
static STATE_READY: AtomicBool = AtomicBool::new(false);

/// Call `f(arg)` and place its result in a fresh heap allocation, keeping
/// the unavoidable stack temporary **out of the caller's frame**.
///
/// Rust has no placement-new: a constructor that returns a large struct by
/// value hands it back through an `sret` slot the *caller* provides, and
/// LLVM does not (as of this toolchain -- verified in the disassembly)
/// forward that slot into a heap pointer, so a temporary of the struct's
/// size exists somewhere on the stack. This helper makes that somewhere a
/// short-lived frame ([`write_into`]) rather than `main`, whose frame lives
/// for the whole program. Callers must still budget stack for the largest
/// such temporary at boot (see the memory budget in `main.rs`).
///
/// Pass the constructor directly so it can inline into `write_into` (a
/// separately-compiled callee would add a second copy: its own local plus
/// the `sret` temporary).
pub fn heap_call<A, T>(f: impl FnOnce(A) -> T, arg: A) -> Box<T> {
    let mut slot: Box<MaybeUninit<T>> = Box::new_uninit();
    write_into(&mut slot, f, arg);
    // Safety: `write_into` initialised the slot.
    unsafe { slot.assume_init() }
}

/// The write step of [`heap_call`]; out of line so its frame -- and the
/// temporary in it -- is released as soon as the value has been copied.
#[inline(never)]
fn write_into<A, T>(dst: &mut MaybeUninit<T>, f: impl FnOnce(A) -> T, arg: A) {
    dst.write(f(arg));
}

/// Publish the engine objects. Call exactly once at boot, after the global
/// allocator is initialised (the game state is heap-allocated).
///
/// # Safety
/// Must be called once, before any accessor, on the main thread.
pub unsafe fn init_state() {
    CONSOLE = core::ptr::addr_of_mut!(CONSOLE_MEM);
    AUDIO = core::ptr::addr_of_mut!(AUDIO_MEM);
    GAME_STATE = Box::leak(heap_call(|()| GameState::new(), ()));
    STATE_READY.store(true, Ordering::Release);
}

/// Whether `init_state` has run (used by the fatal/panic path, which may be
/// reached before the engine exists).
pub fn state_ready() -> bool {
    STATE_READY.load(Ordering::Acquire)
}

/// The console (renderer, memory map, input).
///
/// # Safety
/// See the invariants above: `init_state` must have run, and the caller must
/// not hold another reference to the console while using this one.
#[inline]
pub unsafe fn console() -> &'static mut Console {
    &mut *CONSOLE
}

/// The audio engine.
///
/// # Safety
/// Same as [`console`].
#[inline]
pub unsafe fn audio() -> &'static mut Audio {
    &mut *AUDIO
}

/// Frame counter, PRNG and fps bookkeeping.
///
/// # Safety
/// Same as [`console`].
#[inline]
pub unsafe fn game_state() -> &'static mut GameState {
    &mut *GAME_STATE
}

// ============================================================================
// Argument / result helpers
// ============================================================================

/// Read a number as `f32`, or 0.0 if the value is not a number (matches
/// `lua_tonumber`'s coercion: numeric strings convert, everything else is 0).
#[inline]
unsafe fn arg_f32(l: *mut LuaState, idx: i32) -> f32 {
    lua_tonumber(l, idx)
}

/// Optional `f32` argument. `None` if the slot is nil/none.
#[inline]
unsafe fn opt_arg_f32(l: *mut LuaState, idx: i32) -> Option<f32> {
    if lua_isnoneornil(l, idx) {
        None
    } else {
        Some(lua_tonumber(l, idx))
    }
}

/// Read an integer argument with PICO-8 semantics: fractional values are
/// **floored** (`pset(3.7, 2)` sets pixel 3), matching the desktop binding's
/// `val_to_i32`. Plain `lua_tointeger` would return 0 for `3.7`, which put
/// every sprite drawn at a non-integer coordinate at x = 0.
#[inline]
unsafe fn arg_i32(l: *mut LuaState, idx: i32) -> i32 {
    if lua_isinteger(l, idx) != 0 {
        lua_tointeger(l, idx)
    } else {
        // `as i32` saturates and maps NaN to 0.
        libm::floorf(lua_tonumber(l, idx)) as i32
    }
}

/// Optional integer argument. `None` if the slot is nil/none.
#[inline]
unsafe fn opt_arg_i32(l: *mut LuaState, idx: i32) -> Option<i32> {
    if lua_isnoneornil(l, idx) {
        None
    } else {
        Some(arg_i32(l, idx))
    }
}

/// Read a boolean argument (Lua `toboolean` coerces: nil/false -> false, else true).
#[inline]
unsafe fn arg_bool(l: *mut LuaState, idx: i32) -> bool {
    lua_toboolean(l, idx) != 0
}

/// Read a number as `f64` without going through `f32` when it is an integer
/// (so 32-bit bit patterns survive intact for `tostr(x, true)` etc.).
#[inline]
unsafe fn num_f64(l: *mut LuaState, idx: i32) -> f64 {
    if lua_isinteger(l, idx) != 0 {
        lua_tointeger(l, idx) as f64
    } else {
        lua_tonumber(l, idx) as f64
    }
}

/// Raw bytes of the string at `idx` (converting numbers in place like
/// `lua_tolstring`). Empty for nil/none. Valid while the value stays on the
/// stack.
unsafe fn arg_bytes<'a>(l: *mut LuaState, idx: i32) -> &'a [u8] {
    if lua_isnoneornil(l, idx) {
        return &[];
    }
    let mut len: usize = 0;
    let ptr = lua_tolstring(l, idx, &mut len);
    if ptr.is_null() {
        return &[];
    }
    core::slice::from_raw_parts(ptr as *const u8, len)
}

/// String argument. Lua strings are arbitrary bytes (P8SCII uses >= 0x80),
/// so invalid UTF-8 is replaced rather than assumed away -- the old
/// `from_utf8_unchecked` here was undefined behaviour on any such input.
unsafe fn arg_str<'a>(l: *mut LuaState, idx: i32) -> Cow<'a, str> {
    let bytes = arg_bytes(l, idx);
    match core::str::from_utf8(bytes) {
        Ok(s) => Cow::Borrowed(s),
        Err(_) => Cow::Owned(String::from_utf8_lossy(bytes).into_owned()),
    }
}

/// Push a Rust string (any bytes; Lua strings are not NUL-terminated).
#[inline]
unsafe fn push_str(l: *mut LuaState, s: &str) {
    lua_pushlstring(l, s.as_ptr() as *const c_char, s.len());
}

/// Push a number, as a Lua **integer** when it is integral and fits `i32`.
///
/// PICO-8 has one number type and prints `12`, never `12.0`. Lua 5.4 has an
/// integer subtype whose `tostring` omits the `.0`, so returning integral
/// results as integers makes `print(flr(x))`, `..` concatenation and table
/// keys behave like PICO-8 (and like the desktop, where mlua does the same).
#[inline]
unsafe fn push_num(l: *mut LuaState, v: f32) {
    if v == libm::floorf(v) && v.abs() < 2_147_483_648.0 {
        lua_pushinteger(l, v as i32);
    } else {
        lua_pushnumber(l, v);
    }
}

/// Format the number at `idx` the way PICO-8 displays it (mirrors the
/// desktop's `val_to_display`: integral values without a fraction).
unsafe fn fmt_number(l: *mut LuaState, idx: i32) -> String {
    if lua_isinteger(l, idx) != 0 {
        return format!("{}", lua_tointeger(l, idx));
    }
    let v = lua_tonumber(l, idx);
    if v == libm::floorf(v) && v.abs() < 1e15 {
        format!("{}", v as i64)
    } else {
        format!("{v}")
    }
}

/// Display form of any value (desktop `val_to_display`): strings as-is,
/// numbers via [`fmt_number`], booleans, `nil`, and `""` for anything else.
unsafe fn value_display(l: *mut LuaState, idx: i32) -> String {
    match lua_type(l, idx) {
        LUA_TSTRING => arg_str(l, idx).into_owned(),
        LUA_TNUMBER => fmt_number(l, idx),
        LUA_TBOOLEAN => String::from(if arg_bool(l, idx) { "true" } else { "false" }),
        LUA_TNIL => String::from("nil"),
        _ => String::new(),
    }
}

// ============================================================================
// Part 4: PICO-8 API Functions
// ============================================================================

// ---------------------------------------------------------------------------
// Drawing
// ---------------------------------------------------------------------------

/// `cls([col])` -- clear screen
unsafe extern "C" fn api_cls(l: *mut LuaState) -> i32 {
    let col = opt_arg_i32(l, 1).map(|v| v as u8);
    console().cls(col);
    0
}

/// `pset(x, y, [col])` -- set pixel
unsafe extern "C" fn api_pset(l: *mut LuaState) -> i32 {
    let x = arg_i32(l, 1);
    let y = arg_i32(l, 2);
    let col = opt_arg_i32(l, 3).map(|v| v as u8);
    console().pset(x, y, col);
    0
}

/// `pget(x, y)` -- get pixel color
unsafe extern "C" fn api_pget(l: *mut LuaState) -> i32 {
    let x = arg_i32(l, 1);
    let y = arg_i32(l, 2);
    let c = console().pget(x, y);
    lua_pushinteger(l, c as i32);
    1
}

/// `line(x0, y0, x1, y1, [col])` -- draw line
unsafe extern "C" fn api_line(l: *mut LuaState) -> i32 {
    // PICO-8 dispatches `line` by argument count (see desktop binding):
    //   0 -> reset state; 1 -> reset + set color; 2/3 -> continue from last
    //   endpoint; 4/5 -> full segment.
    let n = lua_gettop(l);
    match n {
        0 => console().line_reset(),
        1 => {
            console().draw_color = arg_i32(l, 1) as u8;
            console().line_reset();
        }
        2 | 3 => {
            let x1 = arg_i32(l, 1);
            let y1 = arg_i32(l, 2);
            let col = opt_arg_i32(l, 3).map(|v| v as u8);
            console().line_to(x1, y1, col);
        }
        _ => {
            let x0 = arg_i32(l, 1);
            let y0 = arg_i32(l, 2);
            let x1 = arg_i32(l, 3);
            let y1 = arg_i32(l, 4);
            let col = opt_arg_i32(l, 5).map(|v| v as u8);
            console().line(x0, y0, x1, y1, col);
        }
    }
    0
}

/// `rect(x0, y0, x1, y1, [col])` -- draw rectangle outline
unsafe extern "C" fn api_rect(l: *mut LuaState) -> i32 {
    let x0 = arg_i32(l, 1);
    let y0 = arg_i32(l, 2);
    let x1 = arg_i32(l, 3);
    let y1 = arg_i32(l, 4);
    let col = opt_arg_i32(l, 5).map(|v| v as u8);
    console().rect(x0, y0, x1, y1, col);
    0
}

/// `rectfill(x0, y0, x1, y1, [col])` -- draw filled rectangle
unsafe extern "C" fn api_rectfill(l: *mut LuaState) -> i32 {
    let x0 = arg_i32(l, 1);
    let y0 = arg_i32(l, 2);
    let x1 = arg_i32(l, 3);
    let y1 = arg_i32(l, 4);
    let col = opt_arg_i32(l, 5).map(|v| v as u8);
    console().rectfill(x0, y0, x1, y1, col);
    0
}

/// `circ(cx, cy, r, [col])` -- draw circle outline
unsafe extern "C" fn api_circ(l: *mut LuaState) -> i32 {
    let cx = arg_i32(l, 1);
    let cy = arg_i32(l, 2);
    let r = arg_i32(l, 3);
    let col = opt_arg_i32(l, 4).map(|v| v as u8);
    console().circ(cx, cy, r, col);
    0
}

/// `circfill(cx, cy, r, [col])` -- draw filled circle
unsafe extern "C" fn api_circfill(l: *mut LuaState) -> i32 {
    let cx = arg_i32(l, 1);
    let cy = arg_i32(l, 2);
    let r = arg_i32(l, 3);
    let col = opt_arg_i32(l, 4).map(|v| v as u8);
    console().circfill(cx, cy, r, col);
    0
}

/// `oval(x0, y0, x1, y1, [col])` -- draw ellipse outline
unsafe extern "C" fn api_oval(l: *mut LuaState) -> i32 {
    let x0 = arg_i32(l, 1);
    let y0 = arg_i32(l, 2);
    let x1 = arg_i32(l, 3);
    let y1 = arg_i32(l, 4);
    let col = opt_arg_i32(l, 5).map(|v| v as u8);
    console().oval(x0, y0, x1, y1, col);
    0
}

/// `ovalfill(x0, y0, x1, y1, [col])` -- draw filled ellipse
unsafe extern "C" fn api_ovalfill(l: *mut LuaState) -> i32 {
    let x0 = arg_i32(l, 1);
    let y0 = arg_i32(l, 2);
    let x1 = arg_i32(l, 3);
    let y1 = arg_i32(l, 4);
    let col = opt_arg_i32(l, 5).map(|v| v as u8);
    console().ovalfill(x0, y0, x1, y1, col);
    0
}

/// `cursor(x, y, [col])` -- set the text cursor position; returns the previous one
unsafe extern "C" fn api_cursor(l: *mut LuaState) -> i32 {
    let x = opt_arg_i32(l, 1).unwrap_or(0);
    let y = opt_arg_i32(l, 2).unwrap_or(0);
    let prev = console().cursor(x, y);
    if let Some(c) = opt_arg_i32(l, 3) {
        console().color(c as u8);
    }
    lua_pushinteger(l, prev.0);
    lua_pushinteger(l, prev.1);
    2
}

/// `spr(n, x, y, [w], [h], [flip_x], [flip_y])` -- draw sprite
unsafe extern "C" fn api_spr(l: *mut LuaState) -> i32 {
    let n = arg_i32(l, 1);
    let x = arg_i32(l, 2);
    let y = arg_i32(l, 3);
    let w = opt_arg_f32(l, 4).unwrap_or(1.0) as f64;
    let h = opt_arg_f32(l, 5).unwrap_or(1.0) as f64;
    let flip_x = arg_bool(l, 6);
    let flip_y = arg_bool(l, 7);
    console().spr(n, x, y, w, h, flip_x, flip_y);
    0
}

/// `sspr(sx, sy, sw, sh, dx, dy, [dw], [dh], [flip_x], [flip_y])` -- stretch blit from sprite sheet
unsafe extern "C" fn api_sspr(l: *mut LuaState) -> i32 {
    let sx = arg_i32(l, 1);
    let sy = arg_i32(l, 2);
    let sw = arg_i32(l, 3);
    let sh = arg_i32(l, 4);
    let dx = arg_i32(l, 5);
    let dy = arg_i32(l, 6);
    let dw = opt_arg_i32(l, 7).unwrap_or(sw);
    let dh = opt_arg_i32(l, 8).unwrap_or(sh);
    let flip_x = arg_bool(l, 9);
    let flip_y = arg_bool(l, 10);
    console().sspr(sx, sy, sw, sh, dx, dy, dw, dh, flip_x, flip_y);
    0
}

/// `print(val, [x], [y], [col])` -- print text; returns x advance
unsafe extern "C" fn api_print(l: *mut LuaState) -> i32 {
    // Numbers go through the PICO-8 display formatter so `print(10/2)` shows
    // `5`, not Lua 5.4's `5.0`.
    let text = if lua_isnone(l, 1) {
        String::new()
    } else {
        value_display(l, 1)
    };
    // PICO-8 dispatches `print` by argument count (see desktop binding):
    //   1 -> at cursor; 2 -> at cursor + set color; 3 -> at (x,y);
    //   4 -> at (x,y) + set color.
    let n = lua_gettop(l);
    let advance = match n {
        0 | 1 => console().print(&text, None, None, None),
        2 => {
            let col = opt_arg_i32(l, 2).map(|v| v as u8);
            console().print(&text, None, None, col)
        }
        3 => {
            let x = opt_arg_i32(l, 2);
            let y = opt_arg_i32(l, 3);
            console().print(&text, x, y, None)
        }
        _ => {
            let x = opt_arg_i32(l, 2);
            let y = opt_arg_i32(l, 3);
            let col = opt_arg_i32(l, 4).map(|v| v as u8);
            console().print(&text, x, y, col)
        }
    };
    lua_pushinteger(l, advance);
    1
}

/// `camera([x], [y])` -- set camera offset; returns the previous offset
unsafe extern "C" fn api_camera(l: *mut LuaState) -> i32 {
    let x = opt_arg_i32(l, 1).unwrap_or(0);
    let y = opt_arg_i32(l, 2).unwrap_or(0);
    let prev = console().camera(x, y);
    lua_pushinteger(l, prev.0);
    lua_pushinteger(l, prev.1);
    2
}

/// `clip([x, y, w, h, clip_previous])` -- set clipping rect (or reset if no
/// args) and return the PREVIOUS rect as (x, y, w, h), matching PICO-8.
unsafe extern "C" fn api_clip(l: *mut LuaState) -> i32 {
    let prev = if lua_gettop(l) == 0 || lua_isnoneornil(l, 1) {
        console().clip_rect(0, 0, 128, 128, false)
    } else {
        let x = arg_i32(l, 1);
        let y = arg_i32(l, 2);
        let w = arg_i32(l, 3);
        let h = arg_i32(l, 4);
        // Optional 5th arg (PICO-8 0.2.x): intersect with the current clip.
        let clip_previous = lua_gettop(l) >= 5 && lua_toboolean(l, 5) != 0;
        console().clip_rect(x, y, w, h, clip_previous)
    };
    lua_pushinteger(l, prev.0);
    lua_pushinteger(l, prev.1);
    lua_pushinteger(l, prev.2);
    lua_pushinteger(l, prev.3);
    4
}

/// `pal([c0, c1, p])` -- remap palette, or reset if no args
unsafe extern "C" fn api_pal(l: *mut LuaState) -> i32 {
    // PICO-8 `pal` forms (see desktop binding):
    //   pal()            -> reset both palettes + transparency
    //   pal(tbl[, p])    -> bulk remap from {[c0]=c1, ...}
    //   pal(p)           -> reset palette p
    //   pal(c0, c1[, p]) -> map c0->c1, returns the previous mapping
    let n = lua_gettop(l);
    if n == 0 || lua_isnoneornil(l, 1) {
        console().pal_reset();
        0
    } else if lua_type(l, 1) == LUA_TTABLE {
        let p = opt_arg_i32(l, 2).unwrap_or(0) as u8;
        // Iterate the table: push nil key, then lua_next pops the key and
        // pushes key/value until it returns 0.
        lua_pushnil(l);
        while lua_next(l, 1) != 0 {
            // key at -2, value at -1
            let c0 = arg_i32(l, -2) as u8;
            let c1 = arg_i32(l, -1) as u8;
            console().pal(c0, c1, p);
            lua_pop(l, 1); // pop value, keep key for next iteration
        }
        0
    } else if n == 1 {
        let p = arg_i32(l, 1) as u8;
        console().pal_reset_one(p);
        0
    } else {
        let c0 = arg_i32(l, 1) as u8;
        let c1 = arg_i32(l, 2) as u8;
        let p = opt_arg_i32(l, 3).unwrap_or(0) as u8;
        let prev = console().pal(c0, c1, p);
        lua_pushinteger(l, prev as i32);
        1
    }
}

/// `palt([c, t])` -- set transparency for color, or reset if no args
unsafe extern "C" fn api_palt(l: *mut LuaState) -> i32 {
    if lua_gettop(l) == 0 || lua_isnoneornil(l, 1) {
        console().palt_reset();
    } else {
        let c = arg_i32(l, 1) as u8;
        let t = arg_bool(l, 2);
        console().palt(c, t);
    }
    0
}

/// `color([c])` -- set default draw color; returns the previous pen color
unsafe extern "C" fn api_color(l: *mut LuaState) -> i32 {
    let c = opt_arg_i32(l, 1).unwrap_or(6) as u8;
    let prev = console().color(c);
    lua_pushinteger(l, prev as i32);
    1
}

/// `fillp([p])` -- set fill pattern; returns the previous pattern
unsafe extern "C" fn api_fillp(l: *mut LuaState) -> i32 {
    let p = opt_arg_f32(l, 1).unwrap_or(0.0) as f64;
    let prev = console().fillp(p);
    push_num(l, prev as f32);
    1
}

/// `tline(x0, y0, x1, y1, mx, my, [mdx], [mdy])` -- draw textured line
unsafe extern "C" fn api_tline(l: *mut LuaState) -> i32 {
    let x0 = arg_i32(l, 1);
    let y0 = arg_i32(l, 2);
    let x1 = arg_i32(l, 3);
    let y1 = arg_i32(l, 4);
    let mx = opt_arg_f32(l, 5).unwrap_or(0.0) as f64;
    let my = opt_arg_f32(l, 6).unwrap_or(0.0) as f64;
    let mdx = opt_arg_f32(l, 7).unwrap_or(0.125) as f64;
    let mdy = opt_arg_f32(l, 8).unwrap_or(0.0) as f64;
    console().tline(x0, y0, x1, y1, mx, my, mdx, mdy);
    0
}

// ---------------------------------------------------------------------------
// Sprites / Map
// ---------------------------------------------------------------------------

/// `sget(x, y)` -- get sprite sheet pixel
unsafe extern "C" fn api_sget(l: *mut LuaState) -> i32 {
    let x = arg_i32(l, 1);
    let y = arg_i32(l, 2);
    let c = console().sget(x, y);
    lua_pushinteger(l, c as i32);
    1
}

/// `sset(x, y, [col])` -- set sprite sheet pixel
unsafe extern "C" fn api_sset(l: *mut LuaState) -> i32 {
    let x = arg_i32(l, 1);
    let y = arg_i32(l, 2);
    let col = opt_arg_i32(l, 3).unwrap_or(0) as u8;
    console().sset(x, y, col);
    0
}

/// `map(cel_x, cel_y, sx, sy, cel_w, cel_h, [layer])` -- draw map region
unsafe extern "C" fn api_map(l: *mut LuaState) -> i32 {
    let cel_x = opt_arg_i32(l, 1).unwrap_or(0);
    let cel_y = opt_arg_i32(l, 2).unwrap_or(0);
    let sx = opt_arg_i32(l, 3).unwrap_or(0);
    let sy = opt_arg_i32(l, 4).unwrap_or(0);
    let cel_w = opt_arg_i32(l, 5).unwrap_or(128);
    let cel_h = opt_arg_i32(l, 6).unwrap_or(64);
    let layer = opt_arg_i32(l, 7).unwrap_or(0) as u8;
    console().map_draw(cel_x, cel_y, sx, sy, cel_w, cel_h, layer);
    0
}

/// `mget(x, y)` -- get map tile
unsafe extern "C" fn api_mget(l: *mut LuaState) -> i32 {
    let x = arg_i32(l, 1);
    let y = arg_i32(l, 2);
    let tile = console().mget(x, y);
    lua_pushinteger(l, tile as i32);
    1
}

/// `mset(x, y, v)` -- set map tile
unsafe extern "C" fn api_mset(l: *mut LuaState) -> i32 {
    let x = arg_i32(l, 1);
    let y = arg_i32(l, 2);
    let v = arg_i32(l, 3) as u8;
    console().mset(x, y, v);
    0
}

/// `fget(n, [f])` -- get sprite flags (whole byte, or single bit as boolean)
unsafe extern "C" fn api_fget(l: *mut LuaState) -> i32 {
    let n = arg_i32(l, 1) as usize;
    let f = opt_arg_i32(l, 2).map(|v| v as u8);
    let result = console().fget(n, f);
    if f.is_some() {
        lua_pushboolean(l, result as i32);
    } else {
        lua_pushinteger(l, result as i32);
    }
    1
}

/// `fset(n, [f], v)` -- set sprite flags
///
/// Two-argument form: `fset(n, v)` sets the whole flag byte.
/// Three-argument form: `fset(n, f, v)` sets bit `f` to `v`.
unsafe extern "C" fn api_fset(l: *mut LuaState) -> i32 {
    let n = arg_i32(l, 1) as usize;
    if lua_gettop(l) >= 3 {
        let f = arg_i32(l, 2) as u8;
        let v = if arg_bool(l, 3) { 1u8 } else { 0u8 };
        console().fset(n, Some(f), v);
    } else {
        let v = arg_i32(l, 2) as u8;
        console().fset(n, None, v);
    }
    0
}

// ---------------------------------------------------------------------------
// Input
// ---------------------------------------------------------------------------

/// `btn([i], [p])` -- is button pressed? p defaults to 0 (player 1).
/// With no args, returns bitmask of all buttons for player 0.
unsafe extern "C" fn api_btn(l: *mut LuaState) -> i32 {
    if lua_gettop(l) == 0 || lua_isnoneornil(l, 1) {
        let con = console();
        let mut mask: i32 = 0;
        for bit in 0..6u8 {
            if con.btn(bit, 0) {
                mask |= 1 << bit;
            }
        }
        lua_pushinteger(l, mask);
        return 1;
    }
    let i = arg_i32(l, 1) as u8;
    let p = opt_arg_i32(l, 2).unwrap_or(0) as u8;
    lua_pushboolean(l, console().btn(i, p) as i32);
    1
}

/// `btnp(i, [p])` -- was button just pressed this frame? p defaults to 0 (player 1)
unsafe extern "C" fn api_btnp(l: *mut LuaState) -> i32 {
    let i = arg_i32(l, 1) as u8;
    let p = opt_arg_i32(l, 2).unwrap_or(0) as u8;
    lua_pushboolean(l, console().btnp(i, p) as i32);
    1
}

// ---------------------------------------------------------------------------
// Math (PICO-8 specific)
// ---------------------------------------------------------------------------

/// `sin(x)` -- PICO-8 sin: `-sin(x * 2*pi)` (inverted, angle in turns)
unsafe extern "C" fn api_sin(l: *mut LuaState) -> i32 {
    push_num(l, api_ops::sin(arg_f32(l, 1) as f64) as f32);
    1
}

/// `cos(x)` -- PICO-8 cos: `cos(x * 2*pi)` (angle in turns)
unsafe extern "C" fn api_cos(l: *mut LuaState) -> i32 {
    push_num(l, api_ops::cos(arg_f32(l, 1) as f64) as f32);
    1
}

/// `atan2(dx, dy)` -- PICO-8 atan2: angle as a turn in 0..1 (inverted y)
unsafe extern "C" fn api_atan2(l: *mut LuaState) -> i32 {
    push_num(
        l,
        api_ops::atan2(arg_f32(l, 1) as f64, arg_f32(l, 2) as f64) as f32,
    );
    1
}

/// `sqrt(x)` -- square root (0 for negative input)
unsafe extern "C" fn api_sqrt(l: *mut LuaState) -> i32 {
    let x = arg_f32(l, 1);
    push_num(l, libm::sqrtf(if x < 0.0 { 0.0 } else { x }));
    1
}

/// `abs(x)` -- absolute value
unsafe extern "C" fn api_abs(l: *mut LuaState) -> i32 {
    push_num(l, libm::fabsf(arg_f32(l, 1)));
    1
}

/// `flr(x)` -- floor
unsafe extern "C" fn api_flr(l: *mut LuaState) -> i32 {
    push_num(l, libm::floorf(arg_f32(l, 1)));
    1
}

/// `ceil(x)` -- ceiling
unsafe extern "C" fn api_ceil(l: *mut LuaState) -> i32 {
    push_num(l, libm::ceilf(arg_f32(l, 1)));
    1
}

/// `min(a, b)` -- minimum
unsafe extern "C" fn api_min(l: *mut LuaState) -> i32 {
    let a = arg_f32(l, 1);
    let b = arg_f32(l, 2);
    push_num(l, if a < b { a } else { b });
    1
}

/// `max(a, b)` -- maximum
unsafe extern "C" fn api_max(l: *mut LuaState) -> i32 {
    let a = arg_f32(l, 1);
    let b = arg_f32(l, 2);
    push_num(l, if a > b { a } else { b });
    1
}

/// `mid(a, b, c)` -- middle value of three
unsafe extern "C" fn api_mid(l: *mut LuaState) -> i32 {
    let a = arg_f32(l, 1);
    let b = arg_f32(l, 2);
    let c = arg_f32(l, 3);
    let result = if a > b {
        if b > c {
            b
        } else if a > c {
            c
        } else {
            a
        }
    } else if a > c {
        a
    } else if b > c {
        c
    } else {
        b
    };
    push_num(l, result);
    1
}

/// `sgn(x)` -- sign: returns 1 or -1 (PICO-8 never returns 0)
unsafe extern "C" fn api_sgn(l: *mut LuaState) -> i32 {
    push_num(l, api_ops::sgn(arg_f32(l, 1) as f64) as f32);
    1
}

/// `rnd([max])` -- random number in 0..<max (default max=1).
/// Uses PICO-8's exact integer PRNG so seeded sequences are reproducible.
unsafe extern "C" fn api_rnd(l: *mut LuaState) -> i32 {
    let max = opt_arg_f32(l, 1).unwrap_or(1.0);
    push_num(l, game_state().rnd(max as f64) as f32);
    1
}

/// `srand(seed)` -- seed the PRNG (matches PICO-8's srand).
unsafe extern "C" fn api_srand(l: *mut LuaState) -> i32 {
    let seed = opt_arg_f32(l, 1).unwrap_or(0.0);
    game_state().srand(seed as f64);
    0
}

// -- Bitwise operations --
// The exact PICO-8 semantics (operating on the full 16.16 fixed-point bit
// pattern, shift-count saturation, etc.) live in `spark_core::api_ops`, shared
// with the desktop binding layer so the two targets cannot diverge.

/// `band(a, b)` -- bitwise AND
unsafe extern "C" fn api_band(l: *mut LuaState) -> i32 {
    push_num(l, api_ops::band(num_f64(l, 1), num_f64(l, 2)) as f32);
    1
}

/// `bor(a, b)` -- bitwise OR
unsafe extern "C" fn api_bor(l: *mut LuaState) -> i32 {
    push_num(l, api_ops::bor(num_f64(l, 1), num_f64(l, 2)) as f32);
    1
}

/// `bxor(a, b)` -- bitwise XOR
unsafe extern "C" fn api_bxor(l: *mut LuaState) -> i32 {
    push_num(l, api_ops::bxor(num_f64(l, 1), num_f64(l, 2)) as f32);
    1
}

/// `bnot(a)` -- bitwise NOT
unsafe extern "C" fn api_bnot(l: *mut LuaState) -> i32 {
    push_num(l, api_ops::bnot(num_f64(l, 1)) as f32);
    1
}

/// Shift count argument: non-negative, floored.
#[inline]
unsafe fn shift_count(l: *mut LuaState, idx: i32) -> u32 {
    arg_i32(l, idx).max(0) as u32
}

/// `shl(a, b)` -- shift left
unsafe extern "C" fn api_shl(l: *mut LuaState) -> i32 {
    push_num(l, api_ops::shl(num_f64(l, 1), shift_count(l, 2)) as f32);
    1
}

/// `shr(a, b)` -- shift right (arithmetic)
unsafe extern "C" fn api_shr(l: *mut LuaState) -> i32 {
    push_num(l, api_ops::shr(num_f64(l, 1), shift_count(l, 2)) as f32);
    1
}

/// `lshr(a, b)` -- logical (unsigned) shift right
unsafe extern "C" fn api_lshr(l: *mut LuaState) -> i32 {
    push_num(l, api_ops::lshr(num_f64(l, 1), shift_count(l, 2)) as f32);
    1
}

/// `rotl(a, b)` -- rotate bits left
unsafe extern "C" fn api_rotl(l: *mut LuaState) -> i32 {
    push_num(l, api_ops::rotl(num_f64(l, 1), shift_count(l, 2)) as f32);
    1
}

/// `rotr(a, b)` -- rotate bits right
unsafe extern "C" fn api_rotr(l: *mut LuaState) -> i32 {
    push_num(l, api_ops::rotr(num_f64(l, 1), shift_count(l, 2)) as f32);
    1
}

// ---------------------------------------------------------------------------
// Sound
// ---------------------------------------------------------------------------

/// `sfx(n, [channel], [offset], [length])` -- trigger sound effect
unsafe extern "C" fn api_sfx(l: *mut LuaState) -> i32 {
    let n = arg_i32(l, 1);
    let channel = opt_arg_i32(l, 2).unwrap_or(-1);
    let offset = opt_arg_i32(l, 3).unwrap_or(0);
    let length = opt_arg_i32(l, 4).unwrap_or(0);
    audio().sfx(n, channel, offset, length);
    0
}

/// `music(n, [fade_len], [channel_mask])` -- start/stop music
unsafe extern "C" fn api_music(l: *mut LuaState) -> i32 {
    let n = arg_i32(l, 1);
    let fade_len = opt_arg_i32(l, 2).unwrap_or(0);
    let channel_mask = opt_arg_i32(l, 3).unwrap_or(0);
    audio().music(n, fade_len, channel_mask);
    0
}

// ---------------------------------------------------------------------------
// Strings / conversion (shared semantics via spark_core::api_ops)
// ---------------------------------------------------------------------------

/// `tostr(val, [flags])` -- PICO-8 string conversion.
///
/// `flags` may be `true` (== 1) or a numeric mask: 0x1 -> `"0xWWWW.FFFF"`,
/// 0x2 -> raw 32-bit bit pattern as decimal, 0x3 -> `"0xWWWWFFFF"`. `nil`
/// gives `""`, non-scalars `"[table]"` (mirrors the desktop binding).
unsafe extern "C" fn api_tostr(l: *mut LuaState) -> i32 {
    let flags: u32 = match lua_type(l, 2) {
        LUA_TNONE | LUA_TNIL => 0,
        LUA_TBOOLEAN => arg_bool(l, 2) as u32,
        _ => arg_i32(l, 2) as u32,
    };
    match lua_type(l, 1) {
        LUA_TNONE | LUA_TNIL => push_str(l, ""),
        LUA_TBOOLEAN => push_str(l, if arg_bool(l, 1) { "true" } else { "false" }),
        LUA_TNUMBER => {
            let s = if flags != 0 {
                api_ops::format_tostr_hex(num_f64(l, 1), flags)
            } else {
                fmt_number(l, 1)
            };
            push_str(l, &s);
        }
        LUA_TSTRING => lua_pushvalue(l, 1),
        _ => push_str(l, "[table]"),
    }
    1
}

/// `tonum(val, [flags])` -- PICO-8 number parsing.
///
/// Numbers pass through, booleans become 1/0, strings are parsed with
/// `api_ops::parse_pico8_number_with_flags` (decimal, `0x` hex with fraction,
/// `0b` binary; flags 0x1 = raw hex digits, 0x2 = `>> 16`). Anything else, or
/// an unparseable string, yields `nil`.
unsafe extern "C" fn api_tonum(l: *mut LuaState) -> i32 {
    let flags = opt_arg_i32(l, 2).unwrap_or(0) as u32;
    match lua_type(l, 1) {
        LUA_TNUMBER => lua_pushvalue(l, 1),
        LUA_TBOOLEAN => push_num(l, if arg_bool(l, 1) { 1.0 } else { 0.0 }),
        LUA_TSTRING => {
            let s = arg_str(l, 1);
            match api_ops::parse_pico8_number_with_flags(&s, flags) {
                Some(v) => push_num(l, v as f32),
                None => lua_pushnil(l),
            }
        }
        _ => lua_pushnil(l),
    }
    1
}

/// Append `piece` to the table at the top of the stack, as a number when
/// `convert` is set and it parses as one (mirrors the desktop `split`).
unsafe fn split_push(l: *mut LuaState, idx: &mut LuaInteger, piece: &[u8], convert: bool) {
    *idx += 1;
    let piece_str = String::from_utf8_lossy(piece);
    if convert {
        if let Some(n) = api_ops::parse_pico8_number(&piece_str) {
            push_num(l, n as f32);
            lua_rawseti(l, -2, *idx);
            return;
        }
    }
    push_str(l, &piece_str);
    lua_rawseti(l, -2, *idx);
}

/// `split(str, [sep], [convert])` -- split a string.
///
///   - `sep` is a STRING -> split on its first byte (default `","`)
///   - `sep` is a NUMBER -> split into fixed-size character chunks
///   - `convert` (default true) -> numeric pieces become numbers
///
/// Pieces are NOT trimmed (PICO-8 keeps surrounding spaces).
unsafe extern "C" fn api_split(l: *mut LuaState) -> i32 {
    if lua_isnoneornil(l, 1) {
        lua_pushnil(l);
        return 1;
    }
    let s: String = if lua_type(l, 1) == LUA_TSTRING {
        arg_str(l, 1).into_owned()
    } else {
        value_display(l, 1)
    };
    let chunk: Option<usize> = if lua_type(l, 2) == LUA_TNUMBER {
        let n = arg_i32(l, 2);
        Some(if n <= 0 { 1 } else { n as usize })
    } else {
        None
    };
    let needle: u8 = if lua_type(l, 2) == LUA_TSTRING {
        arg_bytes(l, 2).first().copied().unwrap_or(b',')
    } else {
        b','
    };
    // Missing -> true; an explicit nil/false -> false (desktop `val_to_bool`).
    let convert = lua_isnone(l, 3) || arg_bool(l, 3);

    let bytes = s.as_bytes();
    lua_createtable(l, 0, 0);
    let mut idx: LuaInteger = 0;
    match chunk {
        Some(sz) => {
            if bytes.is_empty() {
                split_push(l, &mut idx, b"", convert);
            } else {
                let mut start = 0;
                while start < bytes.len() {
                    let end = (start + sz).min(bytes.len());
                    split_push(l, &mut idx, &bytes[start..end], convert);
                    start = end;
                }
            }
        }
        None => {
            let mut start = 0;
            loop {
                match bytes[start..].iter().position(|&b| b == needle) {
                    Some(rel) => {
                        let pos = start + rel;
                        split_push(l, &mut idx, &bytes[start..pos], convert);
                        start = pos + 1;
                    }
                    None => {
                        split_push(l, &mut idx, &bytes[start..], convert);
                        break;
                    }
                }
            }
        }
    }
    1
}

// ---------------------------------------------------------------------------
// Utility
// ---------------------------------------------------------------------------

/// `time()` / `t()` -- seconds since the cart started (frames / target fps)
unsafe extern "C" fn api_time(l: *mut LuaState) -> i32 {
    let gs = game_state();
    push_num(l, gs.frame_count as f32 / gs.target_fps.max(1) as f32);
    1
}

/// `peek(addr)` -- read a byte from the PICO-8 memory map
unsafe extern "C" fn api_peek(l: *mut LuaState) -> i32 {
    let addr = arg_i32(l, 1) as u16;
    lua_pushinteger(l, console().peek(addr) as i32);
    1
}

/// `poke(addr, val)` -- write a byte to the PICO-8 memory map
unsafe extern "C" fn api_poke(l: *mut LuaState) -> i32 {
    let addr = arg_i32(l, 1) as u16;
    let val = arg_i32(l, 2) as u8;
    console().poke(addr, val);
    0
}

/// `memcpy(dest, src, len)` -- copy bytes within the memory map
unsafe extern "C" fn api_memcpy(l: *mut LuaState) -> i32 {
    let dest = arg_i32(l, 1) as u16;
    let src = arg_i32(l, 2) as u16;
    let len = arg_i32(l, 3) as u16;
    console().memcpy(dest, src, len);
    0
}

/// `memset(dest, val, len)` -- fill bytes in the memory map
unsafe extern "C" fn api_memset(l: *mut LuaState) -> i32 {
    let dest = arg_i32(l, 1) as u16;
    let val = arg_i32(l, 2) as u8;
    let len = arg_i32(l, 3) as u16;
    console().memset(dest, val, len);
    0
}

/// `peek2(addr)` -- read 16-bit value from PICO-8 memory map
unsafe extern "C" fn api_peek2(l: *mut LuaState) -> i32 {
    let addr = arg_i32(l, 1) as u16;
    lua_pushinteger(l, console().peek2(addr) as i32);
    1
}

/// `peek4(addr)` -- read 32-bit value from PICO-8 memory map
unsafe extern "C" fn api_peek4(l: *mut LuaState) -> i32 {
    let addr = arg_i32(l, 1) as u16;
    // The 4 bytes are the number's 16.16 fixed-point bit pattern.
    push_num(l, from_bits(console().peek4(addr)) as f32);
    1
}

/// `poke2(addr, val)` -- write 16-bit value to PICO-8 memory map
unsafe extern "C" fn api_poke2(l: *mut LuaState) -> i32 {
    let addr = arg_i32(l, 1) as u16;
    let val = arg_i32(l, 2) as u16;
    console().poke2(addr, val);
    0
}

/// `poke4(addr, val)` -- write 32-bit value to PICO-8 memory map
unsafe extern "C" fn api_poke4(l: *mut LuaState) -> i32 {
    let addr = arg_i32(l, 1) as u16;
    // Store the number's 16.16 fixed-point bit pattern little-endian.
    console().poke4(addr, to_bits(num_f64(l, 2)));
    0
}

/// `cstore(dest, src, len)` -- copy RAM to cart ROM snapshot
unsafe extern "C" fn api_cstore(l: *mut LuaState) -> i32 {
    let dest = arg_i32(l, 1) as u16;
    let src = arg_i32(l, 2) as u16;
    let len = arg_i32(l, 3) as u16;
    console().cstore(dest, src, len);
    0
}

/// `reload(dest, src, len)` -- copy cart ROM snapshot back to RAM
unsafe extern "C" fn api_reload(l: *mut LuaState) -> i32 {
    let dest = arg_i32(l, 1) as u16;
    let src = arg_i32(l, 2) as u16;
    let len = arg_i32(l, 3) as u16;
    console().reload(dest, src, len);
    0
}

/// `stat(n)` -- system status query
unsafe extern "C" fn api_stat(l: *mut LuaState) -> i32 {
    let n = arg_i32(l, 1);
    match n {
        // stat(0): Lua heap memory used, in kilobytes (PICO-8 reports KB).
        // Backed by the allocator Lua actually lives in (PSRAM on the Tufty,
        // the SRAM heap on the Pico 2) so the on-device budget can be
        // verified empirically.
        0 => lua_pushnumber(l, crate::lua_heap_used() as f32 / 1024.0),
        // stat(1): CPU usage. Not measured on embedded; report 0 rather than
        // a fake load.
        1 => lua_pushinteger(l, 0),
        // stat(4): clipboard; stat(6): parameter string; stat(46): load()
        // param -- all empty on a single baked-in cart.
        4 | 6 | 46 => push_str(l, ""),
        // stat(7): target FPS (30, or 60 when the cart defines _update60).
        7 => lua_pushinteger(l, game_state().target_fps as i32),
        // stat(8): measured FPS from the main loop's frame timer.
        8 => push_num(l, game_state().actual_fps),
        // stat(16-19): SFX currently playing on channel 0-3
        16..=19 => lua_pushinteger(l, audio().sfx_on_channel((n - 16) as usize) as i32),
        // stat(20-23): current note number on channel 0-3
        20..=23 => lua_pushinteger(l, audio().note_on_channel((n - 20) as usize) as i32),
        // stat(24)/stat(54): current music pattern index
        24 | 54 => lua_pushinteger(l, audio().current_music_pattern() as i32),
        // stat(28): keyboard scancode held -- no keyboard, always false.
        28 => lua_pushboolean(l, 0),
        // stat(29): unknown PICO-8 field, observed to be 0.
        29 => lua_pushinteger(l, 0),
        // stat(30): key pressed this frame
        30 => lua_pushboolean(l, console().key_pressed as i32),
        // stat(31): last key char (empty when none)
        31 => match console().last_key_char {
            Some(ch) => {
                let mut buf = [0u8; 4];
                let s = ch.encode_utf8(&mut buf);
                push_str(l, s);
            }
            None => push_str(l, ""),
        },
        // stat(32-34): mouse x / y / buttons
        32 => lua_pushinteger(l, console().mouse_x),
        33 => lua_pushinteger(l, console().mouse_y),
        34 => lua_pushinteger(l, console().mouse_btn as i32),
        // stat(47-49): SFX on channels 1-3 (alias for 17-19)
        47..=49 => lua_pushinteger(l, audio().sfx_on_channel((n - 46) as usize) as i32),
        // stat(50-53): current note number (alias for 20-23)
        50..=53 => lua_pushinteger(l, audio().note_on_channel((n - 50) as usize) as i32),
        _ => lua_pushinteger(l, 0),
    }
    1
}

/// `printh(str)` -- host debug output. No-op on embedded (no UART wired).
unsafe extern "C" fn api_printh(_l: *mut LuaState) -> i32 {
    0
}

/// `dget(index)` -- get persistent data value
unsafe extern "C" fn api_dget(l: *mut LuaState) -> i32 {
    let index = arg_i32(l, 1);
    push_num(l, console().dget(index) as f32);
    1
}

/// `dset(index, value)` -- set persistent data value
unsafe extern "C" fn api_dset(l: *mut LuaState) -> i32 {
    let index = arg_i32(l, 1);
    let value = num_f64(l, 2);
    console().dset(index, value);
    0
}

/// `menuitem(index, label, callback)` -- no pause menu on embedded; no-op
unsafe extern "C" fn api_menuitem(_l: *mut LuaState) -> i32 {
    0
}

/// `extcmd(cmd)` -- extended command. `"reset"` triggers a full system reset.
unsafe extern "C" fn api_extcmd(l: *mut LuaState) -> i32 {
    if &*arg_str(l, 1) == "reset" {
        cortex_m::peripheral::SCB::sys_reset();
    }
    0
}

/// `cartdata(id)` -- persistent data lives in RAM for the session; no-op
unsafe extern "C" fn api_cartdata(_l: *mut LuaState) -> i32 {
    0
}

// ---------------------------------------------------------------------------
// System functions (load, flip, run, stop, resume)
// ---------------------------------------------------------------------------

/// `load(filename, [breadcrumb], [param])` -- no-op: one baked-in cart.
unsafe extern "C" fn api_load(_l: *mut LuaState) -> i32 {
    0
}

/// `flip()` -- no-op: the screen is presented at the end of every frame.
unsafe extern "C" fn api_flip(_l: *mut LuaState) -> i32 {
    0
}

/// `run()` -- restart the cart via a full MCU reset.
unsafe extern "C" fn api_run(_l: *mut LuaState) -> i32 {
    cortex_m::peripheral::SCB::sys_reset();
}

/// `stop()` -- halt. No editor to return to on bare metal; park in `wfi`.
unsafe extern "C" fn api_stop(_l: *mut LuaState) -> i32 {
    loop {
        cortex_m::asm::wfi();
    }
}

/// `resume()` -- nothing to resume from on embedded; no-op
unsafe extern "C" fn api_resume(_l: *mut LuaState) -> i32 {
    0
}

// ============================================================================
// Part 5: Registration
// ============================================================================

/// PICO-8 helpers written in Lua (table helpers and the deletion-safe `all`
/// iterator). See the file header for why these are Lua.
const PRELUDE: &str = include_str!("prelude.lua");

/// The definition header of every global `prelude.lua` must provide.
///
/// After the prelude runs, `register_api` checks that each header occurs in
/// the source and that the global is a function, so a typo in the Lua file
/// fails at boot with a readable message instead of at the first call.
/// The entries are spelled as full Lua definition headers (`function` +
/// name + open paren) because `spark-desktop/tests/api_parity.rs` collects
/// Lua-glue names from this file by scanning for exactly that pattern.
const PRELUDE_DEFS: &[&str] = &[
    "function del(",
    "function add(",
    "function deli(",
    "function count(",
    "function all(",
    "function foreach(",
    "function sub(",
    "function chr(",
    "function ord(",
];

/// Register all PICO-8 API functions as Lua globals and run the prelude.
///
/// # Safety
/// Must be called with a valid Lua state.
pub unsafe fn register_api(l: *mut LuaState) -> Result<(), String> {
    // -- Drawing --
    lua_register(l, c"cls".as_ptr(), api_cls);
    lua_register(l, c"pset".as_ptr(), api_pset);
    lua_register(l, c"pget".as_ptr(), api_pget);
    lua_register(l, c"line".as_ptr(), api_line);
    lua_register(l, c"rect".as_ptr(), api_rect);
    lua_register(l, c"rectfill".as_ptr(), api_rectfill);
    lua_register(l, c"circ".as_ptr(), api_circ);
    lua_register(l, c"circfill".as_ptr(), api_circfill);
    lua_register(l, c"oval".as_ptr(), api_oval);
    lua_register(l, c"ovalfill".as_ptr(), api_ovalfill);
    lua_register(l, c"cursor".as_ptr(), api_cursor);
    lua_register(l, c"spr".as_ptr(), api_spr);
    lua_register(l, c"sspr".as_ptr(), api_sspr);
    lua_register(l, c"print".as_ptr(), api_print);
    lua_register(l, c"camera".as_ptr(), api_camera);
    lua_register(l, c"clip".as_ptr(), api_clip);
    lua_register(l, c"pal".as_ptr(), api_pal);
    lua_register(l, c"palt".as_ptr(), api_palt);
    lua_register(l, c"color".as_ptr(), api_color);
    lua_register(l, c"fillp".as_ptr(), api_fillp);
    lua_register(l, c"tline".as_ptr(), api_tline);

    // -- Sprites / Map --
    lua_register(l, c"sget".as_ptr(), api_sget);
    lua_register(l, c"sset".as_ptr(), api_sset);
    lua_register(l, c"map".as_ptr(), api_map);
    lua_register(l, c"mget".as_ptr(), api_mget);
    lua_register(l, c"mset".as_ptr(), api_mset);
    lua_register(l, c"fget".as_ptr(), api_fget);
    lua_register(l, c"fset".as_ptr(), api_fset);

    // -- Input --
    lua_register(l, c"btn".as_ptr(), api_btn);
    lua_register(l, c"btnp".as_ptr(), api_btnp);

    // -- Math --
    lua_register(l, c"sin".as_ptr(), api_sin);
    lua_register(l, c"cos".as_ptr(), api_cos);
    lua_register(l, c"atan2".as_ptr(), api_atan2);
    lua_register(l, c"sqrt".as_ptr(), api_sqrt);
    lua_register(l, c"abs".as_ptr(), api_abs);
    lua_register(l, c"flr".as_ptr(), api_flr);
    lua_register(l, c"ceil".as_ptr(), api_ceil);
    lua_register(l, c"min".as_ptr(), api_min);
    lua_register(l, c"max".as_ptr(), api_max);
    lua_register(l, c"mid".as_ptr(), api_mid);
    lua_register(l, c"sgn".as_ptr(), api_sgn);
    lua_register(l, c"rnd".as_ptr(), api_rnd);
    lua_register(l, c"srand".as_ptr(), api_srand);
    lua_register(l, c"band".as_ptr(), api_band);
    lua_register(l, c"bor".as_ptr(), api_bor);
    lua_register(l, c"bxor".as_ptr(), api_bxor);
    lua_register(l, c"bnot".as_ptr(), api_bnot);
    lua_register(l, c"shl".as_ptr(), api_shl);
    lua_register(l, c"shr".as_ptr(), api_shr);
    lua_register(l, c"lshr".as_ptr(), api_lshr);
    lua_register(l, c"rotl".as_ptr(), api_rotl);
    lua_register(l, c"rotr".as_ptr(), api_rotr);

    // -- Sound --
    lua_register(l, c"sfx".as_ptr(), api_sfx);
    lua_register(l, c"music".as_ptr(), api_music);

    // -- Strings / conversion --
    lua_register(l, c"tostr".as_ptr(), api_tostr);
    lua_register(l, c"tonum".as_ptr(), api_tonum);
    lua_register(l, c"split".as_ptr(), api_split);

    // -- Memory --
    lua_register(l, c"peek".as_ptr(), api_peek);
    lua_register(l, c"peek2".as_ptr(), api_peek2);
    lua_register(l, c"peek4".as_ptr(), api_peek4);
    lua_register(l, c"poke".as_ptr(), api_poke);
    lua_register(l, c"poke2".as_ptr(), api_poke2);
    lua_register(l, c"poke4".as_ptr(), api_poke4);
    lua_register(l, c"memcpy".as_ptr(), api_memcpy);
    lua_register(l, c"memset".as_ptr(), api_memset);
    lua_register(l, c"cstore".as_ptr(), api_cstore);
    lua_register(l, c"reload".as_ptr(), api_reload);

    // -- Utility --
    lua_register(l, c"time".as_ptr(), api_time);
    lua_register(l, c"t".as_ptr(), api_time); // alias
    lua_register(l, c"stat".as_ptr(), api_stat);
    lua_register(l, c"printh".as_ptr(), api_printh);
    lua_register(l, c"dget".as_ptr(), api_dget);
    lua_register(l, c"dset".as_ptr(), api_dset);
    lua_register(l, c"menuitem".as_ptr(), api_menuitem);
    lua_register(l, c"extcmd".as_ptr(), api_extcmd);
    lua_register(l, c"cartdata".as_ptr(), api_cartdata);

    // -- System --
    lua_register(l, c"load".as_ptr(), api_load);
    lua_register(l, c"flip".as_ptr(), api_flip);
    lua_register(l, c"run".as_ptr(), api_run);
    lua_register(l, c"stop".as_ptr(), api_stop);
    lua_register(l, c"resume".as_ptr(), api_resume);

    // -- Lua-side helpers (add/del/deli/count/all/foreach/sub/chr/ord) --
    load_code(l, PRELUDE, c"=prelude").map_err(|e| format!("prelude: {e}"))?;
    run_top(l).map_err(|e| format!("prelude: {e}"))?;
    for def in PRELUDE_DEFS {
        // Strip the `function ` prefix and the trailing `(` to get the name.
        let name = &def["function ".len()..def.len() - 1];
        if !PRELUDE.contains(def) {
            return Err(format!("prelude.lua does not define {name}"));
        }
        // Build a NUL-terminated name for lua_getglobal (names are short ASCII).
        let mut buf = [0u8; 16];
        if name.len() >= buf.len() {
            return Err(format!("prelude name too long: {name}"));
        }
        buf[..name.len()].copy_from_slice(name.as_bytes());
        let cname =
            CStr::from_bytes_until_nul(&buf).map_err(|_| String::from("bad prelude name"))?;
        if !has_global_function(l, cname) {
            return Err(format!("prelude did not define function {name}"));
        }
    }
    Ok(())
}

// ============================================================================
// Part 6: Lua Lifecycle Helpers
// ============================================================================

/// Create a new Lua state whose every allocation comes from `heap`
/// (`lua_newstate` + [`lua_alloc_fn`]), with [`lua_panic`] installed the way
/// `luaL_newstate` installs its own handler.
///
/// `heap` must already be initialised. If even the state itself cannot be
/// allocated there is nothing to run, so this halts with a message.
///
/// # Safety
/// The returned pointer must only be used from the main thread, and `heap`
/// must outlive the state (it is `'static`).
pub unsafe fn new_lua_state(heap: &'static Heap) -> *mut LuaState {
    let l = lua_newstate(lua_alloc_fn, heap as *const Heap as *mut c_void);
    if l.is_null() {
        crate::fatal::halt("out of memory creating the lua state");
    }
    lua_atpanic(l, lua_panic);
    l
}

/// Take the error object off the top of the stack as a `String`.
unsafe fn pop_error(l: *mut LuaState) -> String {
    let mut len: usize = 0;
    let ptr = lua_tolstring(l, -1, &mut len);
    let msg = if ptr.is_null() {
        String::from("(error object is not a string)")
    } else {
        String::from_utf8_lossy(core::slice::from_raw_parts(ptr as *const u8, len)).into_owned()
    };
    lua_pop(l, 1);
    msg
}

/// Compile `code` and leave the resulting chunk on the stack.
///
/// Takes the source by length (`luaL_loadbuffer`), so no NUL-terminated copy
/// is made; the caller may drop `code` as soon as this returns and then call
/// [`run_top`]. `chunk_name` prefixes error messages (`=cart` -> `cart:12:`).
///
/// # Safety
/// `l` must be a valid Lua state.
pub unsafe fn load_code(l: *mut LuaState, code: &str, chunk_name: &CStr) -> Result<(), String> {
    let status = luaL_loadbufferx(
        l,
        code.as_ptr() as *const c_char,
        code.len(),
        chunk_name.as_ptr(),
        core::ptr::null(),
    );
    if status != LUA_OK {
        return Err(pop_error(l));
    }
    Ok(())
}

/// Run the function on top of the stack (from [`load_code`]) with no
/// arguments, in protected mode.
///
/// # Safety
/// `l` must be a valid Lua state with a function on top.
pub unsafe fn run_top(l: *mut LuaState) -> Result<(), String> {
    if lua_pcall(l, 0, 0, 0) != LUA_OK {
        return Err(pop_error(l));
    }
    Ok(())
}

/// Tune the collector for the heap Lua lives in.
///
/// `small_heap` (Lua in the few-hundred-KB SRAM heap, i.e. the Pico 2):
/// Lua's incremental collector defaults to `pause = 200`: a new cycle only
/// starts once the heap has doubled since the last collection, so a cart
/// with N KB live can transiently occupy ~2N KB. With a few hundred KB total
/// that is the difference between fitting and an out-of-memory halt.
/// `pause = 120` starts the next cycle at 1.2x live, and `stepmul = 200`
/// makes each incremental step do twice the default work so cycles finish
/// sooner. Cost: somewhat more GC CPU per frame; peak memory stays close to
/// live data. `stepsize` is left at the default (0 = keep).
///
/// Otherwise (Lua in the Tufty's 8 MB PSRAM) memory is plentiful and CPU
/// is the scarcer resource -- PSRAM accesses are slower than SRAM, so extra
/// GC work costs more -- and the Lua defaults (`pause = 200`,
/// `stepmul = 100`) are kept.
///
/// # Safety
/// `l` must be a valid Lua state.
pub unsafe fn tune_gc(l: *mut LuaState, small_heap: bool) {
    if small_heap {
        lua_gc(l, LUA_GCINC, 120i32, 200i32, 0i32);
    } else {
        lua_gc(l, LUA_GCINC, 200i32, 100i32, 0i32);
    }
}

/// Run a full collection now (used once after the main chunk has run, to
/// free load-time garbage before the first frame).
///
/// # Safety
/// `l` must be a valid Lua state.
pub unsafe fn full_gc(l: *mut LuaState) {
    lua_gc(l, LUA_GCCOLLECT);
}

/// Is there a global function called `name`? Used to detect `_update60`.
///
/// # Safety
/// `l` must be a valid Lua state.
pub unsafe fn has_global_function(l: *mut LuaState, name: &CStr) -> bool {
    let ty = lua_getglobal(l, name.as_ptr());
    lua_pop(l, 1);
    ty == LUA_TFUNCTION
}

/// Call a global Lua function by name (`_init`, `_update`, `_draw`, ...).
///
/// A missing global is not an error (PICO-8 callbacks are optional). A Lua
/// error inside the function is returned as its message; the caller decides
/// what to do (the firmware shows it and halts, like the desktop).
///
/// # Safety
/// `l` must be a valid Lua state.
pub unsafe fn call_global(l: *mut LuaState, name: &CStr) -> Result<(), String> {
    lua_getglobal(l, name.as_ptr());
    if lua_isnone(l, -1) || lua_isnil(l, -1) {
        lua_pop(l, 1);
        return Ok(());
    }
    if lua_pcall(l, 0, 0, 0) != LUA_OK {
        return Err(pop_error(l));
    }
    Ok(())
}
