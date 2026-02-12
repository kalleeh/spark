//! Lua 5.4 C FFI bindings for the Spark fantasy console on RP2350.
//!
//! This module provides:
//! - Raw `extern "C"` bindings to the Lua 5.4 C API (compiled from vendored sources)
//! - A custom Lua allocator backed by `embedded-alloc` / the global allocator
//! - Global state pointers for single-threaded access from Lua callbacks
//! - All PICO-8 API functions registered into Lua as globals
//! - Lifecycle helpers: `new_lua_state`, `load_and_run`, `call_global`
//!
//! **Important:** The Lua C library is compiled with `LUA_32BITS`, so
//! `lua_Integer` is `i32` and `lua_Number` is `f32`.

use core::alloc::Layout;
use core::f32::consts::TAU;

use spark_core::audio::Audio;
use spark_core::console::Console;
use spark_core::game_state::GameState;

// ============================================================================
// Part 1: Raw Lua C API Bindings
// ============================================================================

/// Opaque Lua state handle.
pub type LuaState = core::ffi::c_void;

/// Lua C function signature.
pub type LuaCFunction = unsafe extern "C" fn(L: *mut LuaState) -> i32;

/// Lua allocator function signature.
pub type LuaAlloc = unsafe extern "C" fn(
    ud: *mut core::ffi::c_void,
    ptr: *mut core::ffi::c_void,
    osize: usize,
    nsize: usize,
) -> *mut core::ffi::c_void;

// Lua type constants
const LUA_TNONE: i32 = -1;
const LUA_TNIL: i32 = 0;
const LUA_TBOOLEAN: i32 = 1;
#[allow(dead_code)]
const LUA_TNUMBER: i32 = 3;
#[allow(dead_code)]
const LUA_TSTRING: i32 = 4;
#[allow(dead_code)]
const LUA_TTABLE: i32 = 5;

extern "C" {
    // -- State management --
    pub fn lua_newstate(f: LuaAlloc, ud: *mut core::ffi::c_void) -> *mut LuaState;
    pub fn lua_close(L: *mut LuaState);
    pub fn luaL_openlibs(L: *mut LuaState);

    // -- Stack operations --
    pub fn lua_gettop(L: *mut LuaState) -> i32;
    pub fn lua_settop(L: *mut LuaState, idx: i32);
    pub fn lua_pushnil(L: *mut LuaState);
    pub fn lua_pushboolean(L: *mut LuaState, b: i32);
    /// Push an integer. With `LUA_32BITS`, `lua_Integer` is `i32`.
    pub fn lua_pushinteger(L: *mut LuaState, n: i32);
    /// Push a number. With `LUA_32BITS`, `lua_Number` is `f32`.
    pub fn lua_pushnumber(L: *mut LuaState, n: f32);
    pub fn lua_pushstring(
        L: *mut LuaState,
        s: *const core::ffi::c_char,
    ) -> *const core::ffi::c_char;
    pub fn lua_pushcclosure(L: *mut LuaState, f: LuaCFunction, n: i32);

    // -- Get from stack --
    pub fn lua_toboolean(L: *mut LuaState, idx: i32) -> i32;
    pub fn lua_tointegerx(L: *mut LuaState, idx: i32, isnum: *mut i32) -> i32;
    pub fn lua_tonumberx(L: *mut LuaState, idx: i32, isnum: *mut i32) -> f32;
    pub fn lua_tolstring(
        L: *mut LuaState,
        idx: i32,
        len: *mut usize,
    ) -> *const core::ffi::c_char;
    pub fn lua_type(L: *mut LuaState, idx: i32) -> i32;

    // -- Table operations --
    pub fn lua_getglobal(L: *mut LuaState, name: *const core::ffi::c_char) -> i32;
    pub fn lua_setglobal(L: *mut LuaState, name: *const core::ffi::c_char);
    pub fn lua_rawgeti(L: *mut LuaState, idx: i32, n: i64) -> i32;
    pub fn lua_rawseti(L: *mut LuaState, idx: i32, n: i64);
    pub fn lua_rawlen(L: *mut LuaState, idx: i32) -> usize;
    pub fn lua_createtable(L: *mut LuaState, narr: i32, nrec: i32);
    pub fn lua_next(L: *mut LuaState, idx: i32) -> i32;

    // -- Execution --
    pub fn luaL_loadstring(L: *mut LuaState, s: *const core::ffi::c_char) -> i32;
    pub fn lua_pcallk(
        L: *mut LuaState,
        nargs: i32,
        nresults: i32,
        errfunc: i32,
        ctx: isize,
        k: Option<unsafe extern "C" fn(*mut LuaState, i32, isize) -> i32>,
    ) -> i32;

    // -- Stack manipulation --
    pub fn lua_pushvalue(L: *mut LuaState, idx: i32);

    // -- Extra space --
    pub fn lua_getextraspace(L: *mut LuaState) -> *mut core::ffi::c_void;
}

// ---------------------------------------------------------------------------
// Macro-equivalent helper functions
// ---------------------------------------------------------------------------

/// Pop `n` values from the Lua stack.  (C macro: `lua_settop(L, -(n)-1)`.)
#[inline]
pub unsafe fn lua_pop(L: *mut LuaState, n: i32) {
    lua_settop(L, -(n) - 1);
}

/// Read an integer from the stack without an `isnum` out-parameter.
#[inline]
pub unsafe fn lua_tointeger(L: *mut LuaState, idx: i32) -> i32 {
    lua_tointegerx(L, idx, core::ptr::null_mut())
}

/// Read a number (f32) from the stack without an `isnum` out-parameter.
#[inline]
pub unsafe fn lua_tonumber(L: *mut LuaState, idx: i32) -> f32 {
    lua_tonumberx(L, idx, core::ptr::null_mut())
}

/// Check if the value at `idx` is nil.
#[inline]
pub unsafe fn lua_isnil(L: *mut LuaState, idx: i32) -> bool {
    lua_type(L, idx) == LUA_TNIL
}

/// Check if the stack slot at `idx` contains no value.
#[inline]
pub unsafe fn lua_isnone(L: *mut LuaState, idx: i32) -> bool {
    lua_type(L, idx) == LUA_TNONE
}

/// Check if the value at `idx` is nil **or** none (no value).
#[inline]
pub unsafe fn lua_isnoneornil(L: *mut LuaState, idx: i32) -> bool {
    lua_type(L, idx) <= 0
}

/// Check if the value at `idx` is a boolean.
#[inline]
#[allow(dead_code)]
pub unsafe fn lua_isboolean(L: *mut LuaState, idx: i32) -> bool {
    lua_type(L, idx) == LUA_TBOOLEAN
}

/// Protected call without continuation.
#[inline]
pub unsafe fn lua_pcall(L: *mut LuaState, nargs: i32, nresults: i32, errfunc: i32) -> i32 {
    lua_pcallk(L, nargs, nresults, errfunc, 0, None)
}

/// Push a C function (a closure with zero upvalues).
#[inline]
pub unsafe fn lua_pushcfunction(L: *mut LuaState, f: LuaCFunction) {
    lua_pushcclosure(L, f, 0);
}

/// Register a C function as a global.
#[inline]
pub unsafe fn lua_register(L: *mut LuaState, name: *const core::ffi::c_char, f: LuaCFunction) {
    lua_pushcfunction(L, f);
    lua_setglobal(L, name);
}

// ============================================================================
// Part 2: Lua Allocator Bridge
// ============================================================================

/// Custom Lua allocator that delegates to the Rust global allocator.
///
/// All allocations use 8-byte alignment, which satisfies the alignment
/// requirements of all Lua-internal structures.
unsafe extern "C" fn lua_alloc_fn(
    _ud: *mut core::ffi::c_void,
    ptr: *mut core::ffi::c_void,
    osize: usize,
    nsize: usize,
) -> *mut core::ffi::c_void {
    if nsize == 0 {
        // Free
        if !ptr.is_null() && osize > 0 {
            let layout = Layout::from_size_align_unchecked(osize, 8);
            alloc::alloc::dealloc(ptr as *mut u8, layout);
        }
        core::ptr::null_mut()
    } else if ptr.is_null() {
        // Allocate
        let layout = Layout::from_size_align_unchecked(nsize, 8);
        alloc::alloc::alloc(layout) as *mut core::ffi::c_void
    } else {
        // Realloc
        let layout = Layout::from_size_align_unchecked(osize, 8);
        alloc::alloc::realloc(ptr as *mut u8, layout, nsize) as *mut core::ffi::c_void
    }
}

// ============================================================================
// Part 3: Global State Access
// ============================================================================

/// Global console pointer, accessible from Lua C callbacks.
/// Safety: Only accessed from the main loop (single-threaded on RP2350).
pub static mut CONSOLE: Option<*mut Console> = None;

/// Global audio pointer.
pub static mut AUDIO: Option<*mut Audio> = None;

/// Global game state pointer.
pub static mut GAME_STATE: Option<*mut GameState> = None;

/// Set the global state pointers. Must be called before any Lua execution.
///
/// # Safety
/// The caller must ensure that the references remain valid for the entire
/// duration of Lua execution and that no other threads access them.
pub unsafe fn set_state(console: &mut Console, audio: &mut Audio, gs: &mut GameState) {
    CONSOLE = Some(console as *mut Console);
    AUDIO = Some(audio as *mut Audio);
    GAME_STATE = Some(gs as *mut GameState);
}

/// Clear the global state pointers.
#[allow(dead_code)]
pub unsafe fn clear_state() {
    CONSOLE = None;
    AUDIO = None;
    GAME_STATE = None;
}

/// Get a mutable reference to the global `Console`.
///
/// # Safety
/// Must only be called when `set_state` has been called and the pointer is
/// still valid.
#[inline]
unsafe fn console() -> &'static mut Console {
    &mut *CONSOLE.unwrap()
}

/// Get a mutable reference to the global `Audio`.
#[inline]
unsafe fn audio() -> &'static mut Audio {
    &mut *AUDIO.unwrap()
}

/// Get a mutable reference to the global `GameState`.
#[inline]
unsafe fn game_state() -> &'static mut GameState {
    &mut *GAME_STATE.unwrap()
}

// ============================================================================
// Argument helpers
// ============================================================================

/// Read a required i32 argument from the Lua stack at position `idx`.
#[inline]
unsafe fn arg_i32(L: *mut LuaState, idx: i32) -> i32 {
    lua_tointeger(L, idx)
}

/// Read an optional i32 argument. Returns `None` if the slot is nil/none.
#[inline]
unsafe fn opt_arg_i32(L: *mut LuaState, idx: i32) -> Option<i32> {
    if lua_isnoneornil(L, idx) {
        None
    } else {
        Some(lua_tointeger(L, idx))
    }
}

/// Read a required f32 argument from the Lua stack at position `idx`.
#[inline]
unsafe fn arg_f32(L: *mut LuaState, idx: i32) -> f32 {
    lua_tonumber(L, idx)
}

/// Read an optional f32 argument.
#[inline]
unsafe fn opt_arg_f32(L: *mut LuaState, idx: i32) -> Option<f32> {
    if lua_isnoneornil(L, idx) {
        None
    } else {
        Some(lua_tonumber(L, idx))
    }
}

/// Read a boolean argument (Lua `toboolean` coerces: nil/false -> false, else true).
#[inline]
unsafe fn arg_bool(L: *mut LuaState, idx: i32) -> bool {
    lua_toboolean(L, idx) != 0
}

/// Read a string argument. Returns an empty slice if nil/none.
/// The returned pointer is valid as long as the string is on the Lua stack.
unsafe fn arg_str<'a>(L: *mut LuaState, idx: i32) -> &'a str {
    if lua_isnoneornil(L, idx) {
        return "";
    }
    let mut len: usize = 0;
    let ptr = lua_tolstring(L, idx, &mut len);
    if ptr.is_null() {
        return "";
    }
    let bytes = core::slice::from_raw_parts(ptr as *const u8, len);
    core::str::from_utf8_unchecked(bytes)
}

// ============================================================================
// Part 4: PICO-8 API Functions
// ============================================================================

// ---------------------------------------------------------------------------
// Drawing
// ---------------------------------------------------------------------------

/// `cls([col])` -- clear screen
unsafe extern "C" fn api_cls(L: *mut LuaState) -> i32 {
    let col = opt_arg_i32(L, 1).map(|v| v as u8);
    console().cls(col);
    0
}

/// `pset(x, y, [col])` -- set pixel
unsafe extern "C" fn api_pset(L: *mut LuaState) -> i32 {
    let x = arg_i32(L, 1);
    let y = arg_i32(L, 2);
    let col = opt_arg_i32(L, 3).map(|v| v as u8);
    console().pset(x, y, col);
    0
}

/// `pget(x, y)` -- get pixel color
unsafe extern "C" fn api_pget(L: *mut LuaState) -> i32 {
    let x = arg_i32(L, 1);
    let y = arg_i32(L, 2);
    let c = console().pget(x, y);
    lua_pushinteger(L, c as i32);
    1
}

/// `line(x0, y0, x1, y1, [col])` -- draw line
unsafe extern "C" fn api_line(L: *mut LuaState) -> i32 {
    let x0 = arg_i32(L, 1);
    let y0 = arg_i32(L, 2);
    let x1 = arg_i32(L, 3);
    let y1 = arg_i32(L, 4);
    let col = opt_arg_i32(L, 5).map(|v| v as u8);
    console().line(x0, y0, x1, y1, col);
    0
}

/// `rect(x0, y0, x1, y1, [col])` -- draw rectangle outline
unsafe extern "C" fn api_rect(L: *mut LuaState) -> i32 {
    let x0 = arg_i32(L, 1);
    let y0 = arg_i32(L, 2);
    let x1 = arg_i32(L, 3);
    let y1 = arg_i32(L, 4);
    let col = opt_arg_i32(L, 5).map(|v| v as u8);
    console().rect(x0, y0, x1, y1, col);
    0
}

/// `rectfill(x0, y0, x1, y1, [col])` -- draw filled rectangle
unsafe extern "C" fn api_rectfill(L: *mut LuaState) -> i32 {
    let x0 = arg_i32(L, 1);
    let y0 = arg_i32(L, 2);
    let x1 = arg_i32(L, 3);
    let y1 = arg_i32(L, 4);
    let col = opt_arg_i32(L, 5).map(|v| v as u8);
    console().rectfill(x0, y0, x1, y1, col);
    0
}

/// `circ(cx, cy, r, [col])` -- draw circle outline
unsafe extern "C" fn api_circ(L: *mut LuaState) -> i32 {
    let cx = arg_i32(L, 1);
    let cy = arg_i32(L, 2);
    let r = arg_i32(L, 3);
    let col = opt_arg_i32(L, 4).map(|v| v as u8);
    console().circ(cx, cy, r, col);
    0
}

/// `circfill(cx, cy, r, [col])` -- draw filled circle
unsafe extern "C" fn api_circfill(L: *mut LuaState) -> i32 {
    let cx = arg_i32(L, 1);
    let cy = arg_i32(L, 2);
    let r = arg_i32(L, 3);
    let col = opt_arg_i32(L, 4).map(|v| v as u8);
    console().circfill(cx, cy, r, col);
    0
}

/// `oval(x0, y0, x1, y1, [col])` -- draw ellipse outline
unsafe extern "C" fn api_oval(L: *mut LuaState) -> i32 {
    let x0 = arg_i32(L, 1);
    let y0 = arg_i32(L, 2);
    let x1 = arg_i32(L, 3);
    let y1 = arg_i32(L, 4);
    let col = opt_arg_i32(L, 5).map(|v| v as u8);
    console().oval(x0, y0, x1, y1, col);
    0
}

/// `ovalfill(x0, y0, x1, y1, [col])` -- draw filled ellipse
unsafe extern "C" fn api_ovalfill(L: *mut LuaState) -> i32 {
    let x0 = arg_i32(L, 1);
    let y0 = arg_i32(L, 2);
    let x1 = arg_i32(L, 3);
    let y1 = arg_i32(L, 4);
    let col = opt_arg_i32(L, 5).map(|v| v as u8);
    console().ovalfill(x0, y0, x1, y1, col);
    0
}

/// `cursor(x, y)` -- set the text cursor position
unsafe extern "C" fn api_cursor(L: *mut LuaState) -> i32 {
    let x = opt_arg_i32(L, 1).unwrap_or(0);
    let y = opt_arg_i32(L, 2).unwrap_or(0);
    console().cursor(x, y);
    if let Some(c) = opt_arg_i32(L, 3) {
        console().color(c as u8);
    }
    0
}

/// `spr(n, x, y, [w], [h], [flip_x], [flip_y])` -- draw sprite
unsafe extern "C" fn api_spr(L: *mut LuaState) -> i32 {
    let n = arg_i32(L, 1);
    let x = arg_i32(L, 2);
    let y = arg_i32(L, 3);
    let w = opt_arg_f32(L, 4).unwrap_or(1.0) as f64;
    let h = opt_arg_f32(L, 5).unwrap_or(1.0) as f64;
    let flip_x = arg_bool(L, 6);
    let flip_y = arg_bool(L, 7);
    console().spr(n, x, y, w, h, flip_x, flip_y);
    0
}

/// `sspr(sx, sy, sw, sh, dx, dy, [dw], [dh], [flip_x], [flip_y])` -- stretch blit from sprite sheet
unsafe extern "C" fn api_sspr(L: *mut LuaState) -> i32 {
    let sx = arg_i32(L, 1);
    let sy = arg_i32(L, 2);
    let sw = arg_i32(L, 3);
    let sh = arg_i32(L, 4);
    let dx = arg_i32(L, 5);
    let dy = arg_i32(L, 6);
    let dw = opt_arg_i32(L, 7).unwrap_or(sw);
    let dh = opt_arg_i32(L, 8).unwrap_or(sh);
    let flip_x = arg_bool(L, 9);
    let flip_y = arg_bool(L, 10);
    console().sspr(sx, sy, sw, sh, dx, dy, dw, dh, flip_x, flip_y);
    0
}

/// `print(str, [x], [y], [col])` -- print text; returns x advance
unsafe extern "C" fn api_print(L: *mut LuaState) -> i32 {
    let text = arg_str(L, 1);
    let x = opt_arg_i32(L, 2);
    let y = opt_arg_i32(L, 3);
    let col = opt_arg_i32(L, 4).map(|v| v as u8);
    let advance = console().print(text, x, y, col);
    lua_pushinteger(L, advance);
    1
}

/// `camera([x], [y])` -- set camera offset
unsafe extern "C" fn api_camera(L: *mut LuaState) -> i32 {
    let x = opt_arg_i32(L, 1).unwrap_or(0);
    let y = opt_arg_i32(L, 2).unwrap_or(0);
    console().camera(x, y);
    0
}

/// `clip([x, y, w, h])` -- set clipping rect, or reset if no args
unsafe extern "C" fn api_clip(L: *mut LuaState) -> i32 {
    if lua_gettop(L) == 0 || lua_isnoneornil(L, 1) {
        console().clip_rect(0, 0, 128, 128);
    } else {
        let x = arg_i32(L, 1);
        let y = arg_i32(L, 2);
        let w = arg_i32(L, 3);
        let h = arg_i32(L, 4);
        console().clip_rect(x, y, w, h);
    }
    0
}

/// `pal([c0, c1, p])` -- remap palette, or reset if no args
unsafe extern "C" fn api_pal(L: *mut LuaState) -> i32 {
    if lua_gettop(L) == 0 || lua_isnoneornil(L, 1) {
        console().pal_reset();
    } else {
        let c0 = arg_i32(L, 1) as u8;
        let c1 = arg_i32(L, 2) as u8;
        let p = opt_arg_i32(L, 3).unwrap_or(0) as u8;
        console().pal(c0, c1, p);
    }
    0
}

/// `palt([c, t])` -- set transparency for color, or reset if no args
unsafe extern "C" fn api_palt(L: *mut LuaState) -> i32 {
    if lua_gettop(L) == 0 || lua_isnoneornil(L, 1) {
        console().palt_reset();
    } else {
        let c = arg_i32(L, 1) as u8;
        let t = arg_bool(L, 2);
        console().palt(c, t);
    }
    0
}

/// `color(c)` -- set default draw color
unsafe extern "C" fn api_color(L: *mut LuaState) -> i32 {
    let c = opt_arg_i32(L, 1).unwrap_or(6) as u8;
    console().color(c);
    0
}

// ---------------------------------------------------------------------------
// Sprites / Map
// ---------------------------------------------------------------------------

/// `sget(x, y)` -- get sprite sheet pixel
unsafe extern "C" fn api_sget(L: *mut LuaState) -> i32 {
    let x = arg_i32(L, 1);
    let y = arg_i32(L, 2);
    let c = console().sget(x, y);
    lua_pushinteger(L, c as i32);
    1
}

/// `sset(x, y, [col])` -- set sprite sheet pixel
unsafe extern "C" fn api_sset(L: *mut LuaState) -> i32 {
    let x = arg_i32(L, 1);
    let y = arg_i32(L, 2);
    let col = opt_arg_i32(L, 3).unwrap_or(0) as u8;
    console().sset(x, y, col);
    0
}

/// `map(cel_x, cel_y, sx, sy, cel_w, cel_h, [layer])` -- draw map region
unsafe extern "C" fn api_map(L: *mut LuaState) -> i32 {
    let cel_x = opt_arg_i32(L, 1).unwrap_or(0);
    let cel_y = opt_arg_i32(L, 2).unwrap_or(0);
    let sx = opt_arg_i32(L, 3).unwrap_or(0);
    let sy = opt_arg_i32(L, 4).unwrap_or(0);
    let cel_w = opt_arg_i32(L, 5).unwrap_or(128);
    let cel_h = opt_arg_i32(L, 6).unwrap_or(64);
    let layer = opt_arg_i32(L, 7).unwrap_or(0) as u8;
    console().map_draw(cel_x, cel_y, sx, sy, cel_w, cel_h, layer);
    0
}

/// `mget(x, y)` -- get map tile
unsafe extern "C" fn api_mget(L: *mut LuaState) -> i32 {
    let x = arg_i32(L, 1);
    let y = arg_i32(L, 2);
    let tile = console().mget(x, y);
    lua_pushinteger(L, tile as i32);
    1
}

/// `mset(x, y, v)` -- set map tile
unsafe extern "C" fn api_mset(L: *mut LuaState) -> i32 {
    let x = arg_i32(L, 1);
    let y = arg_i32(L, 2);
    let v = arg_i32(L, 3) as u8;
    console().mset(x, y, v);
    0
}

/// `fget(n, [f])` -- get sprite flags (whole byte, or single bit)
unsafe extern "C" fn api_fget(L: *mut LuaState) -> i32 {
    let n = arg_i32(L, 1) as usize;
    let f = opt_arg_i32(L, 2).map(|v| v as u8);
    let result = console().fget(n, f);
    // When querying a single bit, PICO-8 returns a boolean
    if f.is_some() {
        lua_pushboolean(L, result as i32);
    } else {
        lua_pushinteger(L, result as i32);
    }
    1
}

/// `fset(n, [f], v)` -- set sprite flags
///
/// Two-argument form: `fset(n, v)` sets the whole flag byte.
/// Three-argument form: `fset(n, f, v)` sets bit `f` to `v`.
unsafe extern "C" fn api_fset(L: *mut LuaState) -> i32 {
    let n = arg_i32(L, 1) as usize;
    let nargs = lua_gettop(L);
    if nargs >= 3 {
        // fset(n, f, v)
        let f = arg_i32(L, 2) as u8;
        let v = if arg_bool(L, 3) { 1u8 } else { 0u8 };
        console().fset(n, Some(f), v);
    } else {
        // fset(n, v)
        let v = arg_i32(L, 2) as u8;
        console().fset(n, None, v);
    }
    0
}

// ---------------------------------------------------------------------------
// Input
// ---------------------------------------------------------------------------

/// `btn([i], [p])` -- is button pressed? p defaults to 0 (player 1)
/// With no args, returns bitmask of all buttons for player 0.
unsafe extern "C" fn api_btn(L: *mut LuaState) -> i32 {
    if lua_gettop(L) == 0 || lua_isnoneornil(L, 1) {
        // No-args form: return bitmask of all buttons for player 0
        let con = console();
        let mut mask: i32 = 0;
        for bit in 0..6u8 {
            if con.btn(bit, 0) {
                mask |= 1 << bit;
            }
        }
        lua_pushinteger(L, mask);
        return 1;
    }
    let i = arg_i32(L, 1) as u8;
    let p = opt_arg_i32(L, 2).unwrap_or(0) as u8;
    let pressed = console().btn(i, p);
    lua_pushboolean(L, pressed as i32);
    1
}

/// `btnp(i, [p])` -- was button just pressed this frame? p defaults to 0 (player 1)
unsafe extern "C" fn api_btnp(L: *mut LuaState) -> i32 {
    let i = arg_i32(L, 1) as u8;
    let p = opt_arg_i32(L, 2).unwrap_or(0) as u8;
    let pressed = console().btnp(i, p);
    lua_pushboolean(L, pressed as i32);
    1
}

// ---------------------------------------------------------------------------
// Math (PICO-8 specific)
// ---------------------------------------------------------------------------

/// `sin(x)` -- PICO-8 sin: returns `-sin(x * 2*pi)` (inverted, 0-1 input range)
unsafe extern "C" fn api_sin(L: *mut LuaState) -> i32 {
    let x = arg_f32(L, 1);
    let result = -libm::sinf(x * TAU);
    lua_pushnumber(L, result);
    1
}

/// `cos(x)` -- PICO-8 cos: returns `cos(x * 2*pi)` (0-1 input range)
unsafe extern "C" fn api_cos(L: *mut LuaState) -> i32 {
    let x = arg_f32(L, 1);
    let result = libm::cosf(x * TAU);
    lua_pushnumber(L, result);
    1
}

/// `atan2(dx, dy)` -- PICO-8 atan2: returns 0..1 (note: PICO-8 convention, inverted y)
unsafe extern "C" fn api_atan2(L: *mut LuaState) -> i32 {
    let dx = arg_f32(L, 1);
    let dy = arg_f32(L, 2);
    // PICO-8 atan2 takes (dx, dy) and returns 0..1.
    // It uses inverted y (screen coords) and the angle is measured
    // clockwise from the right.
    let angle = libm::atan2f(-dy, dx);
    // Convert from radians (-pi..pi) to 0..1
    let result = (angle / TAU + 1.0) % 1.0;
    lua_pushnumber(L, result);
    1
}

/// `sqrt(x)` -- square root
unsafe extern "C" fn api_sqrt(L: *mut LuaState) -> i32 {
    let x = arg_f32(L, 1);
    let result = libm::sqrtf(if x < 0.0 { 0.0 } else { x });
    lua_pushnumber(L, result);
    1
}

/// `abs(x)` -- absolute value
unsafe extern "C" fn api_abs(L: *mut LuaState) -> i32 {
    let x = arg_f32(L, 1);
    let result = libm::fabsf(x);
    lua_pushnumber(L, result);
    1
}

/// `flr(x)` -- floor
unsafe extern "C" fn api_flr(L: *mut LuaState) -> i32 {
    let x = arg_f32(L, 1);
    let result = libm::floorf(x);
    lua_pushnumber(L, result);
    1
}

/// `ceil(x)` -- ceiling
unsafe extern "C" fn api_ceil(L: *mut LuaState) -> i32 {
    let x = arg_f32(L, 1);
    let result = libm::ceilf(x);
    lua_pushnumber(L, result);
    1
}

/// `min(a, b)` -- minimum
unsafe extern "C" fn api_min(L: *mut LuaState) -> i32 {
    let a = arg_f32(L, 1);
    let b = arg_f32(L, 2);
    let result = if a < b { a } else { b };
    lua_pushnumber(L, result);
    1
}

/// `max(a, b)` -- maximum
unsafe extern "C" fn api_max(L: *mut LuaState) -> i32 {
    let a = arg_f32(L, 1);
    let b = arg_f32(L, 2);
    let result = if a > b { a } else { b };
    lua_pushnumber(L, result);
    1
}

/// `mid(a, b, c)` -- middle value of three
unsafe extern "C" fn api_mid(L: *mut LuaState) -> i32 {
    let a = arg_f32(L, 1);
    let b = arg_f32(L, 2);
    let c = arg_f32(L, 3);
    // mid(a,b,c) returns the middle value
    let result = if a > b {
        if b > c {
            b
        } else if a > c {
            c
        } else {
            a
        }
    } else {
        // a <= b
        if a > c {
            a
        } else if b > c {
            c
        } else {
            b
        }
    };
    lua_pushnumber(L, result);
    1
}

/// `sgn(x)` -- sign: returns 1 or -1 (PICO-8 never returns 0)
unsafe extern "C" fn api_sgn(L: *mut LuaState) -> i32 {
    let x = arg_f32(L, 1);
    let result = if x < 0.0 { -1.0f32 } else { 1.0f32 };
    lua_pushnumber(L, result);
    1
}

/// `rnd([max])` -- random number in 0..<max (default max=1)
unsafe extern "C" fn api_rnd(L: *mut LuaState) -> i32 {
    let max = opt_arg_f32(L, 1).unwrap_or(1.0);
    let r = game_state().next_random() as f32;
    let result = r * max;
    lua_pushnumber(L, result);
    1
}

// -- Bitwise operations --
// PICO-8 bitwise ops work on the integer part of f32 numbers.

/// `band(a, b)` -- bitwise AND
unsafe extern "C" fn api_band(L: *mut LuaState) -> i32 {
    let a = arg_f32(L, 1) as i32;
    let b = arg_f32(L, 2) as i32;
    lua_pushnumber(L, (a & b) as f32);
    1
}

/// `bor(a, b)` -- bitwise OR
unsafe extern "C" fn api_bor(L: *mut LuaState) -> i32 {
    let a = arg_f32(L, 1) as i32;
    let b = arg_f32(L, 2) as i32;
    lua_pushnumber(L, (a | b) as f32);
    1
}

/// `bxor(a, b)` -- bitwise XOR
unsafe extern "C" fn api_bxor(L: *mut LuaState) -> i32 {
    let a = arg_f32(L, 1) as i32;
    let b = arg_f32(L, 2) as i32;
    lua_pushnumber(L, (a ^ b) as f32);
    1
}

/// `bnot(a)` -- bitwise NOT
unsafe extern "C" fn api_bnot(L: *mut LuaState) -> i32 {
    let a = arg_f32(L, 1) as i32;
    lua_pushnumber(L, (!a) as f32);
    1
}

/// `shl(a, b)` -- shift left
unsafe extern "C" fn api_shl(L: *mut LuaState) -> i32 {
    let a = arg_f32(L, 1) as i32;
    let b = arg_f32(L, 2) as u32;
    let result = if b >= 32 { 0 } else { a << b };
    lua_pushnumber(L, result as f32);
    1
}

/// `shr(a, b)` -- shift right (arithmetic)
unsafe extern "C" fn api_shr(L: *mut LuaState) -> i32 {
    let a = arg_f32(L, 1) as i32;
    let b = arg_f32(L, 2) as u32;
    let result = if b >= 32 {
        if a < 0 { -1 } else { 0 }
    } else {
        a >> b
    };
    lua_pushnumber(L, result as f32);
    1
}

/// `lshr(a, b)` -- logical (unsigned) shift right
unsafe extern "C" fn api_lshr(L: *mut LuaState) -> i32 {
    let a = arg_f32(L, 1) as i32;
    let b = (arg_f32(L, 2) as u32) & 31;
    let bits = (a as u32) >> b;
    lua_pushnumber(L, bits as i32 as f32);
    1
}

/// `rotl(a, b)` -- rotate bits left
unsafe extern "C" fn api_rotl(L: *mut LuaState) -> i32 {
    let a = arg_f32(L, 1) as i32 as u32;
    let b = (arg_f32(L, 2) as u32) & 31;
    let result = a.rotate_left(b);
    lua_pushnumber(L, result as i32 as f32);
    1
}

/// `rotr(a, b)` -- rotate bits right
unsafe extern "C" fn api_rotr(L: *mut LuaState) -> i32 {
    let a = arg_f32(L, 1) as i32 as u32;
    let b = (arg_f32(L, 2) as u32) & 31;
    let result = a.rotate_right(b);
    lua_pushnumber(L, result as i32 as f32);
    1
}

// ---------------------------------------------------------------------------
// Sound
// ---------------------------------------------------------------------------

/// `sfx(n, [channel], [offset], [length])` -- trigger sound effect
unsafe extern "C" fn api_sfx(L: *mut LuaState) -> i32 {
    let n = arg_i32(L, 1);
    let channel = opt_arg_i32(L, 2).unwrap_or(-1);
    let offset = opt_arg_i32(L, 3).unwrap_or(0);
    let length = opt_arg_i32(L, 4).unwrap_or(0);
    audio().sfx(n, channel, offset, length);
    0
}

/// `music(n, [fade_len], [channel_mask])` -- start/stop music
unsafe extern "C" fn api_music(L: *mut LuaState) -> i32 {
    let n = arg_i32(L, 1);
    let fade_len = opt_arg_i32(L, 2).unwrap_or(0);
    let channel_mask = opt_arg_i32(L, 3).unwrap_or(0);
    audio().music(n, fade_len, channel_mask);
    0
}

// ---------------------------------------------------------------------------
// Utility
// ---------------------------------------------------------------------------

/// `time()` / `t()` -- returns seconds elapsed (frame_count / 30)
unsafe extern "C" fn api_time(L: *mut LuaState) -> i32 {
    let gs = game_state();
    let seconds = gs.frame_count as f32 / 30.0;
    lua_pushnumber(L, seconds);
    1
}

/// `peek(addr)` -- read a byte from the PICO-8 memory map
unsafe extern "C" fn api_peek(L: *mut LuaState) -> i32 {
    let addr = arg_i32(L, 1) as u16;
    let val = console().peek(addr);
    lua_pushnumber(L, val as f32);
    1
}

/// `poke(addr, val)` -- write a byte to the PICO-8 memory map
unsafe extern "C" fn api_poke(L: *mut LuaState) -> i32 {
    let addr = arg_i32(L, 1) as u16;
    let val = arg_i32(L, 2) as u8;
    console().poke(addr, val);
    0
}

/// `memcpy(dest, src, len)` -- copy bytes within the memory map
unsafe extern "C" fn api_memcpy(L: *mut LuaState) -> i32 {
    let dest = arg_i32(L, 1) as u16;
    let src = arg_i32(L, 2) as u16;
    let len = arg_i32(L, 3) as u16;
    console().memcpy(dest, src, len);
    0
}

/// `memset(dest, val, len)` -- fill bytes in the memory map
unsafe extern "C" fn api_memset(L: *mut LuaState) -> i32 {
    let dest = arg_i32(L, 1) as u16;
    let val = arg_i32(L, 2) as u8;
    let len = arg_i32(L, 3) as u16;
    console().memset(dest, val, len);
    0
}

/// `peek2(addr)` -- read 16-bit value from PICO-8 memory map
unsafe extern "C" fn api_peek2(L: *mut LuaState) -> i32 {
    let addr = arg_i32(L, 1) as u16;
    let val = console().peek2(addr);
    lua_pushnumber(L, val as f32);
    1
}

/// `peek4(addr)` -- read 32-bit value from PICO-8 memory map
unsafe extern "C" fn api_peek4(L: *mut LuaState) -> i32 {
    let addr = arg_i32(L, 1) as u16;
    let val = console().peek4(addr);
    lua_pushnumber(L, val as f32);
    1
}

/// `poke2(addr, val)` -- write 16-bit value to PICO-8 memory map
unsafe extern "C" fn api_poke2(L: *mut LuaState) -> i32 {
    let addr = arg_i32(L, 1) as u16;
    let val = arg_i32(L, 2) as u16;
    console().poke2(addr, val);
    0
}

/// `poke4(addr, val)` -- write 32-bit value to PICO-8 memory map
unsafe extern "C" fn api_poke4(L: *mut LuaState) -> i32 {
    let addr = arg_i32(L, 1) as u16;
    let val = arg_f32(L, 2) as u32;
    console().poke4(addr, val);
    0
}

/// `cstore(dest, src, len)` -- copy RAM to cart ROM snapshot
unsafe extern "C" fn api_cstore(L: *mut LuaState) -> i32 {
    let dest = arg_i32(L, 1) as u16;
    let src = arg_i32(L, 2) as u16;
    let len = arg_i32(L, 3) as u16;
    console().cstore(dest, src, len);
    0
}

/// `reload(dest, src, len)` -- copy cart ROM snapshot back to RAM
unsafe extern "C" fn api_reload(L: *mut LuaState) -> i32 {
    let dest = arg_i32(L, 1) as u16;
    let src = arg_i32(L, 2) as u16;
    let len = arg_i32(L, 3) as u16;
    console().reload(dest, src, len);
    0
}

/// `stat(n)` -- system status query
///
/// Returns useful values for common stat() queries including audio state.
unsafe extern "C" fn api_stat(L: *mut LuaState) -> i32 {
    let n = arg_i32(L, 1);
    match n {
        // stat(0): memory usage (fake value)
        0 => { lua_pushnumber(L, 64.0); 1 }
        // stat(1): CPU usage
        1 => { lua_pushnumber(L, 0.0); 1 }
        // stat(4): clipboard (empty string)
        4 => { lua_pushstring(L, c"".as_ptr()); 1 }
        // stat(6): parameter string (empty string)
        6 => { lua_pushstring(L, c"".as_ptr()); 1 }
        // stat(7): target FPS
        7 => { lua_pushnumber(L, 30.0); 1 }
        // stat(16-19): SFX currently playing on channel 0-3
        16..=19 => {
            let ch = (n - 16) as usize;
            let val = audio().sfx_on_channel(ch);
            lua_pushnumber(L, val as f32);
            1
        }
        // stat(20-23): current note number on channel 0-3
        20..=23 => {
            let ch = (n - 20) as usize;
            let val = audio().note_on_channel(ch);
            lua_pushnumber(L, val as f32);
            1
        }
        // stat(24): current music pattern index
        24 => {
            let val = audio().current_music_pattern();
            lua_pushnumber(L, val as f32);
            1
        }
        // stat(26-29): SFX index on channel 0-3 (same as 16-19)
        26..=29 => {
            let ch = (n - 26) as usize;
            let val = audio().sfx_on_channel(ch);
            lua_pushnumber(L, val as f32);
            1
        }
        // stat(30): key pressed this frame (boolean-like, false on embedded)
        30 => { lua_pushboolean(L, console().key_pressed as i32); 1 }
        // stat(31): last key char (empty string on embedded)
        31 => {
            match console().last_key_char {
                Some(ch) => {
                    let mut buf = [0u8; 5]; // 4 bytes max UTF-8 + null terminator
                    let encoded = ch.encode_utf8(&mut buf[..4]);
                    let len = encoded.len();
                    buf[len] = 0; // null terminator
                    lua_pushstring(L, buf.as_ptr() as *const core::ffi::c_char);
                    1
                }
                None => { lua_pushstring(L, c"".as_ptr()); 1 }
            }
        }
        // stat(32): mouse x
        32 => { lua_pushnumber(L, console().mouse_x as f32); 1 }
        // stat(33): mouse y
        33 => { lua_pushnumber(L, console().mouse_y as f32); 1 }
        // stat(34): mouse buttons bitmask
        34 => { lua_pushnumber(L, console().mouse_btn as f32); 1 }
        // stat(46-49): SFX currently playing (alias for 16-19)
        46..=49 => {
            let ch = (n - 46) as usize;
            let val = audio().sfx_on_channel(ch);
            lua_pushnumber(L, val as f32);
            1
        }
        // stat(50-53): current note number (alias for 20-23)
        50..=53 => {
            let ch = (n - 50) as usize;
            let val = audio().note_on_channel(ch);
            lua_pushnumber(L, val as f32);
            1
        }
        _ => { lua_pushnumber(L, 0.0); 1 }
    }
}

/// `printh(str)` -- print to host console (no-op on embedded, could be debug UART)
unsafe extern "C" fn api_printh(_L: *mut LuaState) -> i32 {
    // No-op on embedded targets. If debug UART is available, could
    // output the string there. For now, silently ignore.
    0
}

/// `fillp([p])` -- set fill pattern for subsequent draw operations
unsafe extern "C" fn api_fillp(L: *mut LuaState) -> i32 {
    let p = opt_arg_f32(L, 1).unwrap_or(0.0) as f64;
    console().fillp(p);
    0
}

/// `tline(x0, y0, x1, y1, mx, my, mdx, mdy)` -- draw textured line
unsafe extern "C" fn api_tline(L: *mut LuaState) -> i32 {
    let x0  = arg_i32(L, 1);
    let y0  = arg_i32(L, 2);
    let x1  = arg_i32(L, 3);
    let y1  = arg_i32(L, 4);
    let mx  = opt_arg_f32(L, 5).unwrap_or(0.0) as f64;
    let my  = opt_arg_f32(L, 6).unwrap_or(0.0) as f64;
    let mdx = opt_arg_f32(L, 7).unwrap_or(0.125) as f64;
    let mdy = opt_arg_f32(L, 8).unwrap_or(0.0) as f64;
    console().tline(x0, y0, x1, y1, mx, my, mdx, mdy);
    0
}

/// `dget(index)` -- get persistent data value
unsafe extern "C" fn api_dget(L: *mut LuaState) -> i32 {
    let index = arg_i32(L, 1);
    let val = console().dget(index);
    lua_pushnumber(L, val as f32);
    1
}

/// `dset(index, value)` -- set persistent data value
unsafe extern "C" fn api_dset(L: *mut LuaState) -> i32 {
    let index = arg_i32(L, 1);
    let value = arg_f32(L, 2) as f64;
    console().dset(index, value);
    0
}

/// `menuitem(index, label, callback)` -- MVP no-op
unsafe extern "C" fn api_menuitem(_L: *mut LuaState) -> i32 {
    0
}

/// `extcmd(cmd)` -- MVP no-op on embedded
unsafe extern "C" fn api_extcmd(_L: *mut LuaState) -> i32 {
    0
}

/// `cartdata(id)` -- MVP no-op (persistent data works in-session)
unsafe extern "C" fn api_cartdata(_L: *mut LuaState) -> i32 {
    0
}

// ---------------------------------------------------------------------------
// System functions (flip, run, stop, resume)
// ---------------------------------------------------------------------------

/// `load(filename, [breadcrumb], [param])` -- Load another cart.
/// No-op on embedded target (only one baked-in cart).
unsafe extern "C" fn api_load(_L: *mut LuaState) -> i32 {
    0
}

/// `flip()` -- Force screen update. No-op on embedded (screen updates at end of frame).
unsafe extern "C" fn api_flip(_L: *mut LuaState) -> i32 {
    0
}

/// `run()` -- Restart the cart. MVP no-op stub.
unsafe extern "C" fn api_run(_L: *mut LuaState) -> i32 {
    0
}

/// `stop()` -- Return to editor. MVP no-op stub.
unsafe extern "C" fn api_stop(_L: *mut LuaState) -> i32 {
    0
}

/// `resume()` -- Resume from stop. MVP no-op stub.
unsafe extern "C" fn api_resume(_L: *mut LuaState) -> i32 {
    0
}

// ---------------------------------------------------------------------------
// Table helpers
// ---------------------------------------------------------------------------

/// `add(tbl, val)` -- append value to end of table; returns val
unsafe extern "C" fn api_add(L: *mut LuaState) -> i32 {
    if lua_isnoneornil(L, 1) {
        return 0;
    }
    let len = lua_rawlen(L, 1) as i64;
    // Duplicate val (arg 2) onto the top of the stack so rawseti can consume it.
    lua_pushvalue(L, 2);
    // tbl[len+1] = val  (pops the duplicated value)
    lua_rawseti(L, 1, len + 1);
    // Return the original val (still at position 2)
    lua_pushvalue(L, 2);
    1
}

/// `del(tbl, val)` -- remove first occurrence of val from sequential table
///
/// This is a placeholder C stub; the real implementation is overridden by the
/// pure-Lua version registered in `register_api`, which can perform correct
/// value comparison and `table.remove` shifting.
unsafe extern "C" fn api_del(L: *mut LuaState) -> i32 {
    // Overridden by pure Lua in register_api. This stub is a no-op fallback.
    let _ = L;
    0
}

/// `count(tbl)` -- return #tbl (length of sequence part)
unsafe extern "C" fn api_count(L: *mut LuaState) -> i32 {
    if lua_isnoneornil(L, 1) {
        lua_pushinteger(L, 0);
        return 1;
    }
    let len = lua_rawlen(L, 1);
    lua_pushinteger(L, len as i32);
    1
}

// ============================================================================
// Part 5: Registration
// ============================================================================

/// Register all PICO-8 API functions as Lua globals.
///
/// # Safety
/// Must be called with a valid Lua state.
pub unsafe fn register_api(L: *mut LuaState) {
    // -- Drawing --
    lua_register(L, c"cls".as_ptr(), api_cls);
    lua_register(L, c"pset".as_ptr(), api_pset);
    lua_register(L, c"pget".as_ptr(), api_pget);
    lua_register(L, c"line".as_ptr(), api_line);
    lua_register(L, c"rect".as_ptr(), api_rect);
    lua_register(L, c"rectfill".as_ptr(), api_rectfill);
    lua_register(L, c"circ".as_ptr(), api_circ);
    lua_register(L, c"circfill".as_ptr(), api_circfill);
    lua_register(L, c"oval".as_ptr(), api_oval);
    lua_register(L, c"ovalfill".as_ptr(), api_ovalfill);
    lua_register(L, c"cursor".as_ptr(), api_cursor);
    lua_register(L, c"spr".as_ptr(), api_spr);
    lua_register(L, c"sspr".as_ptr(), api_sspr);
    lua_register(L, c"print".as_ptr(), api_print);
    lua_register(L, c"camera".as_ptr(), api_camera);
    lua_register(L, c"clip".as_ptr(), api_clip);
    lua_register(L, c"pal".as_ptr(), api_pal);
    lua_register(L, c"palt".as_ptr(), api_palt);
    lua_register(L, c"color".as_ptr(), api_color);
    lua_register(L, c"fillp".as_ptr(), api_fillp);
    lua_register(L, c"tline".as_ptr(), api_tline);

    // -- Sprites / Map --
    lua_register(L, c"sget".as_ptr(), api_sget);
    lua_register(L, c"sset".as_ptr(), api_sset);
    lua_register(L, c"map".as_ptr(), api_map);
    lua_register(L, c"mget".as_ptr(), api_mget);
    lua_register(L, c"mset".as_ptr(), api_mset);
    lua_register(L, c"fget".as_ptr(), api_fget);
    lua_register(L, c"fset".as_ptr(), api_fset);

    // -- Input --
    lua_register(L, c"btn".as_ptr(), api_btn);
    lua_register(L, c"btnp".as_ptr(), api_btnp);

    // -- Math --
    lua_register(L, c"sin".as_ptr(), api_sin);
    lua_register(L, c"cos".as_ptr(), api_cos);
    lua_register(L, c"atan2".as_ptr(), api_atan2);
    lua_register(L, c"sqrt".as_ptr(), api_sqrt);
    lua_register(L, c"abs".as_ptr(), api_abs);
    lua_register(L, c"flr".as_ptr(), api_flr);
    lua_register(L, c"ceil".as_ptr(), api_ceil);
    lua_register(L, c"min".as_ptr(), api_min);
    lua_register(L, c"max".as_ptr(), api_max);
    lua_register(L, c"mid".as_ptr(), api_mid);
    lua_register(L, c"sgn".as_ptr(), api_sgn);
    lua_register(L, c"rnd".as_ptr(), api_rnd);
    lua_register(L, c"band".as_ptr(), api_band);
    lua_register(L, c"bor".as_ptr(), api_bor);
    lua_register(L, c"bxor".as_ptr(), api_bxor);
    lua_register(L, c"bnot".as_ptr(), api_bnot);
    lua_register(L, c"shl".as_ptr(), api_shl);
    lua_register(L, c"shr".as_ptr(), api_shr);
    lua_register(L, c"lshr".as_ptr(), api_lshr);
    lua_register(L, c"rotl".as_ptr(), api_rotl);
    lua_register(L, c"rotr".as_ptr(), api_rotr);

    // -- Sound --
    lua_register(L, c"sfx".as_ptr(), api_sfx);
    lua_register(L, c"music".as_ptr(), api_music);

    // -- Memory --
    lua_register(L, c"peek".as_ptr(), api_peek);
    lua_register(L, c"peek2".as_ptr(), api_peek2);
    lua_register(L, c"peek4".as_ptr(), api_peek4);
    lua_register(L, c"poke".as_ptr(), api_poke);
    lua_register(L, c"poke2".as_ptr(), api_poke2);
    lua_register(L, c"poke4".as_ptr(), api_poke4);
    lua_register(L, c"memcpy".as_ptr(), api_memcpy);
    lua_register(L, c"memset".as_ptr(), api_memset);
    lua_register(L, c"cstore".as_ptr(), api_cstore);
    lua_register(L, c"reload".as_ptr(), api_reload);

    // -- Utility --
    lua_register(L, c"time".as_ptr(), api_time);
    lua_register(L, c"t".as_ptr(), api_time); // alias
    lua_register(L, c"stat".as_ptr(), api_stat);
    lua_register(L, c"printh".as_ptr(), api_printh);
    lua_register(L, c"dget".as_ptr(), api_dget);
    lua_register(L, c"dset".as_ptr(), api_dset);
    lua_register(L, c"menuitem".as_ptr(), api_menuitem);
    lua_register(L, c"extcmd".as_ptr(), api_extcmd);
    lua_register(L, c"cartdata".as_ptr(), api_cartdata);

    // -- System --
    lua_register(L, c"load".as_ptr(), api_load);
    lua_register(L, c"flip".as_ptr(), api_flip);
    lua_register(L, c"run".as_ptr(), api_run);
    lua_register(L, c"stop".as_ptr(), api_stop);
    lua_register(L, c"resume".as_ptr(), api_resume);

    // -- Table helpers --
    lua_register(L, c"add".as_ptr(), api_add);
    lua_register(L, c"del".as_ptr(), api_del);
    lua_register(L, c"count".as_ptr(), api_count);

    // Register `del` and `all` as Lua functions (easier to implement correctly
    // in Lua since they require table value comparison and iterators).
    let del_lua = c"function del(t,v)\n  if t==nil then return end\n  for i=1,#t do\n    if t[i]==v then\n      table.remove(t,i)\n      return v\n    end\n  end\nend";
    luaL_loadstring(L, del_lua.as_ptr());
    lua_pcall(L, 0, 0, 0);

    // PICO-8's `all()` iterator
    let all_lua = c"function all(t)\n  if t==nil then return function() end end\n  local i=0\n  local n=#t\n  return function()\n    i=i+1\n    if i<=n then return t[i] end\n  end\nend";
    luaL_loadstring(L, all_lua.as_ptr());
    lua_pcall(L, 0, 0, 0);

    // PICO-8's `foreach()`
    let foreach_lua = c"function foreach(t,f)\n  if t==nil then return end\n  for i=1,#t do\n    f(t[i])\n  end\nend";
    luaL_loadstring(L, foreach_lua.as_ptr());
    lua_pcall(L, 0, 0, 0);

    // PICO-8's `sub()` (string.sub alias)
    let sub_lua = c"function sub(s,i,j)\n  if j then return string.sub(s,i,j) end\n  return string.sub(s,i)\nend";
    luaL_loadstring(L, sub_lua.as_ptr());
    lua_pcall(L, 0, 0, 0);

    // PICO-8's `tostr()` and `tonum()`
    let tostr_lua = c"function tostr(v)\n  if v==nil then return 'nil' end\n  return tostring(v)\nend";
    luaL_loadstring(L, tostr_lua.as_ptr());
    lua_pcall(L, 0, 0, 0);

    let tonum_lua = c"function tonum(v)\n  return tonumber(v)\nend";
    luaL_loadstring(L, tonum_lua.as_ptr());
    lua_pcall(L, 0, 0, 0);

    // chr(n) -- character from code point
    let chr_lua = c"function chr(n)\n  return string.char(n)\nend";
    luaL_loadstring(L, chr_lua.as_ptr());
    lua_pcall(L, 0, 0, 0);

    // ord(s, [i]) -- code point from character
    let ord_lua = c"function ord(s,i)\n  if s==nil then return nil end\n  i=i or 1\n  return string.byte(s,i)\nend";
    luaL_loadstring(L, ord_lua.as_ptr());
    lua_pcall(L, 0, 0, 0);

    // split(s, [sep], [convert]) -- split string
    let split_lua = c"function split(s,sep,convert)\n  if s==nil then return nil end\n  sep=sep or ','\n  if convert==nil then convert=true end\n  local t={}\n  for v in string.gmatch(s,'([^'..sep..']*)')do\n    if convert then\n      local n=tonumber(v)\n      if n then t[#t+1]=n else t[#t+1]=v end\n    else\n      t[#t+1]=v\n    end\n  end\n  return t\nend";
    luaL_loadstring(L, split_lua.as_ptr());
    lua_pcall(L, 0, 0, 0);

    // deli(tbl, [i]) -- delete element at index, shift remaining down
    let deli_lua = c"function deli(t,i)\n  if t==nil then return nil end\n  i=i or #t\n  if i<1 or i>#t then return nil end\n  return table.remove(t,i)\nend";
    luaL_loadstring(L, deli_lua.as_ptr());
    lua_pcall(L, 0, 0, 0);

    // PICO-8 uses `#` as a shorthand for print; not handled here since it's
    // preprocessed by lua_preprocess.
}

// ============================================================================
// Part 6: Lua Lifecycle Helpers
// ============================================================================

/// Create a new Lua state with the embedded allocator.
///
/// # Safety
/// The returned pointer must eventually be passed to `lua_close`.
pub unsafe fn new_lua_state() -> *mut LuaState {
    lua_newstate(lua_alloc_fn, core::ptr::null_mut())
}

/// Load and execute a Lua code string. Returns 0 on success.
///
/// The string does not need to be null-terminated; this function adds the
/// null terminator internally.
///
/// # Safety
/// `L` must be a valid Lua state.
pub unsafe fn load_and_run(L: *mut LuaState, code: &str) -> i32 {
    // Null-terminate the code string for the C API
    let mut buf = alloc::vec::Vec::with_capacity(code.len() + 1);
    buf.extend_from_slice(code.as_bytes());
    buf.push(0);

    let result = luaL_loadstring(L, buf.as_ptr() as *const core::ffi::c_char);
    if result != 0 {
        // Load error — error message is on the stack. Pop it.
        lua_pop(L, 1);
        return result;
    }
    let pcall_result = lua_pcall(L, 0, 0, 0);
    if pcall_result != 0 {
        // Runtime error — pop the error message.
        lua_pop(L, 1);
    }
    pcall_result
}

/// Call a global Lua function by name (e.g., `_init`, `_update`, `_draw`).
///
/// Returns 0 on success, or if the function does not exist (which is not an
/// error — PICO-8 callbacks are optional). Returns a non-zero Lua error code
/// if the function exists but throws an error.
///
/// # Safety
/// `L` must be a valid Lua state. `name` must be a null-terminated byte string.
pub unsafe fn call_global(L: *mut LuaState, name: &[u8]) -> i32 {
    lua_getglobal(L, name.as_ptr() as *const core::ffi::c_char);
    if lua_isnone(L, -1) || lua_isnil(L, -1) {
        lua_pop(L, 1);
        return 0; // Function doesn't exist — not an error
    }
    let result = lua_pcall(L, 0, 0, 0);
    if result != 0 {
        // Runtime error — pop the error message
        lua_pop(L, 1);
    }
    result
}
