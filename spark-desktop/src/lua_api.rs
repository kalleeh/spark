//! Lua API layer for the Spark fantasy console (desktop target).
//!
//! Registers all PICO-8 compatible Lua functions against an `mlua::Lua` state.
//! Console and Audio state are shared via `Lua::set_app_data` / `app_data_mut`.

use mlua::prelude::*;
use std::f64::consts::TAU;

use spark_core::audio::Audio;
use spark_core::console::Console;
use spark_core::game_state::GameState;
use spark_core::lua_preprocess::preprocess_pico8;

// ---------------------------------------------------------------------------
// Load-cart signal: used by load() to communicate back to the main loop
// ---------------------------------------------------------------------------

/// Prefix used in the special error message that signals a cart load request.
/// The main loop checks for this prefix to distinguish a load() call from a
/// real Lua error.
pub const LOAD_CART_PREFIX: &str = "__SPARK_LOAD_CART__:";

/// Wrapper type stored in Lua app_data to hold the breadcrumb string
/// passed by `load(filename, breadcrumb)` and accessible via `stat(6)`.
#[derive(Clone, Default)]
pub struct Breadcrumb(pub String);

/// Parse a load-cart signal error message.
///
/// If `msg` contains the `LOAD_CART_PREFIX`, returns `Some((filename, breadcrumb))`.
/// Otherwise returns `None`.
pub fn parse_load_signal(msg: &str) -> Option<(String, String)> {
    if let Some(rest) = msg.split(LOAD_CART_PREFIX).nth(1) {
        // Find the first occurrence of the prefix payload (it may be wrapped
        // in additional error context by mlua).  The payload format is:
        //   filename|breadcrumb
        // where breadcrumb may be empty.
        let payload = rest.lines().next().unwrap_or(rest);
        let mut parts = payload.splitn(2, '|');
        let filename = parts.next().unwrap_or("").to_string();
        let breadcrumb = parts.next().unwrap_or("").to_string();
        Some((filename, breadcrumb))
    } else {
        None
    }
}

// ---------------------------------------------------------------------------
// Helpers for pulling numeric values out of Lua multi-values
// ---------------------------------------------------------------------------

/// Convert a Lua value to f64, returning `None` for nil/none.
fn val_to_f64(v: &LuaValue) -> Option<f64> {
    match v {
        LuaValue::Integer(n) => Some(*n as f64),
        LuaValue::Number(n) => Some(*n),
        LuaValue::Boolean(b) => Some(if *b { 1.0 } else { 0.0 }),
        LuaValue::String(s) => s.to_str().ok().and_then(|s| s.parse::<f64>().ok()),
        _ => None,
    }
}

fn val_to_i32(v: &LuaValue) -> Option<i32> {
    val_to_f64(v).map(|n| n.floor() as i32)
}

fn val_to_u8(v: &LuaValue) -> Option<u8> {
    val_to_f64(v).map(|n| (n.floor() as i32 & 0xFF) as u8)
}

fn val_to_bool(v: &LuaValue) -> bool {
    match v {
        LuaValue::Nil => false,
        LuaValue::Boolean(b) => *b,
        _ => true,
    }
}

/// Convert a Lua value to a display string (used by `print`, `printh`, `tostr`).
fn val_to_display(v: &LuaValue) -> String {
    match v {
        LuaValue::String(s) => s.to_str().map(|s| s.to_string()).unwrap_or_default(),
        LuaValue::Integer(n) => n.to_string(),
        LuaValue::Number(n) => format!("{n}"),
        LuaValue::Boolean(b) => (if *b { "true" } else { "false" }).to_string(),
        LuaValue::Nil => "nil".to_string(),
        _ => String::new(),
    }
}

/// Common error for missing Console.
const CONSOLE_ERR: &str = "Console not available";
/// Common error for missing Audio.
const AUDIO_ERR: &str = "Audio not available";

/// Borrow the Console from Lua app_data (mutable).
macro_rules! get_console_mut {
    ($lua:expr) => {
        $lua.app_data_mut::<Console>()
            .ok_or_else(|| mlua::Error::runtime(CONSOLE_ERR))
    };
}

/// Borrow the Console from Lua app_data (read-only).
macro_rules! get_console_ref {
    ($lua:expr) => {
        $lua.app_data_ref::<Console>()
            .ok_or_else(|| mlua::Error::runtime(CONSOLE_ERR))
    };
}

/// Borrow the Audio engine from Lua app_data (mutable).
macro_rules! get_audio_mut {
    ($lua:expr) => {
        $lua.app_data_mut::<Audio>()
            .ok_or_else(|| mlua::Error::runtime(AUDIO_ERR))
    };
}

/// Borrow the Audio engine from Lua app_data (read-only).
macro_rules! get_audio_ref {
    ($lua:expr) => {
        $lua.app_data_ref::<Audio>()
            .ok_or_else(|| mlua::Error::runtime(AUDIO_ERR))
    };
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Create and configure a new Lua state with all PICO-8 API functions registered.
pub fn create_lua() -> LuaResult<Lua> {
    let lua = Lua::new();

    // Insert initial GameState and Breadcrumb
    lua.set_app_data(GameState::new());
    lua.set_app_data(Breadcrumb::default());

    // Register every function into the Lua globals table.
    register_graphics(&lua)?;
    register_map(&lua)?;
    register_input(&lua)?;
    register_math(&lua)?;
    register_sound(&lua)?;
    register_memory(&lua)?;
    register_utility(&lua)?;
    register_system(&lua)?;

    Ok(lua)
}

/// Load and execute cart code (the `__lua__` section).
pub fn load_code(lua: &Lua, code: &str) -> LuaResult<()> {
    let processed = preprocess_pico8(code);
    match lua.load(&processed).set_name("cart").exec() {
        Ok(()) => Ok(()),
        Err(e) => {
            let err_str = e.to_string();
            eprintln!("Lua error: {}", err_str);
            let lines: Vec<&str> = processed.lines().collect();
            let mut found_line = None;
            if let Some(bracket_pos) = err_str.find("]:") {
                let after = &err_str[bracket_pos + 2..];
                if let Some(colon_pos) = after.find(':') {
                    if let Ok(n) = after[..colon_pos].parse::<usize>() {
                        found_line = Some(n);
                    }
                }
            }
            if let Some(line_num) = found_line {
                let start = if line_num > 5 { line_num - 5 } else { 0 };
                let end = (line_num + 5).min(lines.len());
                eprintln!("--- Context around line {} (total {} lines) ---", line_num, lines.len());
                for i in start..end {
                    let marker = if i + 1 == line_num { ">>>" } else { "   " };
                    eprintln!("{} {:4}: {}", marker, i + 1, lines[i]);
                }
                eprintln!("---");
            }
            eprintln!("--- Full preprocessed code (lines 550-570) ---");
            for (i, line) in lines.iter().enumerate() {
                if i >= 549 && i < 570 {
                    eprintln!("{:4}: {}", i + 1, line);
                }
            }
            Err(e)
        }
    }
}

/// Call `_init()` if it exists in the Lua environment.
pub fn call_init(lua: &Lua) -> LuaResult<()> {
    let globals = lua.globals();
    if let Ok(func) = globals.get::<LuaFunction>("_init") {
        func.call::<()>(())?;
    }
    Ok(())
}

/// Call `_update()` if it exists.
/// Also increments the frame counter used by `time()` / `t()`.
pub fn call_update(lua: &Lua) -> LuaResult<()> {
    {
        let mut gs = lua.app_data_mut::<GameState>()
            .expect("GameState missing from app_data");
        gs.frame_count += 1;
    }

    let globals = lua.globals();
    if let Ok(func) = globals.get::<LuaFunction>("_update") {
        if let Err(e) = func.call::<()>(()) {
            return Err(add_traceback(lua, e));
        }
    }
    Ok(())
}

/// Call `_update60()` if it exists.
/// Also increments the frame counter used by `time()` / `t()`.
pub fn call_update60(lua: &Lua) -> LuaResult<()> {
    {
        let mut gs = lua.app_data_mut::<GameState>()
            .expect("GameState missing from app_data");
        gs.frame_count += 1;
    }

    let globals = lua.globals();
    if let Ok(func) = globals.get::<LuaFunction>("_update60") {
        if let Err(e) = func.call::<()>(()) {
            return Err(add_traceback(lua, e));
        }
    }
    Ok(())
}

/// Check whether the cart defines `_update60` as a global function.
/// Used by the main loop to decide between 30fps and 60fps mode.
pub fn has_update60(lua: &Lua) -> bool {
    lua.globals().get::<LuaValue>("_update60")
        .map(|v| v.is_function())
        .unwrap_or(false)
}

fn add_traceback(lua: &Lua, err: mlua::Error) -> mlua::Error {
    if let Ok(debug_lib) = lua.globals().get::<LuaTable>("debug") {
        if let Ok(tb_func) = debug_lib.get::<LuaFunction>("traceback") {
            if let Ok(tb) = tb_func.call::<String>(format!("{}", err)) {
                return mlua::Error::runtime(tb);
            }
        }
    }
    err
}

/// Call `_draw()` if it exists.
pub fn call_draw(lua: &Lua) -> LuaResult<()> {
    let globals = lua.globals();
    if let Ok(func) = globals.get::<LuaFunction>("_draw") {
        func.call::<()>(())?;
    }
    Ok(())
}

// ===========================================================================
//  Graphics functions
// ===========================================================================

fn register_graphics(lua: &Lua) -> LuaResult<()> {
    let globals = lua.globals();

    globals.set("cls", lua.create_function(|lua, args: LuaMultiValue| {
        let col = args.get(0).and_then(val_to_u8);
        let mut con = get_console_mut!(lua)?;
        con.cls(col);
        Ok(())
    })?)?;

    globals.set("pset", lua.create_function(|lua, args: LuaMultiValue| {
        let x = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let y = args.get(1).and_then(val_to_i32).unwrap_or(0);
        let col = args.get(2).and_then(val_to_u8);
        let mut con = get_console_mut!(lua)?;
        con.pset(x, y, col);
        Ok(())
    })?)?;

    globals.set("pget", lua.create_function(|lua, args: LuaMultiValue| {
        let x = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let y = args.get(1).and_then(val_to_i32).unwrap_or(0);
        let con = get_console_ref!(lua)?;
        Ok(con.pget(x, y))
    })?)?;

    globals.set("line", lua.create_function(|lua, args: LuaMultiValue| {
        let x0 = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let y0 = args.get(1).and_then(val_to_i32).unwrap_or(0);
        let x1 = args.get(2).and_then(val_to_i32).unwrap_or(0);
        let y1 = args.get(3).and_then(val_to_i32).unwrap_or(0);
        let col = args.get(4).and_then(val_to_u8);
        let mut con = get_console_mut!(lua)?;
        con.line(x0, y0, x1, y1, col);
        Ok(())
    })?)?;

    globals.set("rect", lua.create_function(|lua, args: LuaMultiValue| {
        let x0 = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let y0 = args.get(1).and_then(val_to_i32).unwrap_or(0);
        let x1 = args.get(2).and_then(val_to_i32).unwrap_or(0);
        let y1 = args.get(3).and_then(val_to_i32).unwrap_or(0);
        let col = args.get(4).and_then(val_to_u8);
        let mut con = get_console_mut!(lua)?;
        con.rect(x0, y0, x1, y1, col);
        Ok(())
    })?)?;

    globals.set("rectfill", lua.create_function(|lua, args: LuaMultiValue| {
        let x0 = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let y0 = args.get(1).and_then(val_to_i32).unwrap_or(0);
        let x1 = args.get(2).and_then(val_to_i32).unwrap_or(0);
        let y1 = args.get(3).and_then(val_to_i32).unwrap_or(0);
        let col = args.get(4).and_then(val_to_u8);
        let mut con = get_console_mut!(lua)?;
        con.rectfill(x0, y0, x1, y1, col);
        Ok(())
    })?)?;

    globals.set("circ", lua.create_function(|lua, args: LuaMultiValue| {
        let cx = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let cy = args.get(1).and_then(val_to_i32).unwrap_or(0);
        let r  = args.get(2).and_then(val_to_i32).unwrap_or(4);
        let col = args.get(3).and_then(val_to_u8);
        let mut con = get_console_mut!(lua)?;
        con.circ(cx, cy, r, col);
        Ok(())
    })?)?;

    globals.set("circfill", lua.create_function(|lua, args: LuaMultiValue| {
        let cx = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let cy = args.get(1).and_then(val_to_i32).unwrap_or(0);
        let r  = args.get(2).and_then(val_to_i32).unwrap_or(4);
        let col = args.get(3).and_then(val_to_u8);
        let mut con = get_console_mut!(lua)?;
        con.circfill(cx, cy, r, col);
        Ok(())
    })?)?;

    globals.set("oval", lua.create_function(|lua, args: LuaMultiValue| {
        let x0  = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let y0  = args.get(1).and_then(val_to_i32).unwrap_or(0);
        let x1  = args.get(2).and_then(val_to_i32).unwrap_or(0);
        let y1  = args.get(3).and_then(val_to_i32).unwrap_or(0);
        let col = args.get(4).and_then(val_to_u8);
        let mut con = get_console_mut!(lua)?;
        con.oval(x0, y0, x1, y1, col);
        Ok(())
    })?)?;

    globals.set("ovalfill", lua.create_function(|lua, args: LuaMultiValue| {
        let x0  = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let y0  = args.get(1).and_then(val_to_i32).unwrap_or(0);
        let x1  = args.get(2).and_then(val_to_i32).unwrap_or(0);
        let y1  = args.get(3).and_then(val_to_i32).unwrap_or(0);
        let col = args.get(4).and_then(val_to_u8);
        let mut con = get_console_mut!(lua)?;
        con.ovalfill(x0, y0, x1, y1, col);
        Ok(())
    })?)?;

    globals.set("spr", lua.create_function(|lua, args: LuaMultiValue| {
        let n = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let x = args.get(1).and_then(val_to_i32).unwrap_or(0);
        let y = args.get(2).and_then(val_to_i32).unwrap_or(0);
        let w = args.get(3).and_then(val_to_f64).unwrap_or(1.0);
        let h = args.get(4).and_then(val_to_f64).unwrap_or(1.0);
        let flip_x = args.get(5).map_or(false, val_to_bool);
        let flip_y = args.get(6).map_or(false, val_to_bool);
        let mut con = get_console_mut!(lua)?;
        con.spr(n, x, y, w, h, flip_x, flip_y);
        Ok(())
    })?)?;

    globals.set("sspr", lua.create_function(|lua, args: LuaMultiValue| {
        let sx = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let sy = args.get(1).and_then(val_to_i32).unwrap_or(0);
        let sw = args.get(2).and_then(val_to_i32).unwrap_or(0);
        let sh = args.get(3).and_then(val_to_i32).unwrap_or(0);
        let dx = args.get(4).and_then(val_to_i32).unwrap_or(0);
        let dy = args.get(5).and_then(val_to_i32).unwrap_or(0);
        let dw = args.get(6).and_then(val_to_i32).unwrap_or(sw);
        let dh = args.get(7).and_then(val_to_i32).unwrap_or(sh);
        let flip_x = args.get(8).map_or(false, val_to_bool);
        let flip_y = args.get(9).map_or(false, val_to_bool);
        let mut con = get_console_mut!(lua)?;
        con.sspr(sx, sy, sw, sh, dx, dy, dw, dh, flip_x, flip_y);
        Ok(())
    })?)?;

    globals.set("sget", lua.create_function(|lua, args: LuaMultiValue| {
        let x = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let y = args.get(1).and_then(val_to_i32).unwrap_or(0);
        let con = get_console_ref!(lua)?;
        Ok(con.sget(x, y))
    })?)?;

    globals.set("sset", lua.create_function(|lua, args: LuaMultiValue| {
        let x   = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let y   = args.get(1).and_then(val_to_i32).unwrap_or(0);
        let col = args.get(2).and_then(val_to_u8).unwrap_or(0);
        let mut con = get_console_mut!(lua)?;
        con.sset(x, y, col);
        Ok(())
    })?)?;

    globals.set("print", lua.create_function(|lua, args: LuaMultiValue| {
        let text = args.get(0).map(val_to_display).unwrap_or_default();
        let x   = args.get(1).and_then(val_to_i32);
        let y   = args.get(2).and_then(val_to_i32);
        let col = args.get(3).and_then(val_to_u8);
        let mut con = get_console_mut!(lua)?;
        let result = con.print(&text, x, y, col);
        Ok(result)
    })?)?;

    globals.set("cursor", lua.create_function(|lua, args: LuaMultiValue| {
        let x   = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let y   = args.get(1).and_then(val_to_i32).unwrap_or(0);
        let col = args.get(2).and_then(val_to_u8);
        let mut con = get_console_mut!(lua)?;
        con.cursor(x, y);
        if let Some(c) = col {
            con.color(c);
        }
        Ok(())
    })?)?;

    globals.set("camera", lua.create_function(|lua, args: LuaMultiValue| {
        let x = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let y = args.get(1).and_then(val_to_i32).unwrap_or(0);
        let mut con = get_console_mut!(lua)?;
        con.camera(x, y);
        Ok(())
    })?)?;

    globals.set("clip", lua.create_function(|lua, args: LuaMultiValue| {
        let mut con = get_console_mut!(lua)?;
        if args.is_empty() || args.get(0).map_or(true, |v| matches!(v, LuaValue::Nil)) {
            con.clip_rect(0, 0, 128, 128);
        } else {
            let x = args.get(0).and_then(val_to_i32).unwrap_or(0);
            let y = args.get(1).and_then(val_to_i32).unwrap_or(0);
            let w = args.get(2).and_then(val_to_i32).unwrap_or(128);
            let h = args.get(3).and_then(val_to_i32).unwrap_or(128);
            con.clip_rect(x, y, w, h);
        }
        Ok(())
    })?)?;

    globals.set("pal", lua.create_function(|lua, args: LuaMultiValue| {
        let mut con = get_console_mut!(lua)?;
        if args.is_empty() || args.get(0).map_or(true, |v| matches!(v, LuaValue::Nil)) {
            con.pal_reset();
        } else {
            let c0 = args.get(0).and_then(val_to_u8).unwrap_or(0);
            let c1 = args.get(1).and_then(val_to_u8).unwrap_or(0);
            let p  = args.get(2).and_then(val_to_u8).unwrap_or(0);
            con.pal(c0, c1, p);
        }
        Ok(())
    })?)?;

    globals.set("palt", lua.create_function(|lua, args: LuaMultiValue| {
        let mut con = get_console_mut!(lua)?;
        if args.is_empty() || args.get(0).map_or(true, |v| matches!(v, LuaValue::Nil)) {
            con.palt_reset();
        } else {
            let c = args.get(0).and_then(val_to_u8).unwrap_or(0);
            let t = args.get(1).map_or(true, val_to_bool);
            con.palt(c, t);
        }
        Ok(())
    })?)?;

    globals.set("color", lua.create_function(|lua, args: LuaMultiValue| {
        let c = args.get(0).and_then(val_to_u8).unwrap_or(6);
        let mut con = get_console_mut!(lua)?;
        con.color(c);
        Ok(())
    })?)?;

    globals.set("fillp", lua.create_function(|lua, args: LuaMultiValue| {
        let p = args.get(0).and_then(val_to_f64).unwrap_or(0.0);
        let mut con = get_console_mut!(lua)?;
        con.fillp(p);
        Ok(())
    })?)?;

    globals.set("tline", lua.create_function(|lua, args: LuaMultiValue| {
        let x0  = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let y0  = args.get(1).and_then(val_to_i32).unwrap_or(0);
        let x1  = args.get(2).and_then(val_to_i32).unwrap_or(0);
        let y1  = args.get(3).and_then(val_to_i32).unwrap_or(0);
        let mx  = args.get(4).and_then(val_to_f64).unwrap_or(0.0);
        let my  = args.get(5).and_then(val_to_f64).unwrap_or(0.0);
        let mdx = args.get(6).and_then(val_to_f64).unwrap_or(0.125);
        let mdy = args.get(7).and_then(val_to_f64).unwrap_or(0.0);
        let mut con = get_console_mut!(lua)?;
        con.tline(x0, y0, x1, y1, mx, my, mdx, mdy);
        Ok(())
    })?)?;

    Ok(())
}

// ===========================================================================
//  Map functions
// ===========================================================================

fn register_map(lua: &Lua) -> LuaResult<()> {
    let globals = lua.globals();

    globals.set("map", lua.create_function(|lua, args: LuaMultiValue| {
        let cel_x = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let cel_y = args.get(1).and_then(val_to_i32).unwrap_or(0);
        let sx    = args.get(2).and_then(val_to_i32).unwrap_or(0);
        let sy    = args.get(3).and_then(val_to_i32).unwrap_or(0);
        let cel_w = args.get(4).and_then(val_to_i32).unwrap_or(128);
        let cel_h = args.get(5).and_then(val_to_i32).unwrap_or(64);
        let layer = args.get(6).and_then(val_to_u8).unwrap_or(0);
        let mut con = get_console_mut!(lua)?;
        con.map_draw(cel_x, cel_y, sx, sy, cel_w, cel_h, layer);
        Ok(())
    })?)?;

    globals.set("mget", lua.create_function(|lua, args: LuaMultiValue| {
        let x = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let y = args.get(1).and_then(val_to_i32).unwrap_or(0);
        let con = get_console_ref!(lua)?;
        Ok(con.mget(x, y))
    })?)?;

    globals.set("mset", lua.create_function(|lua, args: LuaMultiValue| {
        let x = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let y = args.get(1).and_then(val_to_i32).unwrap_or(0);
        let v = args.get(2).and_then(val_to_u8).unwrap_or(0);
        let mut con = get_console_mut!(lua)?;
        con.mset(x, y, v);
        Ok(())
    })?)?;

    globals.set("fget", lua.create_function(|lua, args: LuaMultiValue| {
        let n = args.get(0).and_then(val_to_f64).map(|v| v.floor() as usize).unwrap_or(0);
        let f = args.get(1).and_then(val_to_u8);
        let con = get_console_ref!(lua)?;
        Ok(con.fget(n, f))
    })?)?;

    globals.set("fset", lua.create_function(|lua, args: LuaMultiValue| {
        let n = args.get(0).and_then(val_to_f64).map(|v| v.floor() as usize).unwrap_or(0);
        let mut con = get_console_mut!(lua)?;

        if args.len() >= 3 {
            let f = args.get(1).and_then(val_to_u8);
            let v = args.get(2).and_then(val_to_u8).unwrap_or(0);
            con.fset(n, f, v);
        } else {
            let v = args.get(1).and_then(val_to_u8).unwrap_or(0);
            con.fset(n, None, v);
        }
        Ok(())
    })?)?;

    Ok(())
}

// ===========================================================================
//  Input functions
// ===========================================================================

fn register_input(lua: &Lua) -> LuaResult<()> {
    let globals = lua.globals();

    globals.set("btn", lua.create_function(|lua, args: LuaMultiValue| {
        let con = get_console_ref!(lua)?;
        // No-args form: return bitmask of all buttons for player 0
        if args.is_empty() || args.get(0).map_or(true, |v| matches!(v, LuaValue::Nil)) {
            let mut mask: u8 = 0;
            for bit in 0..6u8 {
                if con.btn(bit, 0) {
                    mask |= 1 << bit;
                }
            }
            return Ok(LuaValue::Integer(mask as i64));
        }
        let i = args.get(0).and_then(val_to_u8).unwrap_or(0);
        let p = args.get(1).and_then(val_to_u8).unwrap_or(0);
        Ok(LuaValue::Boolean(con.btn(i, p)))
    })?)?;

    globals.set("btnp", lua.create_function(|lua, args: LuaMultiValue| {
        let i = args.get(0).and_then(val_to_u8).unwrap_or(0);
        let p = args.get(1).and_then(val_to_u8).unwrap_or(0);
        let con = get_console_ref!(lua)?;
        Ok(con.btnp(i, p))
    })?)?;

    Ok(())
}

// ===========================================================================
//  Math functions  (PICO-8 conventions!)
// ===========================================================================

fn register_math(lua: &Lua) -> LuaResult<()> {
    let globals = lua.globals();

    globals.set("sin", lua.create_function(|_lua, args: LuaMultiValue| {
        let x = args.get(0).and_then(val_to_f64).unwrap_or(0.0);
        Ok(-(x * TAU).sin())
    })?)?;

    globals.set("cos", lua.create_function(|_lua, args: LuaMultiValue| {
        let x = args.get(0).and_then(val_to_f64).unwrap_or(0.0);
        Ok((x * TAU).cos())
    })?)?;

    globals.set("atan2", lua.create_function(|_lua, args: LuaMultiValue| {
        let dx = args.get(0).and_then(val_to_f64).unwrap_or(0.0);
        let dy = args.get(1).and_then(val_to_f64).unwrap_or(0.0);
        let a = f64::atan2(-dy, dx) / TAU;
        Ok(((a % 1.0) + 1.0) % 1.0)
    })?)?;

    globals.set("sqrt", lua.create_function(|_lua, args: LuaMultiValue| {
        let x = args.get(0).and_then(val_to_f64).unwrap_or(0.0);
        Ok(if x < 0.0 { 0.0 } else { x.sqrt() })
    })?)?;

    globals.set("abs", lua.create_function(|_lua, args: LuaMultiValue| {
        let x = args.get(0).and_then(val_to_f64).unwrap_or(0.0);
        Ok(x.abs())
    })?)?;

    globals.set("flr", lua.create_function(|_lua, args: LuaMultiValue| {
        let x = args.get(0).and_then(val_to_f64).unwrap_or(0.0);
        Ok(x.floor())
    })?)?;

    globals.set("ceil", lua.create_function(|_lua, args: LuaMultiValue| {
        let x = args.get(0).and_then(val_to_f64).unwrap_or(0.0);
        Ok(x.ceil())
    })?)?;

    globals.set("min", lua.create_function(|_lua, args: LuaMultiValue| {
        let a = args.get(0).and_then(val_to_f64).unwrap_or(0.0);
        let b = args.get(1).and_then(val_to_f64).unwrap_or(0.0);
        Ok(a.min(b))
    })?)?;

    globals.set("max", lua.create_function(|_lua, args: LuaMultiValue| {
        let a = args.get(0).and_then(val_to_f64).unwrap_or(0.0);
        let b = args.get(1).and_then(val_to_f64).unwrap_or(0.0);
        Ok(a.max(b))
    })?)?;

    globals.set("mid", lua.create_function(|_lua, args: LuaMultiValue| {
        let a = args.get(0).and_then(val_to_f64).unwrap_or(0.0);
        let b = args.get(1).and_then(val_to_f64).unwrap_or(0.0);
        let c = args.get(2).and_then(val_to_f64).unwrap_or(0.0);
        let mut vals = [a, b, c];
        vals.sort_by(|x, y| x.partial_cmp(y).unwrap_or(std::cmp::Ordering::Equal));
        Ok(vals[1])
    })?)?;

    globals.set("sgn", lua.create_function(|_lua, args: LuaMultiValue| {
        let x = args.get(0).and_then(val_to_f64).unwrap_or(0.0);
        Ok(if x < 0.0 { -1.0 } else { 1.0 })
    })?)?;

    globals.set("band", lua.create_function(|_lua, args: LuaMultiValue| {
        let a = args.get(0).and_then(val_to_f64).unwrap_or(0.0).floor() as i32;
        let b = args.get(1).and_then(val_to_f64).unwrap_or(0.0).floor() as i32;
        Ok((a & b) as f64)
    })?)?;

    globals.set("bor", lua.create_function(|_lua, args: LuaMultiValue| {
        let a = args.get(0).and_then(val_to_f64).unwrap_or(0.0).floor() as i32;
        let b = args.get(1).and_then(val_to_f64).unwrap_or(0.0).floor() as i32;
        Ok((a | b) as f64)
    })?)?;

    globals.set("bxor", lua.create_function(|_lua, args: LuaMultiValue| {
        let a = args.get(0).and_then(val_to_f64).unwrap_or(0.0).floor() as i32;
        let b = args.get(1).and_then(val_to_f64).unwrap_or(0.0).floor() as i32;
        Ok((a ^ b) as f64)
    })?)?;

    globals.set("bnot", lua.create_function(|_lua, args: LuaMultiValue| {
        let a = args.get(0).and_then(val_to_f64).unwrap_or(0.0).floor() as i32;
        Ok((!a) as f64)
    })?)?;

    globals.set("shl", lua.create_function(|_lua, args: LuaMultiValue| {
        let a = args.get(0).and_then(val_to_f64).unwrap_or(0.0).floor() as i32;
        let b = (args.get(1).and_then(val_to_f64).unwrap_or(0.0).floor() as u32) & 31;
        Ok((a << b) as f64)
    })?)?;

    globals.set("shr", lua.create_function(|_lua, args: LuaMultiValue| {
        let a = args.get(0).and_then(val_to_f64).unwrap_or(0.0).floor() as i32;
        let b = (args.get(1).and_then(val_to_f64).unwrap_or(0.0).floor() as u32) & 31;
        Ok((a >> b) as f64)
    })?)?;

    globals.set("lshr", lua.create_function(|_lua, args: LuaMultiValue| {
        let a = args.get(0).and_then(val_to_f64).unwrap_or(0.0).floor() as i32;
        let b = (args.get(1).and_then(val_to_f64).unwrap_or(0.0).floor() as u32) & 31;
        let bits = (a as u32) >> b;
        Ok(bits as i32 as f64)
    })?)?;

    globals.set("rotl", lua.create_function(|_lua, args: LuaMultiValue| {
        let a = args.get(0).and_then(val_to_f64).unwrap_or(0.0).floor() as i32 as u32;
        let b = (args.get(1).and_then(val_to_f64).unwrap_or(0.0).floor() as u32) & 31;
        let result = a.rotate_left(b);
        Ok(result as i32 as f64)
    })?)?;

    globals.set("rotr", lua.create_function(|_lua, args: LuaMultiValue| {
        let a = args.get(0).and_then(val_to_f64).unwrap_or(0.0).floor() as i32 as u32;
        let b = (args.get(1).and_then(val_to_f64).unwrap_or(0.0).floor() as u32) & 31;
        let result = a.rotate_right(b);
        Ok(result as i32 as f64)
    })?)?;

    globals.set("rnd", lua.create_function(|lua, args: LuaMultiValue| {
        let x = args.get(0).and_then(val_to_f64).unwrap_or(1.0);
        let mut gs = lua.app_data_mut::<GameState>()
            .ok_or_else(|| mlua::Error::runtime("GameState not available"))?;
        let r = gs.next_random();
        Ok(r * x)
    })?)?;

    Ok(())
}

// ===========================================================================
//  Sound functions
// ===========================================================================

fn register_sound(lua: &Lua) -> LuaResult<()> {
    let globals = lua.globals();

    globals.set("sfx", lua.create_function(|lua, args: LuaMultiValue| {
        let n       = args.get(0).and_then(val_to_i32).unwrap_or(-1);
        let channel = args.get(1).and_then(val_to_i32).unwrap_or(-1);
        let offset  = args.get(2).and_then(val_to_i32).unwrap_or(0);
        let length  = args.get(3).and_then(val_to_i32).unwrap_or(-1);
        let mut audio = get_audio_mut!(lua)?;
        audio.sfx(n, channel, offset, length);
        Ok(())
    })?)?;

    globals.set("music", lua.create_function(|lua, args: LuaMultiValue| {
        let n            = args.get(0).and_then(val_to_i32).unwrap_or(-1);
        let fade_len     = args.get(1).and_then(val_to_i32).unwrap_or(0);
        let channel_mask = args.get(2).and_then(val_to_i32).unwrap_or(0);
        let mut audio = get_audio_mut!(lua)?;
        audio.music(n, fade_len, channel_mask);
        Ok(())
    })?)?;

    Ok(())
}

// ===========================================================================
//  Memory-mapped I/O functions
// ===========================================================================

fn register_memory(lua: &Lua) -> LuaResult<()> {
    let globals = lua.globals();

    globals.set("peek", lua.create_function(|lua, args: LuaMultiValue| {
        let addr = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let con = get_console_ref!(lua)?;
        Ok(con.peek(addr as u16) as f64)
    })?)?;

    globals.set("poke", lua.create_function(|lua, args: LuaMultiValue| {
        let addr = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let val  = args.get(1).and_then(val_to_u8).unwrap_or(0);
        let mut con = get_console_mut!(lua)?;
        con.poke(addr as u16, val);
        Ok(())
    })?)?;

    globals.set("peek2", lua.create_function(|lua, args: LuaMultiValue| {
        let addr = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let con = get_console_ref!(lua)?;
        Ok(con.peek2(addr as u16) as f64)
    })?)?;

    globals.set("peek4", lua.create_function(|lua, args: LuaMultiValue| {
        let addr = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let con = get_console_ref!(lua)?;
        Ok(con.peek4(addr as u16) as f64)
    })?)?;

    globals.set("poke2", lua.create_function(|lua, args: LuaMultiValue| {
        let addr = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let val  = args.get(1).and_then(val_to_i32).unwrap_or(0);
        let mut con = get_console_mut!(lua)?;
        con.poke2(addr as u16, val as u16);
        Ok(())
    })?)?;

    globals.set("poke4", lua.create_function(|lua, args: LuaMultiValue| {
        let addr = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let val  = args.get(1).and_then(val_to_f64).unwrap_or(0.0);
        let mut con = get_console_mut!(lua)?;
        con.poke4(addr as u16, val as u32);
        Ok(())
    })?)?;

    globals.set("memcpy", lua.create_function(|lua, args: LuaMultiValue| {
        let dest = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let src  = args.get(1).and_then(val_to_i32).unwrap_or(0);
        let len  = args.get(2).and_then(val_to_i32).unwrap_or(0);
        let mut con = get_console_mut!(lua)?;
        con.memcpy(dest as u16, src as u16, len as u16);
        Ok(())
    })?)?;

    globals.set("memset", lua.create_function(|lua, args: LuaMultiValue| {
        let dest = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let val  = args.get(1).and_then(val_to_u8).unwrap_or(0);
        let len  = args.get(2).and_then(val_to_i32).unwrap_or(0);
        let mut con = get_console_mut!(lua)?;
        con.memset(dest as u16, val, len as u16);
        Ok(())
    })?)?;

    globals.set("cstore", lua.create_function(|lua, args: LuaMultiValue| {
        let dest = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let src  = args.get(1).and_then(val_to_i32).unwrap_or(0);
        let len  = args.get(2).and_then(val_to_i32).unwrap_or(0);
        let mut con = get_console_mut!(lua)?;
        con.cstore(dest as u16, src as u16, len as u16);
        Ok(())
    })?)?;

    globals.set("reload", lua.create_function(|lua, args: LuaMultiValue| {
        let dest = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let src  = args.get(1).and_then(val_to_i32).unwrap_or(0);
        let len  = args.get(2).and_then(val_to_i32).unwrap_or(0);
        let mut con = get_console_mut!(lua)?;
        con.reload(dest as u16, src as u16, len as u16);
        Ok(())
    })?)?;

    Ok(())
}

// ===========================================================================
//  Utility functions
// ===========================================================================

fn register_utility(lua: &Lua) -> LuaResult<()> {
    let globals = lua.globals();

    globals.set("add", lua.create_function(|_lua, (tbl, val): (LuaTable, LuaValue)| {
        let len = tbl.raw_len();
        tbl.raw_set(len + 1, val)?;
        Ok(())
    })?)?;

    globals.set("del", lua.create_function(|_lua, (tbl, val): (LuaTable, LuaValue)| {
        let len = tbl.raw_len() as i64;
        let mut found_idx: Option<i64> = None;
        for i in 1..=len {
            let v: LuaValue = tbl.raw_get(i)?;
            if v == val {
                found_idx = Some(i);
                break;
            }
        }
        if let Some(idx) = found_idx {
            for i in idx..len {
                let next: LuaValue = tbl.raw_get(i + 1)?;
                tbl.raw_set(i, next)?;
            }
            tbl.raw_set(len, LuaValue::Nil)?;
        }
        Ok(())
    })?)?;

    globals.set("count", lua.create_function(|_lua, tbl: LuaValue| {
        match tbl {
            LuaValue::Table(t) => Ok(t.raw_len() as i64),
            _ => Ok(0i64),
        }
    })?)?;

    globals.set("foreach", lua.create_function(|_lua, (tbl, func): (LuaTable, LuaFunction)| {
        let len = tbl.raw_len();
        for i in 1..=len {
            let v: LuaValue = tbl.raw_get(i)?;
            func.call::<()>(v)?;
        }
        Ok(())
    })?)?;

    globals.set("all", lua.create_function(|lua, tbl: LuaValue| {
        let tbl = match tbl {
            LuaValue::Table(t) => t,
            _ => return lua.pack_multi((LuaValue::Nil,)),
        };

        let state = lua.create_table()?;
        state.raw_set("t", tbl)?;
        state.raw_set("i", 0i64)?;

        let iter = lua.create_function(|_lua, args: LuaMultiValue| {
            let st = match args.get(0) {
                Some(LuaValue::Table(t)) => t.clone(),
                _ => return Ok(LuaValue::Nil),
            };
            let i: i64 = st.raw_get("i").unwrap_or(0);
            let tbl: LuaTable = match st.raw_get::<LuaTable>("t") {
                Ok(t) => t,
                Err(_) => return Ok(LuaValue::Nil),
            };
            let next_i = i + 1;
            let len = tbl.raw_len() as i64;
            if next_i > len {
                return Ok(LuaValue::Nil);
            }
            st.raw_set("i", next_i).ok();
            let val: LuaValue = tbl.raw_get(next_i).unwrap_or(LuaValue::Nil);
            Ok(val)
        })?;

        lua.pack_multi((iter, state, LuaValue::Nil))
    })?)?;

    globals.set("sub", lua.create_function(|_lua, args: LuaMultiValue| {
        let s_str: String = match args.get(0) {
            Some(LuaValue::String(s)) => s.to_str().map(|s| s.to_string()).unwrap_or_default(),
            Some(v) => val_to_f64(v).map(|n| format!("{n}")).unwrap_or_default(),
            None => return Ok(String::new()),
        };
        let bytes = s_str.as_bytes();
        let len = bytes.len() as i64;
        let start = args.get(1).and_then(val_to_i32).unwrap_or(1) as i64;
        let end: Option<i64> = args.get(2).and_then(val_to_i32).map(|n| n as i64);

        let start_idx = if start >= 1 {
            (start - 1) as usize
        } else if start < 0 {
            let idx = len + start;
            if idx < 0 { 0usize } else { idx as usize }
        } else {
            0usize
        };

        let end_idx = match end {
            Some(e) if e >= 0 => (e as usize).min(len as usize),
            Some(e) => {
                let idx = len + e + 1;
                if idx < 0 { 0usize } else { idx as usize }
            }
            None => len as usize,
        };

        if start_idx >= end_idx || start_idx >= bytes.len() {
            return Ok(String::new());
        }

        let slice = &bytes[start_idx..end_idx.min(bytes.len())];
        Ok(String::from_utf8_lossy(slice).into_owned())
    })?)?;

    globals.set("tostr", lua.create_function(|_lua, val: LuaValue| {
        let s = match &val {
            LuaValue::Nil => "".to_string(),
            LuaValue::Boolean(b) => (if *b { "true" } else { "false" }).to_string(),
            LuaValue::Integer(n) => n.to_string(),
            LuaValue::Number(n) => {
                if *n == n.floor() && n.abs() < 1e15 {
                    format!("{}", *n as i64)
                } else {
                    format!("{n}")
                }
            }
            LuaValue::String(s) => s.to_str().map(|s| s.to_string()).unwrap_or_default(),
            _ => "[table]".to_string(),
        };
        Ok(s)
    })?)?;

    globals.set("tonum", lua.create_function(|_lua, val: LuaValue| {
        let result = match &val {
            LuaValue::Integer(n) => Some(*n as f64),
            LuaValue::Number(n) => Some(*n),
            LuaValue::Boolean(b) => Some(if *b { 1.0 } else { 0.0 }),
            LuaValue::String(s) => {
                s.to_str().ok().and_then(|st| st.trim().parse::<f64>().ok())
            }
            _ => None,
        };
        Ok(result)
    })?)?;

    globals.set("printh", lua.create_function(|_lua, args: LuaMultiValue| {
        let text = args.get(0).map(val_to_display).unwrap_or_default();
        eprintln!("{text}");
        Ok(())
    })?)?;

    globals.set("dget", lua.create_function(|lua, args: LuaMultiValue| {
        let index = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let con = get_console_ref!(lua)?;
        Ok(con.dget(index))
    })?)?;

    globals.set("dset", lua.create_function(|lua, args: LuaMultiValue| {
        let index = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let value = args.get(1).and_then(val_to_f64).unwrap_or(0.0);
        let mut con = get_console_mut!(lua)?;
        con.dset(index, value);
        Ok(())
    })?)?;

    globals.set("menuitem", lua.create_function(|_lua, _args: LuaMultiValue| {
        // MVP no-op: accept and ignore arguments so carts don't error.
        Ok(())
    })?)?;

    globals.set("extcmd", lua.create_function(|_lua, args: LuaMultiValue| {
        let cmd = args.get(0).map(val_to_display).unwrap_or_default();
        eprintln!("[extcmd] {cmd}");
        Ok(())
    })?)?;

    // cartdata() — MVP no-op, accept and ignore (persistent data works in-session)
    globals.set("cartdata", lua.create_function(|_lua, _args: LuaMultiValue| {
        Ok(())
    })?)?;

    // chr(n) — character from code point
    globals.set("chr", lua.create_function(|_lua, args: LuaMultiValue| {
        let n = args.get(0).and_then(val_to_i32).unwrap_or(0);
        let ch = char::from_u32(n as u32).unwrap_or('\0');
        let mut buf = [0u8; 4];
        let s = ch.encode_utf8(&mut buf);
        Ok(s.to_string())
    })?)?;

    // ord(s, [i]) — code point from character
    globals.set("ord", lua.create_function(|_lua, args: LuaMultiValue| {
        let s: String = match args.get(0) {
            Some(LuaValue::String(s)) => s.to_str().map(|s| s.to_string()).unwrap_or_default(),
            _ => return Ok(LuaValue::Nil),
        };
        let i = args.get(1).and_then(val_to_i32).unwrap_or(1);
        // PICO-8 uses 1-based indexing
        let idx = if i >= 1 { (i - 1) as usize } else { 0 };
        let bytes = s.as_bytes();
        if idx < bytes.len() {
            Ok(LuaValue::Integer(bytes[idx] as i64))
        } else {
            Ok(LuaValue::Nil)
        }
    })?)?;

    // split(s, [sep], [convert]) — split string
    globals.set("split", lua.create_function(|lua, args: LuaMultiValue| {
        let s: String = match args.get(0) {
            Some(LuaValue::String(s)) => s.to_str().map(|s| s.to_string()).unwrap_or_default(),
            _ => return Ok(LuaValue::Nil),
        };
        let sep: String = match args.get(1) {
            Some(LuaValue::String(s)) => s.to_str().map(|s| s.to_string()).unwrap_or_else(|_| ",".to_string()),
            _ => ",".to_string(),
        };
        // Default: convert numeric strings to numbers (PICO-8 behavior)
        let convert = args.get(2).map_or(true, val_to_bool);

        let tbl = lua.create_table()?;
        let parts: Vec<&str> = s.split(&sep).collect();
        for (i, part) in parts.iter().enumerate() {
            let trimmed = part.trim();
            if convert {
                if let Ok(n) = trimmed.parse::<f64>() {
                    tbl.raw_set(i as i64 + 1, n)?;
                } else {
                    tbl.raw_set(i as i64 + 1, part.to_string())?;
                }
            } else {
                tbl.raw_set(i as i64 + 1, part.to_string())?;
            }
        }
        Ok(LuaValue::Table(tbl))
    })?)?;

    // deli(tbl, [i]) — delete element at index, shift remaining down, return deleted value
    globals.set("deli", lua.create_function(|_lua, args: LuaMultiValue| {
        let tbl = match args.get(0) {
            Some(LuaValue::Table(t)) => t.clone(),
            _ => return Ok(LuaValue::Nil),
        };
        let len = tbl.raw_len() as i64;
        if len == 0 {
            return Ok(LuaValue::Nil);
        }
        let idx = args.get(1).and_then(val_to_i32).map(|n| n as i64).unwrap_or(len);
        if idx < 1 || idx > len {
            return Ok(LuaValue::Nil);
        }
        let deleted: LuaValue = tbl.raw_get(idx)?;
        // Shift elements down
        for i in idx..len {
            let next: LuaValue = tbl.raw_get(i + 1)?;
            tbl.raw_set(i, next)?;
        }
        tbl.raw_set(len, LuaValue::Nil)?;
        Ok(deleted)
    })?)?;

    let time_fn = lua.create_function(|lua, _args: LuaMultiValue| {
        let gs = lua.app_data_ref::<GameState>()
            .ok_or_else(|| mlua::Error::runtime("GameState not available"))?;
        Ok(gs.frame_count as f64 / 30.0)
    })?;
    globals.set("time", time_fn.clone())?;
    globals.set("t", time_fn)?;

    globals.set("stat", lua.create_function(|lua, args: LuaMultiValue| {
        let n = args.get(0).and_then(val_to_i32).unwrap_or(0);
        match n {
            // stat(0): memory usage (return a fake value — 64 = minimal)
            0 => Ok(LuaValue::Number(64.0)),
            // stat(1): CPU usage (return 0.0 — no profiling yet)
            1 => Ok(LuaValue::Number(0.0)),
            // stat(4): clipboard contents (return empty string)
            4 => Ok(LuaValue::String(lua.create_string("")?)),
            // stat(6): parameter string / breadcrumb from load()
            6 => {
                let s = lua.app_data_ref::<Breadcrumb>()
                    .map(|b| b.0.clone())
                    .unwrap_or_default();
                Ok(LuaValue::String(lua.create_string(&s)?))
            }
            // stat(7): target FPS
            7 => Ok(LuaValue::Number(30.0)),
            // stat(16-19): SFX currently playing on channel 0-3
            16..=19 => {
                let ch = (n - 16) as usize;
                let audio = get_audio_ref!(lua)?;
                Ok(LuaValue::Number(audio.sfx_on_channel(ch) as f64))
            }
            // stat(20-23): current note number on channel 0-3
            20..=23 => {
                let ch = (n - 20) as usize;
                let audio = get_audio_ref!(lua)?;
                Ok(LuaValue::Number(audio.note_on_channel(ch) as f64))
            }
            // stat(24): current music pattern index
            24 => {
                let audio = get_audio_ref!(lua)?;
                Ok(LuaValue::Number(audio.current_music_pattern() as f64))
            }
            // stat(26-29): SFX index on channel 0-3 (same as 16-19 in our impl)
            26..=29 => {
                let ch = (n - 26) as usize;
                let audio = get_audio_ref!(lua)?;
                Ok(LuaValue::Number(audio.sfx_on_channel(ch) as f64))
            }
            // stat(30): key pressed this frame (boolean-like)
            30 => {
                let con = get_console_ref!(lua)?;
                Ok(LuaValue::Boolean(con.key_pressed))
            }
            // stat(31): last key character typed
            31 => {
                let con = get_console_ref!(lua)?;
                match con.last_key_char {
                    Some(ch) => {
                        let mut buf = [0u8; 4];
                        let s = ch.encode_utf8(&mut buf);
                        Ok(LuaValue::String(lua.create_string(s)?))
                    }
                    None => Ok(LuaValue::String(lua.create_string("")?)),
                }
            }
            // stat(32): mouse x
            32 => {
                let con = get_console_ref!(lua)?;
                Ok(LuaValue::Number(con.mouse_x as f64))
            }
            // stat(33): mouse y
            33 => {
                let con = get_console_ref!(lua)?;
                Ok(LuaValue::Number(con.mouse_y as f64))
            }
            // stat(34): mouse buttons bitmask
            34 => {
                let con = get_console_ref!(lua)?;
                Ok(LuaValue::Number(con.mouse_btn as f64))
            }
            // stat(46-49): SFX currently playing (alias for 16-19)
            46..=49 => {
                let ch = (n - 46) as usize;
                let audio = get_audio_ref!(lua)?;
                Ok(LuaValue::Number(audio.sfx_on_channel(ch) as f64))
            }
            // stat(50-53): current note number (alias for 20-23)
            50..=53 => {
                let ch = (n - 50) as usize;
                let audio = get_audio_ref!(lua)?;
                Ok(LuaValue::Number(audio.note_on_channel(ch) as f64))
            }
            _ => Ok(LuaValue::Number(0.0)),
        }
    })?)?;

    Ok(())
}

// ===========================================================================
//  System functions (flip, run, stop, resume)
// ===========================================================================

fn register_system(lua: &Lua) -> LuaResult<()> {
    let globals = lua.globals();

    // flip() — Force screen update. In PICO-8 this forces the screen to
    // render mid-frame for loading screens, transitions, and effects.
    // On desktop Spark it is a no-op since the screen updates at the end
    // of each frame, but it must be registered so carts that call it do
    // not error.
    globals.set("flip", lua.create_function(|_lua, _args: LuaMultiValue| {
        Ok(())
    })?)?;

    // load(filename, [breadcrumb], [param]) — Load and run another .p8 cart.
    //
    // Since we cannot tear down the Lua state from within a Lua callback,
    // this function signals the main loop by raising a special Lua error
    // that contains the requested filename and breadcrumb.  The main loop
    // catches the error, parses the payload, and performs the actual cart
    // transition.
    globals.set("load", lua.create_function(|_lua, args: LuaMultiValue| {
        let filename = match args.get(0) {
            Some(LuaValue::String(s)) => s.to_str().map(|s| s.to_string()).unwrap_or_default(),
            _ => String::new(),
        };
        let breadcrumb = match args.get(1) {
            Some(LuaValue::String(s)) => s.to_str().map(|s| s.to_string()).unwrap_or_default(),
            _ => String::new(),
        };
        // Raise a special error that the main loop will intercept.
        Err::<(), _>(mlua::Error::runtime(format!(
            "{}{}|{}",
            LOAD_CART_PREFIX, filename, breadcrumb
        )))
    })?)?;

    // run() — Restart the cart. In PICO-8 this re-calls _init and resets
    // game state. For MVP: log and no-op (most carts do not call this).
    globals.set("run", lua.create_function(|_lua, _args: LuaMultiValue| {
        eprintln!("[spark] run() called");
        Ok(())
    })?)?;

    // stop() — Return to editor. In PICO-8 this exits running mode.
    // For MVP: registered as a no-op stub so carts do not error.
    globals.set("stop", lua.create_function(|_lua, _args: LuaMultiValue| {
        eprintln!("[spark] stop() called");
        Ok(())
    })?)?;

    // resume() — Resume from stop. In PICO-8 this resumes a stopped cart.
    // For MVP: registered as a no-op stub so carts do not error.
    globals.set("resume", lua.create_function(|_lua, _args: LuaMultiValue| {
        eprintln!("[spark] resume() called");
        Ok(())
    })?)?;

    Ok(())
}

// ===========================================================================
//  Integration tests — verify real .p8 carts can be loaded and executed
// ===========================================================================

#[cfg(test)]
mod integration_tests {
    use super::*;
    use spark_core::audio::Audio;
    use spark_core::cart::{parse_cart, serialize_cart};
    use spark_core::console::Console;
    use spark_core::audio::{NUM_SFX, NUM_MUSIC, NOTES_PER_SFX};
    use spark_core::game_state::GameState;
    use spark_core::lua_preprocess::preprocess_pico8;

    // Embed carts at compile time so tests work without filesystem access.
    const STARFALL_P8: &str = include_str!("../../carts/starfall.p8");
    const HELLO_P8: &str = include_str!("../../carts/hello.p8");

    // -----------------------------------------------------------------
    // Helper: set up a Lua state with Console, Audio, and GameState
    // installed, load code from a CartData, and return everything needed
    // to drive frames.
    // -----------------------------------------------------------------

    struct TestHarness {
        lua: Lua,
    }

    impl TestHarness {
        /// Build a fully-wired Lua environment from raw cart source text.
        fn from_cart_source(source: &str) -> Self {
            let cart = parse_cart(source);
            let lua = create_lua().expect("create_lua failed");

            let mut console = Console::new();
            console.sprites = cart.sprites;
            console.map = cart.map;
            console.flags = cart.flags;

            let mut audio = Audio::new();
            audio.sfx_data = cart.sfx;
            audio.music_data = cart.music;

            lua.set_app_data(console);
            lua.set_app_data(audio);

            // load_code does its own preprocessing, so pass raw cart code.
            load_code(&lua, &cart.code).expect("load_code failed");

            TestHarness { lua }
        }

        fn call_init(&self) {
            call_init(&self.lua).expect("_init() failed");
        }

        fn run_frames(&self, n: usize) {
            for _ in 0..n {
                call_update(&self.lua).expect("_update() failed");
                call_draw(&self.lua).expect("_draw() failed");
            }
        }

        /// Borrow the Console out of app_data (read-only snapshot).
        fn with_screen<F, R>(&self, f: F) -> R
        where
            F: FnOnce(&Console) -> R,
        {
            let con = self.lua.app_data_ref::<Console>()
                .expect("Console missing from app_data");
            f(&*con)
        }
    }

    impl Drop for TestHarness {
        fn drop(&mut self) {
            // Clean up app_data to avoid leaks / poisoned state.
            let _ = self.lua.remove_app_data::<Console>();
            let _ = self.lua.remove_app_data::<Audio>();
            let _ = self.lua.remove_app_data::<GameState>();
        }
    }

    // =================================================================
    // Test 1: Parse and preprocess starfall.p8
    // =================================================================

    #[test]
    fn test_parse_and_preprocess_starfall() {
        let cart = parse_cart(STARFALL_P8);

        // Code section must be non-empty.
        assert!(!cart.code.is_empty(), "starfall.p8 code section is empty");

        let processed = preprocess_pico8(&cart.code);
        assert!(!processed.is_empty(), "preprocessed code is empty");

        // Must contain the three standard PICO-8 callback definitions.
        assert!(
            processed.contains("function _init()"),
            "preprocessed code missing _init"
        );
        assert!(
            processed.contains("function _update()"),
            "preprocessed code missing _update"
        );
        assert!(
            processed.contains("function _draw()"),
            "preprocessed code missing _draw"
        );
    }

    // =================================================================
    // Test 2: Parse and preprocess hello.p8
    // =================================================================

    #[test]
    fn test_parse_and_preprocess_hello() {
        let cart = parse_cart(HELLO_P8);

        assert!(!cart.code.is_empty(), "hello.p8 code section is empty");

        let processed = preprocess_pico8(&cart.code);
        assert!(!processed.is_empty(), "preprocessed code is empty");

        assert!(
            processed.contains("function _init()"),
            "preprocessed code missing _init"
        );
        assert!(
            processed.contains("function _update()"),
            "preprocessed code missing _update"
        );
        assert!(
            processed.contains("function _draw()"),
            "preprocessed code missing _draw"
        );
    }

    // =================================================================
    // Test 3: Full execution of hello.p8
    // =================================================================

    #[test]
    fn test_execute_hello_p8() {
        let harness = TestHarness::from_cart_source(HELLO_P8);

        // _init should succeed.
        harness.call_init();

        // Run 5 frames of _update + _draw without error.
        harness.run_frames(5);

        // The screen should have been drawn to (not all zeros).
        let has_pixels = harness.with_screen(|con| {
            con.screen.iter().any(|&px| px != 0)
        });
        assert!(has_pixels, "hello.p8: screen is all zeros after 5 frames");
    }

    // =================================================================
    // Test 4: Full execution of starfall.p8
    // =================================================================

    #[test]
    fn test_execute_starfall_p8() {
        let harness = TestHarness::from_cart_source(STARFALL_P8);

        // _init should succeed.
        harness.call_init();

        // Run 10 frames of _update + _draw without Lua errors.
        harness.run_frames(10);

        // Verify the console screen has non-zero pixels (the game draws something).
        let has_pixels = harness.with_screen(|con| {
            con.screen.iter().any(|&px| px != 0)
        });
        assert!(has_pixels, "starfall.p8: screen is all zeros after 10 frames");
    }

    // =================================================================
    // Test 5: Cart round-trip integrity (starfall.p8)
    // =================================================================

    #[test]
    fn test_cart_roundtrip_starfall() {
        let cart1 = parse_cart(STARFALL_P8);
        let serialized = serialize_cart(&cart1);
        let cart2 = parse_cart(&serialized);

        // Code
        assert_eq!(cart1.code, cart2.code, "code mismatch after round-trip");

        // Sprites
        assert_eq!(
            &cart1.sprites[..], &cart2.sprites[..],
            "sprites mismatch after round-trip"
        );

        // Flags
        assert_eq!(
            &cart1.flags[..], &cart2.flags[..],
            "flags mismatch after round-trip"
        );

        // Map
        assert_eq!(
            &cart1.map[..], &cart2.map[..],
            "map mismatch after round-trip"
        );

        // SFX
        for s in 0..NUM_SFX {
            assert_eq!(
                cart1.sfx[s].speed, cart2.sfx[s].speed,
                "sfx {} speed mismatch", s
            );
            assert_eq!(
                cart1.sfx[s].loop_start, cart2.sfx[s].loop_start,
                "sfx {} loop_start mismatch", s
            );
            assert_eq!(
                cart1.sfx[s].loop_end, cart2.sfx[s].loop_end,
                "sfx {} loop_end mismatch", s
            );
            for n in 0..NOTES_PER_SFX {
                assert_eq!(
                    cart1.sfx[s].notes[n].pitch, cart2.sfx[s].notes[n].pitch,
                    "sfx {} note {} pitch mismatch", s, n
                );
                assert_eq!(
                    cart1.sfx[s].notes[n].waveform, cart2.sfx[s].notes[n].waveform,
                    "sfx {} note {} waveform mismatch", s, n
                );
                assert_eq!(
                    cart1.sfx[s].notes[n].volume, cart2.sfx[s].notes[n].volume,
                    "sfx {} note {} volume mismatch", s, n
                );
                assert_eq!(
                    cart1.sfx[s].notes[n].effect, cart2.sfx[s].notes[n].effect,
                    "sfx {} note {} effect mismatch", s, n
                );
            }
        }

        // Music
        for m in 0..NUM_MUSIC {
            assert_eq!(
                cart1.music[m].flags, cart2.music[m].flags,
                "music {} flags mismatch", m
            );
            for ch in 0..4 {
                assert_eq!(
                    cart1.music[m].channels[ch], cart2.music[m].channels[ch],
                    "music {} channel {} mismatch", m, ch
                );
            }
        }
    }

    // =================================================================
    // Test 6: Preprocessor handles starfall.p8 code correctly
    // =================================================================

    #[test]
    fn test_preprocessor_transforms_starfall() {
        let cart = parse_cart(STARFALL_P8);
        let raw = &cart.code;
        let processed = preprocess_pico8(raw);

        // The original code uses PICO-8-specific compound assignment
        // operators (+=, -=, *=).  Verify the raw code has them and
        // the preprocessor has transformed them away.

        // 1. The raw code must contain at least += and -= (starfall uses both).
        assert!(
            has_bare_operator(raw, "+="),
            "expected raw starfall code to contain +="
        );
        assert!(
            has_bare_operator(raw, "-="),
            "expected raw starfall code to contain -="
        );
        assert!(
            has_bare_operator(raw, "*="),
            "expected raw starfall code to contain *="
        );

        // 2. No compound-assignment operators should remain outside strings
        //    after preprocessing.
        for op in &["+=", "-=", "*=", "/=", "%="] {
            assert!(
                !has_bare_operator(&processed, op),
                "preprocessed code still contains bare {} operator", op
            );
        }

        // 3. No `!=` should remain outside strings (transformed to `~=`).
        assert!(
            !has_bare_operator(&processed, "!="),
            "preprocessed code still contains bare != operator"
        );

        // 4. No `?` print shorthand at the start of a line.
        for line in processed.lines() {
            let trimmed = line.trim();
            assert!(
                !trimmed.starts_with('?'),
                "preprocessed code still contains ? shorthand: {}", trimmed
            );
        }

        // 5. The transformed code should contain the expanded form of
        //    compound assignments.  For example, `bg_scroll+=spd` should
        //    become something like `bg_scroll = bg_scroll + (spd)`.
        assert!(
            processed.contains("bg_scroll = bg_scroll + (spd)"),
            "preprocessed code should contain expanded form of bg_scroll+=spd"
        );
    }

    // =================================================================
    // Test 7: flip/run/stop/resume can be called without error
    // =================================================================

    #[test]
    fn test_system_stubs_callable() {
        let lua = create_lua().expect("create_lua failed");
        lua.set_app_data(Console::new());
        lua.set_app_data(Audio::new());

        // Each system function should be callable from Lua without error.
        lua.load("flip()").exec().expect("flip() should not error");
        lua.load("run()").exec().expect("run() should not error");
        lua.load("stop()").exec().expect("stop() should not error");
        lua.load("resume()").exec().expect("resume() should not error");
    }

    // =================================================================
    // Test 8: has_update60 returns false when _update60 is not defined
    // =================================================================

    #[test]
    fn test_has_update60_false_when_not_defined() {
        let lua = create_lua().expect("create_lua failed");
        lua.set_app_data(Console::new());
        lua.set_app_data(Audio::new());

        // No _update60 defined, so should return false.
        assert!(!has_update60(&lua), "has_update60 should be false when _update60 is not defined");
    }

    // =================================================================
    // Test 9: has_update60 returns true when _update60 is defined
    // =================================================================

    #[test]
    fn test_has_update60_true_when_defined() {
        let lua = create_lua().expect("create_lua failed");
        lua.set_app_data(Console::new());
        lua.set_app_data(Audio::new());

        // Define _update60 as a global function.
        lua.load("function _update60() end").exec().expect("defining _update60 should not error");
        assert!(has_update60(&lua), "has_update60 should be true when _update60 is defined");
    }

    // =================================================================
    // Test 10: Bit manipulation -- lshr, rotl, rotr
    // =================================================================

    /// Helper: evaluate a Lua expression and return its f64 result.
    fn eval_f64(lua: &Lua, expr: &str) -> f64 {
        let code = format!("return {}", expr);
        lua.load(&code).eval::<f64>().unwrap_or_else(|e| {
            panic!("Lua eval failed for `{}`: {}", expr, e)
        })
    }

    /// Create a minimal Lua state with math functions registered (no Console/Audio needed).
    fn math_lua() -> Lua {
        create_lua().expect("create_lua failed")
    }

    #[test]
    fn test_lshr_basic() {
        let lua = math_lua();
        // lshr(8, 1) = 4
        assert_eq!(eval_f64(&lua, "lshr(8, 1)"), 4.0);
    }

    #[test]
    fn test_lshr_unsigned_no_sign_extension() {
        let lua = math_lua();
        // lshr(-1, 16) should be 65535 (0xFFFFFFFF >> 16 = 0x0000FFFF)
        assert_eq!(eval_f64(&lua, "lshr(-1, 16)"), 65535.0);
    }

    #[test]
    fn test_shr_arithmetic_preserves_sign() {
        let lua = math_lua();
        // shr(-1, 16) should be -1 (arithmetic shift preserves sign bit)
        assert_eq!(eval_f64(&lua, "shr(-1, 16)"), -1.0);
    }

    #[test]
    fn test_rotl_basic() {
        let lua = math_lua();
        // rotl(1, 1) = 2
        assert_eq!(eval_f64(&lua, "rotl(1, 1)"), 2.0);
    }

    #[test]
    fn test_rotl_msb_wraps() {
        let lua = math_lua();
        // 0x80000000 as i32 = -2147483648
        // rotl(0x80000000, 1) = 1 (MSB wraps to LSB)
        assert_eq!(eval_f64(&lua, "rotl(-2147483648, 1)"), 1.0);
    }

    #[test]
    fn test_rotl_byte_shift() {
        let lua = math_lua();
        // rotl(0xFF, 8) = 0xFF00 = 65280
        assert_eq!(eval_f64(&lua, "rotl(255, 8)"), 65280.0);
    }

    #[test]
    fn test_rotr_basic() {
        let lua = math_lua();
        // rotr(2, 1) = 1
        assert_eq!(eval_f64(&lua, "rotr(2, 1)"), 1.0);
    }

    #[test]
    fn test_rotr_lsb_wraps() {
        let lua = math_lua();
        // rotr(1, 1) = 0x80000000 = -2147483648 (as i32)
        assert_eq!(eval_f64(&lua, "rotr(1, 1)"), -2147483648.0);
    }

    #[test]
    fn test_rotr_byte_shift() {
        let lua = math_lua();
        // rotr(0xFF00, 8) = 0xFF = 255
        assert_eq!(eval_f64(&lua, "rotr(65280, 8)"), 255.0);
    }

    #[test]
    fn test_rotl_identity() {
        let lua = math_lua();
        // rotl(x, 0) = x
        assert_eq!(eval_f64(&lua, "rotl(42, 0)"), 42.0);
        assert_eq!(eval_f64(&lua, "rotl(-7, 0)"), -7.0);
    }

    #[test]
    fn test_rotr_identity() {
        let lua = math_lua();
        // rotr(x, 0) = x
        assert_eq!(eval_f64(&lua, "rotr(42, 0)"), 42.0);
        assert_eq!(eval_f64(&lua, "rotr(-7, 0)"), -7.0);
    }

    // =================================================================
    // Test: load() produces the expected signal error format
    // =================================================================

    #[test]
    fn test_load_produces_signal_error() {
        let lua = create_lua().expect("create_lua failed");
        lua.set_app_data(Console::new());
        lua.set_app_data(Audio::new());

        let result = lua.load(r#"load("other.p8", "hello")"#).exec();
        assert!(result.is_err(), "load() should produce an error");
        let err_msg = format!("{}", result.unwrap_err());
        assert!(
            err_msg.contains(LOAD_CART_PREFIX),
            "error should contain the load-cart prefix, got: {}",
            err_msg
        );
    }

    // =================================================================
    // Test: parse_load_signal extracts filename and breadcrumb
    // =================================================================

    #[test]
    fn test_parse_load_signal_with_breadcrumb() {
        let msg = format!("runtime error: {}game.p8|level2", LOAD_CART_PREFIX);
        let result = parse_load_signal(&msg);
        assert!(result.is_some(), "should parse the load signal");
        let (filename, breadcrumb) = result.unwrap();
        assert_eq!(filename, "game.p8");
        assert_eq!(breadcrumb, "level2");
    }

    #[test]
    fn test_parse_load_signal_without_breadcrumb() {
        let msg = format!("runtime error: {}game.p8|", LOAD_CART_PREFIX);
        let result = parse_load_signal(&msg);
        assert!(result.is_some());
        let (filename, breadcrumb) = result.unwrap();
        assert_eq!(filename, "game.p8");
        assert_eq!(breadcrumb, "");
    }

    #[test]
    fn test_parse_load_signal_no_match() {
        let msg = "runtime error: some other error".to_string();
        assert!(parse_load_signal(&msg).is_none());
    }

    // =================================================================
    // Test: stat(6) returns the breadcrumb string
    // =================================================================

    #[test]
    fn test_stat6_returns_breadcrumb() {
        let lua = create_lua().expect("create_lua failed");
        lua.set_app_data(Console::new());
        lua.set_app_data(Audio::new());
        lua.set_app_data(Breadcrumb("from_cart_a".to_string()));

        let val: String = lua.load("return stat(6)").eval().expect("stat(6) failed");
        assert_eq!(val, "from_cart_a");
    }

    #[test]
    fn test_stat6_returns_empty_by_default() {
        let lua = create_lua().expect("create_lua failed");
        lua.set_app_data(Console::new());
        lua.set_app_data(Audio::new());

        let val: String = lua.load("return stat(6)").eval().expect("stat(6) failed");
        assert_eq!(val, "");
    }

    // =================================================================
    // Test: load() signal is correctly round-tripped through parse
    // =================================================================

    #[test]
    fn test_load_signal_roundtrip() {
        let lua = create_lua().expect("create_lua failed");
        lua.set_app_data(Console::new());
        lua.set_app_data(Audio::new());

        let result = lua.load(r#"load("sub/dir/cart.p8", "my breadcrumb")"#).exec();
        let err_msg = format!("{}", result.unwrap_err());
        let parsed = parse_load_signal(&err_msg);
        assert!(parsed.is_some(), "should parse: {}", err_msg);
        let (filename, breadcrumb) = parsed.unwrap();
        assert_eq!(filename, "sub/dir/cart.p8");
        assert_eq!(breadcrumb, "my breadcrumb");
    }

    /// Scan for `op` appearing outside of string literals and comments.
    /// Returns true if at least one bare occurrence is found.
    fn has_bare_operator(code: &str, op: &str) -> bool {
        for line in code.lines() {
            let mut in_string: Option<char> = None;
            let chars: Vec<char> = line.chars().collect();
            let op_chars: Vec<char> = op.chars().collect();
            let mut i = 0;
            while i < chars.len() {
                if let Some(delim) = in_string {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        i += 2;
                        continue;
                    }
                    if chars[i] == delim {
                        in_string = None;
                    }
                    i += 1;
                    continue;
                }
                if chars[i] == '"' || chars[i] == '\'' {
                    in_string = Some(chars[i]);
                    i += 1;
                    continue;
                }
                if chars[i] == '-' && i + 1 < chars.len() && chars[i + 1] == '-' {
                    break; // rest is comment
                }
                if i + op_chars.len() <= chars.len()
                    && chars[i..i + op_chars.len()] == op_chars[..]
                {
                    return true;
                }
                i += 1;
            }
        }
        false
    }
}
