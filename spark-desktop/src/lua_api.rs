//! Lua API layer for the Spark fantasy console (desktop target).
//!
//! Registers all PICO-8 compatible Lua functions against an `mlua::Lua` state.
//! Console and Audio state are shared via `Lua::set_app_data` / `app_data_mut`.

use std::borrow::Cow;

use mlua::prelude::*;
use mlua::BorrowedBytes;

use spark_core::api_ops;
use spark_core::audio::Audio;
use spark_core::console::Console;
use spark_core::fixed::{from_bits, to_bits};
use spark_core::game_state::GameState;
use spark_core::lua_preprocess::preprocess_pico8;

// ---------------------------------------------------------------------------
// Load-cart signal: used by load() to communicate back to the main loop
// ---------------------------------------------------------------------------

/// Prefix used in the special error message that signals a cart load request.
/// The main loop checks for this prefix to distinguish a load() call from a
/// real Lua error.
pub const LOAD_CART_PREFIX: &str = "__SPARK_LOAD_CART__:";

/// Prefix used in the special error message that signals a stop() call.
pub const STOP_CART_PREFIX: &str = "__SPARK_STOP_CART__";

/// Returns true if the error message is a stop() signal.
pub fn is_stop_signal(msg: &str) -> bool {
    msg.contains(STOP_CART_PREFIX)
}

/// Prefix used in the special error message that signals a run() call.
pub const RUN_CART_PREFIX: &str = "__SPARK_RUN_CART__";

/// Returns true if the error message is a run() signal.
pub fn is_run_signal(msg: &str) -> bool {
    msg.contains(RUN_CART_PREFIX)
}

/// Wrapper type stored in Lua app_data to hold the breadcrumb string
/// passed by `load(filename, breadcrumb)` and accessible via `stat(6)`.
#[derive(Clone, Default)]
pub struct Breadcrumb(pub String);

/// Stores the cart data ID set by cartdata(). Used to persist dget/dset
/// values to/from disk between sessions.
#[derive(Clone, Default)]
pub struct CartDataId(pub String);

/// The param string passed as the third argument to load().
/// Accessible via stat(46).
#[derive(Clone, Default)]
pub struct LoadParam(pub String);

/// A single registered pause-menu item (PICO-8 `menuitem`).
pub struct MenuItem {
    pub label: String,
    /// Optional Lua callback stored in the registry.
    pub callback: Option<LuaRegistryKey>,
}

/// Up to 5 pause-menu items, indexed 1-5.
/// Stored in Lua app_data for the lifetime of the cart.
pub struct MenuItems(pub Vec<Option<MenuItem>>);

impl Default for MenuItems {
    fn default() -> Self {
        Self::new()
    }
}

impl MenuItems {
    pub fn new() -> Self {
        MenuItems(vec![None, None, None, None, None])
    }
}

/// Parse a load-cart signal error message.
///
/// If `msg` contains the `LOAD_CART_PREFIX`, returns `Some((filename, breadcrumb, param))`.
/// Otherwise returns `None`.
pub fn parse_load_signal(msg: &str) -> Option<(String, String, String)> {
    if let Some(rest) = msg.split(LOAD_CART_PREFIX).nth(1) {
        // Find the first occurrence of the prefix payload (it may be wrapped
        // in additional error context by mlua).  The payload format is:
        //   filename|breadcrumb|param
        // where breadcrumb and param may be empty.
        let payload = rest.lines().next().unwrap_or(rest);
        let mut parts = payload.splitn(3, '|');
        let filename = parts.next().unwrap_or("").to_string();
        let breadcrumb = parts.next().unwrap_or("").to_string();
        let param = parts.next().unwrap_or("").to_string();
        Some((filename, breadcrumb, param))
    } else {
        None
    }
}

/// Path to the saved persistent data file for the given cart data ID.
/// Creates the directory if it does not exist.
///
/// Location: `~/.local/share/spark/carts/<id>.dat`
/// Falls back to the current directory if the home dir cannot be found.
pub fn cartdata_path(id: &str) -> std::path::PathBuf {
    let base = std::env::var("HOME")
        .map(|h| {
            std::path::PathBuf::from(h)
                .join(".local")
                .join("share")
                .join("spark")
                .join("carts")
        })
        .unwrap_or_else(|_| std::path::PathBuf::from(".spark_carts"));
    let _ = std::fs::create_dir_all(&base);
    base.join(format!("{}.dat", id))
}

/// Serialize persistent_data to 64 little-endian f64 values (512 bytes).
pub fn save_cartdata(id: &str, data: &[f64; 64]) -> Result<(), String> {
    let path = cartdata_path(id);
    let mut bytes = Vec::with_capacity(512);
    for &v in data.iter() {
        bytes.extend_from_slice(&v.to_le_bytes());
    }
    std::fs::write(&path, &bytes).map_err(|e| format!("cartdata save failed: {}", e))
}

/// Deserialize persistent_data from a 512-byte file (64 little-endian f64s).
/// Returns zeros if the file is missing or malformed.
pub fn load_cartdata(id: &str) -> [f64; 64] {
    let path = cartdata_path(id);
    let mut data = [0.0f64; 64];
    if let Ok(bytes) = std::fs::read(&path) {
        if bytes.len() == 512 {
            for (i, chunk) in bytes.chunks_exact(8).enumerate().take(64) {
                let arr: [u8; 8] = chunk.try_into().unwrap_or([0u8; 8]);
                data[i] = f64::from_le_bytes(arr);
            }
        }
    }
    data
}

/// Write the console screen to a PNG file at `path` (`extcmd("screenshot")`).
fn write_screen_png(console: &Console, path: &str) {
    let bytes = crate::encode_screen_png(&console.screen_to_rgba());
    match std::fs::write(path, &bytes) {
        Ok(()) => eprintln!("[extcmd screenshot] saved {}", path),
        Err(e) => eprintln!("[extcmd screenshot] failed to write {}: {}", path, e),
    }
}

// ---------------------------------------------------------------------------
// Helpers for pulling values out of Lua arguments
// ---------------------------------------------------------------------------

/// Positional arguments of an API call, with PICO-8's lenient coercions:
/// numbers/booleans/numeric strings convert, anything else (or a missing
/// argument) falls back to the given default.
struct Args(LuaMultiValue);

impl FromLuaMulti for Args {
    fn from_lua_multi(values: LuaMultiValue, _lua: &Lua) -> LuaResult<Self> {
        Ok(Args(values))
    }
}

impl Args {
    fn get(&self, i: usize) -> Option<&LuaValue> {
        self.0.get(i)
    }

    /// Total argument count, including trailing nils.
    fn len(&self) -> usize {
        self.0.len()
    }

    /// Number of leading non-nil arguments. PICO-8 dispatches several
    /// functions (`line`, `print`) on this count; trailing nils don't count.
    fn nargs(&self) -> usize {
        (0..self.len()).take_while(|&i| !self.is_nil(i)).count()
    }

    fn is_nil(&self, i: usize) -> bool {
        matches!(self.get(i), None | Some(LuaValue::Nil))
    }

    fn f64_opt(&self, i: usize) -> Option<f64> {
        self.get(i).and_then(val_to_f64)
    }

    fn f64(&self, i: usize, default: f64) -> f64 {
        self.f64_opt(i).unwrap_or(default)
    }

    fn i32_opt(&self, i: usize) -> Option<i32> {
        self.get(i).and_then(val_to_i32)
    }

    fn i32(&self, i: usize, default: i32) -> i32 {
        self.i32_opt(i).unwrap_or(default)
    }

    fn u8_opt(&self, i: usize) -> Option<u8> {
        self.get(i).and_then(val_to_u8)
    }

    fn u8(&self, i: usize, default: u8) -> u8 {
        self.u8_opt(i).unwrap_or(default)
    }

    /// Lua truthiness of argument `i`; `default` when it is absent.
    fn bool(&self, i: usize, default: bool) -> bool {
        self.get(i).map_or(default, val_to_bool)
    }

    /// Argument `i` if it is a Lua string (lossy UTF-8), else `None`.
    fn str(&self, i: usize) -> Option<String> {
        match self.get(i) {
            Some(LuaValue::String(s)) => Some(s.to_string_lossy()),
            _ => None,
        }
    }

    /// Argument `i` rendered as PICO-8 string functions see it (see
    /// `val_to_display`); empty when absent.
    fn display(&self, i: usize) -> Text<'_> {
        match self.get(i) {
            Some(v) => val_to_display(v),
            None => Text::Owned(String::new()),
        }
    }
}

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

/// The text of a Lua value as `print`/`sub`/`split`/`ord` see it. Lua strings
/// are borrowed as raw bytes (no copy, P8SCII bytes >= 0x80 intact); other
/// values are formatted into an owned string.
enum Text<'a> {
    Lua(BorrowedBytes<'a>),
    Owned(String),
}

impl Text<'_> {
    fn as_bytes(&self) -> &[u8] {
        match self {
            Text::Lua(b) => b,
            Text::Owned(s) => s.as_bytes(),
        }
    }

    /// As `&str`; only allocates when a Lua string is not valid UTF-8.
    fn as_str(&self) -> Cow<'_, str> {
        match self {
            Text::Lua(b) => String::from_utf8_lossy(b),
            Text::Owned(s) => Cow::Borrowed(s),
        }
    }
}

/// Convert a Lua value to display text (used by `print`, `printh`, `sub`, ...).
fn val_to_display(v: &LuaValue) -> Text<'_> {
    match v {
        LuaValue::String(s) => Text::Lua(s.as_bytes()),
        LuaValue::Integer(n) => Text::Owned(n.to_string()),
        LuaValue::Number(n) => Text::Owned(format!("{n}")),
        LuaValue::Boolean(b) => Text::Owned((if *b { "true" } else { "false" }).to_string()),
        LuaValue::Nil => Text::Owned("nil".to_string()),
        _ => Text::Owned(String::new()),
    }
}

use spark_core::api_ops::format_tostr_hex;

/// Common error for missing Console.
const CONSOLE_ERR: &str = "Console not available";
/// Common error for missing Audio.
const AUDIO_ERR: &str = "Audio not available";
/// Common error for missing GameState.
const GAME_STATE_ERR: &str = "GameState not available";

/// Borrow the GameState from Lua app_data (mutable).
macro_rules! get_game_state_mut {
    ($lua:expr) => {
        $lua.app_data_mut::<GameState>()
            .ok_or_else(|| mlua::Error::runtime(GAME_STATE_ERR))
    };
}

/// Borrow the GameState from Lua app_data (read-only).
macro_rules! get_game_state_ref {
    ($lua:expr) => {
        $lua.app_data_ref::<GameState>()
            .ok_or_else(|| mlua::Error::runtime(GAME_STATE_ERR))
    };
}

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

    // Insert initial GameState, Breadcrumb, CartDataId, LoadParam, and MenuItems
    lua.set_app_data(GameState::new());
    lua.set_app_data(Breadcrumb::default());
    lua.set_app_data(CartDataId::default());
    lua.set_app_data(LoadParam::default());
    lua.set_app_data(MenuItems::new());

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
///
/// On a syntax/runtime error the returned message is followed by the +/-5
/// lines of *preprocessed* source around the failing line, so the reported
/// line number (which refers to the preprocessed text) can be read directly.
pub fn load_code(lua: &Lua, code: &str) -> LuaResult<()> {
    let processed = preprocess_pico8(code);
    lua.load(&processed).set_name("cart").exec().map_err(|e| {
        let err_str = e.to_string();
        match error_line(&err_str) {
            Some(line_num) => {
                let lines: Vec<&str> = processed.lines().collect();
                let start = line_num.saturating_sub(6);
                let end = (line_num + 5).min(lines.len());
                let mut msg = format!("{err_str}\n--- context around line {line_num} ---");
                for (i, line) in lines.iter().enumerate().take(end).skip(start) {
                    let marker = if i + 1 == line_num { ">>>" } else { "   " };
                    msg.push_str(&format!("\n{} {:4}: {}", marker, i + 1, line));
                }
                mlua::Error::runtime(msg)
            }
            None => e,
        }
    })
}

/// Extract the `N` from a Lua error of the form `[string "cart"]:N: ...`.
fn error_line(err: &str) -> Option<usize> {
    let after = &err[err.find("]:")? + 2..];
    after[..after.find(':')?].parse().ok()
}

/// Call `_update()` if it exists.
/// Also increments the frame counter used by `time()` / `t()`.
pub fn call_update(lua: &Lua) -> LuaResult<()> {
    call_update_fn(lua, "_update")
}

/// Call `_update60()` if it exists.
/// Also increments the frame counter used by `time()` / `t()`.
pub fn call_update60(lua: &Lua) -> LuaResult<()> {
    call_update_fn(lua, "_update60")
}

fn call_update_fn(lua: &Lua, name: &str) -> LuaResult<()> {
    get_game_state_mut!(lua)?.frame_count += 1;
    if let Ok(func) = lua.globals().get::<LuaFunction>(name) {
        if let Err(e) = func.call::<()>(()) {
            return Err(add_traceback(lua, e));
        }
    }
    Ok(())
}

/// Check whether the cart defines `_update60` as a global function.
/// Used by the main loop to decide between 30fps and 60fps mode.
pub fn has_update60(lua: &Lua) -> bool {
    lua.globals()
        .get::<LuaValue>("_update60")
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

/// Wrap `_init` in a Lua thread for coroutine-based cart execution.
///
/// This enables `while true do ... flip() end` inside `_init`: the thread
/// yields at every `flip()` call and the main loop resumes it each frame.
///
/// Returns `None` if `_init` is not defined (unlikely but valid).
pub fn create_init_coroutine(lua: &Lua) -> LuaResult<Option<LuaThread>> {
    match lua.globals().get::<LuaFunction>("_init") {
        Ok(f) => Ok(Some(lua.create_thread(f)?)),
        Err(_) => Ok(None),
    }
}

/// Resume a cart coroutine for one frame.
///
/// Returns `Ok(true)` if still running (coroutine yielded — called `flip()`).
/// Returns `Ok(false)` if the coroutine finished normally.
/// Returns `Err` on a Lua error (including stop/run/load signals).
pub fn resume_cart_thread(thread: &LuaThread) -> LuaResult<bool> {
    match thread.resume::<LuaValue>(()) {
        Ok(_) => Ok(matches!(thread.status(), LuaThreadStatus::Resumable)),
        Err(e) => Err(e),
    }
}

// ===========================================================================
//  Graphics functions
// ===========================================================================

fn register_graphics(lua: &Lua) -> LuaResult<()> {
    let globals = lua.globals();

    globals.set(
        "cls",
        lua.create_function(|lua, args: Args| {
            let col = args.u8_opt(0);
            let mut con = get_console_mut!(lua)?;
            con.cls(col);
            Ok(())
        })?,
    )?;

    globals.set(
        "pset",
        lua.create_function(|lua, args: Args| {
            let x = args.i32(0, 0);
            let y = args.i32(1, 0);
            let col = args.u8_opt(2);
            let mut con = get_console_mut!(lua)?;
            con.pset(x, y, col);
            Ok(())
        })?,
    )?;

    globals.set(
        "pget",
        lua.create_function(|lua, args: Args| {
            let x = args.i32(0, 0);
            let y = args.i32(1, 0);
            let con = get_console_ref!(lua)?;
            Ok(con.pget(x, y))
        })?,
    )?;

    globals.set(
        "line",
        lua.create_function(|lua, args: Args| {
            // PICO-8 dispatches `line` by argument count:
            //   line()                  -> reset the polyline state
            //   line(col)               -> reset state, set pen color
            //   line(x1,y1[,col])       -> draw from the last endpoint to (x1,y1)
            //   line(x0,y0,x1,y1[,col]) -> full segment
            // Count only leading non-nil args (trailing nils don't count).
            let n = args.nargs();
            let mut con = get_console_mut!(lua)?;
            match n {
                0 => con.line_reset(),
                1 => {
                    if let Some(c) = args.u8_opt(0) {
                        con.draw_color = c;
                    }
                    con.line_reset();
                }
                2 | 3 => {
                    let x1 = args.i32(0, 0);
                    let y1 = args.i32(1, 0);
                    let col = args.u8_opt(2);
                    con.line_to(x1, y1, col);
                }
                _ => {
                    let x0 = args.i32(0, 0);
                    let y0 = args.i32(1, 0);
                    let x1 = args.i32(2, 0);
                    let y1 = args.i32(3, 0);
                    let col = args.u8_opt(4);
                    con.line(x0, y0, x1, y1, col);
                }
            }
            Ok(())
        })?,
    )?;

    globals.set(
        "rect",
        lua.create_function(|lua, args: Args| {
            let x0 = args.i32(0, 0);
            let y0 = args.i32(1, 0);
            let x1 = args.i32(2, 0);
            let y1 = args.i32(3, 0);
            let col = args.u8_opt(4);
            let mut con = get_console_mut!(lua)?;
            con.rect(x0, y0, x1, y1, col);
            Ok(())
        })?,
    )?;

    globals.set(
        "rectfill",
        lua.create_function(|lua, args: Args| {
            let x0 = args.i32(0, 0);
            let y0 = args.i32(1, 0);
            let x1 = args.i32(2, 0);
            let y1 = args.i32(3, 0);
            let col = args.u8_opt(4);
            let mut con = get_console_mut!(lua)?;
            con.rectfill(x0, y0, x1, y1, col);
            Ok(())
        })?,
    )?;

    globals.set(
        "circ",
        lua.create_function(|lua, args: Args| {
            let cx = args.i32(0, 0);
            let cy = args.i32(1, 0);
            let r = args.i32(2, 4);
            let col = args.u8_opt(3);
            let mut con = get_console_mut!(lua)?;
            con.circ(cx, cy, r, col);
            Ok(())
        })?,
    )?;

    globals.set(
        "circfill",
        lua.create_function(|lua, args: Args| {
            let cx = args.i32(0, 0);
            let cy = args.i32(1, 0);
            let r = args.i32(2, 4);
            let col = args.u8_opt(3);
            let mut con = get_console_mut!(lua)?;
            con.circfill(cx, cy, r, col);
            Ok(())
        })?,
    )?;

    globals.set(
        "oval",
        lua.create_function(|lua, args: Args| {
            let x0 = args.i32(0, 0);
            let y0 = args.i32(1, 0);
            let x1 = args.i32(2, 0);
            let y1 = args.i32(3, 0);
            let col = args.u8_opt(4);
            let mut con = get_console_mut!(lua)?;
            con.oval(x0, y0, x1, y1, col);
            Ok(())
        })?,
    )?;

    globals.set(
        "ovalfill",
        lua.create_function(|lua, args: Args| {
            let x0 = args.i32(0, 0);
            let y0 = args.i32(1, 0);
            let x1 = args.i32(2, 0);
            let y1 = args.i32(3, 0);
            let col = args.u8_opt(4);
            let mut con = get_console_mut!(lua)?;
            con.ovalfill(x0, y0, x1, y1, col);
            Ok(())
        })?,
    )?;

    globals.set(
        "spr",
        lua.create_function(|lua, args: Args| {
            let n = args.i32(0, 0);
            let x = args.i32(1, 0);
            let y = args.i32(2, 0);
            let w = args.f64(3, 1.0);
            let h = args.f64(4, 1.0);
            let flip_x = args.bool(5, false);
            let flip_y = args.bool(6, false);
            let mut con = get_console_mut!(lua)?;
            con.spr(n, x, y, w, h, flip_x, flip_y);
            Ok(())
        })?,
    )?;

    globals.set(
        "sspr",
        lua.create_function(|lua, args: Args| {
            let sx = args.i32(0, 0);
            let sy = args.i32(1, 0);
            let sw = args.i32(2, 0);
            let sh = args.i32(3, 0);
            let dx = args.i32(4, 0);
            let dy = args.i32(5, 0);
            let dw = args.i32(6, sw);
            let dh = args.i32(7, sh);
            let flip_x = args.bool(8, false);
            let flip_y = args.bool(9, false);
            let mut con = get_console_mut!(lua)?;
            con.sspr(sx, sy, sw, sh, dx, dy, dw, dh, flip_x, flip_y);
            Ok(())
        })?,
    )?;

    globals.set(
        "sget",
        lua.create_function(|lua, args: Args| {
            let x = args.i32(0, 0);
            let y = args.i32(1, 0);
            let con = get_console_ref!(lua)?;
            Ok(con.sget(x, y))
        })?,
    )?;

    globals.set(
        "sset",
        lua.create_function(|lua, args: Args| {
            let x = args.i32(0, 0);
            let y = args.i32(1, 0);
            let col = args.u8(2, 0);
            let mut con = get_console_mut!(lua)?;
            con.sset(x, y, col);
            Ok(())
        })?,
    )?;

    globals.set(
        "print",
        lua.create_function(|lua, args: Args| {
            let text = args.display(0);
            let text = text.as_str();
            // PICO-8 dispatches `print` by argument count:
            //   print(str)            -> at the cursor, current pen color
            //   print(str, col)       -> at the cursor, set pen to col
            //   print(str, x, y)      -> at (x,y), current pen color
            //   print(str, x, y, col) -> at (x,y), set pen to col
            // Count leading non-nil args (the string counts as arg 1).
            let n = args.nargs();
            let mut con = get_console_mut!(lua)?;
            let result = match n {
                0 | 1 => con.print(&text, None, None, None),
                2 => {
                    let col = args.u8_opt(1);
                    con.print(&text, None, None, col)
                }
                3 => {
                    let x = args.i32_opt(1);
                    let y = args.i32_opt(2);
                    con.print(&text, x, y, None)
                }
                _ => {
                    let x = args.i32_opt(1);
                    let y = args.i32_opt(2);
                    let col = args.u8_opt(3);
                    con.print(&text, x, y, col)
                }
            };
            Ok(result)
        })?,
    )?;

    globals.set(
        "cursor",
        lua.create_function(|lua, args: Args| {
            let x = args.i32(0, 0);
            let y = args.i32(1, 0);
            let col = args.u8_opt(2);
            let mut con = get_console_mut!(lua)?;
            // PICO-8 `cursor()` returns the previous cursor position (x, y).
            let prev = con.cursor(x, y);
            if let Some(c) = col {
                con.color(c);
            }
            Ok((prev.0, prev.1))
        })?,
    )?;

    globals.set(
        "camera",
        lua.create_function(|lua, args: Args| {
            let x = args.i32(0, 0);
            let y = args.i32(1, 0);
            let mut con = get_console_mut!(lua)?;
            // PICO-8 `camera()` returns the previous offset (x, y).
            let prev = con.camera(x, y);
            Ok((prev.0, prev.1))
        })?,
    )?;

    globals.set(
        "clip",
        lua.create_function(|lua, args: Args| {
            let mut con = get_console_mut!(lua)?;
            // PICO-8 `clip()` returns the PREVIOUS clip rect as (x, y, w, h).
            let prev = if args.is_nil(0) {
                con.clip_rect(0, 0, 128, 128, false)
            } else {
                let x = args.i32(0, 0);
                let y = args.i32(1, 0);
                let w = args.i32(2, 128);
                let h = args.i32(3, 128);
                // Optional 5th arg (PICO-8 0.2.x): intersect with current clip.
                let clip_previous = args
                    .get(4)
                    .map(|v| !matches!(v, LuaValue::Nil | LuaValue::Boolean(false)))
                    .unwrap_or(false);
                con.clip_rect(x, y, w, h, clip_previous)
            };
            Ok((prev.0, prev.1, prev.2, prev.3))
        })?,
    )?;

    globals.set(
        "pal",
        lua.create_function(|lua, args: Args| {
            // PICO-8 `pal` forms:
            //   pal()              -> reset both palettes + transparency
            //   pal(tbl[, p])      -> bulk remap from a table {[c0]=c1, ...}
            //   pal(p)             -> reset palette p (single number arg)
            //   pal(c0, c1[, p])   -> map c0->c1, returns the previous mapping
            let first = args.get(0);
            // Table form: iterate key/value pairs as c0->c1.
            if let Some(LuaValue::Table(t)) = first {
                let p = args.u8(1, 0);
                let mut con = get_console_mut!(lua)?;
                for pair in t.clone().pairs::<i64, i64>() {
                    let (c0, c1) = pair?;
                    con.pal(c0 as u8, c1 as u8, p);
                }
                return Ok(LuaValue::Nil);
            }
            let mut con = get_console_mut!(lua)?;
            if first.is_none_or(|v| matches!(v, LuaValue::Nil)) {
                con.pal_reset();
                Ok(LuaValue::Nil)
            } else if args.len() == 1 {
                // Single number arg: reset palette p.
                let p = args.u8(0, 0);
                con.pal_reset_one(p);
                Ok(LuaValue::Nil)
            } else {
                let c0 = args.u8(0, 0);
                let c1 = args.u8(1, 0);
                let p = args.u8(2, 0);
                let prev = con.pal(c0, c1, p);
                Ok(LuaValue::Integer(prev as i64))
            }
        })?,
    )?;

    globals.set(
        "palt",
        lua.create_function(|lua, args: Args| {
            let mut con = get_console_mut!(lua)?;
            if args.is_nil(0) {
                con.palt_reset();
            } else {
                let c = args.u8(0, 0);
                let t = args.bool(1, true);
                con.palt(c, t);
            }
            Ok(())
        })?,
    )?;

    globals.set(
        "color",
        lua.create_function(|lua, args: Args| {
            // PICO-8 `color()` with no args sets the pen to 6; either form
            // returns the *previous* pen color byte.
            let c = args.u8(0, 6);
            let mut con = get_console_mut!(lua)?;
            let prev = con.color(c);
            Ok(prev as i64)
        })?,
    )?;

    globals.set(
        "fillp",
        lua.create_function(|lua, args: Args| {
            let p = args.f64(0, 0.0);
            let mut con = get_console_mut!(lua)?;
            // PICO-8 `fillp()` returns the previous pattern as a number.
            let prev = con.fillp(p);
            Ok(prev)
        })?,
    )?;

    globals.set(
        "tline",
        lua.create_function(|lua, args: Args| {
            let x0 = args.i32(0, 0);
            let y0 = args.i32(1, 0);
            let x1 = args.i32(2, 0);
            let y1 = args.i32(3, 0);
            let mx = args.f64(4, 0.0);
            let my = args.f64(5, 0.0);
            let mdx = args.f64(6, 0.125);
            let mdy = args.f64(7, 0.0);
            let mut con = get_console_mut!(lua)?;
            con.tline(x0, y0, x1, y1, mx, my, mdx, mdy);
            Ok(())
        })?,
    )?;

    Ok(())
}

// ===========================================================================
//  Map functions
// ===========================================================================

fn register_map(lua: &Lua) -> LuaResult<()> {
    let globals = lua.globals();

    globals.set(
        "map",
        lua.create_function(|lua, args: Args| {
            let cel_x = args.i32(0, 0);
            let cel_y = args.i32(1, 0);
            let sx = args.i32(2, 0);
            let sy = args.i32(3, 0);
            let cel_w = args.i32(4, 128);
            let cel_h = args.i32(5, 64);
            let layer = args.u8(6, 0);
            let mut con = get_console_mut!(lua)?;
            con.map_draw(cel_x, cel_y, sx, sy, cel_w, cel_h, layer);
            Ok(())
        })?,
    )?;

    globals.set(
        "mget",
        lua.create_function(|lua, args: Args| {
            let x = args.i32(0, 0);
            let y = args.i32(1, 0);
            let con = get_console_ref!(lua)?;
            Ok(con.mget(x, y))
        })?,
    )?;

    globals.set(
        "mset",
        lua.create_function(|lua, args: Args| {
            let x = args.i32(0, 0);
            let y = args.i32(1, 0);
            let v = args.u8(2, 0);
            let mut con = get_console_mut!(lua)?;
            con.mset(x, y, v);
            Ok(())
        })?,
    )?;

    globals.set(
        "fget",
        lua.create_function(|lua, args: Args| {
            let n = args.f64_opt(0).map(|v| v.floor() as usize).unwrap_or(0);
            let f = args.u8_opt(1);
            let con = get_console_ref!(lua)?;
            // PICO-8: fget(n, bit) returns a BOOLEAN; fget(n) returns the full
            // flag byte as a number. Returning a number for the bit form breaks
            // carts because Lua treats 0 as truthy, so `if fget(t,b)` would
            // always be true.
            match f {
                Some(_) => Ok(LuaValue::Boolean(con.fget(n, f) != 0)),
                None => Ok(LuaValue::Integer(con.fget(n, f) as i64)),
            }
        })?,
    )?;

    globals.set(
        "fset",
        lua.create_function(|lua, args: Args| {
            let n = args.f64_opt(0).map(|v| v.floor() as usize).unwrap_or(0);
            let mut con = get_console_mut!(lua)?;

            if args.len() >= 3 {
                let f = args.u8_opt(1);
                let v = args.u8(2, 0);
                con.fset(n, f, v);
            } else {
                let v = args.u8(1, 0);
                con.fset(n, None, v);
            }
            Ok(())
        })?,
    )?;

    Ok(())
}

// ===========================================================================
//  Input functions
// ===========================================================================

fn register_input(lua: &Lua) -> LuaResult<()> {
    let globals = lua.globals();

    globals.set(
        "btn",
        lua.create_function(|lua, args: Args| {
            let con = get_console_ref!(lua)?;
            // No-args form: return bitmask of all buttons for player 0
            if args.is_nil(0) {
                let mut mask: u8 = 0;
                for bit in 0..6u8 {
                    if con.btn(bit, 0) {
                        mask |= 1 << bit;
                    }
                }
                return Ok(LuaValue::Integer(mask as i64));
            }
            let i = args.u8(0, 0);
            let p = args.u8(1, 0);
            Ok(LuaValue::Boolean(con.btn(i, p)))
        })?,
    )?;

    globals.set(
        "btnp",
        lua.create_function(|lua, args: Args| {
            let i = args.u8(0, 0);
            let p = args.u8(1, 0);
            let con = get_console_ref!(lua)?;
            Ok(con.btnp(i, p))
        })?,
    )?;

    Ok(())
}

// ===========================================================================
//  Math functions  (PICO-8 conventions!)
// ===========================================================================

fn register_math(lua: &Lua) -> LuaResult<()> {
    let globals = lua.globals();

    globals.set(
        "sin",
        lua.create_function(|_lua, args: Args| {
            let x = args.f64(0, 0.0);
            Ok(api_ops::sin(x))
        })?,
    )?;

    globals.set(
        "cos",
        lua.create_function(|_lua, args: Args| {
            let x = args.f64(0, 0.0);
            Ok(api_ops::cos(x))
        })?,
    )?;

    globals.set(
        "atan2",
        lua.create_function(|_lua, args: Args| {
            let dx = args.f64(0, 0.0);
            let dy = args.f64(1, 0.0);
            Ok(api_ops::atan2(dx, dy))
        })?,
    )?;

    globals.set(
        "sqrt",
        lua.create_function(|_lua, args: Args| {
            let x = args.f64(0, 0.0);
            Ok(if x < 0.0 { 0.0 } else { x.sqrt() })
        })?,
    )?;

    globals.set(
        "abs",
        lua.create_function(|_lua, args: Args| {
            let x = args.f64(0, 0.0);
            Ok(x.abs())
        })?,
    )?;

    globals.set(
        "flr",
        lua.create_function(|_lua, args: Args| {
            let x = args.f64(0, 0.0);
            Ok(x.floor())
        })?,
    )?;

    globals.set(
        "ceil",
        lua.create_function(|_lua, args: Args| {
            let x = args.f64(0, 0.0);
            Ok(x.ceil())
        })?,
    )?;

    globals.set(
        "min",
        lua.create_function(|_lua, args: Args| {
            let a = args.f64(0, 0.0);
            let b = args.f64(1, 0.0);
            Ok(a.min(b))
        })?,
    )?;

    globals.set(
        "max",
        lua.create_function(|_lua, args: Args| {
            let a = args.f64(0, 0.0);
            let b = args.f64(1, 0.0);
            Ok(a.max(b))
        })?,
    )?;

    globals.set(
        "mid",
        lua.create_function(|_lua, args: Args| {
            let a = args.f64(0, 0.0);
            let b = args.f64(1, 0.0);
            let c = args.f64(2, 0.0);
            let mut vals = [a, b, c];
            vals.sort_by(|x, y| x.partial_cmp(y).unwrap_or(std::cmp::Ordering::Equal));
            Ok(vals[1])
        })?,
    )?;

    globals.set(
        "sgn",
        lua.create_function(|_lua, args: Args| {
            let x = args.f64(0, 0.0);
            Ok(api_ops::sgn(x))
        })?,
    )?;

    // PICO-8 bitwise ops act on the full 32-bit 16.16 fixed-point pattern,
    // including the 16 fractional bits (e.g. shl(0.125, 3) == 1). The exact
    // semantics live in `spark_core::api_ops` so the desktop and Pico binding
    // layers share one tested implementation.
    macro_rules! bind_binop {
        ($name:literal, $f:path) => {
            globals.set(
                $name,
                lua.create_function(|_lua, args: Args| {
                    let a = args.f64(0, 0.0);
                    let b = args.f64(1, 0.0);
                    Ok($f(a, b))
                })?,
            )?;
        };
    }
    macro_rules! bind_shift {
        ($name:literal, $f:path) => {
            globals.set(
                $name,
                lua.create_function(|_lua, args: Args| {
                    let a = args.f64(0, 0.0);
                    let n = args.f64(1, 0.0).floor() as u32;
                    Ok($f(a, n))
                })?,
            )?;
        };
    }

    bind_binop!("band", api_ops::band);
    bind_binop!("bor", api_ops::bor);
    bind_binop!("bxor", api_ops::bxor);
    globals.set(
        "bnot",
        lua.create_function(|_lua, args: Args| {
            let a = args.f64(0, 0.0);
            Ok(api_ops::bnot(a))
        })?,
    )?;
    bind_shift!("shl", api_ops::shl);
    bind_shift!("shr", api_ops::shr);
    bind_shift!("lshr", api_ops::lshr);
    bind_shift!("rotl", api_ops::rotl);
    bind_shift!("rotr", api_ops::rotr);

    globals.set(
        "rnd",
        lua.create_function(|lua, args: Args| {
            let x = args.f64(0, 1.0);
            let mut gs = get_game_state_mut!(lua)?;
            Ok(gs.rnd(x))
        })?,
    )?;

    globals.set(
        "srand",
        lua.create_function(|lua, args: Args| {
            let seed = args.f64(0, 0.0);
            let mut gs = get_game_state_mut!(lua)?;
            gs.srand(seed);
            Ok(())
        })?,
    )?;

    Ok(())
}

// ===========================================================================
//  Sound functions
// ===========================================================================

fn register_sound(lua: &Lua) -> LuaResult<()> {
    let globals = lua.globals();

    globals.set(
        "sfx",
        lua.create_function(|lua, args: Args| {
            let n = args.i32(0, -1);
            let channel = args.i32(1, -1);
            let offset = args.i32(2, 0);
            let length = args.i32(3, -1);
            let mut audio = get_audio_mut!(lua)?;
            audio.sfx(n, channel, offset, length);
            Ok(())
        })?,
    )?;

    globals.set(
        "music",
        lua.create_function(|lua, args: Args| {
            let n = args.i32(0, -1);
            let fade_len = args.i32(1, 0);
            let channel_mask = args.i32(2, 0);
            let mut audio = get_audio_mut!(lua)?;
            audio.music(n, fade_len, channel_mask);
            Ok(())
        })?,
    )?;

    Ok(())
}

// ===========================================================================
//  Memory-mapped I/O functions
// ===========================================================================

fn register_memory(lua: &Lua) -> LuaResult<()> {
    let globals = lua.globals();

    globals.set(
        "peek",
        lua.create_function(|lua, args: Args| {
            let addr = args.i32(0, 0);
            let con = get_console_ref!(lua)?;
            Ok(con.peek(addr as u16) as f64)
        })?,
    )?;

    globals.set(
        "poke",
        lua.create_function(|lua, args: Args| {
            let addr = args.i32(0, 0);
            let val = args.u8(1, 0);
            let mut con = get_console_mut!(lua)?;
            con.poke(addr as u16, val);
            Ok(())
        })?,
    )?;

    globals.set(
        "peek2",
        lua.create_function(|lua, args: Args| {
            let addr = args.i32(0, 0);
            let con = get_console_ref!(lua)?;
            Ok(con.peek2(addr as u16) as f64)
        })?,
    )?;

    globals.set(
        "peek4",
        lua.create_function(|lua, args: Args| {
            let addr = args.i32(0, 0);
            let con = get_console_ref!(lua)?;
            // The 4 bytes are the number's 16.16 fixed-point bit pattern, not a
            // plain integer (matches PICO-8 / fake-08 vm_peek4).
            Ok(from_bits(con.peek4(addr as u16)))
        })?,
    )?;

    globals.set(
        "poke2",
        lua.create_function(|lua, args: Args| {
            let addr = args.i32(0, 0);
            let val = args.i32(1, 0);
            let mut con = get_console_mut!(lua)?;
            con.poke2(addr as u16, val as u16);
            Ok(())
        })?,
    )?;

    globals.set(
        "poke4",
        lua.create_function(|lua, args: Args| {
            let addr = args.i32(0, 0);
            let val = args.f64(1, 0.0);
            let mut con = get_console_mut!(lua)?;
            // Store the number's 16.16 fixed-point bit pattern little-endian.
            con.poke4(addr as u16, to_bits(val));
            Ok(())
        })?,
    )?;

    globals.set(
        "memcpy",
        lua.create_function(|lua, args: Args| {
            let dest = args.i32(0, 0);
            let src = args.i32(1, 0);
            let len = args.i32(2, 0);
            let mut con = get_console_mut!(lua)?;
            con.memcpy(dest as u16, src as u16, len as u16);
            Ok(())
        })?,
    )?;

    globals.set(
        "memset",
        lua.create_function(|lua, args: Args| {
            let dest = args.i32(0, 0);
            let val = args.u8(1, 0);
            let len = args.i32(2, 0);
            let mut con = get_console_mut!(lua)?;
            con.memset(dest as u16, val, len as u16);
            Ok(())
        })?,
    )?;

    globals.set(
        "cstore",
        lua.create_function(|lua, args: Args| {
            let dest = args.i32(0, 0);
            let src = args.i32(1, 0);
            let len = args.i32(2, 0);
            let mut con = get_console_mut!(lua)?;
            con.cstore(dest as u16, src as u16, len as u16);
            Ok(())
        })?,
    )?;

    globals.set(
        "reload",
        lua.create_function(|lua, args: Args| {
            let dest = args.i32(0, 0);
            let src = args.i32(1, 0);
            let len = args.i32(2, 0);
            let mut con = get_console_mut!(lua)?;
            con.reload(dest as u16, src as u16, len as u16);
            Ok(())
        })?,
    )?;

    Ok(())
}

// ===========================================================================
//  Utility functions
// ===========================================================================

fn register_utility(lua: &Lua) -> LuaResult<()> {
    let globals = lua.globals();

    globals.set(
        "add",
        lua.create_function(|_lua, args: Args| {
            // PICO-8 `add(c, x, [i])`: append x, or insert at 1-based index i
            // (clamped to [1, #c+1]), shifting later elements up. Returns x.
            let tbl = match args.get(0) {
                Some(LuaValue::Table(t)) => t.clone(),
                _ => return Ok(LuaValue::Nil),
            };
            let val = args.get(1).cloned().unwrap_or(LuaValue::Nil);
            let len = tbl.raw_len() as i64;
            let i = match args.i32_opt(2) {
                Some(n) => (n as i64).clamp(1, len + 1),
                None => len + 1,
            };
            // Shift elements up to open a slot at i.
            let mut j = len;
            while j >= i {
                let cur: LuaValue = tbl.raw_get(j)?;
                tbl.raw_set(j + 1, cur)?;
                j -= 1;
            }
            tbl.raw_set(i, val.clone())?;
            Ok(val)
        })?,
    )?;

    globals.set(
        "del",
        lua.create_function(|_lua, (tbl, val): (LuaTable, LuaValue)| {
            // PICO-8 `del(c, v)`: remove the first element equal to v, shifting
            // later elements down, and RETURN the removed value (nil if absent).
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
                Ok(val)
            } else {
                Ok(LuaValue::Nil)
            }
        })?,
    )?;

    globals.set(
        "count",
        lua.create_function(|_lua, args: Args| {
            // PICO-8 `count(c, [v])`: with no v, count NON-NIL elements in
            // [1, #c] (differs from #c when the array has holes); with v, count
            // how many elements equal v.
            let tbl = match args.get(0) {
                Some(LuaValue::Table(t)) => t.clone(),
                _ => return Ok(0i64),
            };
            let max = tbl.raw_len() as i64;
            let mut cnt = 0i64;
            match args.get(1) {
                Some(v) if !matches!(v, LuaValue::Nil) => {
                    for i in 1..=max {
                        let e: LuaValue = tbl.raw_get(i)?;
                        if e == *v {
                            cnt += 1;
                        }
                    }
                }
                _ => {
                    for i in 1..=max {
                        let e: LuaValue = tbl.raw_get(i)?;
                        if !matches!(e, LuaValue::Nil) {
                            cnt += 1;
                        }
                    }
                }
            }
            Ok(cnt)
        })?,
    )?;

    globals.set(
        "foreach",
        lua.create_function(|_lua, (tbl, func): (LuaValue, LuaFunction)| {
            // PICO-8 `foreach` must be safe against the table being mutated
            // during iteration — carts routinely `del()` the current element
            // from inside the callback (e.g. destroying a game object). The
            // reference iterates like `all()`: re-read `#c` each step, skip nil
            // holes, and DON'T advance when a deletion shifted a new element
            // into the current slot. A fixed `1..=len` loop would pass a nil to
            // the callback once the table shrinks.
            let tbl = match tbl {
                LuaValue::Table(t) => t,
                _ => return Ok(()), // nil/other: nothing to iterate
            };
            let mut i: i64 = 1;
            let mut prev = LuaValue::Nil;
            loop {
                // If the slot still holds what we just returned, advance past it.
                let cur: LuaValue = tbl.raw_get(i).unwrap_or(LuaValue::Nil);
                if cur == prev {
                    i += 1;
                }
                // Skip nil holes, re-reading the (possibly shrunk) length.
                loop {
                    let len = tbl.raw_len() as i64;
                    if i > len {
                        break;
                    }
                    let v: LuaValue = tbl.raw_get(i).unwrap_or(LuaValue::Nil);
                    if !matches!(v, LuaValue::Nil) {
                        break;
                    }
                    i += 1;
                }
                let v: LuaValue = tbl.raw_get(i).unwrap_or(LuaValue::Nil);
                if matches!(v, LuaValue::Nil) {
                    break; // reached the end
                }
                prev = v.clone();
                func.call::<()>(v)?;
            }
            Ok(())
        })?,
    )?;

    globals.set(
        "all",
        lua.create_function(|lua, tbl: LuaValue| {
            let tbl = match tbl {
                LuaValue::Table(t) => t,
                _ => {
                    // all(nil) -> an iterator that immediately ends.
                    let empty = lua.create_function(|_, ()| Ok(LuaValue::Nil))?;
                    return lua.pack_multi((empty,));
                }
            };

            // State carries the table, the current index, and the last value
            // returned. Matches the reference `all()`: advance only when the
            // current slot still holds the previously returned value (so a
            // deletion that shifts a new element into the slot is not skipped),
            // skip nil holes, and re-read `#c` each step so a shrinking table
            // never yields a nil.
            let state = lua.create_table()?;
            state.raw_set("t", tbl)?;
            state.raw_set("i", 1i64)?;
            state.raw_set("prev", LuaValue::Nil)?;

            let iter = lua.create_function(|_lua, st: LuaTable| {
                let tbl: LuaTable = match st.raw_get::<LuaTable>("t") {
                    Ok(t) => t,
                    Err(_) => return Ok(LuaValue::Nil),
                };
                let mut i: i64 = st.raw_get("i").unwrap_or(1);
                let prev: LuaValue = st.raw_get("prev").unwrap_or(LuaValue::Nil);

                // Advance past the element we just returned if it's still there.
                let cur: LuaValue = tbl.raw_get(i).unwrap_or(LuaValue::Nil);
                if cur == prev {
                    i += 1;
                }
                // Skip nil holes, re-reading the live length each step.
                loop {
                    let len = tbl.raw_len() as i64;
                    if i > len {
                        break;
                    }
                    let v: LuaValue = tbl.raw_get(i).unwrap_or(LuaValue::Nil);
                    if !matches!(v, LuaValue::Nil) {
                        break;
                    }
                    i += 1;
                }
                let val: LuaValue = tbl.raw_get(i).unwrap_or(LuaValue::Nil);
                st.raw_set("i", i).ok();
                st.raw_set("prev", val.clone()).ok();
                Ok(val)
            })?;

            lua.pack_multi((iter, state, LuaValue::Nil))
        })?,
    )?;

    globals.set(
        "sub",
        lua.create_function(|lua, args: Args| {
            // Operates on raw bytes: Lua strings are borrowed, not copied, and
            // the result is a fresh Lua string of the byte range (P8SCII safe).
            let text = args.display(0);
            let bytes = text.as_bytes();
            let len = bytes.len() as i64;
            let start = args.i32(1, 1) as i64;
            let end: Option<i64> = args.i32_opt(2).map(|n| n as i64);

            let start_idx = if start >= 1 {
                (start - 1) as usize
            } else if start < 0 {
                let idx = len + start;
                if idx < 0 {
                    0usize
                } else {
                    idx as usize
                }
            } else {
                0usize
            };

            let end_idx = match end {
                Some(e) if e >= 0 => (e as usize).min(len as usize),
                Some(e) => {
                    let idx = len + e + 1;
                    if idx < 0 {
                        0usize
                    } else {
                        idx as usize
                    }
                }
                None => len as usize,
            };

            if start_idx >= end_idx || start_idx >= bytes.len() {
                return lua.create_string("");
            }

            lua.create_string(&bytes[start_idx..end_idx.min(bytes.len())])
        })?,
    )?;

    globals.set(
        "tostr",
        lua.create_function(|_lua, args: Args| {
            let val = args.get(0).cloned().unwrap_or(LuaValue::Nil);
            // Optional 2nd arg: boolean true == flag 1, or a numeric flag mask.
            // 0x1 -> "0xWWWW.FFFF", 0x2 -> raw 32-bit int, 0x3 -> "0xWWWWFFFF".
            let flags: u32 = match args.get(1) {
                Some(LuaValue::Boolean(true)) => 1,
                Some(v) => val_to_i32(v).unwrap_or(0) as u32,
                None => 0,
            };
            let s = match &val {
                LuaValue::Nil => "".to_string(),
                LuaValue::Boolean(b) => (if *b { "true" } else { "false" }).to_string(),
                LuaValue::Integer(n) => {
                    if flags != 0 {
                        format_tostr_hex(*n as f64, flags)
                    } else {
                        n.to_string()
                    }
                }
                LuaValue::Number(n) => {
                    if flags != 0 {
                        format_tostr_hex(*n, flags)
                    } else if *n == n.floor() && n.abs() < 1e15 {
                        format!("{}", *n as i64)
                    } else {
                        format!("{n}")
                    }
                }
                LuaValue::String(s) => s.to_string_lossy(),
                _ => "[table]".to_string(),
            };
            Ok(s)
        })?,
    )?;

    globals.set(
        "tonum",
        lua.create_function(|_lua, args: Args| {
            let val = args.get(0).cloned().unwrap_or(LuaValue::Nil);
            // Optional flags arg (z8lua pico8_tonum): 0x1 = hex parse, 0x2 = >>16.
            let flags = args.i32(1, 0) as u32;
            let result = match &val {
                LuaValue::Integer(n) => Some(*n as f64),
                LuaValue::Number(n) => Some(*n),
                LuaValue::Boolean(b) => Some(if *b { 1.0 } else { 0.0 }),
                LuaValue::String(s) => s
                    .to_str()
                    .ok()
                    .and_then(|st| api_ops::parse_pico8_number_with_flags(&st, flags)),
                _ => None,
            };
            Ok(result)
        })?,
    )?;

    globals.set(
        "printh",
        lua.create_function(|_lua, args: Args| {
            let text = args.display(0);
            eprintln!("{}", text.as_str());
            Ok(())
        })?,
    )?;

    globals.set(
        "dget",
        lua.create_function(|lua, args: Args| {
            let index = args.i32(0, 0);
            let con = get_console_ref!(lua)?;
            Ok(con.dget(index))
        })?,
    )?;

    globals.set(
        "dset",
        lua.create_function(|lua, args: Args| {
            let index = args.i32(0, 0);
            let value = args.f64(1, 0.0);
            let mut con = get_console_mut!(lua)?;
            con.dset(index, value);
            Ok(())
        })?,
    )?;

    globals.set(
        "menuitem",
        lua.create_function(|lua, args: Args| {
            let index = args.i32(0, 0) as usize;
            if index == 0 || index > 5 {
                return Ok(());
            }
            let label = match args.get(1) {
                Some(LuaValue::String(s)) => s.to_string_lossy(),
                Some(LuaValue::Nil) | None => {
                    // nil label clears the item
                    if let Some(mut items) = lua.app_data_mut::<MenuItems>() {
                        items.0[index - 1] = None;
                    }
                    return Ok(());
                }
                _ => String::new(),
            };
            let callback = match args.get(2) {
                Some(LuaValue::Function(f)) => Some(lua.create_registry_value(f.clone())?),
                _ => None,
            };
            if let Some(mut items) = lua.app_data_mut::<MenuItems>() {
                items.0[index - 1] = Some(MenuItem { label, callback });
            }
            Ok(())
        })?,
    )?;

    globals.set(
        "extcmd",
        lua.create_function(|lua, args: Args| {
            let cmd = args.display(0);
            match &*cmd.as_str() {
                "screenshot" => {
                    let timestamp = std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .unwrap_or_default()
                        .as_secs();
                    let path = format!("screenshot_{}.png", timestamp);
                    let con = get_console_ref!(lua)?;
                    write_screen_png(&con, &path);
                }
                "label" => {
                    let con = get_console_ref!(lua)?;
                    write_screen_png(&con, "label.png");
                }
                other => {
                    eprintln!("[extcmd] {other}");
                }
            }
            Ok(())
        })?,
    )?;

    globals.set(
        "cartdata",
        lua.create_function(|lua, args: Args| {
            let Some(id) = args.str(0) else {
                return Ok(());
            };
            if id.is_empty() {
                return Ok(());
            }
            // Store the cart data ID for later saving.
            lua.set_app_data(CartDataId(id.clone()));
            // Load saved data from disk into the console's persistent_data.
            let saved = load_cartdata(&id);
            let mut con = get_console_mut!(lua)?;
            con.persistent_data = saved;
            Ok(())
        })?,
    )?;

    // chr(n) — character from code point
    globals.set(
        "chr",
        lua.create_function(|lua, args: Args| {
            // PICO-8 `chr(a, b, c, ...)` builds a string from raw bytes — each
            // arg is one byte (0-255), NOT a Unicode codepoint. Multi-arg form
            // is common for building P8SCII strings.
            let bytes: Vec<u8> = (0..args.len()).map(|i| args.i32(i, 0) as u8).collect();
            Ok(LuaValue::String(lua.create_string(&bytes)?))
        })?,
    )?;

    // ord(s, [i]) — code point from character
    globals.set(
        "ord",
        lua.create_function(|_lua, args: Args| {
            // PICO-8 `ord(s, index, count)`: returns `count` byte values starting
            // at 1-based `index`. Numbers are converted to their string form
            // (e.g. ord(123) -> ord("123")). Returns nothing/nil out of range.
            let text = match args.get(0) {
                Some(v @ LuaValue::String(_)) => val_to_display(v),
                // Mirror Lua's number->string coercion.
                Some(v) if val_to_f64(v).is_some() => val_to_display(v),
                _ => return Ok(LuaMultiValue::new()),
            };
            let bytes = text.as_bytes();
            let i = args.i32(1, 1);
            let n = i - 1;
            let mut count = args.i32(2, 1);
            if n < 0 || n as usize >= bytes.len() || count < 1 {
                return Ok(LuaMultiValue::new());
            }
            if (n + count) as usize > bytes.len() {
                count = bytes.len() as i32 - n;
            }
            let out: Vec<LuaValue> = (0..count)
                .map(|k| LuaValue::Integer(bytes[(n + k) as usize] as i64))
                .collect();
            Ok(LuaMultiValue::from_vec(out))
        })?,
    )?;

    // split(s, [sep], [convert]) — split string
    globals.set(
        "split",
        lua.create_function(|lua, args: Args| {
            // PICO-8 `split(s, sep, convert)`:
            //   - sep is a STRING -> split on its first byte (default ",")
            //   - sep is a NUMBER -> split into fixed-size character chunks
            //   - convert (default true) -> numeric pieces become numbers
            // Pieces are NOT trimmed (PICO-8 keeps surrounding spaces).
            if args.is_nil(0) {
                return Ok(LuaValue::Nil);
            }
            let text = args.display(0);

            // Determine split mode: a numeric 2nd arg means chunk-by-size.
            let size = match args.get(1) {
                Some(LuaValue::Integer(_)) | Some(LuaValue::Number(_)) => {
                    let n = args.i32(1, 1);
                    Some(if n <= 0 { 1 } else { n } as usize)
                }
                _ => None,
            };
            let needle: u8 = match args.get(1) {
                Some(LuaValue::String(sep)) => sep.as_bytes().first().copied().unwrap_or(b','),
                _ => b',',
            };
            let convert = args.bool(2, true);

            let bytes = text.as_bytes();
            let tbl = lua.create_table()?;
            let mut idx: i64 = 0;

            let push = |tbl: &mlua::Table, idx: &mut i64, piece: &[u8]| -> LuaResult<()> {
                *idx += 1;
                // Convert to a number when the piece parses as one (mirrors the
                // reference's luaO_str2d), otherwise store the raw bytes as a
                // Lua string (P8SCII bytes >= 0x80 never parse as numbers).
                if convert {
                    if let Some(num) = std::str::from_utf8(piece)
                        .ok()
                        .and_then(api_ops::parse_pico8_number)
                    {
                        return tbl.raw_set(*idx, num);
                    }
                }
                tbl.raw_set(*idx, lua.create_string(piece)?)
            };

            match size {
                Some(sz) => {
                    if bytes.is_empty() {
                        push(&tbl, &mut idx, b"")?;
                    } else {
                        let mut start = 0;
                        while start < bytes.len() {
                            let end = (start + sz).min(bytes.len());
                            push(&tbl, &mut idx, &bytes[start..end])?;
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
                                push(&tbl, &mut idx, &bytes[start..pos])?;
                                start = pos + 1;
                            }
                            None => {
                                push(&tbl, &mut idx, &bytes[start..])?;
                                break;
                            }
                        }
                    }
                }
            }
            Ok(LuaValue::Table(tbl))
        })?,
    )?;

    // deli(tbl, [i]) — delete element at index, shift remaining down, return deleted value
    globals.set(
        "deli",
        lua.create_function(|_lua, args: Args| {
            let tbl = match args.get(0) {
                Some(LuaValue::Table(t)) => t.clone(),
                _ => return Ok(LuaValue::Nil),
            };
            let len = tbl.raw_len() as i64;
            if len == 0 {
                return Ok(LuaValue::Nil);
            }
            let idx = args.i32_opt(1).map(|n| n as i64).unwrap_or(len);
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
        })?,
    )?;

    let time_fn = lua.create_function(|lua, _args: Args| {
        let gs = get_game_state_ref!(lua)?;
        Ok(gs.frame_count as f64 / 30.0)
    })?;
    globals.set("time", time_fn.clone())?;
    globals.set("t", time_fn)?;

    globals.set(
        "stat",
        lua.create_function(|lua, args: Args| {
            let n = args.i32(0, 0);
            match n {
                // stat(0): Lua memory used, in kilobytes (PICO-8 reports KB).
                0 => Ok(LuaValue::Number(lua.used_memory() as f64 / 1024.0)),
                // stat(1): CPU usage (return 0.0 — no profiling yet)
                1 => Ok(LuaValue::Number(0.0)),
                // stat(4): clipboard contents (return empty string)
                4 => Ok(LuaValue::String(lua.create_string("")?)),
                // stat(6): parameter string / breadcrumb from load()
                6 => {
                    let s = lua
                        .app_data_ref::<Breadcrumb>()
                        .map(|b| b.0.clone())
                        .unwrap_or_default();
                    Ok(LuaValue::String(lua.create_string(&s)?))
                }
                // stat(7): target FPS (30 or 60)
                7 => {
                    let gs = get_game_state_ref!(lua)?;
                    Ok(LuaValue::Number(gs.target_fps as f64))
                }
                // stat(8): actual measured FPS
                8 => {
                    let gs = get_game_state_ref!(lua)?;
                    Ok(LuaValue::Number(gs.actual_fps as f64))
                }
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
                // stat(24)/stat(54): current music pattern index
                24 | 54 => {
                    let audio = get_audio_ref!(lua)?;
                    Ok(LuaValue::Number(audio.current_music_pattern() as f64))
                }
                // stat(28): keyboard scancode held — not supported (always false).
                28 => Ok(LuaValue::Boolean(false)),
                // stat(29): unknown PICO-8 field, observed to be 0.
                29 => Ok(LuaValue::Number(0.0)),
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
                // stat(46): param string from the third argument of load()
                46 => {
                    let s = lua
                        .app_data_ref::<LoadParam>()
                        .map(|p| p.0.clone())
                        .unwrap_or_default();
                    Ok(LuaValue::String(lua.create_string(&s)?))
                }
                // stat(47-49): SFX currently playing on channels 1-3 (alias for 17-19)
                47..=49 => {
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
        })?,
    )?;

    Ok(())
}

// ===========================================================================
//  System functions (flip, run, stop, resume)
// ===========================================================================

fn register_system(lua: &Lua) -> LuaResult<()> {
    let globals = lua.globals();

    // flip() — yields when called inside a coroutine (manual-loop carts),
    // no-op in callback mode.  Implemented in Lua so coroutine.yield() works.
    // In Lua 5.4, coroutine.running() returns (thread, is_main); we only yield
    // when we are inside a non-main coroutine.
    lua.load(
        "function flip()\n  local co, is_main = coroutine.running()\n  if co and not is_main then\n    coroutine.yield()\n  end\nend"
    ).exec()?;

    // load(filename, [breadcrumb], [param]) — Load and run another .p8 cart.
    //
    // Since we cannot tear down the Lua state from within a Lua callback,
    // this function signals the main loop by raising a special Lua error
    // that contains the requested filename and breadcrumb.  The main loop
    // catches the error, parses the payload, and performs the actual cart
    // transition.
    globals.set(
        "load",
        lua.create_function(|_lua, args: Args| {
            let filename = args.str(0).unwrap_or_default();
            let breadcrumb = args.str(1).unwrap_or_default();
            let param = args.str(2).unwrap_or_default();
            // Raise a special error that the main loop will intercept.
            Err::<(), _>(mlua::Error::runtime(format!(
                "{}{}|{}|{}",
                LOAD_CART_PREFIX, filename, breadcrumb, param
            )))
        })?,
    )?;

    // run() — Restart the cart. Signals the main loop via a special error.
    globals.set(
        "run",
        lua.create_function(|_lua, _args: Args| {
            eprintln!("[spark] run() called");
            Err::<(), _>(mlua::Error::runtime(RUN_CART_PREFIX))
        })?,
    )?;

    // stop() — Return to editor. Signals the main loop via a special error.
    globals.set(
        "stop",
        lua.create_function(|_lua, _args: Args| {
            eprintln!("[spark] stop() called");
            Err::<(), _>(mlua::Error::runtime(STOP_CART_PREFIX))
        })?,
    )?;

    // resume() — Resume from stop. In PICO-8 this resumes a stopped cart.
    // For MVP: registered as a no-op stub so carts do not error.
    globals.set(
        "resume",
        lua.create_function(|_lua, _args: Args| {
            eprintln!("[spark] resume() called");
            Ok(())
        })?,
    )?;

    Ok(())
}

// ===========================================================================
//  Integration tests — verify real .p8 carts can be loaded and executed
// ===========================================================================

#[cfg(test)]
mod integration_tests {
    use super::*;
    use crate::runtime::{CartRuntime, Tick};
    use spark_core::audio::Audio;
    use spark_core::audio::{NOTES_PER_SFX, NUM_MUSIC, NUM_SFX};
    use spark_core::cart::{parse_cart, serialize_cart};
    use spark_core::console::Console;
    use spark_core::lua_preprocess::preprocess_pico8;

    // Embed carts at compile time so tests work without filesystem access.
    const STARFALL_P8: &str = include_str!("../../carts/starfall.p8");
    const HELLO_P8: &str = include_str!("../../carts/hello.p8");

    // -----------------------------------------------------------------
    // Helper: a started cart (Console/Audio/GameState resident, `_init`
    // already run) that can be stepped and inspected.
    // -----------------------------------------------------------------

    struct TestHarness {
        rt: CartRuntime,
    }

    impl TestHarness {
        /// Build a fully-wired Lua environment from raw cart source text.
        fn from_cart_source(source: &str) -> Self {
            let rt = CartRuntime::start(&parse_cart(source), "", "").expect("cart should start");
            TestHarness { rt }
        }

        fn run_frames(&mut self, n: usize) {
            for _ in 0..n {
                assert_eq!(self.rt.tick(), Tick::Continue, "frame should run cleanly");
            }
        }

        /// Borrow the Console (read-only snapshot).
        fn with_screen<F, R>(&self, f: F) -> R
        where
            F: FnOnce(&Console) -> R,
        {
            f(&self.rt.console())
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
        let mut harness = TestHarness::from_cart_source(HELLO_P8);

        // Run 5 frames of _update + _draw without error.
        harness.run_frames(5);

        // The screen should have been drawn to (not all zeros).
        let has_pixels = harness.with_screen(|con| con.screen.iter().any(|&px| px != 0));
        assert!(has_pixels, "hello.p8: screen is all zeros after 5 frames");
    }

    // =================================================================
    // Test 4: Full execution of starfall.p8
    // =================================================================

    #[test]
    fn test_execute_starfall_p8() {
        let mut harness = TestHarness::from_cart_source(STARFALL_P8);

        // Run 10 frames of _update + _draw without Lua errors.
        harness.run_frames(10);

        // Verify the console screen has non-zero pixels (the game draws something).
        let has_pixels = harness.with_screen(|con| con.screen.iter().any(|&px| px != 0));
        assert!(
            has_pixels,
            "starfall.p8: screen is all zeros after 10 frames"
        );
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
            &cart1.sprites[..],
            &cart2.sprites[..],
            "sprites mismatch after round-trip"
        );

        // Flags
        assert_eq!(
            &cart1.flags[..],
            &cart2.flags[..],
            "flags mismatch after round-trip"
        );

        // Map
        assert_eq!(
            &cart1.map[..],
            &cart2.map[..],
            "map mismatch after round-trip"
        );

        // SFX
        for s in 0..NUM_SFX {
            assert_eq!(
                cart1.sfx[s].speed, cart2.sfx[s].speed,
                "sfx {} speed mismatch",
                s
            );
            assert_eq!(
                cart1.sfx[s].loop_start, cart2.sfx[s].loop_start,
                "sfx {} loop_start mismatch",
                s
            );
            assert_eq!(
                cart1.sfx[s].loop_end, cart2.sfx[s].loop_end,
                "sfx {} loop_end mismatch",
                s
            );
            for n in 0..NOTES_PER_SFX {
                assert_eq!(
                    cart1.sfx[s].notes[n].pitch, cart2.sfx[s].notes[n].pitch,
                    "sfx {} note {} pitch mismatch",
                    s, n
                );
                assert_eq!(
                    cart1.sfx[s].notes[n].waveform, cart2.sfx[s].notes[n].waveform,
                    "sfx {} note {} waveform mismatch",
                    s, n
                );
                assert_eq!(
                    cart1.sfx[s].notes[n].volume, cart2.sfx[s].notes[n].volume,
                    "sfx {} note {} volume mismatch",
                    s, n
                );
                assert_eq!(
                    cart1.sfx[s].notes[n].effect, cart2.sfx[s].notes[n].effect,
                    "sfx {} note {} effect mismatch",
                    s, n
                );
            }
        }

        // Music
        for m in 0..NUM_MUSIC {
            assert_eq!(
                cart1.music[m].flags, cart2.music[m].flags,
                "music {} flags mismatch",
                m
            );
            for ch in 0..4 {
                assert_eq!(
                    cart1.music[m].channels[ch], cart2.music[m].channels[ch],
                    "music {} channel {} mismatch",
                    m, ch
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
                "preprocessed code still contains bare {} operator",
                op
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
                "preprocessed code still contains ? shorthand: {}",
                trimmed
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
    // Test 7: flip/run/stop/resume behave as expected
    // =================================================================

    #[test]
    fn test_system_stubs_callable() {
        let lua = create_lua().expect("create_lua failed");
        lua.set_app_data(Console::new());
        lua.set_app_data(Audio::new());

        // flip() called outside a coroutine should be a no-op (no error).
        lua.load("flip()")
            .exec()
            .expect("flip() should not error outside coroutine");

        // run() and stop() now signal the main loop via a special error.
        let run_result = lua.load("run()").exec();
        assert!(run_result.is_err(), "run() should produce a signal error");
        let run_err = format!("{}", run_result.unwrap_err());
        assert!(
            is_run_signal(&run_err),
            "run() error should be a run signal, got: {}",
            run_err
        );

        let stop_result = lua.load("stop()").exec();
        assert!(stop_result.is_err(), "stop() should produce a signal error");
        let stop_err = format!("{}", stop_result.unwrap_err());
        assert!(
            is_stop_signal(&stop_err),
            "stop() error should be a stop signal, got: {}",
            stop_err
        );

        // resume() is still a no-op stub.
        lua.load("resume()")
            .exec()
            .expect("resume() should not error");
    }

    #[test]
    fn test_fget_bit_form_returns_boolean() {
        // PICO-8: fget(n, bit) returns a BOOLEAN, fget(n) returns a number.
        // Critical because Lua treats 0 as truthy — returning the number 0 for
        // a clear bit makes `if fget(t,b)` always true, which (e.g.) made the
        // Kid Bludd cart's roof check fire on empty tiles and fling the player.
        let lua = create_lua().expect("create_lua failed");
        let mut con = Console::new();
        con.flags[5] = 0b0000_0001; // sprite 5: bit 0 set, bit 1 clear
        con.flags[7] = 0b0000_0010; // sprite 7: bit 1 set, bit 0 clear
        lua.set_app_data(con);
        lua.set_app_data(Audio::new());

        // Bit form must return an actual boolean (not a number).
        let set_bit: LuaValue = lua.load("return fget(5,0)").eval().unwrap();
        assert!(
            matches!(set_bit, LuaValue::Boolean(true)),
            "fget(5,0) should be boolean true, got {set_bit:?}"
        );
        let clear_bit: LuaValue = lua.load("return fget(5,1)").eval().unwrap();
        assert!(
            matches!(clear_bit, LuaValue::Boolean(false)),
            "fget(5,1) should be boolean false (not 0), got {clear_bit:?}"
        );

        // The crucial behavior: a clear bit must be falsy in an `if`.
        let took_branch: bool = lua
            .load("if fget(5,1) then return true else return false end")
            .eval()
            .unwrap();
        assert!(
            !took_branch,
            "`if fget(n, clear_bit)` must be falsy (0 is truthy in Lua!)"
        );

        // Whole-byte form (no bit arg) still returns the numeric flag byte.
        let byte: i64 = lua.load("return fget(7)").eval().unwrap();
        assert_eq!(byte, 0b0000_0010, "fget(n) should return the flag byte");
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
        assert!(
            !has_update60(&lua),
            "has_update60 should be false when _update60 is not defined"
        );
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
        lua.load("function _update60() end")
            .exec()
            .expect("defining _update60 should not error");
        assert!(
            has_update60(&lua),
            "has_update60 should be true when _update60 is defined"
        );
    }

    // =================================================================
    // Test 10: Bit manipulation -- lshr, rotl, rotr
    // =================================================================

    /// Helper: evaluate a Lua expression and return its f64 result.
    fn eval_f64(lua: &Lua, expr: &str) -> f64 {
        let code = format!("return {}", expr);
        lua.load(&code)
            .eval::<f64>()
            .unwrap_or_else(|e| panic!("Lua eval failed for `{}`: {}", expr, e))
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
        // PICO-8 operates on the 16.16 bit pattern: -1 is 0xFFFF.FFFF, and
        // lshr by 16 gives 0x0000.FFFF = 65535/65536 = 0.9999847412109375.
        assert!((eval_f64(&lua, "lshr(-1, 16)") - 0.9999847412109375).abs() < 1e-12);
    }

    #[test]
    fn test_shr_arithmetic_preserves_sign() {
        let lua = math_lua();
        // -1 == 0xFFFF.FFFF; arithmetic shr by 16 sign-extends to 0xFFFF.FFFF
        // >> 16 = 0xFFFFFFFF.0000... i.e. -1/65536 (the smallest negative step).
        assert!((eval_f64(&lua, "shr(-1, 16)") - (-1.0 / 65536.0)).abs() < 1e-12);
    }

    #[test]
    fn test_rotl_basic() {
        let lua = math_lua();
        // rotl(1, 1) = 2 (1.0 == 0x0001.0000, rotated left 1 == 0x0002.0000)
        assert_eq!(eval_f64(&lua, "rotl(1, 1)"), 2.0);
    }

    #[test]
    fn test_rotl_msb_wraps() {
        let lua = math_lua();
        // -32768.0 is PICO-8's minimum, whose 16.16 bit pattern is 0x8000.0000
        // (only bit 31 set). Rotating left 1 wraps it to bit 0 = 0x0000.0001 =
        // 1/65536 = 0.0000152587890625.
        assert!((eval_f64(&lua, "rotl(-32768, 1)") - (1.0 / 65536.0)).abs() < 1e-12);
    }

    #[test]
    fn test_rotl_byte_shift() {
        let lua = math_lua();
        // 255 == 0x00FF.0000; rotl 8 == 0xFF00.0000, whose signed 16.16 value
        // is -256.0 (top byte set makes it negative).
        assert_eq!(eval_f64(&lua, "rotl(255, 8)"), -256.0);
    }

    #[test]
    fn test_bitwise_operate_on_fractional_bits() {
        let lua = math_lua();
        // The defining PICO-8 property: bitwise ops include the fractional 16
        // bits. These are the canonical manual/wiki examples.
        assert_eq!(eval_f64(&lua, "shl(0.125, 3)"), 1.0); // 0x0.2000 << 3 == 0x1.0000
        assert_eq!(eval_f64(&lua, "shr(1, 3)"), 0.125); // 0x1.0000 >> 3 == 0x0.2000
        assert_eq!(eval_f64(&lua, "shr(-1, 3)"), -0.125); // arithmetic, sign kept
        assert_eq!(eval_f64(&lua, "lshr(8, 3)"), 1.0);
        assert_eq!(eval_f64(&lua, "rotl(8, 3)"), 64.0);
        assert_eq!(eval_f64(&lua, "rotr(64, 3)"), 8.0);
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
        // 1 == 0x0001.0000 (bit 16 set). Rotating right 1 == bit 15 set ==
        // 0x0000.8000 = 0.5.
        assert_eq!(eval_f64(&lua, "rotr(1, 1)"), 0.5);
    }

    #[test]
    fn test_rotr_byte_shift() {
        let lua = math_lua();
        // 65280 == 0xFF00.0000; rotr 8 == 0x00FF.0000 == 255.0.
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

    #[test]
    fn test_poke4_peek4_fixed_point_roundtrip() {
        let lua = create_lua().expect("create_lua failed");
        lua.set_app_data(Console::new());
        lua.set_app_data(Audio::new());

        // poke4/peek4 store the 16.16 bit pattern, so a fractional value must
        // round-trip exactly (this is what carts doing number<->byte tricks
        // depend on).
        let v: f64 = lua
            .load("poke4(0x4300, 1.5) return peek4(0x4300)")
            .eval()
            .expect("poke4/peek4 roundtrip");
        assert_eq!(v, 1.5);

        // And the stored bytes are little-endian 16.16: 1.5 == 0x0001.8000,
        // bytes = 00 80 01 00.
        let bytes: Vec<f64> = lua
            .load("return {peek(0x4300), peek(0x4301), peek(0x4302), peek(0x4303)}")
            .eval()
            .expect("peek bytes");
        assert_eq!(bytes, vec![0.0, 128.0, 1.0, 0.0]);
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
        let (filename, breadcrumb, _param) = result.unwrap();
        assert_eq!(filename, "game.p8");
        assert_eq!(breadcrumb, "level2");
    }

    #[test]
    fn test_parse_load_signal_without_breadcrumb() {
        let msg = format!("runtime error: {}game.p8|", LOAD_CART_PREFIX);
        let result = parse_load_signal(&msg);
        assert!(result.is_some());
        let (filename, breadcrumb, _param) = result.unwrap();
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

        let result = lua
            .load(r#"load("sub/dir/cart.p8", "my breadcrumb")"#)
            .exec();
        let err_msg = format!("{}", result.unwrap_err());
        let parsed = parse_load_signal(&err_msg);
        assert!(parsed.is_some(), "should parse: {}", err_msg);
        let (filename, breadcrumb, _param) = parsed.unwrap();
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
                if i + op_chars.len() <= chars.len() && chars[i..i + op_chars.len()] == op_chars[..]
                {
                    return true;
                }
                i += 1;
            }
        }
        false
    }

    // =================================================================
    // tonum: hex/binary parsing is exercised at the Lua binding here; the
    // parser itself (`api_ops::parse_pico8_number`) is unit-tested in
    // spark-core and shared with the Pico target.
    // =================================================================

    #[test]
    fn test_tonum_hex_string_in_lua() {
        let lua = create_lua().expect("create_lua failed");
        lua.set_app_data(Console::new());
        lua.set_app_data(Audio::new());
        // Mirrors the cart's decoder: tonum("0x"..sub(s,v,v+1)).
        let v: f64 = lua
            .load(r#"return tonum("0x"..sub("ff6c00",3,4))"#)
            .eval()
            .expect("tonum hex eval failed");
        assert_eq!(v, 108.0);
    }
}
