//! Cart lifecycle: one `CartRuntime` owns a Lua state with the `Console`,
//! `Audio` and `GameState` installed as *resident* app data for the whole run.
//! `start` loads the code and runs `_init`; `tick` steps one cart frame and
//! classifies the outcome (continue / stop() / run() / load() / Lua error).
//!
//! Used by the windowed app (`main.rs`), the headless `cart_check` example and
//! the integration tests, so the start/step/teardown sequence exists once.

use mlua::{AppDataRef, AppDataRefMut, Lua, Thread as LuaThread};
use spark_core::audio::Audio;
use spark_core::cart::CartData;
use spark_core::console::Console;
use spark_core::game_state::GameState;

use crate::lua_api::{self, Breadcrumb, CartDataId, LoadParam, MenuItems};

/// Outcome of stepping the cart one frame.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Tick {
    /// The frame ran; keep going.
    Continue,
    /// The cart called `stop()` or its manual main loop returned.
    Stop,
    /// The cart called `run()`: restart it from the same cart data.
    Restart,
    /// The cart called `load(file, breadcrumb, param)`.
    Load {
        file: String,
        breadcrumb: String,
        param: String,
    },
    /// A Lua error escaped `_update`/`_draw`/the main loop.
    Error(String),
}

/// A running cart: the Lua state plus the resident engine state inside it.
pub struct CartRuntime {
    lua: Lua,
    /// The `_init` coroutine while it is still yielding via `flip()`
    /// (manual-main-loop carts). `None` in callback (`_update`/`_draw`) mode.
    thread: Option<LuaThread>,
    use_60fps: bool,
    /// A stop/run/load signal raised while `_init` was running (or from a
    /// pause-menu callback); delivered by the next `tick`.
    pending: Option<Tick>,
}

impl CartRuntime {
    /// Create the Lua state, install the cart's data, load its code and run
    /// `_init` up to its first `flip()` (or to completion).
    ///
    /// `breadcrumb`/`param` are the values a preceding `load()` passed
    /// (exposed via `stat(6)`/`stat(46)`); use `""` when starting directly.
    pub fn start(cart: &CartData, breadcrumb: &str, param: &str) -> Result<Self, String> {
        let lua = lua_api::create_lua().map_err(|e| format!("Lua error: {e}"))?;

        let mut console = Console::new();
        let mut audio = Audio::new();
        crate::install_cart(&mut console, &mut audio, cart);
        lua.set_app_data(console);
        lua.set_app_data(audio);
        lua.set_app_data(Breadcrumb(breadcrumb.to_string()));
        lua.set_app_data(LoadParam(param.to_string()));

        lua_api::load_code(&lua, &cart.code).map_err(|e| format!("Load error: {e}"))?;

        let mut rt = CartRuntime {
            lua,
            thread: None,
            use_60fps: false,
            pending: None,
        };

        // Wrap _init in a coroutine so a `while true do ... flip() end` main
        // loop can yield back to us once per frame.
        let init =
            lua_api::create_init_coroutine(&rt.lua).map_err(|e| format!("_init error: {e}"))?;
        if let Some(thread) = init {
            match lua_api::resume_cart_thread(&thread) {
                Ok(true) => rt.thread = Some(thread),
                Ok(false) => {}
                Err(e) => match classify(&e, "_init") {
                    Tick::Error(msg) => return Err(msg),
                    signal => rt.pending = Some(signal),
                },
            }
        }

        rt.use_60fps = lua_api::has_update60(&rt.lua);
        rt.game_state_mut().target_fps = if rt.use_60fps { 60 } else { 30 };
        Ok(rt)
    }

    /// Step one cart frame: resume the manual loop, or call `_update[60]`
    /// then `_draw`.
    pub fn tick(&mut self) -> Tick {
        if let Some(signal) = self.pending.take() {
            return signal;
        }
        if let Some(thread) = &self.thread {
            return match lua_api::resume_cart_thread(thread) {
                Ok(true) => Tick::Continue,
                Ok(false) => Tick::Stop,
                Err(e) => classify(&e, "Update"),
            };
        }
        let update = if self.use_60fps {
            lua_api::call_update60(&self.lua)
        } else {
            lua_api::call_update(&self.lua)
        };
        if let Err(e) = update {
            return classify(&e, "Update");
        }
        match lua_api::call_draw(&self.lua) {
            Ok(()) => Tick::Continue,
            Err(e) => classify(&e, "Draw"),
        }
    }

    pub fn lua(&self) -> &Lua {
        &self.lua
    }

    /// True when the cart defines `_update60` (60 Hz tick rate).
    pub fn use_60fps(&self) -> bool {
        self.use_60fps
    }

    /// Seconds per cart frame (1/30 or 1/60).
    pub fn frame_period(&self) -> f64 {
        if self.use_60fps {
            1.0 / 60.0
        } else {
            1.0 / 30.0
        }
    }

    /// True when the cart runs its own main loop inside `_init` via `flip()`.
    pub fn is_coroutine_mode(&self) -> bool {
        self.thread.is_some()
    }

    pub fn console(&self) -> AppDataRef<'_, Console> {
        self.lua
            .app_data_ref::<Console>()
            .expect("Console is resident for the cart's lifetime")
    }

    pub fn console_mut(&self) -> AppDataRefMut<'_, Console> {
        self.lua
            .app_data_mut::<Console>()
            .expect("Console is resident for the cart's lifetime")
    }

    pub fn audio_mut(&self) -> AppDataRefMut<'_, Audio> {
        self.lua
            .app_data_mut::<Audio>()
            .expect("Audio is resident for the cart's lifetime")
    }

    pub fn game_state_mut(&self) -> AppDataRefMut<'_, GameState> {
        self.lua
            .app_data_mut::<GameState>()
            .expect("GameState is resident for the cart's lifetime")
    }

    /// Pause-menu entries: `"Continue"`, the cart's `menuitem` labels in slot
    /// order, then `"Quit"`.
    pub fn menu_labels(&self) -> Vec<String> {
        let mut v = vec!["Continue".to_string()];
        if let Some(items) = self.lua.app_data_ref::<MenuItems>() {
            v.extend(items.0.iter().flatten().map(|mi| mi.label.clone()));
        }
        v.push("Quit".to_string());
        v
    }

    /// True if the cart registered at least one `menuitem`.
    pub fn has_menu_items(&self) -> bool {
        self.lua
            .app_data_ref::<MenuItems>()
            .is_some_and(|items| items.0.iter().any(|i| i.is_some()))
    }

    /// Invoke the callback of the `idx`-th registered menu item (0-based over
    /// the *registered* items, matching `menu_labels` minus "Continue").
    /// Returns true if the callback asked to keep the menu open (truthy return).
    ///
    /// PICO-8 passes the callback a bitmask of the button that triggered it
    /// (1=left, 2=right, 4=x); we trigger via the X/O button, so pass 4.
    pub fn call_menu_item(&mut self, idx: usize) -> bool {
        let callback = self.lua.app_data_ref::<MenuItems>().and_then(|items| {
            items
                .0
                .iter()
                .flatten()
                .nth(idx)
                .and_then(|mi| mi.callback.as_ref())
                .and_then(|key| self.lua.registry_value::<mlua::Function>(key).ok())
        });
        let Some(f) = callback else {
            return false;
        };
        match f.call::<mlua::Value>(4i64) {
            Ok(mlua::Value::Nil) | Ok(mlua::Value::Boolean(false)) => false,
            Ok(_) => true,
            Err(e) => {
                // stop()/run()/load() from a menu item take effect next tick.
                match classify(&e, "Menu") {
                    Tick::Error(msg) => eprintln!("[spark] {msg}"),
                    signal => self.pending = Some(signal),
                }
                false
            }
        }
    }

    /// Persist `dset` data to disk if the cart called `cartdata(id)`.
    pub fn save_cartdata(&self) {
        let Some(id) = self.lua.app_data_ref::<CartDataId>() else {
            return;
        };
        if id.0.is_empty() {
            return;
        }
        match lua_api::save_cartdata(&id.0, &self.console().persistent_data) {
            Ok(()) => eprintln!("[spark] cartdata saved: {}", id.0),
            Err(e) => eprintln!("[spark] cartdata save error: {e}"),
        }
    }

    /// Tear the cart down: save cartdata and hand back the engine state (the
    /// editor keeps runtime `sset`/`mset` edits, as PICO-8 does).
    pub fn finish(self) -> (Console, Audio) {
        self.save_cartdata();
        let console = self.lua.remove_app_data::<Console>().unwrap_or_default();
        let audio = self.lua.remove_app_data::<Audio>().unwrap_or_default();
        (console, audio)
    }
}

/// Turn a Lua error from cart code into a `Tick`: the stop/run/load control
/// signals are raised as specially-prefixed errors by the API layer.
fn classify(e: &mlua::Error, context: &str) -> Tick {
    let msg = e.to_string();
    if let Some((file, breadcrumb, param)) = lua_api::parse_load_signal(&msg) {
        Tick::Load {
            file,
            breadcrumb,
            param,
        }
    } else if lua_api::is_stop_signal(&msg) {
        Tick::Stop
    } else if lua_api::is_run_signal(&msg) {
        Tick::Restart
    } else {
        Tick::Error(format!("{context} error: {e}"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use spark_core::cart::parse_cart;

    fn cart(code: &str) -> CartData {
        parse_cart(&format!("pico-8 cartridge\n__lua__\n{code}\n"))
    }

    #[test]
    fn callback_cart_runs_and_draws() {
        let mut rt = CartRuntime::start(&cart("function _draw() cls(1) end"), "", "").unwrap();
        assert!(!rt.is_coroutine_mode());
        assert_eq!(rt.tick(), Tick::Continue);
        assert_eq!(crate::lit_pixels(&rt.console()), 128 * 128);
    }

    #[test]
    fn manual_loop_cart_uses_coroutine_mode_and_stops_when_loop_ends() {
        let src = "n=0 function _init() while n<2 do n=n+1 flip() end end";
        let mut rt = CartRuntime::start(&cart(src), "", "").unwrap();
        assert!(rt.is_coroutine_mode());
        assert_eq!(rt.tick(), Tick::Continue);
        assert_eq!(rt.tick(), Tick::Stop);
    }

    #[test]
    fn signals_are_classified() {
        let mut rt = CartRuntime::start(&cart("function _update() stop() end"), "", "").unwrap();
        assert_eq!(rt.tick(), Tick::Stop);
        let mut rt = CartRuntime::start(&cart("function _update() run() end"), "", "").unwrap();
        assert_eq!(rt.tick(), Tick::Restart);
        let mut rt =
            CartRuntime::start(&cart("function _draw() load('b.p8','bc','pp') end"), "", "")
                .unwrap();
        assert_eq!(
            rt.tick(),
            Tick::Load {
                file: "b.p8".into(),
                breadcrumb: "bc".into(),
                param: "pp".into()
            }
        );
        let mut rt =
            CartRuntime::start(&cart("function _draw() error('boom') end"), "", "").unwrap();
        assert!(
            matches!(rt.tick(), Tick::Error(m) if m.starts_with("Draw error:") && m.contains("boom"))
        );
    }

    #[test]
    fn signal_during_init_is_delivered_by_first_tick() {
        let mut rt = CartRuntime::start(&cart("function _init() run() end"), "", "").unwrap();
        assert_eq!(rt.tick(), Tick::Restart);
    }

    #[test]
    fn init_error_fails_start() {
        let err = CartRuntime::start(&cart("function _init() error('bad') end"), "", "")
            .err()
            .unwrap();
        assert!(err.starts_with("_init error:") && err.contains("bad"));
        let err = CartRuntime::start(&cart("this is not lua"), "", "")
            .err()
            .unwrap();
        assert!(err.starts_with("Load error:"));
    }

    #[test]
    fn breadcrumb_param_and_fps_are_wired() {
        let rt = CartRuntime::start(&cart("function _update60() end"), "lvl2", "p").unwrap();
        assert!(rt.use_60fps());
        assert_eq!(rt.game_state_mut().target_fps, 60);
        let s: String = rt
            .lua()
            .load("return stat(6)..'|'..stat(46)")
            .eval()
            .unwrap();
        assert_eq!(s, "lvl2|p");
    }

    #[test]
    fn menu_items_and_finish() {
        let src = "kept=0 menuitem(1,'retry',function() kept=kept+1 return true end) \
                   function _draw() sset(0,0,7) end";
        let mut rt = CartRuntime::start(&cart(src), "", "").unwrap();
        assert!(rt.has_menu_items());
        assert_eq!(rt.menu_labels(), vec!["Continue", "retry", "Quit"]);
        assert!(rt.call_menu_item(0), "truthy return keeps the menu open");
        assert_eq!(rt.tick(), Tick::Continue);
        let (console, _audio) = rt.finish();
        assert_eq!(console.sprites[0], 7, "runtime sprite edits come back out");
    }
}
