//! Headless cart compatibility checker and visual-bug diagnostic tool.
//!
//! Loads each cart given on the command line, runs `_init` plus a number of
//! `_update`/`_draw` frames through the real Lua API layer (no window), and
//! reports whether it executes without a Lua error and draws to the screen.
//!
//! Diagnostics (additive, no engine changes):
//!   --png            write a PNG of the final frame next to each cart
//!   --every N        also dump frame_NNNN.png every N frames into --outdir
//!   --outdir DIR     directory for --every dumps (default: alongside the cart)
//!   --press SPEC     scripted input, e.g. "x@5,right@20-70,o@40,up@10-12"
//!                    buttons: left right up down o x  (frames are 0-based)
//!   --mouse SPEC     scripted mouse (stat(32/33/34)), ';'-separated, e.g.
//!                    "move@0-200:64,80;left@40-60;right@100"
//!                    move@<frames>:x,y positions; left/right/mid@<frames> hold
//!   --trace          wrap draw API calls and print a per-frame summary of
//!                    geometry-call counts + state-call args (camera/pal/fillp…)
//!   --eval EXPR      after the run, evaluate a Lua expression and print it.
//!                    This is the supported way to inspect cart/engine state
//!                    (player position, tile reads, flags) WITHOUT writing a
//!                    throwaway example. May be passed multiple times.
//!                    e.g. --eval "p1.x..','..p1.y" --eval "fget(mget(2,15),0)"
//!
//! Usage:
//!   cargo run -p spark-desktop --example cart_check -- \
//!       [--frames N] [--png] [--every N] [--outdir DIR] \
//!       [--press SPEC] [--trace] [--eval EXPR ...] <cart.p8|.p8.png> ...

use spark_desktop_lib as lib;
use spark_desktop_lib::{CartRuntime, Tick};

/// Button names accepted by --press, in PICO-8 player-1 order (indices 0..6).
const BUTTONS: [&str; 6] = ["left", "right", "up", "down", "o", "x"];

/// A parsed input schedule: for each of the 6 P1 buttons, the (start,end)
/// inclusive frame ranges during which it is held.
#[derive(Default, Clone)]
struct InputScript {
    held: [Vec<(usize, usize)>; 6],
}

impl InputScript {
    /// Parse "x@5,right@20-70,o@40" into per-button held ranges.
    fn parse(spec: &str) -> Result<Self, String> {
        let mut script = InputScript::default();
        for tok in spec.split(',').map(str::trim).filter(|s| !s.is_empty()) {
            let (name, range) = tok
                .split_once('@')
                .ok_or_else(|| format!("bad press token '{tok}' (expected name@frame)"))?;
            let idx = BUTTONS
                .iter()
                .position(|&b| b == name)
                .ok_or_else(|| format!("unknown button '{name}' in '{tok}'"))?;
            let (start, end) = match range.split_once('-') {
                Some((a, b)) => (
                    a.parse::<usize>().map_err(|_| format!("bad frame '{a}'"))?,
                    b.parse::<usize>().map_err(|_| format!("bad frame '{b}'"))?,
                ),
                None => {
                    let f = range
                        .parse::<usize>()
                        .map_err(|_| format!("bad frame '{range}'"))?;
                    (f, f)
                }
            };
            script.held[idx].push((start, end));
        }
        Ok(script)
    }

    /// Compute the 12-element button state for a given frame (P2 always false).
    fn state_at(&self, frame: usize) -> [bool; 12] {
        let mut state = [false; 12];
        for (i, ranges) in self.held.iter().enumerate() {
            state[i] = ranges.iter().any(|&(a, b)| frame >= a && frame <= b);
        }
        state
    }

    fn is_empty(&self) -> bool {
        self.held.iter().all(|r| r.is_empty())
    }
}

/// A scripted mouse event applied at a given frame: move to (x,y) and/or hold a
/// button mask for a range of frames. PICO-8 exposes the mouse via
/// `stat(32)`=x, `stat(33)`=y, `stat(34)`=button bitmask (1=left,2=right,4=mid).
#[derive(Default, Clone)]
struct MouseScript {
    /// (start, end, x, y) position keyframes (position held over the range).
    moves: Vec<(usize, usize, i32, i32)>,
    /// (start, end, button_mask) button holds.
    buttons: Vec<(usize, usize, u8)>,
}

impl MouseScript {
    /// Parse a spec like "move@10-200:64,80;left@40-60;right@100".
    /// Tokens separated by ';'. Forms:
    ///   move@<frames>:<x>,<y>   — position (held across the frame range)
    ///   left|right|mid@<frames> — button held across the range
    /// <frames> is `N` or `A-B` (0-based, inclusive).
    fn parse(spec: &str) -> Result<Self, String> {
        let mut ms = MouseScript::default();
        for tok in spec.split(';').map(str::trim).filter(|s| !s.is_empty()) {
            let (head, frames_and_args) = tok
                .split_once('@')
                .ok_or_else(|| format!("bad mouse token '{tok}' (expected kind@frames)"))?;
            // Split optional ":x,y" payload off the frame range.
            let (frange, payload) = match frames_and_args.split_once(':') {
                Some((f, p)) => (f, Some(p)),
                None => (frames_and_args, None),
            };
            let (start, end) = parse_range(frange)?;
            match head {
                "move" => {
                    let p = payload.ok_or_else(|| format!("move needs ':x,y' in '{tok}'"))?;
                    let (xs, ys) = p
                        .split_once(',')
                        .ok_or_else(|| format!("move payload must be 'x,y' in '{tok}'"))?;
                    let x = xs
                        .trim()
                        .parse::<i32>()
                        .map_err(|_| format!("bad x '{xs}'"))?;
                    let y = ys
                        .trim()
                        .parse::<i32>()
                        .map_err(|_| format!("bad y '{ys}'"))?;
                    ms.moves.push((start, end, x, y));
                }
                "left" => ms.buttons.push((start, end, 1)),
                "right" => ms.buttons.push((start, end, 2)),
                "mid" => ms.buttons.push((start, end, 4)),
                other => return Err(format!("unknown mouse kind '{other}' in '{tok}'")),
            }
        }
        Ok(ms)
    }

    fn is_empty(&self) -> bool {
        self.moves.is_empty() && self.buttons.is_empty()
    }

    /// Resolve (x, y, button_mask) for a frame. Later keyframes win for moves.
    fn state_at(&self, frame: usize) -> (Option<(i32, i32)>, u8) {
        let mut pos = None;
        for &(a, b, x, y) in &self.moves {
            if frame >= a && frame <= b {
                pos = Some((x, y));
            }
        }
        let mut mask = 0u8;
        for &(a, b, m) in &self.buttons {
            if frame >= a && frame <= b {
                mask |= m;
            }
        }
        (pos, mask)
    }
}

/// Parse a frame range "N" or "A-B" into an inclusive (start, end).
fn parse_range(s: &str) -> Result<(usize, usize), String> {
    match s.split_once('-') {
        Some((a, b)) => Ok((
            a.trim().parse().map_err(|_| format!("bad frame '{a}'"))?,
            b.trim().parse().map_err(|_| format!("bad frame '{b}'"))?,
        )),
        None => {
            let f = s.trim().parse().map_err(|_| format!("bad frame '{s}'"))?;
            Ok((f, f))
        }
    }
}

struct Options {
    frames: usize,
    write_png: bool,
    every: Option<usize>,
    outdir: Option<String>,
    script: InputScript,
    mouse: MouseScript,
    trace: bool,
    /// Lua expressions to evaluate and print after the run finishes.
    evals: Vec<String>,
}

fn main() {
    let mut args: Vec<String> = std::env::args().skip(1).collect();

    let mut frames = 60usize;
    let mut write_png = false;
    let mut every: Option<usize> = None;
    let mut outdir: Option<String> = None;
    let mut script = InputScript::default();
    let mut mouse = MouseScript::default();
    let mut trace = false;
    let mut evals: Vec<String> = Vec::new();

    // Pull options out of the arg list.
    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--frames" => {
                frames = args.get(i + 1).and_then(|s| s.parse().ok()).unwrap_or(60);
                args.drain(i..=i + 1);
            }
            "--png" => {
                write_png = true;
                args.remove(i);
            }
            "--every" => {
                every = args.get(i + 1).and_then(|s| s.parse().ok());
                args.drain(i..=i + 1);
            }
            "--outdir" => {
                outdir = args.get(i + 1).cloned();
                args.drain(i..=i + 1);
            }
            "--press" => {
                let spec = args.get(i + 1).cloned().unwrap_or_default();
                match InputScript::parse(&spec) {
                    Ok(s) => script = s,
                    Err(e) => {
                        eprintln!("--press: {e}");
                        std::process::exit(2);
                    }
                }
                args.drain(i..=i + 1);
            }
            "--mouse" => {
                let spec = args.get(i + 1).cloned().unwrap_or_default();
                match MouseScript::parse(&spec) {
                    Ok(m) => mouse = m,
                    Err(e) => {
                        eprintln!("--mouse: {e}");
                        std::process::exit(2);
                    }
                }
                args.drain(i..=i + 1);
            }
            "--trace" => {
                trace = true;
                args.remove(i);
            }
            "--eval" => {
                if let Some(expr) = args.get(i + 1).cloned() {
                    evals.push(expr);
                }
                args.drain(i..=i + 1);
            }
            _ => i += 1,
        }
    }

    if args.is_empty() {
        eprintln!("usage: cart_check [--frames N] [--png] [--every N] [--outdir DIR] [--press SPEC] [--mouse SPEC] [--trace] [--eval EXPR ...] <cart> ...");
        std::process::exit(2);
    }

    let opts = Options {
        frames,
        write_png,
        every,
        outdir,
        script,
        mouse,
        trace,
        evals,
    };

    println!(
        "{:<28} {:<10} {:>8} {:>10}  DETAIL",
        "CART", "RESULT", "FRAMES", "PIXELS"
    );
    println!("{}", "-".repeat(78));

    let mut pass = 0;
    let mut total = 0;
    for path in &args {
        total += 1;
        let report = check_cart(path, &opts);
        if report.ok {
            pass += 1;
        }
        let name = std::path::Path::new(path)
            .file_name()
            .map(|s| s.to_string_lossy().into_owned())
            .unwrap_or_else(|| path.clone());
        println!(
            "{:<28} {:<10} {:>8} {:>10}  {}",
            truncate(&name, 28),
            if report.ok { "OK" } else { "FAIL" },
            report.frames_run,
            report.lit_pixels,
            report.detail
        );
    }
    println!("{}", "-".repeat(78));
    println!("{pass}/{total} carts ran cleanly");
}

struct Report {
    ok: bool,
    frames_run: usize,
    lit_pixels: usize,
    detail: String,
}

/// Lua prelude that wraps the draw API to count calls per frame. Geometry
/// calls are counted; state calls also record their last argument list.
/// `__t_reset()` clears the accumulators between frames.
const TRACE_PRELUDE: &str = r#"
__t = {}
__ta = {}
local function wrap_count(name)
  local orig = _G[name]
  if not orig then return end
  _G[name] = function(...)
    __t[name] = (__t[name] or 0) + 1
    return orig(...)
  end
end
local function wrap_state(name)
  local orig = _G[name]
  if not orig then return end
  _G[name] = function(...)
    __t[name] = (__t[name] or 0) + 1
    local a = {...}
    local s = {}
    for i=1,#a do s[i] = tostring(a[i]) end
    __ta[name] = table.concat(s, ",")
    return orig(...)
  end
end
for _,n in ipairs({"spr","map","line","rect","rectfill","circ","circfill",
                   "oval","ovalfill","print","tline","sspr","pset","cls"}) do
  wrap_count(n)
end
for _,n in ipairs({"camera","clip","pal","palt","fillp","color"}) do
  wrap_state(n)
end
function __t_reset() __t = {} __ta = {} end
"#;

fn check_cart(path: &str, opts: &Options) -> Report {
    // --- load file ---
    let cart = match lib::load_cart_from_file(path) {
        Ok(c) => c,
        Err(e) => {
            return Report {
                ok: false,
                frames_run: 0,
                lit_pixels: 0,
                detail: format!("load failed: {e}"),
            }
        }
    };

    // --- start the cart exactly like the real app (runs _init) ---
    let mut rt = match CartRuntime::start(&cart, "", "") {
        Ok(rt) => rt,
        Err(e) => {
            return Report {
                ok: false,
                frames_run: 0,
                lit_pixels: 0,
                detail: first_line(&e),
            }
        }
    };

    // Install the trace wrappers once the cart's globals exist. The cart's
    // _draw references API names at call time, so wrapping now takes effect.
    if opts.trace {
        if let Err(e) = rt.lua().load(TRACE_PRELUDE).set_name("trace").exec() {
            eprintln!("[trace] prelude failed: {}", first_line(&e.to_string()));
        }
    }

    let fail = |rt: &CartRuntime, frames_run: usize, detail: String| Report {
        ok: false,
        frames_run,
        lit_pixels: lib::lit_pixels(&rt.console()),
        detail,
    };

    // --- run frames ---
    let mut frames_run = 0;
    for frame in 0..opts.frames {
        // Apply scripted input for this frame before update.
        if !opts.script.is_empty() {
            rt.console_mut().update_input(opts.script.state_at(frame));
        }
        // Apply scripted mouse position/buttons (stat(32)/(33)/(34)).
        if !opts.mouse.is_empty() {
            let mut con = rt.console_mut();
            let (pos, mask) = opts.mouse.state_at(frame);
            if let Some((mx, my)) = pos {
                con.mouse_x = mx;
                con.mouse_y = my;
            }
            con.mouse_btn = mask;
        }

        match rt.tick() {
            Tick::Continue => {}
            Tick::Error(e) => {
                return fail(
                    &rt,
                    frames_run,
                    format!("frame {frames_run}: {}", first_line(&e)),
                )
            }
            // Control-flow signals end the run; they are not failures.
            Tick::Stop => {
                eprintln!("[cart_check] stop() at frame {frames_run}");
                break;
            }
            Tick::Restart => {
                eprintln!("[cart_check] run() at frame {frames_run}");
                break;
            }
            Tick::Load { file, .. } => {
                eprintln!("[cart_check] load({file:?}) at frame {frames_run}");
                break;
            }
        }
        frames_run += 1;

        // Emit trace summary for this frame, then reset accumulators.
        if opts.trace {
            print_trace_line(rt.lua(), frame);
            let _ = rt
                .lua()
                .globals()
                .get::<mlua::Function>("__t_reset")
                .map(|f| f.call::<()>(()));
        }

        // Periodic frame dump.
        if let Some(n) = opts.every {
            if n > 0 && frame % n == 0 {
                let dir = opts
                    .outdir
                    .clone()
                    .or_else(|| {
                        std::path::Path::new(path)
                            .parent()
                            .map(|p| p.to_string_lossy().into_owned())
                    })
                    .unwrap_or_else(|| ".".into());
                let _ = std::fs::create_dir_all(&dir);
                write_screen_png(&rt, &format!("{dir}/frame_{frame:04}.png"));
            }
        }
    }

    let lit = lib::lit_pixels(&rt.console());
    if opts.write_png {
        write_screen_png(&rt, &format!("{path}.spark.png"));
    }

    // Evaluate any --eval expressions against the post-run Lua state. This is
    // the supported alternative to writing a throwaway example for inspection.
    for expr in &opts.evals {
        let wrapped = format!("return tostr({expr})");
        match rt.lua().load(&wrapped).eval::<String>() {
            Ok(v) => eprintln!("[eval] {expr} = {v}"),
            Err(e) => eprintln!("[eval] {expr} -> error: {}", first_line(&e.to_string())),
        }
    }

    Report {
        ok: true,
        frames_run,
        lit_pixels: lit,
        detail: if lit == 0 {
            "ran, but screen is blank".into()
        } else {
            "ran and drew".into()
        },
    }
}

/// Print one line summarizing the draw calls made during `frame`.
fn print_trace_line(lua: &mlua::Lua, frame: usize) {
    let globals = lua.globals();
    let counts = globals.get::<mlua::Table>("__t").ok();
    let args = globals.get::<mlua::Table>("__ta").ok();

    // Geometry counts, in a stable order.
    let mut parts: Vec<String> = Vec::new();
    if let Some(t) = &counts {
        for name in [
            "cls", "spr", "map", "tline", "sspr", "line", "rect", "rectfill", "circ", "circfill",
            "oval", "ovalfill", "print", "pset",
        ] {
            if let Ok(n) = t.get::<i64>(name) {
                if n > 0 {
                    parts.push(format!("{name}={n}"));
                }
            }
        }
    }

    // State-call args (last value seen this frame).
    let mut state: Vec<String> = Vec::new();
    if let Some(t) = &args {
        for name in ["camera", "clip", "pal", "palt", "fillp", "color"] {
            if let Ok(s) = t.get::<String>(name) {
                state.push(format!("{name}({s})"));
            }
        }
    }

    let counts_str = if parts.is_empty() {
        "(no draws)".to_string()
    } else {
        parts.join(" ")
    };
    if state.is_empty() {
        eprintln!("[trace f{frame:04}] {counts_str}");
    } else {
        eprintln!("[trace f{frame:04}] {counts_str} | {}", state.join(" "));
    }
}

fn first_line(s: &str) -> String {
    s.lines().next().unwrap_or(s).to_string()
}

fn truncate(s: &str, n: usize) -> String {
    if s.len() <= n {
        s.to_string()
    } else {
        format!("{}…", &s[..n - 1])
    }
}

fn write_screen_png(rt: &CartRuntime, path: &str) {
    let png = lib::encode_screen_png(&rt.console().screen_to_rgba());
    if let Err(e) = std::fs::write(path, &png) {
        eprintln!("[cart_check] could not write {path}: {e}");
    }
}
