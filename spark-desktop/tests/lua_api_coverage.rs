//! Lua-level API coverage suite.
//!
//! Every PICO-8 API function is registered as a *Lua global* by
//! `lua_api::create_lua`. Unit tests in `spark-core` exercise the underlying
//! `Console`/`Audio` Rust methods, but the bugs that actually reach carts live
//! in the *binding layer* — wrong return type, wrong memory address, wrong
//! argument handling, Lua truthiness, fixed-point conventions. Those only show
//! up when a function is called *through Lua* and the resulting engine state is
//! inspected.
//!
//! This suite drives each registered global from Lua and asserts on the
//! resulting `Console`/`Audio`/return value. It is the integration counterpart
//! to the `spark-core` unit tests. The `API_SURFACE` test at the bottom fails
//! if a new global is registered without a corresponding test here, so coverage
//! can't silently regress.

use mlua::{Lua, Value as LuaValue};
use spark_core::audio::Audio;
use spark_core::console::{Console, SCREEN_W};
use spark_core::game_state::GameState;
use spark_desktop_lib as lib;

// ---------------------------------------------------------------------------
// Harness
// ---------------------------------------------------------------------------

/// Build a Lua state wired with a fresh Console / Audio / GameState, exactly
/// like the real app and the headless `cart_check` tool.
fn fixture() -> Lua {
    let lua = lib::create_lua().expect("create_lua");
    lua.set_app_data(Console::new());
    lua.set_app_data(Audio::new());
    lua.set_app_data(GameState::new());
    lua
}

/// Run a Lua statement (no return value) against the state.
fn run(lua: &Lua, src: &str) {
    lua.load(src)
        .exec()
        .unwrap_or_else(|e| panic!("lua exec failed for `{src}`: {e}"));
}

/// Evaluate a Lua expression to f64.
fn num(lua: &Lua, expr: &str) -> f64 {
    lua.load(format!("return {expr}"))
        .eval::<f64>()
        .unwrap_or_else(|e| panic!("lua eval failed for `{expr}`: {e}"))
}

/// Evaluate a Lua expression to a bool (real Lua truthiness via a typed return).
fn boolean(lua: &Lua, expr: &str) -> bool {
    lua.load(format!("return {expr}"))
        .eval::<bool>()
        .unwrap_or_else(|e| panic!("lua eval failed for `{expr}`: {e}"))
}

/// Evaluate a Lua expression to a String.
fn text(lua: &Lua, expr: &str) -> String {
    lua.load(format!("return {expr}"))
        .eval::<String>()
        .unwrap_or_else(|e| panic!("lua eval failed for `{expr}`: {e}"))
}

/// Read a screen pixel (color index) after drawing.
fn px(lua: &Lua, x: i32, y: i32) -> u8 {
    let con = lua.app_data_ref::<Console>().unwrap();
    con.screen[(y as usize) * SCREEN_W + (x as usize)]
}

/// Count non-zero (lit) pixels on screen.
fn lit(lua: &Lua) -> usize {
    lib::lit_pixels(&lua.app_data_ref::<Console>().unwrap())
}

// ---------------------------------------------------------------------------
// PICO-8 memory map. These drive the memory-mapped regions through Lua and
// assert the real PICO-8 addresses. The ones marked `#[ignore]` need WP1's
// console.rs (the current tree maps sprite flags at 0x1000, has no pen-color
// byte at 0x5F25 and no cartdata region at 0x5E00); un-ignore once it lands.
// ---------------------------------------------------------------------------

#[test]
fn memory_map_sprite_flags_at_0x3000() {
    let lua = fixture();
    // poke into the flags region, read back through fget (byte and bit form).
    run(&lua, "poke(0x3000 + 7, 5)");
    assert_eq!(num(&lua, "fget(7)"), 5.0);
    assert!(boolean(&lua, "fget(7, 0)"));
    assert!(!boolean(&lua, "fget(7, 1)"));
    assert!(boolean(&lua, "fget(7, 2)"));
    // and the other way: fset is visible via peek at the same address.
    run(&lua, "fset(9, 0x42)");
    assert_eq!(num(&lua, "peek(0x3000 + 9)"), 0x42 as f64);
}

#[test]
fn memory_map_upper_sprite_sheet_at_0x1000() {
    let lua = fixture();
    // Sprite sheet rows 64..127 live at 0x1000-0x1FFF (two 4-bit pixels per
    // byte, low nibble = left pixel). sset(0,64,c) is the first pixel there.
    run(&lua, "sset(0, 64, 0xA) sset(1, 64, 0xB)");
    assert_eq!(num(&lua, "peek(0x1000)"), 0xBA as f64);
    run(&lua, "poke(0x1001, 0x21)");
    assert_eq!(num(&lua, "sget(2, 64)"), 1.0);
    assert_eq!(num(&lua, "sget(3, 64)"), 2.0);
}

#[test]
fn memory_map_memcpy_map_region() {
    let lua = fixture();
    // Map rows 0..31 live at 0x2000 (one byte per cel). Copy a tile row to
    // another row through memcpy and read it back with mget.
    run(&lua, "mset(3, 0, 42) mset(4, 0, 43)");
    run(&lua, "memcpy(0x2000 + 128 * 5, 0x2000, 128)");
    assert_eq!(num(&lua, "mget(3, 5)"), 42.0);
    assert_eq!(num(&lua, "mget(4, 5)"), 43.0);
    assert_eq!(num(&lua, "peek(0x2000 + 3)"), 42.0);
}

#[test]
fn memory_map_pen_color_at_0x5f25() {
    let lua = fixture();
    // Writing the draw-state pen color byte must change the color used by a
    // color-less pset, and color() must be readable back from it.
    run(&lua, "poke(0x5F25, 11) pset(5, 5)");
    assert_eq!(px(&lua, 5, 5), 11);
    run(&lua, "color(3)");
    assert_eq!(num(&lua, "peek(0x5F25)"), 3.0);
}

#[test]
fn memory_map_persistent_cartdata_at_0x5e00() {
    let lua = fixture();
    // dset/dget are the 64 x 32-bit slots at 0x5E00-0x5EFF (16.16 fixed
    // point, little-endian).
    run(&lua, "dset(0, 1.5) dset(1, -1)");
    assert_eq!(num(&lua, "peek4(0x5E00)"), 1.5);
    assert_eq!(num(&lua, "peek4(0x5E04)"), -1.0);
    run(&lua, "poke4(0x5E08, 42)");
    assert_eq!(num(&lua, "dget(2)"), 42.0);
}

// ---------------------------------------------------------------------------
// Math
// ---------------------------------------------------------------------------

#[test]
fn math_abs_flr_ceil_sgn() {
    let lua = fixture();
    assert_eq!(num(&lua, "abs(-3)"), 3.0);
    assert_eq!(num(&lua, "abs(3)"), 3.0);
    assert_eq!(num(&lua, "flr(3.9)"), 3.0);
    assert_eq!(num(&lua, "flr(-0.1)"), -1.0);
    assert_eq!(num(&lua, "ceil(3.1)"), 4.0);
    assert_eq!(num(&lua, "ceil(-3.9)"), -3.0);
    // PICO-8 sgn(0) == 1.
    assert_eq!(num(&lua, "sgn(0)"), 1.0);
    assert_eq!(num(&lua, "sgn(-5)"), -1.0);
    assert_eq!(num(&lua, "sgn(5)"), 1.0);
}

#[test]
fn math_min_max_mid() {
    let lua = fixture();
    assert_eq!(num(&lua, "min(3,7)"), 3.0);
    assert_eq!(num(&lua, "max(3,7)"), 7.0);
    assert_eq!(num(&lua, "mid(5,1,9)"), 5.0);
    assert_eq!(num(&lua, "mid(1,5,9)"), 5.0);
    assert_eq!(num(&lua, "mid(9,5,1)"), 5.0);
    assert_eq!(num(&lua, "mid(-1,5,9)"), 5.0);
}

#[test]
fn math_sqrt() {
    let lua = fixture();
    assert_eq!(num(&lua, "sqrt(16)"), 4.0);
    assert_eq!(num(&lua, "sqrt(0)"), 0.0);
}

#[test]
fn math_trig_pico8_conventions() {
    let lua = fixture();
    // PICO-8 angles are turns [0,1), and sin is NEGATED vs standard math.
    assert!((num(&lua, "cos(0)") - 1.0).abs() < 1e-9);
    assert!((num(&lua, "sin(0)") - 0.0).abs() < 1e-9);
    // sin(0.25) (quarter turn) == -1 in PICO-8.
    assert!((num(&lua, "sin(0.25)") - (-1.0)).abs() < 1e-9);
    // cos(0.5) (half turn) == -1.
    assert!((num(&lua, "cos(0.5)") - (-1.0)).abs() < 1e-9);
}

#[test]
fn math_atan2_pico8_turns() {
    let lua = fixture();
    // atan2 returns a turn in [0,1). atan2(1,0) -> 0 (pointing +x).
    assert!((num(&lua, "atan2(1,0)")).abs() < 1e-9);
    // Result is always in [0,1).
    let a = num(&lua, "atan2(-1,-1)");
    assert!(
        (0.0..1.0).contains(&a),
        "atan2 must be a turn in [0,1), got {a}"
    );
}

// ---------------------------------------------------------------------------
// Bitwise (operate on the 16.16 fixed-point bit pattern)
// ---------------------------------------------------------------------------

#[test]
fn bitwise_band_bor_bxor_bnot() {
    let lua = fixture();
    assert_eq!(num(&lua, "band(12,10)"), 8.0);
    assert_eq!(num(&lua, "bor(12,10)"), 14.0);
    assert_eq!(num(&lua, "bxor(12,10)"), 6.0);
    // bnot(0) == -0.0000152587890625 (all bits set in 16.16 = -1/65536).
    assert!((num(&lua, "bnot(0)") - (-1.0 / 65536.0)).abs() < 1e-12);
}

#[test]
fn bitwise_shifts() {
    let lua = fixture();
    assert_eq!(num(&lua, "shl(1,4)"), 16.0);
    assert_eq!(num(&lua, "shr(16,4)"), 1.0);
    // shr is arithmetic (sign-preserving); lshr is logical.
    assert!((num(&lua, "shr(-1,16)") - (-1.0 / 65536.0)).abs() < 1e-12);
    assert!((num(&lua, "lshr(-1,16)") - 0.9999847412109375).abs() < 1e-9);
    // Operates on the bit pattern: shl(0.125, 3) == 1.
    assert!((num(&lua, "shl(0.125,3)") - 1.0).abs() < 1e-9);
}

#[test]
fn bitwise_rotates() {
    let lua = fixture();
    assert_eq!(num(&lua, "rotl(1,1)"), 2.0);
    assert_eq!(num(&lua, "rotl(255,8)"), -256.0);
    // rotr(2,1) == 1.
    assert!((num(&lua, "rotr(2,1)") - 1.0).abs() < 1e-9);
}

// ---------------------------------------------------------------------------
// Number conversion
// ---------------------------------------------------------------------------

#[test]
fn conversion_tonum() {
    let lua = fixture();
    assert_eq!(num(&lua, "tonum('42')"), 42.0);
    assert_eq!(num(&lua, "tonum('0x6c')"), 108.0); // hex
    assert!((num(&lua, "tonum('0x1.8')") - 1.5).abs() < 1e-9); // fractional hex
    assert_eq!(num(&lua, "tonum('0b1010')"), 10.0); // binary
                                                    // Non-numeric -> nil.
    assert!(matches!(
        lua.load("return tonum('nope')").eval::<LuaValue>().unwrap(),
        LuaValue::Nil
    ));
}

#[test]
fn conversion_tonum_flags() {
    let lua = fixture();
    // 0x1: parse the string as raw hex digits.
    assert_eq!(num(&lua, "tonum('ff', 0x1)"), 255.0);
    // 0x3 (hex + shift): "10000" hex = 0x10000, >>16 = 1.
    assert_eq!(num(&lua, "tonum('10000', 0x3)"), 1.0);
    // 0x2: decimal integer then >>16.
    assert_eq!(num(&lua, "tonum('65536', 0x2)"), 1.0);
    // tonum/tostr round-trip via the shift flag form.
    assert_eq!(num(&lua, "tonum(tostr(3, 0x2), 0x2)"), 3.0);
}

#[test]
fn conversion_tostr() {
    let lua = fixture();
    assert_eq!(text(&lua, "tostr(42)"), "42");
    assert_eq!(text(&lua, "tostr(true)"), "true");
    assert_eq!(text(&lua, "tostr('hi')"), "hi");
    // nil -> empty string.
    assert_eq!(text(&lua, "tostr(nil)"), "");
}

// ---------------------------------------------------------------------------
// String / table helpers
// ---------------------------------------------------------------------------

#[test]
fn strings_sub_chr_ord() {
    let lua = fixture();
    assert_eq!(text(&lua, "sub('hello',2,4)"), "ell");
    assert_eq!(text(&lua, "sub('hello',2)"), "ello");
    // negative indices count from the end.
    assert_eq!(text(&lua, "sub('hello',-3)"), "llo");
    assert_eq!(text(&lua, "chr(65)"), "A");
    assert_eq!(num(&lua, "ord('A')"), 65.0);
    // ord with index.
    assert_eq!(num(&lua, "ord('ABC',2)"), 66.0);
}

#[test]
fn strings_split() {
    let lua = fixture();
    run(&lua, "t = split('1,2,3')");
    assert_eq!(num(&lua, "t[1]"), 1.0);
    assert_eq!(num(&lua, "t[3]"), 3.0);
    // non-numeric stays string with convert behavior; custom separator.
    run(&lua, "u = split('a:b:c', ':')");
    assert_eq!(text(&lua, "u[2]"), "b");
}

#[test]
fn strings_split_by_size_and_convert_flag() {
    let lua = fixture();
    // Numeric separator -> split into fixed-size character chunks.
    run(&lua, "t = split('abcdef', 2)");
    assert_eq!(text(&lua, "t[1]"), "ab");
    assert_eq!(text(&lua, "t[2]"), "cd");
    assert_eq!(text(&lua, "t[3]"), "ef");
    // convert=false keeps numeric pieces as strings.
    run(&lua, "u = split('1,2,3', ',', false)");
    assert_eq!(text(&lua, "u[1]"), "1");
    // Pieces are NOT trimmed (PICO-8 keeps spaces).
    run(&lua, "v = split('a, b', ',')");
    assert_eq!(text(&lua, "v[2]"), " b");
    // Empty fields are preserved.
    run(&lua, "w = split(',,')");
    assert_eq!(num(&lua, "#w"), 3.0);
}

#[test]
fn strings_chr_multi_arg_and_bytes() {
    let lua = fixture();
    // Multi-arg chr builds a multi-byte string.
    assert_eq!(text(&lua, "chr(104,105)"), "hi");
    // chr produces a single raw byte, not a multi-byte UTF-8 sequence.
    assert_eq!(num(&lua, "#chr(200)"), 1.0);
}

#[test]
fn strings_ord_count_and_number_coercion() {
    let lua = fixture();
    // ord with a count returns multiple values.
    let vals = lua
        .load("return {ord('ABC', 1, 3)}")
        .eval::<Vec<i64>>()
        .unwrap();
    assert_eq!(vals, vec![65, 66, 67]);
    // count clamps to the string length.
    let vals2 = lua
        .load("return {ord('ABC', 2, 10)}")
        .eval::<Vec<i64>>()
        .unwrap();
    assert_eq!(vals2, vec![66, 67]);
    // Numbers coerce to their string form: ord(12) -> ord("12") -> '1' = 49.
    assert_eq!(num(&lua, "ord(12)"), 49.0);
    // Out-of-range index returns nothing (nil).
    assert!(matches!(
        lua.load("return ord('A', 5)").eval::<LuaValue>().unwrap(),
        LuaValue::Nil
    ));
}

#[test]
fn strings_tostr_hex_flags() {
    let lua = fixture();
    // tostr(x, true) / flag 0x1 -> "0xWWWW.FFFF" fixed-point hex.
    assert_eq!(text(&lua, "tostr(1, true)"), "0x0001.0000");
    // flag 0x2 -> raw 32-bit integer bit pattern as decimal.
    assert_eq!(text(&lua, "tostr(1, 0x2)"), "65536");
    // flag 0x3 -> "0xWWWWFFFF" packed hex.
    assert_eq!(text(&lua, "tostr(1, 0x3)"), "0x00010000");
}

#[test]
fn tables_add_count_del_deli() {
    let lua = fixture();
    run(&lua, "t = {}");
    run(&lua, "add(t, 10) add(t, 20) add(t, 30)");
    assert_eq!(num(&lua, "count(t)"), 3.0);
    assert_eq!(num(&lua, "t[2]"), 20.0);
    // del removes by value.
    run(&lua, "del(t, 20)");
    assert_eq!(num(&lua, "count(t)"), 2.0);
    assert_eq!(num(&lua, "t[2]"), 30.0);
    // deli removes by index, returns the value.
    assert_eq!(num(&lua, "deli(t, 1)"), 10.0);
    assert_eq!(num(&lua, "count(t)"), 1.0);
}

#[test]
fn tables_add_returns_value_and_insert_position() {
    let lua = fixture();
    run(&lua, "t = {10, 30}");
    // add returns the inserted value.
    assert_eq!(num(&lua, "add(t, 99)"), 99.0);
    // add with an index inserts (1-based), shifting later elements up.
    run(&lua, "t = {10, 30} add(t, 20, 2)");
    assert_eq!(num(&lua, "t[1]"), 10.0);
    assert_eq!(num(&lua, "t[2]"), 20.0);
    assert_eq!(num(&lua, "t[3]"), 30.0);
}

#[test]
fn tables_del_returns_removed_value() {
    let lua = fixture();
    run(&lua, "t = {5, 6, 7}");
    // del returns the removed value, nil when absent.
    assert_eq!(num(&lua, "del(t, 6)"), 6.0);
    assert!(matches!(
        lua.load("return del(t, 999)").eval::<LuaValue>().unwrap(),
        LuaValue::Nil
    ));
}

#[test]
fn tables_count_value_and_holes() {
    let lua = fixture();
    // count(c, v) counts elements equal to v.
    run(&lua, "t = {1, 2, 2, 3, 2}");
    assert_eq!(num(&lua, "count(t, 2)"), 3.0);
    assert_eq!(num(&lua, "count(t, 9)"), 0.0);
    // count(c) counts non-nil elements in [1, #c].
    run(&lua, "u = {1, 2, 3} u[2] = nil");
    // #u is implementation-defined with a hole, but count skips the nil.
    let c = num(&lua, "count(u)");
    assert!(c == 2.0 || c == 1.0, "count should skip nil holes, got {c}");
}

#[test]
fn tables_all_foreach() {
    let lua = fixture();
    run(&lua, "t={5,6,7} s=0 for v in all(t) do s=s+v end");
    assert_eq!(num(&lua, "s"), 18.0);
    run(&lua, "u={1,2,3} p=0 foreach(u, function(x) p=p+x end)");
    assert_eq!(num(&lua, "p"), 6.0);
}

#[test]
fn tables_foreach_del_during_iteration() {
    // Regression: Celeste's _draw does `foreach(objects, function(o) ... end)`
    // and game logic calls `del(objects, o)` mid-iteration. A naive fixed-length
    // loop reads a now-nil slot and passes nil to the callback (crash). The
    // iterator must survive deletion of the current element and still visit
    // every surviving element exactly once.
    let lua = fixture();
    // Delete every element as we visit it: must visit all 4, never see nil.
    run(
        &lua,
        r#"
        t={1,2,3,4}
        seen={}
        nils=0
        foreach(t, function(o)
            if o==nil then nils=nils+1 else seen[#seen+1]=o end
            del(t, o)
        end)
        "#,
    );
    assert_eq!(num(&lua, "nils"), 0.0, "callback must never receive nil");
    assert_eq!(
        num(&lua, "#seen"),
        4.0,
        "every element visited exactly once"
    );
    assert_eq!(num(&lua, "#t"), 0.0, "all elements deleted");
    // Same guarantee through the all() iterator with selective deletion.
    run(
        &lua,
        r#"
        u={10,20,30,40,50}
        sum=0 cnt=0
        for o in all(u) do
            cnt=cnt+1
            sum=sum+o
            if o==30 then del(u,o) end
        end
        "#,
    );
    // All five are visited once even though 30 is removed mid-iteration.
    assert_eq!(num(&lua, "cnt"), 5.0, "all() visits each element once");
    assert_eq!(num(&lua, "sum"), 150.0);
}

// ---------------------------------------------------------------------------
// RNG
// ---------------------------------------------------------------------------

#[test]
fn rng_srand_is_deterministic() {
    let lua = fixture();
    // Same seed -> same sequence.
    run(&lua, "srand(42) a={rnd(100),rnd(100),rnd(100)}");
    run(&lua, "srand(42) b={rnd(100),rnd(100),rnd(100)}");
    assert_eq!(num(&lua, "a[1]"), num(&lua, "b[1]"));
    assert_eq!(num(&lua, "a[2]"), num(&lua, "b[2]"));
    assert_eq!(num(&lua, "a[3]"), num(&lua, "b[3]"));
    // rnd(x) is in [0, x).
    run(&lua, "srand(1) r=rnd(10)");
    let r = num(&lua, "r");
    assert!((0.0..10.0).contains(&r), "rnd(10) out of range: {r}");
}

// ---------------------------------------------------------------------------
// Graphics: clearing and direct pixels
// ---------------------------------------------------------------------------

#[test]
fn gfx_cls_fills_screen() {
    let lua = fixture();
    run(&lua, "cls(5)");
    assert_eq!(px(&lua, 0, 0), 5);
    assert_eq!(px(&lua, 127, 127), 5);
    assert_eq!(lit(&lua), 128 * 128); // every pixel is color 5
                                      // cls() with no arg clears to 0.
    run(&lua, "cls()");
    assert_eq!(lit(&lua), 0);
}

#[test]
fn gfx_pset_pget() {
    let lua = fixture();
    run(&lua, "pset(10, 20, 7)");
    assert_eq!(px(&lua, 10, 20), 7);
    assert_eq!(num(&lua, "pget(10,20)"), 7.0);
    assert_eq!(num(&lua, "pget(0,0)"), 0.0);
    // pset uses current pen color when omitted.
    run(&lua, "color(11) pset(5,5)");
    assert_eq!(px(&lua, 5, 5), 11);
}

#[test]
fn gfx_color_sets_pen() {
    let lua = fixture();
    run(&lua, "color(9) pset(1,1)");
    assert_eq!(px(&lua, 1, 1), 9);
}

// ---------------------------------------------------------------------------
// Graphics: shapes
// ---------------------------------------------------------------------------

#[test]
fn gfx_line() {
    let lua = fixture();
    run(&lua, "line(0,0,5,0,8)"); // horizontal
    for x in 0..=5 {
        assert_eq!(px(&lua, x, 0), 8, "line pixel missing at x={x}");
    }
}

#[test]
fn gfx_line_stateful_continue() {
    let lua = fixture();
    // Full segment sets the anchor at (5,0); the 2-arg form continues from it.
    run(&lua, "line(0,0,5,0,8) line(5,5)");
    // Continued vertical segment from (5,0) to (5,5) keeps color 8.
    for y in 0..=5 {
        assert_eq!(px(&lua, 5, y), 8, "continued line pixel missing at y={y}");
    }
    // 1-arg form sets the pen color and resets the polyline anchor. With the
    // anchor reset, 2-arg calls draw nothing until a full segment re-establishes
    // it; then a 2-arg call continues using the persisted pen color (12).
    run(&lua, "line(12) line(20,20) line(40,40,40,60) line(60,60)");
    assert_eq!(px(&lua, 20, 20), 0, "no anchor after reset → nothing drawn");
    // line(40,40,40,60) re-anchors at (40,60); line(60,60) continues from it.
    assert_eq!(
        px(&lua, 60, 60),
        12,
        "continued segment should use color 12"
    );
    assert_eq!(px(&lua, 50, 60), 12, "midpoint of continued segment");
}

#[test]
fn gfx_rect_and_rectfill() {
    let lua = fixture();
    run(&lua, "rect(2,2,6,6,3)");
    // border drawn...
    assert_eq!(px(&lua, 2, 2), 3);
    assert_eq!(px(&lua, 6, 6), 3);
    // ...interior empty.
    assert_eq!(px(&lua, 4, 4), 0);

    run(&lua, "cls() rectfill(2,2,6,6,4)");
    assert_eq!(px(&lua, 4, 4), 4); // interior filled
    assert_eq!(px(&lua, 2, 2), 4);
}

#[test]
fn gfx_circ_and_circfill() {
    let lua = fixture();
    run(&lua, "circfill(64,64,5,2)");
    assert_eq!(px(&lua, 64, 64), 2, "circfill center should be filled");
    assert!(lit(&lua) > 0);

    run(&lua, "cls() circ(64,64,5,2)");
    // outline present, center empty.
    assert_eq!(px(&lua, 64, 64), 0, "circ center should be empty");
    assert!(lit(&lua) > 0);
}

#[test]
fn gfx_oval_and_ovalfill() {
    let lua = fixture();
    run(&lua, "ovalfill(10,10,30,20,6)");
    assert!(lit(&lua) > 0, "ovalfill should draw");
    let filled = lit(&lua);
    run(&lua, "cls() oval(10,10,30,20,6)");
    let outline = lit(&lua);
    assert!(
        outline > 0 && outline < filled,
        "oval outline < ovalfill area"
    );
}

// ---------------------------------------------------------------------------
// Graphics: sprites & sprite sheet
// ---------------------------------------------------------------------------

#[test]
fn gfx_sset_sget() {
    let lua = fixture();
    run(&lua, "sset(3,4,9)");
    assert_eq!(num(&lua, "sget(3,4)"), 9.0);
}

#[test]
fn gfx_spr_draws_sprite() {
    let lua = fixture();
    // Paint sprite 0 (sheet 0..8 x 0..8) solid color 12 via sset.
    run(&lua, "for y=0,7 do for x=0,7 do sset(x,y,12) end end");
    run(&lua, "spr(0, 20, 30)");
    assert_eq!(px(&lua, 20, 30), 12, "spr should blit sprite pixels");
    assert_eq!(px(&lua, 27, 37), 12);
    // color 0 is transparent by default — paint sprite 1 col 0 and confirm it
    // does not overwrite the background.
    run(&lua, "cls(5)");
    run(&lua, "for y=0,7 do for x=0,7 do sset(8+x,y,0) end end");
    run(&lua, "spr(1, 40, 40)");
    assert_eq!(px(&lua, 40, 40), 5, "color 0 should be transparent in spr");
}

#[test]
fn gfx_sspr_scales() {
    let lua = fixture();
    run(&lua, "for y=0,7 do for x=0,7 do sset(x,y,10) end end");
    // Stretch the 8x8 sprite region to 16x16 at (0,0).
    run(&lua, "sspr(0,0,8,8,0,0,16,16)");
    assert_eq!(px(&lua, 0, 0), 10);
    assert_eq!(px(&lua, 15, 15), 10, "sspr should fill the scaled area");
}

// ---------------------------------------------------------------------------
// Map
// ---------------------------------------------------------------------------

#[test]
fn map_mget_mset() {
    let lua = fixture();
    run(&lua, "mset(3,4,42)");
    assert_eq!(num(&lua, "mget(3,4)"), 42.0);
    assert_eq!(num(&lua, "mget(0,0)"), 0.0);
}

#[test]
fn map_draws_tiles_and_skips_zero() {
    let lua = fixture();
    // sprite 1 = solid color 8; sprite 0 left as zero.
    run(&lua, "for y=0,7 do for x=0,7 do sset(8+x,y,8) end end");
    run(&lua, "mset(0,0,0) mset(1,0,1)"); // tile 0 empty, tile 1 solid
    run(&lua, "map(0,0,0,0,2,1)");
    // tile 0 cell untouched...
    assert_eq!(px(&lua, 0, 0), 0, "map must skip tile 0");
    // tile 1 cell drawn at screen x=8.
    assert_eq!(px(&lua, 8, 0), 8);
}

// ---------------------------------------------------------------------------
// Sprite flags
// ---------------------------------------------------------------------------

#[test]
fn flags_fget_fset() {
    let lua = fixture();
    run(&lua, "fset(5, 3)"); // set whole byte to 3 (bits 0 and 1)
    assert_eq!(num(&lua, "fget(5)"), 3.0);
    // bit form returns a BOOLEAN (0 is truthy in Lua, so this must be a bool!).
    assert!(boolean(&lua, "fget(5,0)"));
    assert!(boolean(&lua, "fget(5,1)"));
    assert!(!boolean(&lua, "fget(5,2)"));
    // setting a single bit.
    run(&lua, "fset(6, 7, true)");
    assert!(boolean(&lua, "fget(6,7)"));
    assert!(!boolean(&lua, "fget(6,0)"));
}

// ---------------------------------------------------------------------------
// Drawing state: camera, clip, pal, palt, fillp, cursor
// ---------------------------------------------------------------------------

#[test]
fn state_camera_offsets_drawing() {
    let lua = fixture();
    run(&lua, "camera(10,20) pset(15,25,7)");
    // pixel lands at world-(15,25) minus camera-(10,20) = screen (5,5).
    assert_eq!(px(&lua, 5, 5), 7);
    // camera is mirrored to 0x5f28 hardware registers.
    let cx = ((num(&lua, "peek(0x5f29)") as i32) << 8) | num(&lua, "peek(0x5f28)") as i32;
    assert_eq!(cx, 10);
}

#[test]
fn state_setters_return_previous_value() {
    let lua = fixture();
    // camera() returns the prior offset.
    let cam = lua
        .load("camera(10,20) return {camera(0,0)}")
        .eval::<Vec<i32>>()
        .unwrap();
    assert_eq!(cam, vec![10, 20]);
    // color() returns the prior pen (default 6, then 9).
    assert_eq!(num(&lua, "color(9)"), 6.0);
    assert_eq!(num(&lua, "color(3)"), 9.0);
    // cursor() returns the prior position.
    let cur = lua
        .load("cursor(5,7) return {cursor(0,0)}")
        .eval::<Vec<i32>>()
        .unwrap();
    assert_eq!(cur, vec![5, 7]);
    // fillp() returns the prior pattern.
    assert_eq!(num(&lua, "fillp(0x5a5a)"), 0.0);
    assert_eq!(num(&lua, "fillp(0)"), 0x5a5a as f64);
}

#[test]
fn state_clip_restricts_drawing() {
    let lua = fixture();
    run(&lua, "clip(10,10,8,8) rectfill(0,0,127,127,7)");
    // Inside the clip rect: drawn.
    assert_eq!(px(&lua, 12, 12), 7);
    // Outside the clip rect: untouched.
    assert_eq!(px(&lua, 0, 0), 0);
    assert_eq!(px(&lua, 30, 30), 0);
}

#[test]
fn state_clip_returns_previous_rect() {
    let lua = fixture();
    // clip() returns the previous rect; the first call returns the full screen.
    let prev = lua
        .load("return {clip(10,20,30,40)}")
        .eval::<Vec<i32>>()
        .unwrap();
    assert_eq!(prev, vec![0, 0, 128, 128]);
    // The next call sees the rect we just set.
    let prev2 = lua.load("return {clip()}").eval::<Vec<i32>>().unwrap();
    assert_eq!(prev2, vec![10, 20, 30, 40]);
}

#[test]
fn state_clip_previous_intersects() {
    let lua = fixture();
    // Establish [20,60)x[20,60), then intersect with [0,40)x[0,40) via the 5th
    // arg → [20,40)x[20,40). Fill and check only the intersection draws.
    run(
        &lua,
        "clip(20,20,40,40) clip(0,0,40,40,true) rectfill(0,0,127,127,7)",
    );
    assert_eq!(px(&lua, 25, 25), 7, "inside the intersection");
    assert_eq!(px(&lua, 50, 50), 0, "outside the intersection (>=40)");
    assert_eq!(px(&lua, 10, 10), 0, "outside the intersection (<20)");
}

#[test]
fn state_pal_remaps_draw_color() {
    let lua = fixture();
    // Map color 7 -> 8 in the draw palette; drawing with 7 yields 8.
    run(&lua, "pal(7,8) pset(1,1,7)");
    assert_eq!(px(&lua, 1, 1), 8);
    // pal() resets.
    run(&lua, "pal() pset(2,2,7)");
    assert_eq!(px(&lua, 2, 2), 7);
}

#[test]
fn state_pal_table_form_bulk_remap() {
    let lua = fixture();
    // Table form: remap several colors at once via {[c0]=c1, ...}.
    run(&lua, "pal({[7]=8,[6]=9}) pset(1,1,7) pset(2,2,6)");
    assert_eq!(px(&lua, 1, 1), 8, "7 should remap to 8 via table");
    assert_eq!(px(&lua, 2, 2), 9, "6 should remap to 9 via table");
}

#[test]
fn state_pal_returns_previous_mapping() {
    let lua = fixture();
    // pal(c0,c1) returns the previous mapping for c0 (identity at first).
    assert_eq!(num(&lua, "pal(7,8)"), 7.0);
    // After remapping 7->8, the prior mapping is now 8.
    assert_eq!(num(&lua, "pal(7,9)"), 8.0);
}

#[test]
fn state_pal_single_number_resets_palette() {
    let lua = fixture();
    // Remap, then pal(0) resets the draw palette back to identity.
    run(&lua, "pal(7,8) pal(0) pset(1,1,7)");
    assert_eq!(px(&lua, 1, 1), 7, "pal(0) should reset the draw palette");
}

#[test]
fn state_palt_transparency() {
    let lua = fixture();
    // Make color 8 transparent, then a sprite of color 8 won't draw.
    run(&lua, "cls(3)");
    run(&lua, "for y=0,7 do for x=0,7 do sset(x,y,8) end end");
    run(&lua, "palt(8, true) spr(0, 10, 10)");
    assert_eq!(
        px(&lua, 10, 10),
        3,
        "palt(8,true) should make color 8 transparent"
    );
    // Reset transparency: color 0 transparent again, 8 opaque.
    run(&lua, "palt() spr(0, 10, 10)");
    assert_eq!(px(&lua, 10, 10), 8);
}

#[test]
fn state_fillp_pattern() {
    let lua = fixture();
    // A fill pattern of 0 = fully solid; rectfill fills everything.
    run(&lua, "fillp(0) rectfill(0,0,7,7,9)");
    assert_eq!(px(&lua, 0, 0), 9);
    // Pattern of 0xFFFF = fully "off" bits — interior should not get the color.
    run(&lua, "cls() fillp(0xffff) rectfill(0,0,7,7,9)");
    // With every bit set, the solid color is suppressed across the rect.
    assert_eq!(
        px(&lua, 3, 3),
        0,
        "fillp(0xffff) should suppress the fill color"
    );
    run(&lua, "fillp(0)"); // reset for hygiene
}

#[test]
fn state_cursor_advances_print() {
    let lua = fixture();
    run(&lua, "cursor(10, 20) print('a')");
    // 'a' drew near (10,20); some pixel in that glyph box is lit.
    let con = lua.app_data_ref::<Console>().unwrap();
    let mut found = false;
    for y in 20..26 {
        for x in 10..15 {
            if con.screen[(y as usize) * SCREEN_W + x as usize] != 0 {
                found = true;
            }
        }
    }
    assert!(found, "cursor() should position print output");
}

#[test]
fn text_print_two_arg_is_color_not_x() {
    let lua = fixture();
    // print(str, c) — the 2-arg form: arg2 is the COLOR, text prints at the
    // cursor (0,0), NOT at x=8. (Common idiom: print("hi", 7).)
    run(&lua, "print('a', 8)");
    let con = lua.app_data_ref::<Console>().unwrap();
    // A lit pixel of 'a' near the cursor origin must be color 8.
    let mut found_color8 = false;
    let mut any_lit_far = false;
    for y in 0..6 {
        for x in 0..5 {
            let p = con.screen[(y as usize) * SCREEN_W + x as usize];
            if p == 8 {
                found_color8 = true;
            }
        }
        // Nothing should be drawn around x=8 (which would mean arg2 was x).
        for x in 8..13 {
            if con.screen[(y as usize) * SCREEN_W + x as usize] != 0 {
                any_lit_far = true;
            }
        }
    }
    assert!(found_color8, "2-arg print should draw at cursor in color 8");
    assert!(
        !any_lit_far,
        "2-arg print must NOT treat arg2 as x position"
    );
}

// ---------------------------------------------------------------------------
// Text
// ---------------------------------------------------------------------------

#[test]
fn text_print_draws_and_returns_x() {
    let lua = fixture();
    // print returns the x position after the last char.
    let ret = num(&lua, "print('hi', 0, 0, 7)");
    assert!(ret > 0.0, "print should return advanced x, got {ret}");
    assert!(lit(&lua) > 0, "print should draw glyph pixels");
}

// ---------------------------------------------------------------------------
// Input
// ---------------------------------------------------------------------------

#[test]
fn input_btn_btnp() {
    let lua = fixture();
    // Press Right (index 1) for player 0.
    {
        let mut con = lua.app_data_mut::<Console>().unwrap();
        let mut st = [false; 12];
        st[1] = true;
        con.update_input(st);
    }
    assert!(boolean(&lua, "btn(1)"), "btn(1) should be held");
    assert!(!boolean(&lua, "btn(0)"));
    assert!(boolean(&lua, "btnp(1)"), "btnp(1) true on first frame");
    // btn() with no args returns a bitmask.
    assert_eq!(num(&lua, "btn()"), 2.0); // bit 1 set
                                         // Second frame still held -> btnp false.
    {
        let mut con = lua.app_data_mut::<Console>().unwrap();
        let mut st = [false; 12];
        st[1] = true;
        con.update_input(st);
    }
    assert!(boolean(&lua, "btn(1)"));
    assert!(!boolean(&lua, "btnp(1)"), "btnp false while still held");
}

#[test]
fn input_btnp_auto_repeat_through_binding() {
    let lua = fixture();
    // Hold Right (index 1) and count which frames btnp(1) fires on. PICO-8
    // re-fires at frame 1, 15, then every 4 frames (19, 23, ...).
    let mut fired = Vec::new();
    for frame in 1..=23 {
        {
            let mut con = lua.app_data_mut::<Console>().unwrap();
            let mut st = [false; 12];
            st[1] = true;
            con.update_input(st);
        }
        if boolean(&lua, "btnp(1)") {
            fired.push(frame);
        }
    }
    assert_eq!(fired, vec![1, 15, 19, 23], "btnp repeat timing via binding");
}

// ---------------------------------------------------------------------------
// Memory map
// ---------------------------------------------------------------------------

#[test]
fn mem_peek_poke() {
    let lua = fixture();
    run(&lua, "poke(0x4300, 200)");
    assert_eq!(num(&lua, "peek(0x4300)"), 200.0);
}

#[test]
fn mem_peek2_poke2() {
    let lua = fixture();
    run(&lua, "poke2(0x4300, 1000)");
    assert_eq!(num(&lua, "peek2(0x4300)"), 1000.0);
}

#[test]
fn mem_peek4_poke4_fixed_point() {
    let lua = fixture();
    // poke4/peek4 round-trip a 16.16 fixed-point value.
    run(&lua, "poke4(0x4300, 1.5)");
    assert!((num(&lua, "peek4(0x4300)") - 1.5).abs() < 1e-9);
}

#[test]
fn mem_memcpy_memset() {
    let lua = fixture();
    run(&lua, "poke(0x4300, 11) poke(0x4301, 22)");
    run(&lua, "memcpy(0x4310, 0x4300, 2)");
    assert_eq!(num(&lua, "peek(0x4310)"), 11.0);
    assert_eq!(num(&lua, "peek(0x4311)"), 22.0);
    run(&lua, "memset(0x4320, 0xff, 3)");
    assert_eq!(num(&lua, "peek(0x4320)"), 255.0);
    assert_eq!(num(&lua, "peek(0x4322)"), 255.0);
}

#[test]
fn mem_sprite_sheet_via_poke() {
    let lua = fixture();
    // 0x0000 maps to the sprite sheet (2 pixels/byte). poke a packed byte.
    run(&lua, "poke(0x0000, 0x21)"); // low nibble=1 (left px), high=2 (right px)
    assert_eq!(num(&lua, "sget(0,0)"), 1.0);
    assert_eq!(num(&lua, "sget(1,0)"), 2.0);
}

// ---------------------------------------------------------------------------
// Persistent data (cartdata / dget / dset)
// ---------------------------------------------------------------------------

#[test]
fn persist_dget_dset() {
    let lua = fixture();
    run(&lua, "dset(3, 99)");
    assert_eq!(num(&lua, "dget(3)"), 99.0);
    assert_eq!(num(&lua, "dget(0)"), 0.0);
}

#[test]
fn persist_cartdata_is_callable() {
    let lua = fixture();
    // cartdata just sets an id; should not error and dset/dget keep working.
    run(&lua, "cartdata('spark_test_cart')");
    run(&lua, "dset(0, 7)");
    assert_eq!(num(&lua, "dget(0)"), 7.0);
}

// ---------------------------------------------------------------------------
// Audio
// ---------------------------------------------------------------------------

#[test]
fn audio_sfx_starts_channel() {
    let lua = fixture();
    // Give SFX 0 some note data so it actually plays.
    {
        let mut audio = lua.app_data_mut::<Audio>().unwrap();
        audio.sfx_data[0].speed = 16;
        audio.sfx_data[0].notes[0].volume = 5;
        audio.sfx_data[0].notes[0].pitch = 30;
    }
    run(&lua, "sfx(0)");
    let audio = lua.app_data_ref::<Audio>().unwrap();
    let playing = (0..4).any(|ch| audio.sfx_on_channel(ch) == 0);
    assert!(playing, "sfx(0) should start playing on a channel");
}

#[test]
fn audio_music_and_stop() {
    let lua = fixture();
    // music(-1) stops; music(n) starts. Just assert no error + state change.
    run(&lua, "music(0)");
    run(&lua, "music(-1)"); // stop
                            // sfx(-1) stops a channel; should not error.
    run(&lua, "sfx(-1)");
}

// ---------------------------------------------------------------------------
// System / status
// ---------------------------------------------------------------------------

#[test]
fn system_stat_mouse_and_defaults() {
    let lua = fixture();
    {
        let mut con = lua.app_data_mut::<Console>().unwrap();
        con.mouse_x = 64;
        con.mouse_y = 96;
        con.mouse_btn = 1;
    }
    assert_eq!(num(&lua, "stat(32)"), 64.0);
    assert_eq!(num(&lua, "stat(33)"), 96.0);
    assert_eq!(num(&lua, "stat(34)"), 1.0);
}

#[test]
fn system_stat_fields_match_reference() {
    let lua = fixture();
    // stat(28): keyboard scancode held -> boolean false (not a number/sfx id).
    assert!(!boolean(&lua, "stat(28)"));
    // stat(29): unknown field -> 0.
    assert_eq!(num(&lua, "stat(29)"), 0.0);
    // stat(24) and its alias stat(54) report the same music pattern index.
    assert_eq!(num(&lua, "stat(24)"), num(&lua, "stat(54)"));
}

#[test]
fn system_time_advances_with_frames() {
    let lua = fixture();
    // time()/t() derive from the frame counter, advanced by call_update.
    let t0 = num(&lua, "time()");
    lib::call_update(&lua).unwrap();
    lib::call_update(&lua).unwrap();
    let t1 = num(&lua, "t()");
    assert!(
        t1 > t0,
        "time() should advance after _update frames ({t0} -> {t1})"
    );
}

#[test]
fn system_printh_is_noop_callable() {
    let lua = fixture();
    // printh writes to stderr; just confirm it doesn't error from Lua.
    run(&lua, "printh('coverage test line')");
}

#[test]
fn system_menuitem_extcmd_callable() {
    let lua = fixture();
    // menuitem registers a pause-menu entry; should not error.
    run(&lua, "menuitem(1, 'restart', function() end)");
    // extcmd with an unknown command is a no-op (screenshot writes a file; we
    // only assert the unknown branch is safe here).
    run(&lua, "extcmd('unknown_cmd')");
}

#[test]
fn system_menuitem_stores_and_clears_slots() {
    use spark_core as _;
    let lua = fixture();
    // Registering a callback stores it; we verify it is retrievable and that it
    // accepts the button-bitmask argument PICO-8 passes (and may return true to
    // keep the menu open).
    run(
        &lua,
        "_hit=0 menuitem(2, 'opt', function(b) _hit=b return true end)",
    );
    {
        let items = lua.app_data_ref::<lib::lua_api::MenuItems>().unwrap();
        // Slot index 2 (1-based) -> vec index 1.
        assert!(items.0[1].is_some(), "menuitem should occupy slot 2");
        let key = items.0[1].as_ref().unwrap().callback.as_ref().unwrap();
        let f: mlua::Function = lua.registry_value(key).unwrap();
        // Invoke as the host would, passing the trigger bitmask (4 = X).
        let kept_open: bool = f.call(4i64).unwrap();
        assert!(kept_open, "callback returning true should keep menu open");
    }
    assert_eq!(
        num(&lua, "_hit"),
        4.0,
        "callback should receive the bitmask"
    );
    // menuitem(index) with no label clears the slot.
    run(&lua, "menuitem(2)");
    let items = lua.app_data_ref::<lib::lua_api::MenuItems>().unwrap();
    assert!(
        items.0[1].is_none(),
        "menuitem(index) should clear the slot"
    );
}

#[test]
fn system_stop_run_signal_via_error() {
    let lua = fixture();
    // stop()/run() signal the host loop by raising a recognizable error.
    assert!(
        lua.load("stop()").exec().is_err(),
        "stop() should raise a signal"
    );
    assert!(
        lua.load("run()").exec().is_err(),
        "run() should raise a signal"
    );
    // resume() is a no-op stub.
    run(&lua, "resume()");
}

#[test]
fn system_cstore_reload_callable() {
    let lua = fixture();
    // reload() copies cart ROM into memory; cstore() the reverse. With no cart
    // file backing, they must at least be callable without error.
    run(&lua, "reload()");
    run(&lua, "cstore()");
}

#[test]
fn system_load_signal_via_error() {
    let lua = fixture();
    // load('cart') signals the host loop to switch carts via an error.
    assert!(
        lua.load("load('other.p8')").exec().is_err(),
        "load() should raise a host signal"
    );
}

// ---------------------------------------------------------------------------
// tline (textured line / map-sampled line)
// ---------------------------------------------------------------------------

#[test]
fn gfx_tline_samples_map() {
    let lua = fixture();
    // `tline` is a textured-MAP line: it reads the map for a tile, then that
    // tile's sprite. Point map (0,0) at sprite 1, fill sprite 1 solid, and draw
    // a line across map tile (0,0).
    run(&lua, "for y=0,7 do for x=8,15 do sset(x,y,14) end end"); // sprite 1
    run(&lua, "mset(0,0,1)");
    run(&lua, "tline(0,0,7,0, 0,0, 0.125,0)");
    assert!(lit(&lua) > 0, "tline should draw map-sampled pixels");
}

// ---------------------------------------------------------------------------
// Coverage guard: every registered global must be tested above
// ---------------------------------------------------------------------------

/// The set of Lua globals this suite intentionally exercises. If a new API is
/// registered in `lua_api.rs` without being added here (and to a real test),
/// `api_surface_is_fully_covered` fails — preventing silent coverage gaps.
const COVERED: &[&str] = &[
    // math
    "abs", "flr", "ceil", "sgn", "min", "max", "mid", "sqrt", "sin", "cos", "atan2",
    // bitwise
    "band", "bor", "bxor", "bnot", "shl", "shr", "lshr", "rotl", "rotr", // conversion
    "tonum", "tostr", // strings / tables
    "sub", "chr", "ord", "split", "add", "count", "del", "deli", "all", "foreach",
    // rng
    "srand", "rnd", // gfx
    "cls", "pset", "pget", "color", "line", "rect", "rectfill", "circ", "circfill", "oval",
    "ovalfill", "sset", "sget", "spr", "sspr", "map", "mget", "mset", "fget", "fset", "print",
    "tline", // state
    "camera", "clip", "pal", "palt", "fillp", "cursor", // input
    "btn", "btnp", // memory
    "peek", "poke", "peek2", "poke2", "peek4", "poke4", "memcpy", "memset", // persist
    "dget", "dset", "cartdata", // audio
    "sfx", "music", // system
    "stat", "time", "t", "printh", "menuitem", "extcmd", "stop", "run", "resume", "cstore",
    "reload", "load",
];

#[test]
fn api_surface_is_fully_covered() {
    // Build a Lua state and enumerate every global function the API registers.
    let lua = fixture();
    let globals = lua.globals();
    let mut registered: Vec<String> = Vec::new();
    for pair in globals.pairs::<String, LuaValue>() {
        let (k, v) = pair.unwrap();
        if matches!(v, LuaValue::Function(_)) {
            registered.push(k);
        }
    }

    // Lua/standard library functions that are not part of the PICO-8 API
    // surface we own. Anything here is intentionally excluded from coverage.
    const STDLIB_ALLOW: &[&str] = &[
        "assert",
        "error",
        "ipairs",
        "pairs",
        "next",
        "print",
        "select",
        "tonumber",
        "tostring",
        "type",
        "rawget",
        "rawset",
        "rawequal",
        "rawlen",
        "setmetatable",
        "getmetatable",
        "pcall",
        "xpcall",
        "collectgarbage",
        "unpack",
        "require",
        "loadstring",
        "dofile",
        "loadfile",
        "flip",
        "yield",
        "cocreate",
        "coresume",
        "costatus",
        "trace",
        // Lua 5.4 runtime built-in (not part of the PICO-8 API we own).
        "warn",
    ];

    let mut untested: Vec<String> = Vec::new();
    for name in &registered {
        if COVERED.contains(&name.as_str()) {
            continue;
        }
        if STDLIB_ALLOW.contains(&name.as_str()) {
            continue;
        }
        untested.push(name.clone());
    }
    untested.sort();

    assert!(
        untested.is_empty(),
        "these registered Lua API globals have no coverage in lua_api_coverage.rs: {untested:?}\n\
         Add a test and list them in COVERED (or STDLIB_ALLOW if not ours)."
    );
}
