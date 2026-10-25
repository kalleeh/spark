use super::*;
use crate::font;

#[test]
fn test_peek_poke_sprite_sheet() {
    let mut con = Console::new();
    // Set two adjacent pixels in the sprite sheet
    con.sprites[0] = 3; // left pixel of byte 0
    con.sprites[1] = 10; // right pixel of byte 0
                         // PICO-8 packing: low nibble = left pixel, high nibble = right pixel
    let val = con.peek(0x0000);
    assert_eq!(val & 0x0F, 3, "Low nibble should be left pixel (3)");
    assert_eq!(
        (val >> 4) & 0x0F,
        10,
        "High nibble should be right pixel (10)"
    );

    // Poke a packed byte and verify the sprite array is updated
    con.poke(0x0001, 0x75); // left=5, right=7
    assert_eq!(
        con.sprites[2], 5,
        "Poke should set left pixel to low nibble"
    );
    assert_eq!(
        con.sprites[3], 7,
        "Poke should set right pixel to high nibble"
    );

    // Round-trip
    con.poke(0x0000, 0xAB);
    assert_eq!(con.peek(0x0000), 0xAB);
}

#[test]
fn test_peek_poke_sprite_flags() {
    let mut con = Console::new();
    // PICO-8 sprite flags live at 0x3000-0x30FF (NOT 0x1000, which is the
    // upper half of the sprite sheet).
    con.flags[0] = 0xFF;
    assert_eq!(con.peek(0x3000), 0xFF);
    con.flags[255] = 0x42;
    assert_eq!(con.peek(0x30FF), 0x42);

    con.poke(0x3000, 0x12);
    assert_eq!(con.flags[0], 0x12);
    con.poke(0x30FF, 0x34);
    assert_eq!(con.flags[255], 0x34);
    // fget sees the poked flag byte.
    assert_eq!(con.fget(0, None), 0x12);
    assert_eq!(con.fget(0, Some(1)), 1);
    assert_eq!(con.fget(0, Some(0)), 0);
}

#[test]
fn test_peek_poke_map_data() {
    let mut con = Console::new();
    con.map[0] = 42;
    assert_eq!(con.peek(0x2000), 42);
    con.map[4095] = 99;
    assert_eq!(con.peek(0x2FFF), 99);

    con.poke(0x2000, 55);
    assert_eq!(con.map[0], 55);
    con.poke(0x2FFF, 77);
    assert_eq!(con.map[4095], 77);
}

#[test]
fn test_peek_poke_user_data() {
    let mut con = Console::new();
    // Initially all zeros
    assert_eq!(con.peek(0x4300), 0);
    assert_eq!(con.peek(0x5DFF), 0);

    con.poke(0x4300, 0xAA);
    assert_eq!(con.peek(0x4300), 0xAA);
    assert_eq!(con.user_data[0], 0xAA);

    // 0x5DFF is the top of the general-use region (0x5E00-0x5EFF is the
    // persistent cartdata block, 0x5F00+ the draw-state page).
    con.poke(0x5DFF, 0xBB);
    assert_eq!(con.peek(0x5DFF), 0xBB);
    assert_eq!(con.user_data[0x5DFF - 0x4300], 0xBB);
    assert_eq!(USER_DATA_SIZE, 0x1B00);
}

#[test]
fn test_cartdata_at_0x5e00_is_persistent_data() {
    let mut con = Console::new();
    // dset(1, 1.5) -> 16.16 bits 0x0001_8000, little-endian at 0x5E04.
    con.dset(1, 1.5);
    assert_eq!(con.peek(0x5E04), 0x00);
    assert_eq!(con.peek(0x5E05), 0x80);
    assert_eq!(con.peek(0x5E06), 0x01);
    assert_eq!(con.peek(0x5E07), 0x00);
    assert_eq!(con.peek4(0x5E04), 0x0001_8000);
    // Negative values keep their two's-complement pattern.
    con.dset(63, -1.0);
    assert_eq!(con.peek4(0x5EFC), 0xFFFF_0000);
    // Poking bytes updates the slot: write 2.25 (0x0002_4000) to slot 0.
    con.poke4(0x5E00, 0x0002_4000);
    assert_eq!(con.dget(0), 2.25);
    con.poke(0x5E02, 0x05); // integer part low byte -> 5.25
    assert_eq!(con.dget(0), 5.25);
    // user_data is untouched by cartdata writes.
    assert!(con.user_data.iter().all(|&b| b == 0));
}

#[test]
fn test_peek_poke_screen_buffer() {
    let mut con = Console::new();
    con.screen[0] = 7;
    con.screen[1] = 12;
    let val = con.peek(0x6000);
    assert_eq!(val & 0x0F, 7);
    assert_eq!((val >> 4) & 0x0F, 12);

    con.poke(0x6000, 0x95); // left=5, right=9
    assert_eq!(con.screen[0], 5);
    assert_eq!(con.screen[1], 9);
}

#[test]
fn test_peek_out_of_bounds() {
    let con = Console::new();
    // Unmapped regions (audio 0x3100-0x42FF, hardware page tail, above
    // the 32 KB map) should return 0.
    assert_eq!(con.peek(0x3100), 0);
    assert_eq!(con.peek(0x4200), 0);
    assert_eq!(con.peek(0x42FF), 0);
    assert_eq!(con.peek(0x5F80), 0);
    assert_eq!(con.peek(0x8000), 0);
    assert_eq!(con.peek(0xFFFF), 0);
}

#[test]
fn test_poke_out_of_bounds_is_noop() {
    let mut con = Console::new();
    // These should not panic and must not disturb any mapped array.
    con.poke(0x3100, 0xFF);
    con.poke(0x4200, 0xFF);
    con.poke(0x5F80, 0xFF);
    con.poke(0x8000, 0xFF);
    con.poke(0xFFFF, 0xFF);
    assert_eq!(con.peek(0x3100), 0);
    assert!(con.sprites.iter().all(|&b| b == 0));
    assert!(con.map.iter().all(|&b| b == 0));
    assert!(con.flags.iter().all(|&b| b == 0));
    assert!(con.user_data.iter().all(|&b| b == 0));
}

#[test]
fn test_upper_sprite_sheet_at_0x1000_aliases_map_rows_32_63() {
    let mut con = Console::new();
    // 0x1000 is the first byte of sheet row 64 (pixels (0,64),(1,64)) AND
    // map cell (0,32). 0x1FFF is sheet pixels (126,127),(127,127) and map
    // cell (127,63).
    con.poke(0x1000, 0x21); // left px 1, right px 2
    assert_eq!(con.sget(0, 64), 1);
    assert_eq!(con.sget(1, 64), 2);
    assert_eq!(con.mget(0, 32), 0x21);
    assert_eq!(con.map[32 * MAP_W], 0x21);

    con.poke(0x1FFF, 0xAB);
    assert_eq!(con.sget(126, 127), 0xB);
    assert_eq!(con.sget(127, 127), 0xA);
    assert_eq!(con.mget(127, 63), 0xAB);
    assert_eq!(con.peek(0x1FFF), 0xAB);

    // mset in the lower map writes through to the sheet pixels...
    con.mset(5, 40, 0x7C);
    // byte address = 40*128 + 5 = 0x1405 -> pixels 0x280A, 0x280B = row 80
    assert_eq!(con.peek(0x1405), 0x7C);
    assert_eq!(con.sget(10, 80), 0xC);
    assert_eq!(con.sget(11, 80), 0x7);
    // ...and sset in the lower sheet writes through to the map cell.
    con.sset(10, 80, 0x3);
    assert_eq!(con.mget(5, 40), 0x73);
    assert_eq!(con.peek(0x1405), 0x73);

    // The lower sheet (rows 0-63) and upper map (rows 0-31) are NOT
    // aliased.
    con.poke(0x0000, 0xFF);
    assert_eq!(con.mget(0, 0), 0);
    con.poke(0x2000, 0x55);
    assert_eq!(con.sget(0, 0), 0xF);
    assert_eq!(con.mget(0, 0), 0x55);
}

#[test]
fn test_memcpy_within_same_region() {
    let mut con = Console::new();
    // Write some data into user data
    con.poke(0x4300, 0x11);
    con.poke(0x4301, 0x22);
    con.poke(0x4302, 0x33);

    // Copy 3 bytes within user data
    con.memcpy(0x4310, 0x4300, 3);

    assert_eq!(con.peek(0x4310), 0x11);
    assert_eq!(con.peek(0x4311), 0x22);
    assert_eq!(con.peek(0x4312), 0x33);
}

#[test]
fn test_memcpy_across_regions() {
    let mut con = Console::new();
    // Set some sprite flags
    con.flags[0] = 0xAA;
    con.flags[1] = 0xBB;

    // Copy sprite flags (0x3000) into user data (0x4300)
    con.memcpy(0x4300, 0x3000, 2);

    assert_eq!(con.peek(0x4300), 0xAA);
    assert_eq!(con.peek(0x4301), 0xBB);
}

#[test]
fn test_memset() {
    let mut con = Console::new();
    con.memset(0x4300, 0xFF, 10);
    for i in 0..10 {
        assert_eq!(con.peek(0x4300 + i), 0xFF);
    }
    // Byte after should be untouched
    assert_eq!(con.peek(0x430A), 0);
}

#[test]
fn test_memcpy_zero_length() {
    let mut con = Console::new();
    con.poke(0x4300, 0x42);
    con.memcpy(0x4301, 0x4300, 0);
    assert_eq!(con.peek(0x4301), 0, "Zero-length memcpy should be a no-op");
}

#[test]
fn test_memset_zero_length() {
    let mut con = Console::new();
    con.poke(0x4300, 0x42);
    con.memset(0x4300, 0xFF, 0);
    assert_eq!(
        con.peek(0x4300),
        0x42,
        "Zero-length memset should be a no-op"
    );
}

#[test]
fn test_sprite_pixel_packing_round_trip() {
    let mut con = Console::new();
    // Set pixels via the sprite array (1 byte per pixel)
    for i in 0..16u8 {
        con.sprites[i as usize * 2] = i; // left pixel
        con.sprites[i as usize * 2 + 1] = 15 - i; // right pixel
    }

    // Read back via peek (packed format)
    for i in 0..16u16 {
        let packed = con.peek(i);
        let left = packed & 0x0F;
        let right = (packed >> 4) & 0x0F;
        assert_eq!(left, i as u8, "Left pixel mismatch at byte {}", i);
        assert_eq!(right, 15 - i as u8, "Right pixel mismatch at byte {}", i);
    }

    // Write via poke and read back from sprite array
    for i in 0..16u16 {
        let packed = (i as u8) | ((15 - i as u8) << 4);
        con.poke(i, packed);
    }
    for i in 0..16u16 {
        assert_eq!(con.sprites[i as usize * 2], i as u8);
        assert_eq!(con.sprites[i as usize * 2 + 1], 15 - i as u8);
    }
}

fn lit_count(con: &Console) -> usize {
    con.screen.iter().filter(|&&p| p != 0).count()
}

#[test]
fn test_print_p8scii_control_codes() {
    // Control byte 3 (`\-{n}`) moves the cursor and must NOT render its
    // parameter byte as a glyph. "\-8ab" should look identical to "ab".
    let mut plain = Console::new();
    plain.print("ab", Some(10), Some(10), Some(7));
    let mut ctrl = Console::new();
    let s = alloc::string::String::from_utf8(alloc::vec![3u8, b'8', b'a', b'b']).unwrap();
    ctrl.print(&s, Some(10), Some(10), Some(7));
    assert_eq!(
        lit_count(&ctrl),
        lit_count(&plain),
        "control-code parameter byte must not be drawn as a glyph"
    );

    // Control byte 1 (`\*{n}`) repeats the next char n times: "\*3x" == "xxx".
    let mut rep = Console::new();
    let s = alloc::string::String::from_utf8(alloc::vec![1u8, b'3', b'x']).unwrap();
    rep.print(&s, Some(0), Some(0), Some(7));
    let mut three = Console::new();
    three.print("xxx", Some(0), Some(0), Some(7));
    assert_eq!(lit_count(&rep), lit_count(&three), "\\*3x should equal xxx");
}

#[test]
fn test_print_horizontal_cursor_move() {
    // `\-{n}` shifts by (n-16) px. Print "x" at x=20, vs "\-x" (param 'x'
    // => 33, but really we just verify the glyph lands shifted, not at 20).
    let mut c = Console::new();
    // byte 3, param '0' (=0 => delta -16), then 'x'
    let s = alloc::string::String::from_utf8(alloc::vec![3u8, b'0', b'x']).unwrap();
    c.print(&s, Some(20), Some(20), Some(7));
    // Glyph should be drawn ~16px left of 20 (i.e. near x=4), not at 20.
    let mut any_left = false;
    for y in 20..26 {
        for x in 0..10 {
            if c.screen[y * SCREEN_W + x] != 0 {
                any_left = true;
            }
        }
    }
    assert!(any_left, "\\-0 should move the cursor left by 16px");
}

#[test]
fn test_print_background_color() {
    // `\#{c}` fills each subsequent glyph cell with color c behind the fg.
    // "\#7a" in fg color 12: the 4x6 cell holds bg(7) + fg(12) pixels.
    let mut c = Console::new();
    let s = alloc::string::String::from_utf8(alloc::vec![2u8, b'7', b'a']).unwrap();
    c.print(&s, Some(0), Some(0), Some(12));
    let mut bg = 0;
    let mut fg = 0;
    for y in 0..6 {
        for x in 0..4 {
            match c.screen[y * SCREEN_W + x] {
                7 => bg += 1,
                12 => fg += 1,
                _ => {}
            }
        }
    }
    assert!(bg > 0, "\\# should fill the cell background");
    assert!(fg > 0, "glyph foreground should still draw over the bg");
    assert_eq!(bg + fg, 24, "the full 4x6 cell should be bg or fg");

    // Without `\#`, no background is filled.
    let mut plain = Console::new();
    plain.print("a", Some(0), Some(0), Some(12));
    let plain_bg = (0..6)
        .flat_map(|y| (0..4).map(move |x| (y, x)))
        .filter(|&(y, x)| plain.screen[y * SCREEN_W + x] == 7)
        .count();
    assert_eq!(plain_bg, 0, "plain print should not fill a background");
}

#[test]
fn test_print_caret_command_param_counts() {
    // The `\^{cmd}` family consumes a per-command number of parameter bytes.
    // A wrong count would leak bytes as glyphs or eat real text.
    // `\^g` takes 0 params -> "\^gx" must render exactly "x".
    let mut g = Console::new();
    let s = alloc::string::String::from_utf8(alloc::vec![6u8, b'g', b'x']).unwrap();
    g.print(&s, Some(0), Some(0), Some(7));
    let mut plain = Console::new();
    plain.print("x", Some(0), Some(0), Some(7));
    assert_eq!(
        lit_count(&g),
        lit_count(&plain),
        "\\^g takes no params; the 'x' must render normally"
    );

    // `\^c{col}` (1 param) sets the foreground color.
    let mut c = Console::new();
    let s = alloc::string::String::from_utf8(alloc::vec![6u8, b'c', b'7', b'a']).unwrap();
    c.print(&s, Some(0), Some(0), Some(12));
    let col7 = (0..6)
        .flat_map(|y| (0..4).map(move |x| (y, x)))
        .filter(|&(y, x)| c.screen[y * SCREEN_W + x] == 7)
        .count();
    let col12 = (0..6)
        .flat_map(|y| (0..4).map(move |x| (y, x)))
        .filter(|&(y, x)| c.screen[y * SCREEN_W + x] == 12)
        .count();
    assert!(col7 > 0, "\\^c7 should switch the pen to color 7");
    assert_eq!(col12, 0, "the original color 12 should be overridden");
}

#[test]
fn test_print_caret_jump() {
    // `\^j{x}{y}` (2 params) sets an absolute cursor position (param*4 px).
    let mut c = Console::new();
    // jump to (2*4, 4*4) = (8, 16), then draw 'z'.
    let s = alloc::string::String::from_utf8(alloc::vec![6u8, b'j', b'2', b'4', b'z']).unwrap();
    c.print(&s, Some(0), Some(0), Some(7));
    let near = (14..22)
        .flat_map(|y| (6..14).map(move |x| (y, x)))
        .filter(|&(y, x)| c.screen[y * SCREEN_W + x] != 0)
        .count();
    assert!(near > 0, "\\^j24 should move the cursor to (8,16)");
    // Nothing should have been drawn at the origin.
    let at_origin = (0..6)
        .flat_map(|y| (0..4).map(move |x| (y, x)))
        .filter(|&(y, x)| c.screen[y * SCREEN_W + x] != 0)
        .count();
    assert_eq!(at_origin, 0, "the jump should move away from the origin");
}

#[test]
fn test_drawing_extreme_coords_no_panic() {
    // Regression for the fuzzer-found arithmetic-overflow / OOB panics:
    // every drawing primitive must survive i32::MIN/MAX coordinates,
    // extreme cameras/clips, and huge radii without panicking. (Run under
    // debug overflow checks, this would previously abort.)
    let extremes = [
        i32::MIN,
        i32::MAX,
        i32::MIN / 2,
        i32::MAX / 2,
        -1,
        0,
        127,
        128,
    ];
    let mut con = Console::new();
    for &cam in &[0, i32::MIN, i32::MAX, 1000] {
        con.camera(cam, cam);
        con.clip_rect(cam, cam, cam, cam, false);
        for &a in &extremes {
            for &b in &extremes {
                con.pset(a, b, Some(8));
                let _ = con.pget(a, b);
                con.line(a, b, b, a, Some(8));
                con.rect(a, b, b, a, Some(8));
                con.rectfill(a, b, b, a, Some(8));
                con.circ(a, b, a, Some(8));
                con.circfill(a, b, b, Some(8));
                con.oval(a, b, b, a, Some(8));
                con.ovalfill(a, b, b, a, Some(8));
                con.spr(99, a, b, 2.0, 2.0, false, false);
                con.sspr(a, b, 8, 8, b, a, 16, 16, false, false);
                con.tline(a, b, b, a, 0.5, 0.5, 0.1, 0.1);
                con.print("hi", Some(a), Some(b), Some(7));
            }
        }
        // restore a sane clip so the next camera iteration draws normally
        con.clip_rect(0, 0, SCREEN_W as i32, SCREEN_H as i32, false);
    }
    // Reaching here without a panic is the assertion.
}

#[test]
fn test_memory_ops_full_address_space_no_panic() {
    // Every memory accessor must survive arbitrary addresses across the
    // whole u16 space (incl. region boundaries where a 2/4-byte access
    // could straddle the end) and arbitrary lengths, without panicking.
    let mut con = Console::new();
    // A spread of boundary + interior addresses, including the last bytes
    // of each mapped region where multi-byte reads could overrun.
    let addrs: [u16; 14] = [
        0x0000, 0x0FFF, 0x1000, 0x1FFF, 0x2FFF, 0x3000, 0x30FF, 0x42FF, 0x4300, 0x5EFF, 0x5F00,
        0x5FFF, 0x7FFF, 0xFFFF,
    ];
    let lens: [u16; 6] = [0, 1, 2, 4, 0x4300, 0xFFFF];
    for &a in &addrs {
        let _ = con.peek(a);
        let _ = con.peek2(a);
        let _ = con.peek4(a);
        con.poke(a, 0xAB);
        con.poke2(a, 0xBEEF);
        con.poke4(a, 0xDEAD_BEEF);
        for &b in &addrs {
            for &len in &lens {
                con.memcpy(a, b, len);
                con.memset(a, 0x5A, len);
                con.cstore(a, b, len);
                con.reload(a, b, len);
            }
        }
    }
    // peek4/poke4 round-trip at a safe user-data address.
    for &bits in &[0u32, 0x0001_8000, 0xFFFF_FFFF, 0x7FFF_FFFF] {
        con.poke4(0x4300, bits);
        let _ = con.peek4(0x4300);
    }
    // dget/dset across and beyond the 0..63 range.
    for idx in [-1, 0, 63, 64, i32::MAX, i32::MIN] {
        con.dset(idx, 1.5);
        let _ = con.dget(idx);
    }
    // Reaching here without a panic is the assertion.
}

#[test]
fn test_print_custom_glyph() {
    // `\^:` + 16 hex digits defines & draws an 8x8 glyph. A solid glyph
    // (all bits set) fills 64 pixels; a following char must render at +8px
    // (proving the 16 parameter bytes were consumed exactly).
    let mut v = alloc::vec![6u8, b':'];
    v.extend([b'f'; 16]);
    v.push(b'x');
    let mut c = Console::new();
    c.print(
        &alloc::string::String::from_utf8(v).unwrap(),
        Some(0),
        Some(0),
        Some(7),
    );
    let glyph = (0..8)
        .flat_map(|y| (0..8).map(move |x| (y, x)))
        .filter(|&(y, x)| c.screen[y * SCREEN_W + x] == 7)
        .count();
    assert_eq!(glyph, 64, "solid custom glyph should fill its 8x8 cell");
    let trailing = (0..6)
        .flat_map(|y| (8..12).map(move |x| (y, x)))
        .filter(|&(y, x)| c.screen[y * SCREEN_W + x] != 0)
        .count();
    assert!(
        trailing > 0,
        "trailing 'x' must render at x=8 after the glyph"
    );

    // An empty custom glyph draws nothing.
    let mut v2 = alloc::vec![6u8, b':'];
    v2.extend([b'0'; 16]);
    let mut c2 = Console::new();
    c2.print(
        &alloc::string::String::from_utf8(v2).unwrap(),
        Some(0),
        Some(0),
        Some(7),
    );
    assert_eq!(lit_count(&c2), 0, "empty custom glyph should draw nothing");
}

#[test]
fn test_map_skips_tile_zero() {
    // PICO-8's map() never draws cells whose tile index is 0 (the "empty"
    // cell). Regression test for the bottom-band checkerboard bug where
    // empty map cells painted sprite 0 over the screen.
    let mut con = Console::new();

    // Make sprite 0 fully color 8 (so any draw of it is detectable) and
    // sprite 1 fully color 12.
    for y in 0..8 {
        for x in 0..8 {
            con.sset(x, y, 8); // sprite 0 region
            con.sset(8 + x, y, 12); // sprite 1 region
        }
    }
    // Color 0 is transparent by default; make colors 8 and 12 opaque draws
    // visible by leaving palt at defaults (only index 0 transparent).

    // Map: cell (0,0) = tile 0 (empty), cell (1,0) = tile 1.
    con.mset(0, 0, 0);
    con.mset(1, 0, 1);

    con.map_draw(0, 0, 0, 0, 2, 1, 0);

    // The left 8x8 cell (tile 0) must be untouched (still black/0).
    for y in 0..8 {
        for x in 0..8 {
            assert_eq!(
                con.screen[y * 128 + x],
                0,
                "tile-0 cell drew at ({x},{y}); map() should skip tile 0"
            );
        }
    }
    // The right 8x8 cell (tile 1) must be drawn as color 12.
    for y in 0..8 {
        for x in 8..16 {
            assert_eq!(
                con.screen[y * 128 + x],
                12,
                "tile-1 cell missing at ({x},{y})"
            );
        }
    }
}

#[test]
fn test_peek_poke_draw_state() {
    let mut con = Console::new();

    // Draw palette (bit 4 = transparency; color 0 is transparent by default)
    assert_eq!(con.peek(0x5F00), 0x10);
    con.draw_pal[5] = 10;
    assert_eq!(con.peek(0x5F05), 10);
    con.poke(0x5F05, 7);
    assert_eq!(con.draw_pal[5], 7);
    con.poke(0x5F05, 0x17);
    assert!(con.transparent[5], "bit 4 of a draw-pal poke sets palt");
    assert_eq!(con.draw_pal[5], 7);
    assert_eq!(con.peek(0x5F05), 0x17);

    // Screen palette
    con.screen_pal[3] = 14;
    assert_eq!(con.peek(0x5F13), 14);
    con.poke(0x5F13, 2);
    assert_eq!(con.screen_pal[3], 2);

    // Clip is stored as edges x0,y0,x1,y1 at 0x5F20-0x5F23.
    con.clip_rect(10, 20, 30, 40, false);
    assert_eq!(
        [
            con.peek(0x5F20),
            con.peek(0x5F21),
            con.peek(0x5F22),
            con.peek(0x5F23)
        ],
        [10, 20, 40, 60]
    );
    con.poke(0x5F22, 100); // x end
    assert_eq!((con.clip_x, con.clip_w), (10, 90));
    con.poke(0x5F20, 120); // x begin past the end: normalizes to empty
    assert_eq!((con.clip_x, con.clip_w), (120, 0));
    con.poke(0x5F22, 200); // clamps to the screen edge
    assert_eq!((con.clip_x, con.clip_w), (120, 8));

    // 0x5F24 is an unused byte in PICO-8, NOT the pen color.
    con.draw_color = 6;
    con.poke(0x5F24, 8);
    assert_eq!(con.draw_color, 6);
    assert_eq!(con.peek(0x5F24), 0);

    // Pen color is at 0x5F25 (full byte incl. fillp secondary nibble).
    con.poke(0x5F25, 0x38);
    assert_eq!(con.draw_color, 0x38);
    assert_eq!(con.peek(0x5F25), 0x38);
    con.color(9);
    assert_eq!(con.peek(0x5F25), 9);

    // Cursor at 0x5F26/0x5F27.
    con.cursor(12, 34);
    assert_eq!((con.peek(0x5F26), con.peek(0x5F27)), (12, 34));
    con.poke(0x5F27, 50);
    assert_eq!(con.cursor_y, 50);

    // Camera is a little-endian i16 pair; negative values sign-extend.
    con.camera_x = 0;
    con.poke(0x5F28, 50);
    assert_eq!(con.camera_x, 50);
    con.camera(-3, -300);
    assert_eq!(con.peek2(0x5F28), 0xFFFD);
    assert_eq!(con.peek2(0x5F2A), (-300i16) as u16);
    con.poke2(0x5F2A, 0xFF00);
    assert_eq!(con.camera_y, -256);

    // Fill pattern LE at 0x5F31-32, transparency bit at 0x5F33.
    con.fillp(0x5A5A as f64 + 0.5);
    assert_eq!((con.peek(0x5F31), con.peek(0x5F32)), (0x5A, 0x5A));
    assert_eq!(con.peek(0x5F33), 1);
    con.poke(0x5F31, 0xF0);
    con.poke(0x5F33, 0);
    assert_eq!(con.fill_pattern, 0x5AF0);
    assert!(!con.fill_pattern_transparency);

    // tline map registers at 0x5F38-0x5F3B round-trip.
    con.poke(0x5F38, 16);
    con.poke(0x5F3B, 4);
    assert_eq!((con.tline_map_w, con.tline_map_y), (16, 4));
    assert_eq!((con.peek(0x5F38), con.peek(0x5F3B)), (16, 4));
}

#[test]
fn test_camera_registers_at_0x5f28() {
    // PICO-8's real draw-state hardware registers live at 0x5F00-0x5F3F.
    // Carts read the camera back from 0x5F28 (camera_x) / 0x5F2A (camera_y)
    // as little-endian 16-bit values — e.g. the SMB cart scrolls its level
    // this way. Regression test: these reads must reflect camera().
    let mut con = Console::new();
    con.camera(0x1234, 0x0567);

    // Little-endian read-back, the way carts reconstruct it.
    let cx = ((con.peek(0x5f29) as i32) << 8) | con.peek(0x5f28) as i32;
    let cy = ((con.peek(0x5f2b) as i32) << 8) | con.peek(0x5f2a) as i32;
    assert_eq!(cx, 0x1234, "camera_x must read back from 0x5F28/0x5F29");
    assert_eq!(cy, 0x0567, "camera_y must read back from 0x5F2A/0x5F2B");

    // Poking the registers must move the camera too.
    con.poke(0x5f28, 0x78);
    con.poke(0x5f29, 0x56);
    assert_eq!(con.camera_x, 0x5678, "poking 0x5F28/0x5F29 moves camera_x");

    // The user-data region just below (0x4300-0x5EFF) is unaffected.
    con.poke(0x5eff, 0xAB);
    assert_eq!(con.peek(0x5eff), 0xAB);
}

// -----------------------------------------------------------------------
// fillp tests
// -----------------------------------------------------------------------

#[test]
fn test_fillp_pattern_bits_affect_drawing() {
    // PICO-8 reads the pattern MSB-first: bit 15 = top-left (0,0), bit 0 =
    // bottom-right (3,3). (z8lua: `bitPlace = 15 - ((x&3) + 4*(y&3))`.)
    let mut con = Console::new();

    // Only bit 15 set -> affects pixel (0,0), nothing else.
    con.fillp(0b1000_0000_0000_0000 as f64);
    con.raw_pset(0, 0, 7);
    assert_eq!(
        con.screen[0], 0,
        "(0,0) maps to bit 15 and should be patterned"
    );
    con.raw_pset(1, 0, 7);
    assert_eq!(
        con.screen[1], 7,
        "(1,0) is bit 14 (unset) -> drawn normally"
    );

    // Only bit 0 set -> affects pixel (3,3), not (0,0).
    let mut con = Console::new();
    con.fillp(0b0000_0000_0000_0001 as f64);
    con.raw_pset(0, 0, 7);
    assert_eq!(
        con.screen[0], 7,
        "(0,0) is bit 15 (unset) -> drawn normally"
    );
    con.raw_pset(3, 3, 7);
    assert_eq!(
        con.screen[3 * SCREEN_W + 3],
        0,
        "(3,3) maps to bit 0 and should be patterned"
    );
}

#[test]
fn test_fillp_asymmetric_pattern_orientation() {
    // An asymmetric pattern must render in the correct orientation (this is
    // what the MSB-first bit order fixes — previously it was mirrored).
    // Pattern 0x000F = the bottom row (y=3) of the 4x4 grid set, top rows
    // clear. Bits 0..3 (-> y=3, x=3,2,1,0) set; bits 4..15 clear.
    let mut con = Console::new();
    con.fillp(0x000F as f64);
    // rectfill the 4x4 cell at the origin in color 7.
    con.rectfill(0, 0, 3, 3, Some(7));
    // Rows y=0,1,2 are unpatterned -> color 7. Row y=3 is patterned -> 0.
    for x in 0..4 {
        assert_eq!(con.screen[x], 7, "top row (y=0) should be unpatterned");
        assert_eq!(
            con.screen[3 * SCREEN_W + x],
            0,
            "bottom row (y=3) should be patterned to 0"
        );
    }
    con.fillp(0.0);
}

#[test]
fn test_pal_reset_also_resets_transparency() {
    // PICO-8 `pal()` (no args) resets the palette AND transparency (it
    // calls palt()): a cart that set palt(c,true) then pal() expects color
    // c to be opaque again.
    let mut con = Console::new();
    con.palt(8, true);
    con.palt(0, false); // make color 0 opaque (non-default)
    con.pal_reset();
    // Defaults restored: only color 0 transparent.
    assert!(
        con.transparent[0],
        "color 0 should be transparent after pal()"
    );
    assert!(
        !con.transparent[8],
        "color 8 transparency should be cleared"
    );
}

#[test]
fn test_circ_matches_pico8_shape() {
    // The circle outline must match PICO-8's exact midpoint algorithm at
    // radii where the naive `<= 0` decision diverged (1, 4, 6). r=1 is a
    // 4-pixel diamond (NOT an 8-pixel blob with corners).
    let mut con = Console::new();
    con.circ(64, 64, 1, Some(7));
    // The 4 diamond points are lit...
    for &(dx, dy) in &[(1, 0), (-1, 0), (0, 1), (0, -1)] {
        let (x, y) = ((64 + dx) as usize, (64 + dy) as usize);
        assert_eq!(con.screen[y * SCREEN_W + x], 7, "diamond point ({dx},{dy})");
    }
    // ...and the 4 corners are NOT (this was the bug).
    for &(dx, dy) in &[(1, 1), (-1, 1), (1, -1), (-1, -1)] {
        let (x, y) = ((64 + dx) as usize, (64 + dy) as usize);
        assert_eq!(
            con.screen[y * SCREEN_W + x],
            0,
            "corner ({dx},{dy}) must NOT be lit for r=1"
        );
    }

    // r=6: PICO-8 does NOT light the (±5, ±4)/(±4, ±5) points (our old
    // bulged version did). Check one representative point stays clear.
    let mut con = Console::new();
    con.circ(64, 64, 6, Some(7));
    let (x, y) = ((64 + 5) as usize, (64 + 4) as usize);
    assert_eq!(
        con.screen[y * SCREEN_W + x],
        0,
        "r=6 outline must not bulge to (5,4)"
    );
}

#[test]
fn test_circfill_radius_one_shape() {
    // PICO-8 r=1 circfill is a plus/diamond: (0,-1), (-1,0),(0,0),(1,0),
    // (0,1) = 5 pixels — not a 3x3 block.
    let mut con = Console::new();
    con.circfill(64, 64, 1, Some(7));
    let lit = con.screen.iter().filter(|&&p| p == 7).count();
    assert_eq!(lit, 5, "r=1 circfill should be a 5-pixel plus");
    // The corners of the 3x3 box must be clear.
    for &(dx, dy) in &[(1, 1), (-1, 1), (1, -1), (-1, -1)] {
        let (x, y) = ((64 + dx) as usize, (64 + dy) as usize);
        assert_eq!(con.screen[y * SCREEN_W + x], 0, "r=1 fill corner clear");
    }
}

#[test]
fn test_sspr_fixed_point_source_stepping() {
    // PICO-8 steps the source in 16.16 fixed point: scaling an 8px-wide
    // source to 12px dest selects source columns [0,0,1,1,2,3,3,4,5,5,6,7]
    // (NOT the per-pixel `x*8/12` = [0,0,1,2,2,3,4,4,5,6,6,7]). Verify by
    // making each source column a distinct color and reading the result.
    let mut con = Console::new();
    // Paint sprite-sheet row 0, columns 0..8 with colors 1..8 (col c -> c+1)
    // so the destination color reveals which source column was sampled.
    for x in 0..8 {
        con.sset(x, 0, (x + 1) as u8);
    }
    // Stretch source (0,0,8,1) to dest (0,0,12,1).
    con.sspr(0, 0, 8, 1, 0, 0, 12, 1, false, false);
    let expected_src = [0u8, 0, 1, 1, 2, 3, 3, 4, 5, 5, 6, 7];
    for (dx, &src_col) in expected_src.iter().enumerate() {
        let got = con.screen[dx]; // row 0
        assert_eq!(
            got,
            src_col + 1,
            "dest x={dx} should sample source col {src_col} (color {})",
            src_col + 1
        );
    }
}

#[test]
fn test_oval_matches_pico8_shape() {
    // An 8x8 oval outline must match PICO-8's Bresenham ellipse exactly:
    // 24 lit pixels, cardinal points lit, center clear. (Our old scanline
    // version over-filled to ~31 pixels.)
    let mut con = Console::new();
    con.oval(0, 0, 8, 8, Some(7));
    let lit = con.screen[..SCREEN_W * 9]
        .iter()
        .filter(|&&p| p == 7)
        .count();
    assert_eq!(lit, 24, "8x8 oval should be 24 outline pixels");
    for &(x, y) in &[(4usize, 0usize), (0, 4), (4, 8)] {
        assert_eq!(
            con.screen[y * SCREEN_W + x],
            7,
            "cardinal point ({x},{y}) lit"
        );
    }
    assert_eq!(con.screen[4 * SCREEN_W + 4], 0, "oval center must be empty");
}

#[test]
fn test_ovalfill_matches_pico8_shape() {
    // ovalfill of an 8x8 box must fill the interior (center lit), exceed
    // the outline pixel count, and stay within the bounding box.
    let mut con = Console::new();
    con.ovalfill(0, 0, 8, 8, Some(7));
    assert_eq!(
        con.screen[4 * SCREEN_W + 4],
        7,
        "ovalfill center should be lit"
    );
    let lit = con.screen[..SCREEN_W * 9]
        .iter()
        .filter(|&&p| p == 7)
        .count();
    assert!(
        lit > 24,
        "ovalfill should fill more than the outline ({lit})"
    );
    // Corner (0,0) is outside the ellipse and must be clear.
    assert_eq!(con.screen[0], 0, "ovalfill must not paint the box corner");
}

#[test]
fn test_pal_secondary_screen_palette() {
    // pal(c0,c1,1) sets the SCREEN (display) palette, applied at output.
    let mut con = Console::new();
    con.pal(7, 8, 1);
    assert_eq!(
        con.screen_pal[7], 8,
        "pal(7,8,1) should remap screen color 7->8"
    );
    // The draw palette is untouched by the p=1 form.
    assert_eq!(con.draw_pal[7], 7, "draw palette unchanged by p=1");
}

#[test]
fn test_pal_returns_previous_and_reset_one() {
    let mut con = Console::new();
    // Identity at first: mapping 7 returns 7.
    assert_eq!(con.pal(7, 8, 0), 7);
    // After 7->8, the prior mapping is 8.
    assert_eq!(con.pal(7, 9, 0), 8);
    // pal_reset_one(0) restores the draw palette to identity.
    con.pal_reset_one(0);
    assert_eq!(con.draw_pal[7], 7);
    // Screen palette reset is independent.
    con.pal(3, 5, 1);
    con.pal_reset_one(1);
    assert_eq!(con.screen_pal[3], 3);
}

#[test]
fn test_fillp_two_color_form() {
    // PICO-8 two-color fillp: the color byte's low nibble is the primary
    // (drawn where the pattern bit is 0), the high nibble the secondary
    // (drawn where the bit is 1). color(0x21) -> primary 1, secondary 2.
    let mut con = Console::new();
    // Pattern 0x000F = bottom row (y=3) set. Fill a 4x4 cell with 0x21.
    con.fillp(0x000F as f64);
    con.rectfill(0, 0, 3, 3, Some(0x21));
    for x in 0..4 {
        // Top rows: primary color 1.
        assert_eq!(con.screen[x], 1, "unpatterned pixel -> primary color 1");
        // Bottom row: secondary color 2 (NOT 0).
        assert_eq!(
            con.screen[3 * SCREEN_W + x],
            2,
            "patterned pixel -> secondary color 2"
        );
    }
    con.fillp(0.0);

    // Sprites/lines keep the single-color path (secondary defaults to 0):
    // a patterned line pixel is color 0, not a secondary nibble.
    let mut con = Console::new();
    con.fillp(0x000F as f64);
    con.line(0, 3, 3, 3, Some(0x21)); // bottom row, all patterned
    for x in 0..4 {
        assert_eq!(
            con.screen[3 * SCREEN_W + x],
            0,
            "line uses single-color path: patterned pixel -> 0"
        );
    }
    con.fillp(0.0);
}

#[test]
fn test_fillp_transparency_mode() {
    let mut con = Console::new();
    // pattern = 1 (bit 0 -> bottom-right pixel (3,3)), transparency on.
    con.fillp(1.0 + 0.5);
    // Pre-fill the (3,3) pixel with color 5.
    let idx = 3 * SCREEN_W + 3;
    con.screen[idx] = 5;
    // Draw at (3,3) — pattern bit set + transparency means pixel is skipped.
    con.raw_pset(3, 3, 7);
    assert_eq!(
        con.screen[idx], 5,
        "Patterned pixel (3,3) should be unchanged due to transparency"
    );
}

#[test]
fn test_fillp_zero_clears_pattern() {
    let mut con = Console::new();
    // Set a pattern
    con.fillp(0xFFFF as f64);
    assert_eq!(con.fill_pattern, 0xFFFF);
    // Clear it
    con.fillp(0.0);
    assert_eq!(con.fill_pattern, 0, "fillp(0) should clear the pattern");
    assert!(
        !con.fill_pattern_transparency,
        "fillp(0) should clear transparency"
    );

    // Verify drawing works normally again
    con.raw_pset(0, 0, 7);
    assert_eq!(
        con.screen[0], 7,
        "Drawing should work normally after fillp(0)"
    );
}

#[test]
fn test_reset_draw_state_clears_fill_pattern() {
    let mut con = Console::new();
    con.fillp(0xAAAA as f64 + 0.5);
    assert_ne!(con.fill_pattern, 0);
    assert!(con.fill_pattern_transparency);

    con.reset_draw_state();
    assert_eq!(
        con.fill_pattern, 0,
        "reset_draw_state should clear fill pattern"
    );
    assert!(
        !con.fill_pattern_transparency,
        "reset_draw_state should clear fill transparency"
    );
}

// -----------------------------------------------------------------------
// tline tests
// -----------------------------------------------------------------------

#[test]
fn test_tline_samples_map_not_sprite_sheet() {
    // PICO-8 `tline` is a textured-MAP line: (mx,my) integer part indexes
    // the map for a tile, fractional part selects the sub-pixel. Set map
    // cell (0,0) -> sprite 1, fill sprite 1 with color 5, and confirm the
    // line samples it. (The OLD impl wrongly sampled the sprite sheet at
    // (mx*8,my*8) ignoring the map — this test guards against regressing.)
    let mut con = Console::new();
    con.mset(0, 0, 1); // map tile (0,0) = sprite 1
                       // Sprite 1 occupies sheet pixels x=8..16, y=0..8.
    for y in 0..8 {
        for x in 8..16 {
            con.sprites[y * 128 + x] = 5;
        }
    }
    // Horizontal line (0,0)..(7,0), map coords starting (0,0), step 1/8 x:
    // each dest pixel advances mx by 1/8, staying within map tile (0,0).
    con.tline(0, 0, 7, 0, 0.0, 0.0, 1.0 / 8.0, 0.0);
    for x in 0..8 {
        assert_eq!(
            con.screen[x], 5,
            "tline pixel ({x},0) should sample map tile 1's sprite (color 5)"
        );
    }

    // A map cell of 0 (empty) draws nothing.
    let mut con = Console::new();
    // tile (0,0) defaults to 0; sprite sheet has data but must be ignored.
    con.sprites[0] = 7;
    con.tline(0, 0, 7, 0, 0.0, 0.0, 1.0 / 8.0, 0.0);
    assert_eq!(con.screen[0], 0, "tline over empty map tile draws nothing");
}

#[test]
fn test_tline_single_pixel() {
    let mut con = Console::new();
    // map tile (0,0) = sprite 2; fill sprite 2's pixel (0,0) with color 3.
    con.mset(0, 0, 2);
    con.sprites[2 * 8] = 3; // sprite 2 starts at sheet x=16, y=0
    con.tline(10, 10, 10, 10, 0.0, 0.0, 0.0, 0.0);
    assert_eq!(
        con.screen[10 * 128 + 10],
        3,
        "single-pixel tline should sample map tile 2's sprite (color 3)"
    );
}

// -----------------------------------------------------------------------
// clip() tests
// -----------------------------------------------------------------------

#[test]
fn test_clip_clamps_edges_not_origin_and_width() {
    let mut con = Console::new();
    // Negative origin with width 60: PICO-8 clamps the edge x+w=40 to the
    // screen, so the visible region is [0,40), NOT [0,60).
    con.clip_rect(-20, 0, 60, 128, false);
    assert_eq!(con.clip_x, 0);
    assert_eq!(con.clip_w, 40, "visible width should be x+w clamped to 0");
    // Oversized: origin 100, width 50 → edge 150 clamps to 128 → width 28.
    con.clip_rect(100, 0, 50, 128, false);
    assert_eq!(con.clip_x, 100);
    assert_eq!(con.clip_w, 28);
}

#[test]
fn test_clip_returns_previous_rect() {
    let mut con = Console::new();
    // Default clip is the full screen.
    let prev = con.clip_rect(10, 20, 30, 40, false);
    assert_eq!(prev, (0, 0, SCREEN_W as i32, SCREEN_H as i32));
    // The next call returns the rect we just set.
    let prev2 = con.clip_rect(0, 0, 128, 128, false);
    assert_eq!(prev2, (10, 20, 30, 40));
}

#[test]
fn test_clip_previous_intersects() {
    let mut con = Console::new();
    con.clip_rect(20, 20, 40, 40, false); // region [20,60) x [20,60)
                                          // Intersect with [0,40) x [0,40): result is [20,40) x [20,40).
    con.clip_rect(0, 0, 40, 40, true);
    assert_eq!(con.clip_x, 20);
    assert_eq!(con.clip_y, 20);
    assert_eq!(con.clip_w, 20);
    assert_eq!(con.clip_h, 20);
}

// -----------------------------------------------------------------------
// draw-state setters return their previous value
// -----------------------------------------------------------------------

#[test]
fn test_camera_returns_previous_offset() {
    let mut con = Console::new();
    let prev = con.camera(10, 20);
    assert_eq!(prev, (0, 0));
    let prev2 = con.camera(30, 40);
    assert_eq!(prev2, (10, 20));
}

#[test]
fn test_color_returns_previous_pen() {
    let mut con = Console::new();
    // Default pen is 6.
    let prev = con.color(9);
    assert_eq!(prev, 6);
    let prev2 = con.color(3);
    assert_eq!(prev2, 9);
}

#[test]
fn test_cursor_returns_previous_position() {
    let mut con = Console::new();
    let prev = con.cursor(5, 7);
    assert_eq!(prev, (0, 0));
    let prev2 = con.cursor(11, 13);
    assert_eq!(prev2, (5, 7));
}

#[test]
fn test_fillp_returns_previous_pattern() {
    let mut con = Console::new();
    // Default pattern is 0.
    let prev = con.fillp(0x5a5a as f64);
    assert_eq!(prev, 0.0);
    // The next call returns the pattern we just set (no transparency bit).
    let prev2 = con.fillp(0.0);
    assert_eq!(prev2, 0x5a5a as f64);
    // With transparency: integer part + 0.5.
    con.fillp(0x1234 as f64 + 0.5);
    let prev3 = con.fillp(0.0);
    assert_eq!(prev3, 0x1234 as f64 + 0.5);
}

// -----------------------------------------------------------------------
// stateful line() form tests
// -----------------------------------------------------------------------

#[test]
fn test_line_to_continues_from_last_endpoint() {
    let mut con = Console::new();
    // Full segment establishes the last endpoint at (10, 0).
    con.line(0, 0, 10, 0, Some(7));
    // line_to should draw from (10,0) to (10,5).
    con.line_to(10, 5, Some(8));
    assert_eq!(con.line_end, Some((10, 5)));
    // The continued segment should be present.
    assert_eq!(con.screen[5 * SCREEN_W + 10], 8);
    assert_eq!(con.screen[2 * SCREEN_W + 10], 8);
}

#[test]
fn test_line_to_without_prior_endpoint_draws_nothing() {
    let mut con = Console::new();
    // No prior line(): line_end is None, so line_to draws nothing (PICO-8
    // only updates the pen color in this case).
    con.line_to(20, 20, Some(9));
    assert_eq!(con.screen[20 * SCREEN_W + 20], 0);
    assert_eq!(con.line_end, None);
    assert_eq!(con.draw_color, 9, "pen color should still update");
}

#[test]
fn test_line_reset_clears_endpoint() {
    let mut con = Console::new();
    con.line(0, 0, 10, 10, Some(7));
    assert!(con.line_end.is_some());
    con.line_reset();
    assert_eq!(con.line_end, None);
    // After reset, line_to has no anchor and draws nothing.
    con.line_to(30, 30, Some(11));
    assert_eq!(con.screen[30 * SCREEN_W + 30], 0);
}

// -----------------------------------------------------------------------
// dget / dset tests
// -----------------------------------------------------------------------

#[test]
fn test_dget_default_is_zero() {
    let con = Console::new();
    for i in 0..64 {
        assert_eq!(con.dget(i), 0.0, "dget({}) should default to 0.0", i);
    }
}

#[test]
fn test_dset_and_dget() {
    let mut con = Console::new();
    con.dset(0, 42.5);
    con.dset(63, -100.0);
    assert_eq!(con.dget(0), 42.5);
    assert_eq!(con.dget(63), -100.0);
    // Other indices remain 0
    assert_eq!(con.dget(1), 0.0);
    assert_eq!(con.dget(32), 0.0);
}

#[test]
fn test_dget_out_of_bounds_returns_zero() {
    let con = Console::new();
    assert_eq!(con.dget(-1), 0.0, "dget(-1) should return 0.0");
    assert_eq!(con.dget(64), 0.0, "dget(64) should return 0.0");
    assert_eq!(con.dget(100), 0.0, "dget(100) should return 0.0");
}

#[test]
fn test_dset_out_of_bounds_is_noop() {
    let mut con = Console::new();
    con.dset(-1, 999.0); // should not panic
    con.dset(64, 999.0); // should not panic
                         // Verify no data was corrupted
    for i in 0..64 {
        assert_eq!(con.dget(i), 0.0);
    }
}

// -----------------------------------------------------------------------
// btn / btnp with player parameter tests
// -----------------------------------------------------------------------

#[test]
fn test_btn_player0_returns_player1_state() {
    let mut con = Console::new();
    con.btn_state[0] = true; // player 1, button 0 (left)
    assert!(
        con.btn(0, 0),
        "btn(0, 0) should return player 1 left button state"
    );
    assert!(!con.btn(1, 0), "btn(1, 0) should be false when not pressed");
}

#[test]
fn test_btn_player1_returns_player2_state() {
    let mut con = Console::new();
    con.btn_state[6] = true; // player 2, button 0 (left) => index 6
    assert!(
        con.btn(0, 1),
        "btn(0, 1) should return player 2 left button (index 6)"
    );
    assert!(!con.btn(0, 0), "btn(0, 0) should be false for player 1");
}

#[test]
fn test_btn_player_isolation() {
    let mut con = Console::new();
    // Set player 1 button 3 (down)
    con.btn_state[3] = true;
    // Set player 2 button 3 (down) => index 9
    con.btn_state[9] = true;
    assert!(con.btn(3, 0), "player 1 down should be pressed");
    assert!(con.btn(3, 1), "player 2 down should be pressed");
    // Player 1 button 4 is not pressed
    assert!(!con.btn(4, 0));
    // Player 2 button 4 is not pressed
    assert!(!con.btn(4, 1));
}

#[test]
fn test_btnp_player0_vs_player1() {
    let mut con = Console::new();
    // Frame 1: player 1 button 4 (O) and player 2 button 4 (O) just pressed.
    let mut state = [false; 12];
    state[4] = true; // player 1, button 4
    state[10] = true; // player 2, button 4 (index 6+4=10)
    con.update_input(state);
    assert!(
        con.btnp(4, 0),
        "btnp(4, 0) should detect player 1 just-pressed"
    );
    assert!(
        con.btnp(4, 1),
        "btnp(4, 1) should detect player 2 just-pressed"
    );

    // Frame 2: player 1 still holds, player 2 newly presses (was released).
    let mut state2 = [false; 12];
    state2[4] = true; // player 1 still held
    con.update_input(state2);
    assert!(
        !con.btnp(4, 0),
        "btnp(4, 0) should be false on the 2nd held frame (before repeat)"
    );
}

#[test]
fn test_btnp_auto_repeat_timing() {
    // PICO-8: btnp fires on frame 1, then frame 15, then every 4 frames
    // (19, 23, ...) while the button stays held.
    let mut con = Console::new();
    let held = {
        let mut s = [false; 12];
        s[0] = true; // hold left
        s
    };
    let mut fired = alloc::vec::Vec::new();
    for frame in 1..=27 {
        con.update_input(held);
        if con.btnp(0, 0) {
            fired.push(frame);
        }
    }
    assert_eq!(
        fired,
        alloc::vec![1, 15, 19, 23, 27],
        "btnp repeat should fire at 1, 15, then every 4 frames"
    );
    // Releasing resets the counter so the next press fires immediately.
    con.update_input([false; 12]);
    con.update_input(held);
    assert!(con.btnp(0, 0), "btnp should fire again after release+press");
}

#[test]
fn test_btnp_repeat_registers() {
    let mut con = Console::new();
    let held = {
        let mut s = [false; 12];
        s[0] = true;
        s
    };
    // Custom delay 8, interval 2 via 0x5F5C/0x5F5D.
    con.poke(0x5F5C, 8);
    con.poke(0x5F5D, 2);
    assert_eq!(con.peek(0x5F5C), 8);
    assert_eq!(con.peek(0x5F5D), 2);
    let mut fired = alloc::vec::Vec::new();
    for frame in 1..=14 {
        con.update_input(held);
        if con.btnp(0, 0) {
            fired.push(frame);
        }
    }
    // Fire on 1, then delay 8, then every 2: 8, 10, 12, 14.
    assert_eq!(fired, alloc::vec![1, 8, 10, 12, 14]);

    // Delay 255 disables repeat: only the initial press fires.
    let mut con2 = Console::new();
    con2.poke(0x5F5C, 255);
    let mut fired2 = alloc::vec::Vec::new();
    for frame in 1..=40 {
        con2.update_input(held);
        if con2.btnp(0, 0) {
            fired2.push(frame);
        }
    }
    assert_eq!(fired2, alloc::vec![1], "delay 255 disables btnp repeat");
}

#[test]
fn test_btn_out_of_bounds_player_returns_false() {
    let mut con = Console::new();
    // Set all buttons to true
    for i in 0..12 {
        con.btn_state[i] = true;
    }
    // Player 2+ should return false (player * 6 + i >= 12)
    assert!(
        !con.btn(0, 2),
        "btn(0, 2) should return false for out-of-bounds player"
    );
    assert!(
        !con.btn(0, 255),
        "btn(0, 255) should return false for out-of-bounds player"
    );
    assert!(
        !con.btn(5, 2),
        "btn(5, 2) should return false for out-of-bounds player"
    );
}

#[test]
fn test_btnp_out_of_bounds_player_returns_false() {
    let mut con = Console::new();
    for i in 0..12 {
        con.btn_state[i] = true;
    }
    assert!(
        !con.btnp(0, 2),
        "btnp(0, 2) should return false for out-of-bounds player"
    );
    assert!(
        !con.btnp(3, 3),
        "btnp(3, 3) should return false for out-of-bounds player"
    );
}

#[test]
fn test_btn_default_player0_behavior() {
    let mut con = Console::new();
    con.btn_state[2] = true; // player 1, button 2 (up)
                             // Calling with player=0 should behave the same as the old single-arg behavior
    assert!(
        con.btn(2, 0),
        "btn(2, 0) should return true for player 1 up"
    );
    assert!(
        !con.btn(2, 1),
        "btn(2, 1) should return false for player 2 up"
    );
}

// -----------------------------------------------------------------------
// oval / ovalfill tests
// -----------------------------------------------------------------------

#[test]
fn test_oval_draws_outline() {
    let mut con = Console::new();
    // Draw an ellipse in bounding box (10,10) to (20,16)
    // Center = (15, 13), semi-axes = (5, 3)
    con.oval(10, 10, 20, 16, Some(7));
    // Top center and bottom center of the ellipse should be drawn
    assert_eq!(
        con.screen[10 * SCREEN_W + 15],
        7,
        "Top center of oval should be drawn"
    );
    assert_eq!(
        con.screen[16 * SCREEN_W + 15],
        7,
        "Bottom center of oval should be drawn"
    );
    // Left and right extremes at the center row
    assert_eq!(
        con.screen[13 * SCREEN_W + 10],
        7,
        "Left extreme of oval should be drawn"
    );
    assert_eq!(
        con.screen[13 * SCREEN_W + 20],
        7,
        "Right extreme of oval should be drawn"
    );
    // The center of the ellipse should NOT be drawn (it's just an outline)
    assert_ne!(
        con.screen[13 * SCREEN_W + 15],
        7,
        "Center of oval should not be drawn (outline only)"
    );
}

#[test]
fn test_ovalfill_fills_interior() {
    let mut con = Console::new();
    // Draw a filled ellipse in bounding box (10,10) to (20,16)
    con.ovalfill(10, 10, 20, 16, Some(5));
    // Center should be filled
    assert_eq!(
        con.screen[13 * SCREEN_W + 15],
        5,
        "Center of filled oval should be drawn"
    );
    // Top center should be filled
    assert_eq!(
        con.screen[10 * SCREEN_W + 15],
        5,
        "Top center of filled oval should be drawn"
    );
    // Bottom center should be filled
    assert_eq!(
        con.screen[16 * SCREEN_W + 15],
        5,
        "Bottom center of filled oval should be drawn"
    );
    // A pixel clearly outside the ellipse should not be filled
    assert_ne!(
        con.screen[10 * SCREEN_W + 10],
        5,
        "Corner of bounding box should not be filled"
    );
}

#[test]
fn test_oval_single_pixel() {
    let mut con = Console::new();
    // Bounding box of size 0x0 => single pixel
    con.oval(50, 50, 50, 50, Some(8));
    assert_eq!(
        con.screen[50 * SCREEN_W + 50],
        8,
        "Degenerate oval should draw single pixel"
    );
}

#[test]
fn test_ovalfill_single_pixel() {
    let mut con = Console::new();
    con.ovalfill(50, 50, 50, 50, Some(8));
    assert_eq!(
        con.screen[50 * SCREEN_W + 50],
        8,
        "Degenerate ovalfill should draw single pixel"
    );
}

// -----------------------------------------------------------------------
// peek2 / peek4 tests
// -----------------------------------------------------------------------

#[test]
fn test_peek2_reads_little_endian() {
    let mut con = Console::new();
    // Write two individual bytes to user data
    con.poke(0x4300, 0x34); // low byte
    con.poke(0x4301, 0x12); // high byte
    let val = con.peek2(0x4300);
    assert_eq!(val, 0x1234, "peek2 should read little-endian u16");
}

#[test]
fn test_peek4_reads_little_endian() {
    let mut con = Console::new();
    con.poke(0x4300, 0x78);
    con.poke(0x4301, 0x56);
    con.poke(0x4302, 0x34);
    con.poke(0x4303, 0x12);
    let val = con.peek4(0x4300);
    assert_eq!(val, 0x12345678, "peek4 should read little-endian u32");
}

// -----------------------------------------------------------------------
// poke2 / poke4 tests
// -----------------------------------------------------------------------

#[test]
fn test_poke2_writes_little_endian() {
    let mut con = Console::new();
    con.poke2(0x4300, 0xABCD);
    assert_eq!(con.peek(0x4300), 0xCD, "poke2 low byte");
    assert_eq!(con.peek(0x4301), 0xAB, "poke2 high byte");
}

#[test]
fn test_poke4_writes_little_endian() {
    let mut con = Console::new();
    con.poke4(0x4300, 0xDEADBEEF);
    assert_eq!(con.peek(0x4300), 0xEF, "poke4 byte 0");
    assert_eq!(con.peek(0x4301), 0xBE, "poke4 byte 1");
    assert_eq!(con.peek(0x4302), 0xAD, "poke4 byte 2");
    assert_eq!(con.peek(0x4303), 0xDE, "poke4 byte 3");
}

#[test]
fn test_poke2_peek2_round_trip() {
    let mut con = Console::new();
    con.poke2(0x4300, 0xFFEE);
    assert_eq!(con.peek2(0x4300), 0xFFEE, "poke2/peek2 round trip");
}

#[test]
fn test_poke4_peek4_round_trip() {
    let mut con = Console::new();
    con.poke4(0x4300, 0xCAFEBABE);
    assert_eq!(con.peek4(0x4300), 0xCAFEBABE, "poke4/peek4 round trip");
}

// -----------------------------------------------------------------------
// cursor tests
// -----------------------------------------------------------------------

#[test]
fn test_cursor_sets_position() {
    let mut con = Console::new();
    assert_eq!(con.cursor_x, 0);
    assert_eq!(con.cursor_y, 0);
    con.cursor(42, 99);
    assert_eq!(con.cursor_x, 42, "cursor should set cursor_x");
    assert_eq!(con.cursor_y, 99, "cursor should set cursor_y");
}

#[test]
fn test_cursor_negative_values() {
    let mut con = Console::new();
    con.cursor(-10, -20);
    assert_eq!(con.cursor_x, -10);
    assert_eq!(con.cursor_y, -20);
}

// -----------------------------------------------------------------------
// cstore / reload tests
// -----------------------------------------------------------------------

#[test]
fn test_cstore_and_reload() {
    let mut con = Console::new();
    // Write some data to sprite flags (live memory at 0x3000)
    con.poke(0x3000, 0xAA);
    con.poke(0x3001, 0xBB);
    con.poke(0x3002, 0xCC);

    // cstore: copy from live RAM (0x3000) to cart ROM (offset 0x3000)
    con.cstore(0x3000, 0x3000, 3);

    // Verify cart_rom was written
    assert_eq!(con.cart_rom[0x3000], 0xAA);
    assert_eq!(con.cart_rom[0x3001], 0xBB);
    assert_eq!(con.cart_rom[0x3002], 0xCC);

    // Now modify live memory
    con.poke(0x3000, 0x00);
    con.poke(0x3001, 0x00);
    con.poke(0x3002, 0x00);
    assert_eq!(con.peek(0x3000), 0x00, "Live memory should be zeroed");

    // reload: copy from cart ROM (offset 0x3000) back to live RAM (0x3000)
    con.reload(0x3000, 0x3000, 3);

    // Verify live memory was restored
    assert_eq!(con.peek(0x3000), 0xAA, "reload should restore byte 0");
    assert_eq!(con.peek(0x3001), 0xBB, "reload should restore byte 1");
    assert_eq!(con.peek(0x3002), 0xCC, "reload should restore byte 2");
    assert_eq!(con.flags[0], 0xAA);
}

#[test]
fn test_snapshot_cart_rom_and_reload_restore_sprites_map_flags() {
    let mut con = Console::new();
    // "Load a cart": paint distinct data into every ROM-backed array,
    // including the shared lower-map / upper-sheet region.
    for y in 0..SHEET_H as i32 {
        for x in 0..SHEET_W as i32 {
            con.sset(x, y, ((x + y) & 0xF) as u8);
        }
    }
    for y in 0..32 {
        for x in 0..MAP_W as i32 {
            con.mset(x, y, ((x * 3 + y) & 0xFF) as u8);
        }
    }
    for (i, f) in con.flags.iter_mut().enumerate() {
        *f = i as u8;
    }
    con.snapshot_cart_rom();

    let sprites = con.sprites;
    let map = con.map;
    let flags = con.flags;

    // Trash live RAM (via the API and directly).
    con.cls(None);
    con.sprites.fill(0);
    con.map.fill(0);
    con.flags.fill(0);
    con.memset(0x0000, 0, 0x4300);

    // Full reload() (the 0-arg form copies the whole 0x4300 ROM).
    con.reload(0, 0, 0x4300);
    assert_eq!(con.sprites, sprites, "sprite sheet restored");
    assert_eq!(con.map, map, "map (incl. rows 32-63 via 0x1000) restored");
    assert_eq!(con.flags, flags, "flags restored");

    // Partial reload of just the flags leaves the rest zeroed.
    con.sprites.fill(0);
    con.flags.fill(0);
    con.reload(0x3000, 0x3000, 256);
    assert_eq!(con.flags, flags);
    assert!(con.sprites.iter().all(|&p| p == 0));
}

#[test]
fn test_memcpy_overlap_forward_and_backward() {
    // Overlapping memcpy must behave like memmove in both directions, in
    // every kind of region (byte arrays, pixel arrays, generic path).
    let starts: [u16; 5] = [0x4300, 0x2000, 0x3000, 0x0000, 0x6000];
    for &base in &starts {
        let mut con = Console::new();
        for i in 0..16u16 {
            con.poke(base + i, (i as u8) * 0x11);
        }
        // dest above src (backward copy needed): [0..12) -> [4..16)
        con.memcpy(base + 4, base, 12);
        for i in 0..12u16 {
            assert_eq!(
                con.peek(base + 4 + i),
                (i as u8) * 0x11,
                "dest>src overlap at {base:#x}+{i}"
            );
        }
        // dest below src (forward copy): [4..16) -> [0..12)
        let mut con = Console::new();
        for i in 0..16u16 {
            con.poke(base + i, (i as u8) * 0x11);
        }
        con.memcpy(base, base + 4, 12);
        for i in 0..12u16 {
            assert_eq!(
                con.peek(base + i),
                ((i + 4) as u8) * 0x11,
                "dest<src overlap at {base:#x}+{i}"
            );
        }
    }

    // Generic path: a range straddling two regions overlapping itself.
    let mut con = Console::new();
    for i in 0..8u16 {
        con.poke(0x2FFC + i, 0x10 + i as u8); // map tail + flags head
    }
    con.memcpy(0x2FFE, 0x2FFC, 8);
    for i in 0..8u16 {
        assert_eq!(con.peek(0x2FFE + i), 0x10 + i as u8);
    }
    let mut con = Console::new();
    for i in 0..8u16 {
        con.poke(0x2FFC + i, 0x10 + i as u8);
    }
    con.memcpy(0x2FFA, 0x2FFC, 8);
    for i in 0..8u16 {
        assert_eq!(con.peek(0x2FFA + i), 0x10 + i as u8);
    }
}

#[test]
fn test_memcpy_sheet_to_screen_and_back_pixel_packing() {
    let mut con = Console::new();
    for y in 0..SHEET_H as i32 {
        for x in 0..SHEET_W as i32 {
            con.sset(x, y, ((x * 7 + y * 3) & 0xF) as u8);
        }
    }
    // memcpy(0x6000, 0x0, 0x2000): whole sheet -> screen.
    con.memcpy(0x6000, 0x0000, 0x2000);
    assert_eq!(con.screen, con.sprites);
    // Partial, unaligned copy screen -> user data -> sheet round-trips
    // through the packed representation.
    con.memcpy(0x4300, 0x6003, 37);
    for i in 0..37u16 {
        assert_eq!(con.peek(0x4300 + i), con.peek(0x6003 + i));
    }
    con.sprites.fill(0);
    con.map.fill(0);
    con.memcpy(0x1001, 0x4300, 37);
    for i in 0..37u16 {
        assert_eq!(con.peek(0x1001 + i), con.peek(0x6003 + i));
        // The shared map rows follow the sheet write.
        assert_eq!(con.map[0x1001 + i as usize], con.peek(0x1001 + i));
    }
    // memset over the shared sheet region packs the value and syncs map.
    con.memset(0x1800, 0x3A, 0x100);
    assert_eq!(con.sget(0, 96), 0xA);
    assert_eq!(con.sget(1, 96), 0x3);
    assert_eq!(con.mget(0, 48), 0x3A);
    assert_eq!(con.mget(127, 49), 0x3A);
    assert_eq!(con.mget(0, 50), 0);
}

#[test]
fn test_cls_resets_cursor_and_clip() {
    let mut con = Console::new();
    con.cursor(40, 50);
    con.print("abc", None, None, Some(7));
    assert_eq!((con.cursor_x, con.cursor_y), (40, 56));
    con.clip_rect(10, 10, 20, 20, false);
    con.cls(Some(3));
    assert_eq!((con.cursor_x, con.cursor_y), (0, 0), "cls homes the cursor");
    assert_eq!(
        (con.clip_x, con.clip_y, con.clip_w, con.clip_h),
        (0, 0, 128, 128),
        "cls resets the clip rect"
    );
    assert!(con.screen.iter().all(|&p| p == 3));
    // Subsequent print starts at the top-left.
    con.print("x", None, None, Some(7));
    assert_eq!(con.cursor_y, font::CHAR_H);
}

/// Reference map renderer: the old per-cell `spr` path.
#[allow(clippy::too_many_arguments)]
fn map_via_spr(
    con: &mut Console,
    cel_x: i32,
    cel_y: i32,
    sx: i32,
    sy: i32,
    w: i32,
    h: i32,
    layer: u8,
) {
    for cy in 0..h {
        for cx in 0..w {
            let tile = con.mget(cel_x + cx, cel_y + cy);
            if tile == 0 || (layer != 0 && con.flags[tile as usize] & layer == 0) {
                continue;
            }
            con.spr(
                tile as i32,
                sx + cx * 8,
                sy + cy * 8,
                1.0,
                1.0,
                false,
                false,
            );
        }
    }
}

fn textured_console() -> Console {
    let mut con = Console::new();
    // Every sprite gets a distinct, non-uniform texture with some
    // transparent (0) pixels.
    for y in 0..SHEET_H as i32 {
        for x in 0..SHEET_W as i32 {
            con.sset(x, y, ((x * 5 + y * 3 + (x / 8) + (y / 8) * 16) & 0xF) as u8);
        }
    }
    for y in 0..MAP_H as i32 {
        for x in 0..MAP_W as i32 {
            con.mset(x, y, ((x * 13 + y * 7) & 0xFF) as u8);
        }
    }
    for (i, f) in con.flags.iter_mut().enumerate() {
        *f = (i * 37) as u8;
    }
    con.pal(3, 9, 0);
    con.palt(5, true);
    con
}

#[test]
#[allow(clippy::type_complexity)]
fn test_map_culling_matches_spr_loop() {
    // (cel_x, cel_y, sx, sy, w, h, layer, camera, clip)
    let cases: [(
        i32,
        i32,
        i32,
        i32,
        i32,
        i32,
        u8,
        (i32, i32),
        (i32, i32, i32, i32),
    ); 8] = [
        (0, 0, 0, 0, 16, 16, 0, (0, 0), (0, 0, 128, 128)),
        (3, 5, -13, 7, 20, 10, 0, (0, 0), (0, 0, 128, 128)),
        (0, 0, 0, 0, 128, 64, 0, (300, 100), (0, 0, 128, 128)),
        (0, 0, 0, 0, 128, 64, 2, (37, 21), (10, 20, 50, 60)),
        (120, 60, 0, 0, 16, 8, 0, (0, 0), (0, 0, 128, 128)),
        (-5, -3, 0, 0, 16, 16, 1, (-20, -10), (0, 0, 128, 128)),
        (0, 0, 1000, 1000, 16, 16, 0, (0, 0), (0, 0, 128, 128)),
        (2, 2, 5, 3, 200, 200, 4, (-9, 5), (60, 0, 68, 128)),
    ];
    for (i, &(cx, cy, sx, sy, w, h, layer, cam, clip)) in cases.iter().enumerate() {
        let mut expected = textured_console();
        expected.camera(cam.0, cam.1);
        expected.clip_rect(clip.0, clip.1, clip.2, clip.3, false);
        map_via_spr(&mut expected, cx, cy, sx, sy, w, h, layer);

        let mut got = textured_console();
        got.camera(cam.0, cam.1);
        got.clip_rect(clip.0, clip.1, clip.2, clip.3, false);
        got.map_draw(cx, cy, sx, sy, w, h, layer);

        assert!(
            got.screen == expected.screen,
            "map_draw output differs from the spr loop in case {i}"
        );
        assert!(lit_count(&got) > 0 || i == 6, "case {i} drew nothing");
    }
}

#[test]
fn test_sspr_respects_clip_rect() {
    // The clip-intersected destination range must draw exactly what a
    // per-pixel clip test would.
    let mut con = textured_console();
    con.clip_rect(20, 30, 40, 25, false);
    con.sspr(8, 8, 32, 32, 10, 20, 64, 64, true, false);
    for y in 0..128usize {
        for x in 0..128usize {
            let inside = (20..60).contains(&x) && (30..55).contains(&y);
            if !inside {
                assert_eq!(
                    con.screen[y * 128 + x],
                    0,
                    "pixel outside clip drawn at ({x},{y})"
                );
            }
        }
    }
    assert!(lit_count(&con) > 100);
    // And the on-screen culling still holds for a huge destination.
    con.cls(None);
    con.sspr(0, 0, 16, 16, -5000, -5000, 20000, 20000, false, true);
    assert!(lit_count(&con) > 0);
}

#[test]
fn test_hline_pattern_matches_per_pixel_fill() {
    // rectfill's span filler must match pset_fill pixel-for-pixel under a
    // pattern, in both transparency modes, at every row/column phase.
    for transparent in [false, true] {
        for phase in 0..4 {
            let mut a = Console::new();
            a.fillp(0x5A3C as f64 + if transparent { 0.5 } else { 0.0 });
            a.clip_rect(3, 2, 100, 90, false);
            a.rectfill(phase, phase + 1, 90 + phase, 70, Some(0x7E));

            let mut b = Console::new();
            b.fillp(0x5A3C as f64 + if transparent { 0.5 } else { 0.0 });
            b.clip_rect(3, 2, 100, 90, false);
            let (c0, c1) = b.resolve_fill_colors(Some(0x7E));
            for y in (phase + 1)..=70 {
                for x in phase..=(90 + phase) {
                    b.pset_fill(x, y, c0, c1);
                }
            }
            assert!(
                a.screen == b.screen,
                "transparent={transparent} phase={phase}"
            );
        }
    }
}

#[test]
fn test_rect_edges_clip_and_pattern() {
    let mut con = Console::new();
    con.clip_rect(10, 10, 20, 20, false);
    con.rect(0, 0, 127, 127, Some(7));
    // Fully outside the clip on all four edges: nothing drawn.
    assert_eq!(lit_count(&con), 0);
    con.rect(5, 15, 40, 40, Some(7));
    // Top edge row 15 within clip x range [10,30)
    for x in 10..30 {
        assert_eq!(con.screen[15 * 128 + x], 7);
    }
    assert_eq!(con.screen[15 * 128 + 9], 0);
    assert_eq!(con.screen[15 * 128 + 30], 0);
    // Left edge x=5 is outside; right edge x=40 outside; bottom row 40 outside.
    assert_eq!(lit_count(&con), 20);
    // Degenerate 1x1 rect draws one pixel.
    con.cls(None);
    con.rect(12, 12, 12, 12, Some(8));
    assert_eq!(lit_count(&con), 1);
    assert_eq!(con.screen[12 * 128 + 12], 8);
}

#[test]
fn test_console_new_is_const() {
    static CON: Console = Console::new();
    assert_eq!(CON.draw_color, 6);
    assert!(CON.transparent[0]);
    assert!(!CON.transparent[1]);
    assert_eq!(CON.clip_w, 128);
}

#[test]
fn test_tline_fixed_point_matches_reference_sampling() {
    // Tile 1 = a distinct 8x8 gradient; map row 0 = tile 1 everywhere.
    let mut con = Console::new();
    for y in 0..8 {
        for x in 0..8 {
            con.sset(8 + x, y, (1 + ((x + y * 8) % 15)) as u8);
        }
    }
    for x in 0..MAP_W as i32 {
        con.mset(x, 0, 1);
    }
    // Column 0 too, so a vertical walk through tile rows 0-3 stays on tile 1.
    for y in 0..MAP_H as i32 {
        con.mset(0, y, 1);
    }
    // Horizontal line across 64 px, texture advancing 1/8 tile per px:
    // pixel i samples sheet x = 8 + (i & 7), sheet y = 0.
    con.tline(0, 10, 63, 10, 0.0, 0.0, 0.125, 0.0);
    for i in 0..64 {
        let expect = con.sget(8 + (i & 7), 0);
        assert_eq!(con.screen[10 * 128 + i as usize], expect, "px {i}");
    }
    // Vertical line, my steps 1/8 per px: samples sheet y = i & 7.
    con.cls(None);
    con.tline(20, 0, 20, 31, 0.5, 0.0, 0.0, 0.125);
    for i in 0..32 {
        let expect = con.sget(12, i & 7);
        assert_eq!(con.screen[i as usize * 128 + 20], expect, "py {i}");
    }
    // Map x wraps at 256 tiles by default: mx = 256 -> tile column 0.
    con.cls(None);
    con.tline(0, 5, 7, 5, 256.0, 0.0, 0.125, 0.0);
    assert_eq!(con.screen[5 * 128], con.sget(8, 0));
    // With the width register set to 1 the x coordinate wraps every tile.
    con.poke(0x5F38, 1);
    con.mset(1, 0, 0); // tile column 1 is now empty...
    con.cls(None);
    con.tline(0, 5, 15, 5, 0.0, 0.0, 0.125, 0.0);
    // ...but the line wraps back into column 0 instead of hitting it.
    assert_eq!(con.screen[5 * 128 + 8], con.sget(8, 0));
}

#[test]
fn test_cstore_reload_different_addresses() {
    let mut con = Console::new();
    // Write to user data at 0x4300
    con.poke(0x4300, 0x42);

    // cstore from live 0x4300 to ROM offset 0x0000
    con.cstore(0x0000, 0x4300, 1);
    assert_eq!(con.cart_rom[0x0000], 0x42);

    // Clear live memory
    con.poke(0x4300, 0x00);

    // reload from ROM offset 0x0000 to live 0x4300
    con.reload(0x4300, 0x0000, 1);
    assert_eq!(
        con.peek(0x4300),
        0x42,
        "reload with different src/dest should work"
    );
}

#[test]
fn test_cstore_out_of_bounds_rom_is_safe() {
    let mut con = Console::new();
    // Try to write beyond cart_rom size; should not panic
    con.poke(0x4300, 0xFF);
    con.cstore(0x42FF, 0x4300, 5); // starts at last valid, goes past
                                   // Only the first byte should have been written
    assert_eq!(con.cart_rom[0x42FF], 0xFF);
}

#[test]
fn test_reload_out_of_bounds_rom_is_safe() {
    let mut con = Console::new();
    con.cart_rom[0x42FF] = 0xEE;
    // Try to read beyond cart_rom size; should not panic
    con.reload(0x4300, 0x42FF, 5);
    assert_eq!(
        con.peek(0x4300),
        0xEE,
        "First byte within ROM should be copied"
    );
}
