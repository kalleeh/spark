use alloc::string::String;
use alloc::vec::Vec;

use crate::audio::{Sfx, MusicPattern, Note, NUM_SFX, NUM_MUSIC, NOTES_PER_SFX};
use crate::console::{SPRITE_SHEET_SIZE, MAP_SIZE};

/// All data parsed from a .p8 cart file.
pub struct CartData {
    pub code: String,
    pub sprites: [u8; SPRITE_SHEET_SIZE], // 128*128 pixel color indices (0-15)
    pub map: [u8; MAP_SIZE],              // 128*64 tile indices (0-255)
    pub flags: [u8; 256],                 // 256 sprite flags
    pub sfx: [Sfx; NUM_SFX],             // 64 sound effects
    pub music: [MusicPattern; NUM_MUSIC], // 64 music patterns
}

impl CartData {
    pub fn new() -> Self {
        CartData {
            code: String::new(),
            sprites: [0u8; SPRITE_SHEET_SIZE],
            map: [0u8; MAP_SIZE],
            flags: [0u8; 256],
            sfx: [Sfx::default(); NUM_SFX],
            music: [MusicPattern::default(); NUM_MUSIC],
        }
    }
}

// ---------------------------------------------------------------------------
// Parsing
// ---------------------------------------------------------------------------

/// Parse a .p8 cart from its source text (no file I/O).
pub fn parse_cart(source: &str) -> CartData {
    let mut cart = CartData::new();

    let mut current_section = "";
    let mut section_lines: Vec<&str> = Vec::new();

    for line in source.lines() {
        if line.starts_with("__") && line.ends_with("__") {
            process_section(current_section, &section_lines, &mut cart);
            current_section = line.trim();
            section_lines.clear();
        } else if !current_section.is_empty() {
            section_lines.push(line);
        }
    }
    // Flush the final section.
    process_section(current_section, &section_lines, &mut cart);

    cart
}

fn process_section(name: &str, lines: &[&str], cart: &mut CartData) {
    match name {
        "__lua__" => {
            cart.code = join_lines(lines);
        }
        "__gfx__" => {
            for (y, line) in lines.iter().enumerate() {
                if y >= 128 {
                    break;
                }
                for (x, ch) in line.chars().enumerate() {
                    if x >= 128 {
                        break;
                    }
                    cart.sprites[y * 128 + x] = hex_char_to_u8(ch);
                }
            }
        }
        "__gff__" => {
            let mut i = 0;
            for line in lines {
                let bytes = line.as_bytes();
                let mut j = 0;
                while j + 1 < bytes.len() && i < 256 {
                    let hi = hex_char_to_u8(bytes[j] as char);
                    let lo = hex_char_to_u8(bytes[j + 1] as char);
                    cart.flags[i] = (hi << 4) | lo;
                    i += 1;
                    j += 2;
                }
            }
        }
        "__map__" => {
            for (y, line) in lines.iter().enumerate() {
                if y >= 64 {
                    break;
                }
                let bytes = line.as_bytes();
                for x in 0..128 {
                    let idx = x * 2;
                    if idx + 1 < bytes.len() {
                        let hi = hex_char_to_u8(bytes[idx] as char);
                        let lo = hex_char_to_u8(bytes[idx + 1] as char);
                        cart.map[y * 128 + x] = (hi << 4) | lo;
                    }
                }
            }
        }
        "__sfx__" => {
            parse_sfx_section(lines, &mut cart.sfx);
        }
        "__music__" => {
            parse_music_section(lines, &mut cart.music);
        }
        _ => {}
    }
}

/// Join a slice of string slices with newline separators.
fn join_lines(lines: &[&str]) -> String {
    let mut s = String::new();
    for (i, line) in lines.iter().enumerate() {
        if i > 0 {
            s.push('\n');
        }
        s.push_str(line);
    }
    s
}

// ---------------------------------------------------------------------------
// Hex utilities
// ---------------------------------------------------------------------------

pub fn hex_char_to_u8(c: char) -> u8 {
    match c {
        '0'..='9' => c as u8 - b'0',
        'a'..='f' => c as u8 - b'a' + 10,
        'A'..='F' => c as u8 - b'A' + 10,
        _ => 0,
    }
}

pub fn u8_to_hex_char(v: u8) -> char {
    let v = v & 0x0f;
    if v <= 9 {
        (b'0' + v) as char
    } else {
        (b'a' + v - 10) as char
    }
}

// ---------------------------------------------------------------------------
// SFX parsing
// ---------------------------------------------------------------------------

/// Parse the `__sfx__` section lines into the sfx data array.
///
/// Each line is 168 hex characters representing one SFX:
///   Bytes 0-1  (2 hex chars): editor mode (ignored)
///   Bytes 2-3  (2 hex chars): speed
///   Bytes 4-5  (2 hex chars): loop start (note index 0-31)
///   Bytes 6-7  (2 hex chars): loop end   (note index 0-31)
///   Bytes 8-167 (160 hex chars): 32 notes, each encoded as 5 hex digits
///
/// Each note's 5 hex digits `c0 c1 c2 c3 c4` encode:
///   pitch    = c0 + (c2 & 1) * 16 + (c3 & 1) * 32   (0-63)
///   waveform = c1 & 7                                  (0-7)
///   volume   = c2 >> 1                                 (0-7)
///   effect   = c3 >> 1                                 (0-7)
fn parse_sfx_section(lines: &[&str], sfx: &mut [Sfx; NUM_SFX]) {
    for (i, line) in lines.iter().enumerate() {
        if i >= NUM_SFX {
            break;
        }
        parse_sfx_line(line, &mut sfx[i]);
    }
}

fn parse_sfx_line(line: &str, sfx: &mut Sfx) {
    let bytes = line.as_bytes();
    if bytes.len() < 8 {
        return;
    }

    // Bytes 2-3: speed
    let speed_hi = hex_char_to_u8(bytes[2] as char);
    let speed_lo = hex_char_to_u8(bytes[3] as char);
    sfx.speed = (speed_hi << 4) | speed_lo;

    // Bytes 4-5: loop start
    let ls_hi = hex_char_to_u8(bytes[4] as char);
    let ls_lo = hex_char_to_u8(bytes[5] as char);
    sfx.loop_start = (ls_hi << 4) | ls_lo;

    // Bytes 6-7: loop end
    let le_hi = hex_char_to_u8(bytes[6] as char);
    let le_lo = hex_char_to_u8(bytes[7] as char);
    sfx.loop_end = (le_hi << 4) | le_lo;

    // Notes: 32 notes, each 5 hex characters, starting at offset 8
    for n in 0..NOTES_PER_SFX {
        let base = 8 + n * 5;
        if base + 5 > bytes.len() {
            break;
        }
        let c0 = hex_char_to_u8(bytes[base] as char);
        let c1 = hex_char_to_u8(bytes[base + 1] as char);
        let c2 = hex_char_to_u8(bytes[base + 2] as char);
        let c3 = hex_char_to_u8(bytes[base + 3] as char);
        let _c4 = hex_char_to_u8(bytes[base + 4] as char);

        sfx.notes[n] = Note {
            pitch: c0 | ((c2 & 1) << 4) | ((c3 & 1) << 5),
            waveform: c1 & 0x07,
            volume: c2 >> 1,
            effect: c3 >> 1,
        };
    }
}

// ---------------------------------------------------------------------------
// Music parsing
// ---------------------------------------------------------------------------

/// Parse the `__music__` section lines into the music pattern array.
///
/// Each line has the format: `FF AABBCCDD` (11 characters with space)
///   FF       = flags (2 hex chars) — bit 0: loop start, bit 1: loop end, bit 2: stop
///   AA-DD    = channel 0-3 SFX index (2 hex chars each)
///              If bit 6 (0x40) is set the channel is disabled.
fn parse_music_section(lines: &[&str], music: &mut [MusicPattern; NUM_MUSIC]) {
    for (i, line) in lines.iter().enumerate() {
        if i >= NUM_MUSIC {
            break;
        }
        parse_music_line(line, &mut music[i]);
    }
}

fn parse_music_line(line: &str, pat: &mut MusicPattern) {
    // Collect hex characters into a fixed-size stack buffer (max 10 needed).
    let mut hex_buf = [0u8; 16];
    let mut len = 0;
    for b in line.bytes() {
        if !b.is_ascii_whitespace() {
            if len < hex_buf.len() {
                hex_buf[len] = b;
                len += 1;
            }
        }
    }

    if len < 10 {
        return;
    }

    let flags_hi = hex_char_to_u8(hex_buf[0] as char);
    let flags_lo = hex_char_to_u8(hex_buf[1] as char);
    pat.flags = (flags_hi << 4) | flags_lo;

    for ch in 0..4 {
        let base = 2 + ch * 2;
        let hi = hex_char_to_u8(hex_buf[base] as char);
        let lo = hex_char_to_u8(hex_buf[base + 1] as char);
        pat.channels[ch] = (hi << 4) | lo;
    }
}

// ---------------------------------------------------------------------------
// Serialization
// ---------------------------------------------------------------------------

/// Serialize a `CartData` back to .p8 text format.
///
/// The output is compatible with `parse_cart()` — a round-trip
/// `parse_cart(&serialize_cart(&cart))` preserves all data.
pub fn serialize_cart(cart: &CartData) -> String {
    // Pre-allocate a reasonable capacity (a typical .p8 is ~30-40 KB).
    let mut out = String::with_capacity(40_000);

    // Header
    out.push_str("pico-8 cartridge // http://www.pico-8.com\n");
    out.push_str("version 42\n");

    // __lua__
    out.push_str("__lua__\n");
    out.push_str(&cart.code);
    out.push('\n');

    // __gfx__ — 128 lines of 128 hex chars (one nibble per pixel)
    out.push_str("__gfx__\n");
    for y in 0..128 {
        for x in 0..128 {
            out.push(u8_to_hex_char(cart.sprites[y * 128 + x]));
        }
        out.push('\n');
    }

    // __gff__ — 256 sprite flags as 2 hex chars each, split into 2 lines of 256 chars
    out.push_str("__gff__\n");
    for half in 0..2 {
        let start = half * 128;
        for i in start..start + 128 {
            let byte = cart.flags[i];
            out.push(u8_to_hex_char(byte >> 4));
            out.push(u8_to_hex_char(byte & 0x0f));
        }
        out.push('\n');
    }

    // __map__ — 64 lines of 256 hex chars (2 hex chars per tile)
    out.push_str("__map__\n");
    for y in 0..64 {
        for x in 0..128 {
            let byte = cart.map[y * 128 + x];
            out.push(u8_to_hex_char(byte >> 4));
            out.push(u8_to_hex_char(byte & 0x0f));
        }
        out.push('\n');
    }

    // __sfx__ — 64 lines of 168 hex chars each
    out.push_str("__sfx__\n");
    for i in 0..NUM_SFX {
        serialize_sfx_line(&cart.sfx[i], &mut out);
        out.push('\n');
    }

    // __music__ — 64 lines of "FF AABBCCDD" format
    out.push_str("__music__\n");
    for i in 0..NUM_MUSIC {
        serialize_music_line(&cart.music[i], &mut out);
        out.push('\n');
    }

    out
}

/// Serialize a single SFX to 168 hex chars appended to `out`.
///
/// Format: 2 chars editor mode (always "00") + 2 chars speed + 2 chars loop_start
///         + 2 chars loop_end + 32 * 5 chars notes = 8 + 160 = 168.
///
/// Each note is encoded as `c0 c1 c2 c3 c4` where:
///   c0 = pitch & 0x0F
///   c1 = waveform & 0x07
///   c2 = (volume << 1) | ((pitch >> 4) & 1)
///   c3 = (effect << 1) | ((pitch >> 5) & 1)
///   c4 = 0
fn serialize_sfx_line(sfx: &Sfx, out: &mut String) {
    // Editor mode (always 00)
    out.push('0');
    out.push('0');
    // Speed
    out.push(u8_to_hex_char(sfx.speed >> 4));
    out.push(u8_to_hex_char(sfx.speed & 0x0f));
    // Loop start
    out.push(u8_to_hex_char(sfx.loop_start >> 4));
    out.push(u8_to_hex_char(sfx.loop_start & 0x0f));
    // Loop end
    out.push(u8_to_hex_char(sfx.loop_end >> 4));
    out.push(u8_to_hex_char(sfx.loop_end & 0x0f));

    // 32 notes
    for n in 0..NOTES_PER_SFX {
        let note = &sfx.notes[n];
        let c0 = note.pitch & 0x0f;
        let c1 = note.waveform & 0x07;
        let c2 = (note.volume << 1) | ((note.pitch >> 4) & 1);
        let c3 = (note.effect << 1) | ((note.pitch >> 5) & 1);
        let c4 = 0u8;
        out.push(u8_to_hex_char(c0));
        out.push(u8_to_hex_char(c1));
        out.push(u8_to_hex_char(c2));
        out.push(u8_to_hex_char(c3));
        out.push(u8_to_hex_char(c4));
    }
}

/// Serialize a single music pattern to "FF AABBCCDD" format appended to `out`.
fn serialize_music_line(pat: &MusicPattern, out: &mut String) {
    // Flags
    out.push(u8_to_hex_char(pat.flags >> 4));
    out.push(u8_to_hex_char(pat.flags & 0x0f));
    // Space separator
    out.push(' ');
    // 4 channel bytes
    for ch in 0..4 {
        let byte = pat.channels[ch];
        out.push(u8_to_hex_char(byte >> 4));
        out.push(u8_to_hex_char(byte & 0x0f));
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_sfx_line_basic() {
        // Header: editor=00, speed=0x10(16), loop_start=02, loop_end=08
        // First note: pitch=24, waveform=3(square), volume=5, effect=2(vibrato)
        //   pitch=24: c0=8, bit4=1 => c2 bit0=1, bit5=0 => c3 bit0=0
        //   waveform=3: c1=3
        //   volume=5: c2=(5<<1)|1=0xB
        //   effect=2: c3=(2<<1)|0=4
        //   => "83b40"
        let mut line = String::from("00100208");
        line.push_str("83b40");
        for _ in 1..32 {
            line.push_str("00000");
        }
        assert_eq!(line.len(), 168);

        let mut sfx = Sfx::default();
        parse_sfx_line(&line, &mut sfx);

        assert_eq!(sfx.speed, 0x10);
        assert_eq!(sfx.loop_start, 0x02);
        assert_eq!(sfx.loop_end, 0x08);
        assert_eq!(sfx.notes[0].pitch, 24);
        assert_eq!(sfx.notes[0].waveform, 3);
        assert_eq!(sfx.notes[0].volume, 5);
        assert_eq!(sfx.notes[0].effect, 2);

        for i in 1..32 {
            assert_eq!(sfx.notes[i].pitch, 0, "note {} pitch", i);
            assert_eq!(sfx.notes[i].volume, 0, "note {} volume", i);
        }
    }

    #[test]
    fn test_parse_sfx_line_all_zeros() {
        let line = "0".repeat(168);
        let mut sfx = Sfx::default();
        sfx.speed = 99;
        parse_sfx_line(&line, &mut sfx);
        assert_eq!(sfx.speed, 0);
        assert_eq!(sfx.loop_start, 0);
        assert_eq!(sfx.loop_end, 0);
    }

    #[test]
    fn test_parse_sfx_line_max_pitch() {
        // pitch=63: c0=0xF, bit4=1=>c2 bit0=1, bit5=1=>c3 bit0=1
        // waveform=7: c1=7, volume=7: c2=(7<<1)|1=0xF, effect=7: c3=(7<<1)|1=0xF
        let mut line = String::from("00100000");
        line.push_str("f7ff0");
        for _ in 1..32 {
            line.push_str("00000");
        }
        let mut sfx = Sfx::default();
        parse_sfx_line(&line, &mut sfx);
        assert_eq!(sfx.notes[0].pitch, 63);
        assert_eq!(sfx.notes[0].waveform, 7);
        assert_eq!(sfx.notes[0].volume, 7);
        assert_eq!(sfx.notes[0].effect, 7);
    }

    #[test]
    fn test_parse_sfx_line_empty() {
        let mut sfx = Sfx::default();
        parse_sfx_line("", &mut sfx);
        assert_eq!(sfx.speed, 16); // default unchanged
    }

    #[test]
    fn test_parse_sfx_line_header_only() {
        let mut sfx = Sfx::default();
        parse_sfx_line("00200410", &mut sfx);
        assert_eq!(sfx.speed, 0x20);
        assert_eq!(sfx.loop_start, 0x04);
        assert_eq!(sfx.loop_end, 0x10);
    }

    #[test]
    fn test_parse_sfx_note_encoding_pitches() {
        let cases = [
            ("00000", 0u8),
            ("10000", 1),
            ("f0000", 15),
            ("00100", 16),  // c2 bit0=1
            ("00010", 32),  // c3 bit0=1
            ("00110", 48),  // c2+c3 bit0=1
            ("f0110", 63),
        ];

        for (hex_note, exp_pitch) in cases.iter() {
            let mut line = String::from("00100000");
            line.push_str(hex_note);
            for _ in 1..32 {
                line.push_str("00000");
            }
            let mut sfx = Sfx::default();
            parse_sfx_line(&line, &mut sfx);
            assert_eq!(sfx.notes[0].pitch, *exp_pitch, "hex={}", hex_note);
        }
    }

    #[test]
    fn test_parse_sfx_waveform_values() {
        for w in 0..8u8 {
            let c1 = char::from(if w < 10 { b'0' + w } else { b'a' + w - 10 });
            let mut line = String::from("00100000");
            line.push('0');
            line.push(c1);
            line.push_str("000");
            for _ in 1..32 {
                line.push_str("00000");
            }
            let mut sfx = Sfx::default();
            parse_sfx_line(&line, &mut sfx);
            assert_eq!(sfx.notes[0].waveform, w, "waveform {}", w);
        }
    }

    #[test]
    fn test_parse_music_line_basic() {
        let mut pat = MusicPattern::default();
        parse_music_line("01 01024344", &mut pat);
        assert_eq!(pat.flags, 0x01);
        assert_eq!(pat.channels[0], 0x01);
        assert_eq!(pat.channels[1], 0x02);
        assert_eq!(pat.channels[2], 0x43);
        assert_eq!(pat.channels[3], 0x44);
        assert!(pat.is_loop_start());
        assert!(!pat.is_loop_end());
        assert!(pat.channel_enabled(0));
        assert!(pat.channel_enabled(1));
        assert!(!pat.channel_enabled(2));
        assert!(!pat.channel_enabled(3));
    }

    #[test]
    fn test_parse_music_line_flags() {
        let mut pat = MusicPattern::default();
        parse_music_line("02 00010203", &mut pat);
        assert!(pat.is_loop_end());
        assert!(!pat.is_loop_start());

        let mut pat2 = MusicPattern::default();
        parse_music_line("04 00010203", &mut pat2);
        assert!(pat2.is_stop());
    }

    #[test]
    fn test_parse_music_line_empty() {
        let mut pat = MusicPattern::default();
        parse_music_line("", &mut pat);
        assert_eq!(pat.flags, 0);
        for ch in 0..4 {
            assert_eq!(pat.channels[ch], 0x40);
        }
    }

    #[test]
    fn test_parse_music_line_no_space() {
        let mut pat = MusicPattern::default();
        parse_music_line("0100010203", &mut pat);
        assert_eq!(pat.flags, 0x01);
        assert_eq!(pat.channels[0], 0x00);
        assert_eq!(pat.channels[3], 0x03);
    }

    #[test]
    fn test_parse_cart_with_sfx_and_music() {
        let mut source = String::new();
        source.push_str("pico-8 cartridge\n");
        source.push_str("version 0\n");
        source.push_str("__lua__\n");
        source.push_str("-- test\n");
        source.push_str("__gfx__\n");
        source.push_str("__sfx__\n");

        // SFX 0: speed=16, first note pitch=24, wave=1, vol=5, eff=0
        let mut sfx0 = String::from("00100000");
        sfx0.push_str("81b00");
        for _ in 1..32 {
            sfx0.push_str("00000");
        }
        source.push_str(&sfx0);
        source.push('\n');

        // SFX 1: all zeros
        source.push_str(&"0".repeat(168));
        source.push('\n');

        source.push_str("__music__\n");
        source.push_str("01 00014243\n");

        let cart = parse_cart(&source);
        assert_eq!(cart.sfx[0].speed, 16);
        assert_eq!(cart.sfx[0].notes[0].pitch, 24);
        assert_eq!(cart.sfx[0].notes[0].waveform, 1);
        assert_eq!(cart.sfx[0].notes[0].volume, 5);
        assert_eq!(cart.sfx[1].speed, 0);

        assert_eq!(cart.music[0].flags, 0x01);
        assert!(cart.music[0].is_loop_start());
        assert!(cart.music[0].channel_enabled(0));
        assert!(!cart.music[0].channel_enabled(2));
    }

    #[test]
    fn test_parse_sfx_section_overflow() {
        let zero_line = "0".repeat(168);
        let lines: Vec<&str> = (0..100).map(|_| zero_line.as_str()).collect();
        let mut sfx = [Sfx::default(); NUM_SFX];
        parse_sfx_section(&lines, &mut sfx);
        assert_eq!(sfx[63].speed, 0);
    }

    #[test]
    fn test_parse_sfx_line_malformed_chars() {
        let mut line = String::from("ZZXXXXXX");
        for _ in 0..32 {
            line.push_str("ZZZZZ");
        }
        let mut sfx = Sfx::default();
        parse_sfx_line(&line, &mut sfx);
        assert_eq!(sfx.speed, 0);
        assert_eq!(sfx.notes[0].pitch, 0);
    }

    // -----------------------------------------------------------------------
    // Serialization tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_serialize_empty_cart_produces_valid_p8() {
        let cart = CartData::new();
        let text = serialize_cart(&cart);

        // Header
        assert!(text.starts_with("pico-8 cartridge // http://www.pico-8.com\n"));
        assert!(text.contains("version 42\n"));

        // All required sections present
        assert!(text.contains("__lua__\n"));
        assert!(text.contains("__gfx__\n"));
        assert!(text.contains("__gff__\n"));
        assert!(text.contains("__map__\n"));
        assert!(text.contains("__sfx__\n"));
        assert!(text.contains("__music__\n"));
    }

    #[test]
    fn test_serialize_empty_cart_roundtrip() {
        let cart = CartData::new();
        let text = serialize_cart(&cart);
        let cart2 = parse_cart(&text);

        assert_eq!(cart2.code, "");
        assert_eq!(cart2.sprites, [0u8; SPRITE_SHEET_SIZE]);
        assert_eq!(cart2.map, [0u8; MAP_SIZE]);
        assert_eq!(cart2.flags, [0u8; 256]);
        for i in 0..NUM_SFX {
            // Default Sfx has speed=16, but serializing a new CartData
            // writes speed=16 which round-trips correctly.
            assert_eq!(cart2.sfx[i].speed, cart.sfx[i].speed, "sfx {} speed", i);
            assert_eq!(cart2.sfx[i].loop_start, 0, "sfx {} loop_start", i);
            assert_eq!(cart2.sfx[i].loop_end, 0, "sfx {} loop_end", i);
        }
        for i in 0..NUM_MUSIC {
            assert_eq!(cart2.music[i].flags, 0, "music {} flags", i);
            for ch in 0..4 {
                assert_eq!(
                    cart2.music[i].channels[ch], cart.music[i].channels[ch],
                    "music {} channel {}", i, ch
                );
            }
        }
    }

    #[test]
    fn test_serialize_code_roundtrip() {
        let mut cart = CartData::new();
        cart.code = String::from("-- hello world\nprint(\"hi\")\nfunction _update()\nend");
        let text = serialize_cart(&cart);
        let cart2 = parse_cart(&text);
        assert_eq!(cart2.code, cart.code);
    }

    #[test]
    fn test_serialize_sfx_note_all_pitches() {
        // Test that every pitch value 0-63 round-trips correctly.
        for pitch in 0..64u8 {
            let mut cart = CartData::new();
            cart.sfx[0].notes[0] = Note {
                pitch,
                waveform: 0,
                volume: 5,
                effect: 0,
            };
            let text = serialize_cart(&cart);
            let cart2 = parse_cart(&text);
            assert_eq!(
                cart2.sfx[0].notes[0].pitch, pitch,
                "pitch {} failed roundtrip", pitch
            );
        }
    }

    #[test]
    fn test_serialize_sfx_note_all_waveforms() {
        for waveform in 0..8u8 {
            let mut cart = CartData::new();
            cart.sfx[0].notes[0] = Note {
                pitch: 24,
                waveform,
                volume: 5,
                effect: 0,
            };
            let text = serialize_cart(&cart);
            let cart2 = parse_cart(&text);
            assert_eq!(
                cart2.sfx[0].notes[0].waveform, waveform,
                "waveform {} failed roundtrip", waveform
            );
        }
    }

    #[test]
    fn test_serialize_sfx_note_all_volumes() {
        for volume in 0..8u8 {
            let mut cart = CartData::new();
            cart.sfx[0].notes[0] = Note {
                pitch: 24,
                waveform: 3,
                volume,
                effect: 0,
            };
            let text = serialize_cart(&cart);
            let cart2 = parse_cart(&text);
            assert_eq!(
                cart2.sfx[0].notes[0].volume, volume,
                "volume {} failed roundtrip", volume
            );
        }
    }

    #[test]
    fn test_serialize_sfx_note_all_effects() {
        for effect in 0..8u8 {
            let mut cart = CartData::new();
            cart.sfx[0].notes[0] = Note {
                pitch: 24,
                waveform: 3,
                volume: 5,
                effect,
            };
            let text = serialize_cart(&cart);
            let cart2 = parse_cart(&text);
            assert_eq!(
                cart2.sfx[0].notes[0].effect, effect,
                "effect {} failed roundtrip", effect
            );
        }
    }

    #[test]
    fn test_serialize_sfx_note_max_values() {
        let mut cart = CartData::new();
        cart.sfx[0].speed = 0xFF;
        cart.sfx[0].loop_start = 31;
        cart.sfx[0].loop_end = 31;
        cart.sfx[0].notes[0] = Note {
            pitch: 63,
            waveform: 7,
            volume: 7,
            effect: 7,
        };

        let text = serialize_cart(&cart);
        let cart2 = parse_cart(&text);

        assert_eq!(cart2.sfx[0].speed, 0xFF);
        assert_eq!(cart2.sfx[0].loop_start, 31);
        assert_eq!(cart2.sfx[0].loop_end, 31);
        assert_eq!(cart2.sfx[0].notes[0].pitch, 63);
        assert_eq!(cart2.sfx[0].notes[0].waveform, 7);
        assert_eq!(cart2.sfx[0].notes[0].volume, 7);
        assert_eq!(cart2.sfx[0].notes[0].effect, 7);
    }

    #[test]
    fn test_serialize_sfx_multiple_notes() {
        let mut cart = CartData::new();
        // Fill all 32 notes of SFX 5 with distinct values.
        cart.sfx[5].speed = 8;
        cart.sfx[5].loop_start = 4;
        cart.sfx[5].loop_end = 28;
        for n in 0..NOTES_PER_SFX {
            cart.sfx[5].notes[n] = Note {
                pitch: (n as u8 * 2) & 63,
                waveform: (n as u8) & 7,
                volume: (n as u8) & 7,
                effect: (n as u8) & 7,
            };
        }

        let text = serialize_cart(&cart);
        let cart2 = parse_cart(&text);

        assert_eq!(cart2.sfx[5].speed, 8);
        assert_eq!(cart2.sfx[5].loop_start, 4);
        assert_eq!(cart2.sfx[5].loop_end, 28);
        for n in 0..NOTES_PER_SFX {
            assert_eq!(
                cart2.sfx[5].notes[n].pitch, cart.sfx[5].notes[n].pitch,
                "sfx5 note {} pitch", n
            );
            assert_eq!(
                cart2.sfx[5].notes[n].waveform, cart.sfx[5].notes[n].waveform,
                "sfx5 note {} waveform", n
            );
            assert_eq!(
                cart2.sfx[5].notes[n].volume, cart.sfx[5].notes[n].volume,
                "sfx5 note {} volume", n
            );
            assert_eq!(
                cart2.sfx[5].notes[n].effect, cart.sfx[5].notes[n].effect,
                "sfx5 note {} effect", n
            );
        }
    }

    #[test]
    fn test_serialize_sfx_line_length() {
        // Each serialized SFX line must be exactly 168 chars.
        let sfx = Sfx::default();
        let mut line = String::new();
        serialize_sfx_line(&sfx, &mut line);
        assert_eq!(line.len(), 168, "SFX line should be 168 chars, got {}", line.len());
    }

    #[test]
    fn test_serialize_music_roundtrip() {
        let mut cart = CartData::new();
        cart.music[0] = MusicPattern {
            flags: 0x01,
            channels: [0x00, 0x01, 0x42, 0x43],
        };
        cart.music[1] = MusicPattern {
            flags: 0x02,
            channels: [0x10, 0x11, 0x12, 0x13],
        };
        cart.music[2] = MusicPattern {
            flags: 0x04,
            channels: [0x3F, 0x40, 0x41, 0x7F],
        };

        let text = serialize_cart(&cart);
        let cart2 = parse_cart(&text);

        for i in 0..3 {
            assert_eq!(
                cart2.music[i].flags, cart.music[i].flags,
                "music {} flags", i
            );
            for ch in 0..4 {
                assert_eq!(
                    cart2.music[i].channels[ch], cart.music[i].channels[ch],
                    "music {} channel {}", i, ch
                );
            }
        }

        // Verify flag semantics
        assert!(cart2.music[0].is_loop_start());
        assert!(cart2.music[1].is_loop_end());
        assert!(cart2.music[2].is_stop());
    }

    #[test]
    fn test_serialize_music_line_format() {
        let pat = MusicPattern {
            flags: 0x01,
            channels: [0x00, 0x01, 0x42, 0x43],
        };
        let mut line = String::new();
        serialize_music_line(&pat, &mut line);
        assert_eq!(line, "01 00014243");
    }

    #[test]
    fn test_serialize_sprite_data_roundtrip() {
        let mut cart = CartData::new();
        // Set some pixels with various color indices (0-15).
        cart.sprites[0] = 1;
        cart.sprites[1] = 15;
        cart.sprites[127] = 8;
        cart.sprites[128] = 7;      // row 1, col 0
        cart.sprites[128 * 64] = 12; // row 64, col 0
        cart.sprites[128 * 127 + 127] = 5; // last pixel

        let text = serialize_cart(&cart);
        let cart2 = parse_cart(&text);

        assert_eq!(cart2.sprites[0], 1);
        assert_eq!(cart2.sprites[1], 15);
        assert_eq!(cart2.sprites[127], 8);
        assert_eq!(cart2.sprites[128], 7);
        assert_eq!(cart2.sprites[128 * 64], 12);
        assert_eq!(cart2.sprites[128 * 127 + 127], 5);

        // Verify all pixels match
        for i in 0..SPRITE_SHEET_SIZE {
            assert_eq!(
                cart2.sprites[i], cart.sprites[i],
                "sprite pixel {} mismatch", i
            );
        }
    }

    #[test]
    fn test_serialize_gfx_line_length() {
        let cart = CartData::new();
        let text = serialize_cart(&cart);
        // Find __gfx__ section and check line lengths.
        let gfx_start = text.find("__gfx__\n").unwrap() + "__gfx__\n".len();
        let gfx_end = text.find("__gff__").unwrap();
        let gfx_section = &text[gfx_start..gfx_end];
        let gfx_lines: Vec<&str> = gfx_section.lines().collect();
        assert_eq!(gfx_lines.len(), 128, "gfx should have 128 lines");
        for (i, line) in gfx_lines.iter().enumerate() {
            assert_eq!(line.len(), 128, "gfx line {} should be 128 chars, got {}", i, line.len());
        }
    }

    #[test]
    fn test_serialize_map_data_roundtrip() {
        let mut cart = CartData::new();
        // Set some map tiles with various indices (0-255).
        cart.map[0] = 0x21;
        cart.map[1] = 0xFF;
        cart.map[127] = 0xAB;
        cart.map[128] = 0x42;        // row 1, col 0
        cart.map[128 * 32] = 0x10;   // row 32, col 0
        cart.map[128 * 63 + 127] = 0xCD; // last tile

        let text = serialize_cart(&cart);
        let cart2 = parse_cart(&text);

        assert_eq!(cart2.map[0], 0x21);
        assert_eq!(cart2.map[1], 0xFF);
        assert_eq!(cart2.map[127], 0xAB);
        assert_eq!(cart2.map[128], 0x42);
        assert_eq!(cart2.map[128 * 32], 0x10);
        assert_eq!(cart2.map[128 * 63 + 127], 0xCD);

        // Verify all tiles match
        for i in 0..MAP_SIZE {
            assert_eq!(
                cart2.map[i], cart.map[i],
                "map tile {} mismatch", i
            );
        }
    }

    #[test]
    fn test_serialize_map_line_length() {
        let cart = CartData::new();
        let text = serialize_cart(&cart);
        let map_start = text.find("__map__\n").unwrap() + "__map__\n".len();
        let map_end = text.find("__sfx__").unwrap();
        let map_section = &text[map_start..map_end];
        let map_lines: Vec<&str> = map_section.lines().collect();
        assert_eq!(map_lines.len(), 64, "map should have 64 lines");
        for (i, line) in map_lines.iter().enumerate() {
            assert_eq!(line.len(), 256, "map line {} should be 256 chars, got {}", i, line.len());
        }
    }

    #[test]
    fn test_serialize_flag_data_roundtrip() {
        let mut cart = CartData::new();
        cart.flags[0] = 0x01;
        cart.flags[1] = 0xFF;
        cart.flags[127] = 0xAB;
        cart.flags[128] = 0x42;
        cart.flags[255] = 0xCD;

        let text = serialize_cart(&cart);
        let cart2 = parse_cart(&text);

        for i in 0..256 {
            assert_eq!(
                cart2.flags[i], cart.flags[i],
                "flag {} mismatch: expected 0x{:02x}, got 0x{:02x}",
                i, cart.flags[i], cart2.flags[i]
            );
        }
    }

    #[test]
    fn test_serialize_full_roundtrip() {
        // Build a cart with data in every section and verify complete round-trip.
        let mut cart = CartData::new();
        cart.code = String::from("function _init()\n  print(\"hello\")\nend");

        // Sprites: fill a gradient pattern.
        for y in 0..128 {
            for x in 0..128 {
                cart.sprites[y * 128 + x] = ((x + y) & 0x0f) as u8;
            }
        }

        // Flags: ascending values.
        for i in 0..256 {
            cart.flags[i] = i as u8;
        }

        // Map: checkerboard.
        for y in 0..64 {
            for x in 0..128 {
                cart.map[y * 128 + x] = if (x + y) % 2 == 0 { 0x21 } else { 0x00 };
            }
        }

        // SFX: set several with varied data.
        cart.sfx[0].speed = 16;
        cart.sfx[0].loop_start = 0;
        cart.sfx[0].loop_end = 8;
        for n in 0..NOTES_PER_SFX {
            cart.sfx[0].notes[n] = Note {
                pitch: (n as u8 * 2) & 63,
                waveform: (n as u8) & 7,
                volume: (n as u8) & 7,
                effect: (n as u8 + 1) & 7,
            };
        }
        cart.sfx[63].speed = 1;

        // Music: set several patterns.
        cart.music[0] = MusicPattern {
            flags: 1,
            channels: [0, 1, 2, 3],
        };
        cart.music[1] = MusicPattern {
            flags: 2,
            channels: [4, 0x41, 0x42, 0x43],
        };

        let text = serialize_cart(&cart);
        let cart2 = parse_cart(&text);

        // Verify code
        assert_eq!(cart2.code, cart.code);

        // Verify sprites
        for i in 0..SPRITE_SHEET_SIZE {
            assert_eq!(cart2.sprites[i], cart.sprites[i], "sprite pixel {}", i);
        }

        // Verify flags
        for i in 0..256 {
            assert_eq!(cart2.flags[i], cart.flags[i], "flag {}", i);
        }

        // Verify map
        for i in 0..MAP_SIZE {
            assert_eq!(cart2.map[i], cart.map[i], "map tile {}", i);
        }

        // Verify SFX
        for s in 0..NUM_SFX {
            assert_eq!(cart2.sfx[s].speed, cart.sfx[s].speed, "sfx {} speed", s);
            assert_eq!(cart2.sfx[s].loop_start, cart.sfx[s].loop_start, "sfx {} loop_start", s);
            assert_eq!(cart2.sfx[s].loop_end, cart.sfx[s].loop_end, "sfx {} loop_end", s);
            for n in 0..NOTES_PER_SFX {
                assert_eq!(
                    cart2.sfx[s].notes[n].pitch, cart.sfx[s].notes[n].pitch,
                    "sfx {} note {} pitch", s, n
                );
                assert_eq!(
                    cart2.sfx[s].notes[n].waveform, cart.sfx[s].notes[n].waveform,
                    "sfx {} note {} waveform", s, n
                );
                assert_eq!(
                    cart2.sfx[s].notes[n].volume, cart.sfx[s].notes[n].volume,
                    "sfx {} note {} volume", s, n
                );
                assert_eq!(
                    cart2.sfx[s].notes[n].effect, cart.sfx[s].notes[n].effect,
                    "sfx {} note {} effect", s, n
                );
            }
        }

        // Verify music
        for m in 0..NUM_MUSIC {
            assert_eq!(cart2.music[m].flags, cart.music[m].flags, "music {} flags", m);
            for ch in 0..4 {
                assert_eq!(
                    cart2.music[m].channels[ch], cart.music[m].channels[ch],
                    "music {} channel {}", m, ch
                );
            }
        }
    }

    #[test]
    fn test_serialize_starfall_roundtrip() {
        let source = include_str!("../../carts/starfall.p8");
        let cart = parse_cart(source);
        let text = serialize_cart(&cart);
        let cart2 = parse_cart(&text);

        // Code should match
        assert_eq!(cart2.code, cart.code);

        // Sprites
        for i in 0..SPRITE_SHEET_SIZE {
            assert_eq!(cart2.sprites[i], cart.sprites[i], "sprite pixel {}", i);
        }

        // Flags
        for i in 0..256 {
            assert_eq!(cart2.flags[i], cart.flags[i], "flag {}", i);
        }

        // Map
        for i in 0..MAP_SIZE {
            assert_eq!(cart2.map[i], cart.map[i], "map tile {}", i);
        }

        // SFX
        for s in 0..NUM_SFX {
            assert_eq!(cart2.sfx[s].speed, cart.sfx[s].speed, "sfx {} speed", s);
            assert_eq!(cart2.sfx[s].loop_start, cart.sfx[s].loop_start, "sfx {} loop_start", s);
            assert_eq!(cart2.sfx[s].loop_end, cart.sfx[s].loop_end, "sfx {} loop_end", s);
            for n in 0..NOTES_PER_SFX {
                assert_eq!(
                    cart2.sfx[s].notes[n].pitch, cart.sfx[s].notes[n].pitch,
                    "sfx {} note {} pitch", s, n
                );
                assert_eq!(
                    cart2.sfx[s].notes[n].waveform, cart.sfx[s].notes[n].waveform,
                    "sfx {} note {} waveform", s, n
                );
                assert_eq!(
                    cart2.sfx[s].notes[n].volume, cart.sfx[s].notes[n].volume,
                    "sfx {} note {} volume", s, n
                );
                assert_eq!(
                    cart2.sfx[s].notes[n].effect, cart.sfx[s].notes[n].effect,
                    "sfx {} note {} effect", s, n
                );
            }
        }

        // Music
        for m in 0..NUM_MUSIC {
            assert_eq!(cart2.music[m].flags, cart.music[m].flags, "music {} flags", m);
            for ch in 0..4 {
                assert_eq!(
                    cart2.music[m].channels[ch], cart.music[m].channels[ch],
                    "music {} channel {}", m, ch
                );
            }
        }
    }

    #[test]
    fn test_serialize_double_roundtrip_stable() {
        // Verify that serialize -> parse -> serialize produces the same text.
        let mut cart = CartData::new();
        cart.code = String::from("-- test\nprint(42)");
        cart.sprites[0] = 5;
        cart.sprites[128 * 50 + 50] = 12;
        cart.flags[10] = 0xAB;
        cart.map[100] = 0x21;
        cart.sfx[3].speed = 20;
        cart.sfx[3].notes[0] = Note {
            pitch: 36, waveform: 2, volume: 6, effect: 1,
        };
        cart.music[0] = MusicPattern {
            flags: 1,
            channels: [3, 0x41, 0x42, 0x43],
        };

        let text1 = serialize_cart(&cart);
        let cart2 = parse_cart(&text1);
        let text2 = serialize_cart(&cart2);

        assert_eq!(text1, text2, "double round-trip should produce identical output");
    }
}
