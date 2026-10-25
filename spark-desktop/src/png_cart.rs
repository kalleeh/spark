// png_cart.rs -- Parse real PICO-8 .p8.png cartridge images for Spark.
//
// A PICO-8 cart image is a 160x205 RGBA PNG (a 128x128 label with a border).
// Cart data is hidden in the least-significant 2 bits of each channel: one byte
// per pixel, read in raster order, with the byte assembled as
// (A<<6)|(R<<4)|(G<<2)|B. The first 0x8000 (32768) pixels carry the 32 KB
// PICO-8 memory image; pixel 0x8000 is a format version byte.
//
// This matches the real PICO-8 / fake-08 / zepto8 format (verified against
// fake-08's source). The earlier custom 128x128 layout is not produced by
// PICO-8 and is no longer supported.

use spark_core::audio::{MusicPattern, Note, NOTES_PER_SFX, NUM_MUSIC, NUM_SFX};
use spark_core::cart::CartData;
use spark_core::console::{MAP_SIZE, SPRITE_SHEET_SIZE};

/// Real PICO-8 cart image dimensions.
const PNG_WIDTH: u32 = 160;
const PNG_HEIGHT: u32 = 205;
/// Number of pixels (= encoded bytes) in a cart image: 160 * 205 = 32800.
const PNG_PIXELS: usize = (PNG_WIDTH * PNG_HEIGHT) as usize;
/// Size of the PICO-8 memory image carried by the first 0x8000 bytes.
const CART_RAM_SIZE: usize = 0x8000;

// Section byte offsets within the 0x8000-byte PICO-8 memory image.
//
//   0x0000-0x1FFF: Sprite sheet (packed nibbles, 8192 bytes -> 16384 pixels)
//   0x2000-0x2FFF: Map (rows 0-31; rows 32-63 share the upper sprite sheet)
//   0x3000-0x30FF: Sprite flags (256 bytes)
//   0x3100-0x31FF: Song/music data (64 * 4 = 256 bytes)
//   0x3200-0x42FF: SFX data (64 * 68 = 4352 bytes)
//   0x4300-0x7FFF: Lua code (raw, or `:c:` / `\0pxa` compressed)
const GFX_START: usize = 0x0000;
const GFX_END: usize = 0x2000;
const GFF_START: usize = 0x3000;
const GFF_END: usize = 0x3100;
const CODE_START: usize = 0x4300;

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Parse a .p8.png cartridge image from raw PNG file bytes.
///
/// Returns a fully populated `CartData`, or an error message on failure.
pub fn parse_p8_png(png_bytes: &[u8]) -> Result<CartData, String> {
    let raw = extract_steganographic_data(png_bytes)?;
    let mut cart = CartData::new();

    decode_gfx(&raw, &mut cart);
    decode_gff(&raw, &mut cart);
    decode_map(&raw, &mut cart);
    decode_sfx(&raw, &mut cart);
    decode_music(&raw, &mut cart);
    decode_code(&raw, &mut cart)?;

    Ok(cart)
}

// ---------------------------------------------------------------------------
// PNG decoding & steganographic extraction
// ---------------------------------------------------------------------------

/// Decode the PNG and extract 1 byte per pixel from the least significant
/// 2 bits of each RGBA channel, assembling `(A<<6)|(R<<4)|(G<<2)|B` per the
/// real PICO-8 format. Returns the 0x8000-byte PICO-8 memory image.
fn extract_steganographic_data(png_bytes: &[u8]) -> Result<Vec<u8>, String> {
    let decoder = png::Decoder::new(png_bytes);
    let mut reader = decoder
        .read_info()
        .map_err(|e| format!("PNG decode error: {}", e))?;

    // Copy dimensions before the mutable borrow in next_frame().
    let (width, height) = {
        let info = reader.info();
        (info.width, info.height)
    };
    if width != PNG_WIDTH || height != PNG_HEIGHT {
        return Err(format!(
            "Expected a {}x{} PICO-8 cart image, got {}x{}",
            PNG_WIDTH, PNG_HEIGHT, width, height
        ));
    }

    // Allocate a buffer large enough for one frame.
    let mut buf = vec![0u8; reader.output_buffer_size()];
    let frame_info = reader
        .next_frame(&mut buf)
        .map_err(|e| format!("PNG frame error: {}", e))?;
    let buf = &buf[..frame_info.buffer_size()];

    let color_type = frame_info.color_type;
    let channels = color_type.samples();

    // The steganographic encoding uses all four RGBA channels.
    if channels != 4 {
        return Err(format!(
            "Expected an RGBA PNG, got color type {:?} ({} channels)",
            color_type, channels
        ));
    }
    if frame_info.bit_depth != png::BitDepth::Eight {
        return Err(format!(
            "Expected an 8-bit PNG, got {:?}",
            frame_info.bit_depth
        ));
    }
    if buf.len() < PNG_PIXELS * channels {
        return Err(format!(
            "PNG buffer too small: {} bytes for {} pixels",
            buf.len(),
            PNG_PIXELS
        ));
    }

    // We only need the 0x8000-byte PICO-8 memory image (the version byte at
    // pixel 0x8000 and the label border beyond it are ignored).
    let mut data = Vec::with_capacity(CART_RAM_SIZE);
    for i in 0..CART_RAM_SIZE {
        let base = i * channels;
        let r = buf[base] & 3;
        let g = buf[base + 1] & 3;
        let b = buf[base + 2] & 3;
        let a = buf[base + 3] & 3;
        // PICO-8 byte order: (A<<6)|(R<<4)|(G<<2)|B.
        let byte = (a << 6) | (r << 4) | (g << 2) | b;
        data.push(byte);
    }

    Ok(data)
}

// ---------------------------------------------------------------------------
// Section decoders
// ---------------------------------------------------------------------------

/// Decode the sprite/graphics data (0x0000-0x1FFF).
///
/// The PNG stores sprites as packed nibbles: each byte contains two 4-bit
/// color indices (low nibble = left pixel, high nibble = right pixel).
/// 0x2000 bytes = 8192 packed nibble pairs = 16384 pixels = 128*128.
fn decode_gfx(raw: &[u8], cart: &mut CartData) {
    let gfx = &raw[GFX_START..GFX_END.min(raw.len())];
    for (i, &byte) in gfx.iter().enumerate() {
        let px = i * 2;
        if px + 1 < SPRITE_SHEET_SIZE {
            // Low nibble = left pixel, high nibble = right pixel.
            cart.sprites[px] = byte & 0x0F;
            cart.sprites[px + 1] = (byte >> 4) & 0x0F;
        }
    }
}

/// Decode the sprite flags (0x3000-0x30FF).
///
/// 256 bytes, one per sprite, directly mapped.
fn decode_gff(raw: &[u8], cart: &mut CartData) {
    let end = GFF_END.min(raw.len());
    if GFF_START >= end {
        return;
    }
    let gff = &raw[GFF_START..end];
    for (i, &byte) in gff.iter().enumerate() {
        if i < 256 {
            cart.flags[i] = byte;
        }
    }
}

/// Decode the map data.
///
/// The full PICO-8 map is 128 columns x 64 rows = 8192 bytes.
///   - Top 32 rows:    raw offset 0x2000 (4096 bytes)
///   - Bottom 32 rows: raw offset 0x1000 (shared with upper sprite sheet)
fn decode_map(raw: &[u8], cart: &mut CartData) {
    // Upper 32 rows of map (rows 0-31): at raw offset 0x2000
    let map_upper_start = 0x2000;
    let map_upper_end = (0x3000).min(raw.len());
    if map_upper_start < map_upper_end {
        let upper = &raw[map_upper_start..map_upper_end];
        for (i, &byte) in upper.iter().enumerate() {
            if i < 128 * 32 {
                cart.map[i] = byte;
            }
        }
    }

    // Lower 32 rows of map (rows 32-63): shared with upper sprite sheet at 0x1000
    let map_lower_start = 0x1000;
    let map_lower_end = (0x2000).min(raw.len());
    if map_lower_start < map_lower_end {
        let lower = &raw[map_lower_start..map_lower_end];
        for (i, &byte) in lower.iter().enumerate() {
            let map_offset = 128 * 32 + i;
            if map_offset < MAP_SIZE {
                cart.map[map_offset] = byte;
            }
        }
    }
}

/// Decode SFX data.
///
/// Each SFX is 68 bytes at raw offset 0x3200:
///   Byte 0: editor mode (ignored)
///   Byte 1: speed
///   Byte 2: loop_start
///   Byte 3: loop_end
///   Bytes 4-67: 32 notes, 2 bytes each
///
/// Each note (2 bytes, little-endian u16):
///   bits 0-5:  pitch (0-63)
///   bits 6-8:  waveform (0-7)
///   bits 9-11: volume (0-7)
///   bits 12-14: effect (0-7)
///   bit 15: custom instrument flag (ignored)
fn decode_sfx(raw: &[u8], cart: &mut CartData) {
    let sfx_base = 0x3200;
    for i in 0..NUM_SFX {
        let offset = sfx_base + i * 68;
        if offset + 68 > raw.len() {
            break;
        }
        let block = &raw[offset..offset + 68];

        // Byte 0 = editor mode (skip)
        cart.sfx[i].speed = block[1];
        cart.sfx[i].loop_start = block[2];
        cart.sfx[i].loop_end = block[3];

        for n in 0..NOTES_PER_SFX {
            let lo = block[4 + n * 2] as u16;
            let hi = block[4 + n * 2 + 1] as u16;
            let w = lo | (hi << 8);

            cart.sfx[i].notes[n] = Note {
                pitch: (w & 0x3F) as u8,
                waveform: ((w >> 6) & 0x07) as u8,
                volume: ((w >> 9) & 0x07) as u8,
                effect: ((w >> 12) & 0x07) as u8,
            };
        }
    }
}

/// Decode music/song data.
///
/// 64 patterns, 4 bytes each, at raw offset 0x3100:
///   Byte 0: flags (bit0=loop start, bit1=loop end, bit2=stop)
///   Byte 1: channel 0 SFX index (bit 6 = disabled)
///   Byte 2: channel 1 SFX index
///   Byte 3: channel 2 SFX index
///
/// Wait -- PICO-8 has 4 channels but only 4 bytes per pattern. The actual
/// encoding packs the flags into the high bit of each channel byte:
///
///   4 bytes per pattern:
///   bits 0-5 of byte 0: channel 0 SFX index, bit 7: loop start flag
///   bits 0-5 of byte 1: channel 1 SFX index, bit 7: loop end flag
///   bits 0-5 of byte 2: channel 2 SFX index, bit 7: stop flag
///   bits 0-5 of byte 3: channel 3 SFX index
///   For all bytes: bit 6 = channel disabled
fn decode_music(raw: &[u8], cart: &mut CartData) {
    let music_base = 0x3100;
    for i in 0..NUM_MUSIC {
        let offset = music_base + i * 4;
        if offset + 4 > raw.len() {
            break;
        }
        let b = &raw[offset..offset + 4];

        let mut flags: u8 = 0;
        if b[0] & 0x80 != 0 {
            flags |= 1; // loop start
        }
        if b[1] & 0x80 != 0 {
            flags |= 2; // loop end
        }
        if b[2] & 0x80 != 0 {
            flags |= 4; // stop
        }

        cart.music[i] = MusicPattern {
            flags,
            channels: [
                b[0] & 0x7F, // preserve bit 6 (disabled flag) + bits 0-5 (SFX index)
                b[1] & 0x7F,
                b[2] & 0x7F,
                b[3] & 0x7F,
            ],
        };
    }
}

// ---------------------------------------------------------------------------
// Lua code decompression
// ---------------------------------------------------------------------------

/// Decode (and possibly decompress) a Lua code-region byte slice (the bytes
/// starting at `CODE_START` in a cart's memory image) into Lua source.
///
/// Three encodings exist, distinguished by the first bytes:
///
///   * `\0pxa` : PICO-8 v0.2.0+ compression (move-to-front + LZ bitstream)
///   * `:c:\0` : legacy compression (byte-oriented LZ with a char LUT)
///   * neither: raw, NUL-terminated Lua source
///
/// Exposed for fuzzing / unit-testing the decompressors directly without
/// constructing a full PNG.
pub fn decode_code_region(code_data: &[u8]) -> Result<String, String> {
    if code_data.len() >= 4 && &code_data[0..4] == b"\0pxa" {
        decompress_pxa(code_data)
    } else if code_data.len() >= 4 && &code_data[0..4] == b":c:\0" {
        decompress_code_legacy(code_data)
    } else {
        let end = code_data
            .iter()
            .position(|&b| b == 0)
            .unwrap_or(code_data.len());
        Ok(String::from_utf8_lossy(&code_data[..end]).into_owned())
    }
}

/// Decode the Lua code section (0x4300-0x7FFF) of a cart memory image into
/// `cart.code`. See [`decode_code_region`] for the encodings handled.
fn decode_code(raw: &[u8], cart: &mut CartData) -> Result<(), String> {
    if CODE_START >= raw.len() {
        cart.code = String::new();
        return Ok(());
    }
    cart.code = decode_code_region(&raw[CODE_START..])?;
    Ok(())
}

/// Character lookup table for the legacy `:c:\0` compression format.
/// Index 0 is unused; codes 0x01-0x3B map to entries 0-58.
const LEGACY_CHAR_TABLE: &[u8] = b"\n 0123456789abcdefghijklmnopqrstuvwxyz!#%(){}[]<>+=/*:;.,~_";

/// Decompress the legacy `:c:\0` format (verified against fake-08 / the
/// PICO-8 wiki P8PNGFileFormat spec).
///
/// Layout:
///   Bytes 0-3: header `:c:\0`
///   Bytes 4-5: decompressed length, **MSB first** (`[4]*256 + [5]`)
///   Bytes 6-7: reserved (zero)
///   Bytes 8+:  byte-oriented stream:
///     0x00       -> copy the next raw byte verbatim
///     0x01-0x3B  -> emit LEGACY_CHAR_TABLE[byte - 1]
///     0x3C-0xFF  -> back-reference: offset = (byte-0x3C)*16 + (next & 0xF),
///                   length = (next >> 4) + 2
fn decompress_code_legacy(data: &[u8]) -> Result<String, String> {
    if data.len() < 8 {
        return Err("Legacy compressed code section too short".to_string());
    }
    let length = (data[4] as usize) * 256 + (data[5] as usize);

    let mut out: Vec<u8> = Vec::with_capacity(length);
    let mut i = 8;
    while i < data.len() && out.len() < length {
        let b = data[i];
        if b == 0x00 {
            // Literal raw byte follows.
            i += 1;
            if i >= data.len() {
                break;
            }
            out.push(data[i]);
            i += 1;
        } else if b < 0x3C {
            out.push(LEGACY_CHAR_TABLE[(b - 1) as usize]);
            i += 1;
        } else {
            if i + 1 >= data.len() {
                break;
            }
            let next = data[i + 1];
            let offset = (b as usize - 0x3C) * 16 + (next & 0xF) as usize;
            let copy_len = (next >> 4) as usize + 2;
            if offset == 0 || offset > out.len() {
                break; // malformed back-reference
            }
            let start = out.len() - offset;
            for j in 0..copy_len {
                out.push(out[start + j]);
            }
            i += 2;
        }
    }

    out.truncate(length);
    // The declared length can include a terminating NUL; trim at the first one
    // so it doesn't leak into the source as an embedded \0.
    if let Some(nul) = out.iter().position(|&b| b == 0) {
        out.truncate(nul);
    }
    // PICO-8 source can contain P8SCII bytes (>= 0x80) that are not valid
    // UTF-8; decode lossily rather than rejecting the whole cart.
    Ok(String::from_utf8_lossy(&out).into_owned())
}

/// Decompress the modern `\0pxa` format (PICO-8 v0.2.0+).
///
/// Ported from zepto8 / fake-08 `pxa_decompress`. Bits are read LSB-first.
///   Bytes 4-5: decompressed length, MSB first
///   Bytes 6-7: compressed byte count, MSB first
///   Bytes 8+:  bitstream over a move-to-front (MTF) byte table:
///     bit 1 -> emit: nbits = 4 + (count of following 1-bits); index
///              n = get_bits(nbits) + (1<<nbits) - 16; emit mtf.get(n)
///     bit 0 -> back-reference: nbits = 1?(1?5:10):15; offset = get_bits(nbits)+1.
///              If nbits==10 && offset==1: a run of raw bytes until a 0 byte.
///              Else length = 3 + sum of 3-bit groups (continue while group==7).
fn decompress_pxa(data: &[u8]) -> Result<String, String> {
    if data.len() < 8 {
        return Err("pxa compressed code section too short".to_string());
    }
    let length = (data[4] as usize) * 256 + (data[5] as usize);
    let compressed = (data[6] as usize) * 256 + (data[7] as usize);
    // `compressed` comes from the (untrusted) header and may claim more bytes
    // than `data` actually holds. Clamp the bit budget to the real buffer so
    // `get_bits` can never index past `data` (a malformed/truncated cart must
    // error or stop, not panic out of bounds).
    let compressed_bits = (compressed * 8).min(data.len() * 8);

    // Bit cursor (in bits), starting after the 8-byte header. LSB-first.
    let mut pos = 8usize * 8;
    fn get_bits(data: &[u8], pos: &mut usize, compressed_bits: usize, count: usize) -> u32 {
        let mut n = 0u32;
        let mut i = 0;
        while i < count && *pos < compressed_bits {
            let bit = (data[*pos >> 3] >> (*pos & 7)) & 1;
            n |= (bit as u32) << i;
            *pos += 1;
            i += 1;
        }
        n
    }
    macro_rules! get_bits {
        ($count:expr) => {
            get_bits(data, &mut pos, compressed_bits, $count)
        };
    }

    // Move-to-front table.
    let mut mtf: [u8; 256] = core::array::from_fn(|i| i as u8);

    let mut out: Vec<u8> = Vec::with_capacity(length);
    while out.len() < length && pos < compressed_bits {
        if get_bits!(1) == 1 {
            // Emit a single byte via MTF.
            let mut nbits = 4u32;
            while get_bits!(1) == 1 {
                nbits += 1;
            }
            let n = (get_bits!(nbits as usize) + (1u32 << nbits)).wrapping_sub(16) as usize;
            if n >= 256 {
                break;
            }
            let ch = mtf[n];
            if ch == 0 {
                break;
            }
            // Move ch to front.
            mtf.copy_within(0..n, 1);
            mtf[0] = ch;
            out.push(ch);
        } else {
            // Back-reference (or raw-byte run).
            let nbits = if get_bits!(1) == 1 {
                if get_bits!(1) == 1 {
                    5
                } else {
                    10
                }
            } else {
                15
            };
            let offset = get_bits!(nbits) as usize + 1;

            if nbits == 10 && offset == 1 {
                // Run of raw bytes terminated by a 0 byte.
                loop {
                    let ch = get_bits!(8) as u8;
                    if ch == 0 {
                        break;
                    }
                    out.push(ch);
                }
            } else {
                let mut len = 3usize;
                loop {
                    let n = get_bits!(3) as usize;
                    len += n;
                    if n != 7 {
                        break;
                    }
                }
                if offset > out.len() {
                    break; // malformed
                }
                let start = out.len() - offset;
                for j in 0..len {
                    out.push(out[start + j]);
                }
            }
        }
    }

    out.truncate(length);
    // The declared length can include a terminating NUL; trim at the first one
    // so it doesn't leak into the source as an embedded \0.
    if let Some(nul) = out.iter().position(|&b| b == 0) {
        out.truncate(nul);
    }
    // PICO-8 source can contain P8SCII bytes (>= 0x80) that are not valid
    // UTF-8; decode lossily rather than rejecting the whole cart.
    Ok(String::from_utf8_lossy(&out).into_owned())
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pxa_decompress_malformed_no_panic() {
        // Regression (loader fuzzer): a `\0pxa` header that claims more
        // compressed bytes than are present must not index out of bounds.
        // header bytes 6-7 = 0xffff (claims 65535 compressed bytes) but the
        // buffer is tiny.
        let mut data = vec![0u8, b'p', b'x', b'a', 0x10, 0x00, 0xff, 0xff];
        data.extend_from_slice(&[0xaa, 0x55, 0x01]); // a few stray bitstream bytes
                                                     // Must return a value or Err, never panic.
        let _ = decode_code_region(&data);

        // Also exercise a range of truncations of a pxa stream.
        for cut in 8..=data.len() {
            let _ = decode_code_region(&data[..cut]);
        }
    }

    #[test]
    fn test_legacy_decompress_malformed_no_panic() {
        // `:c:\0` with a length header larger than the data, plus random bytes.
        let mut data = vec![b':', b'c', b':', 0u8, 0xff, 0xff];
        data.extend_from_slice(&[0x01, 0x02, 0x3b, 0x00, 0xfe]);
        let _ = decode_code_region(&data);
        for cut in 4..=data.len() {
            let _ = decode_code_region(&data[..cut]);
        }
    }

    /// Build a real-format 160x205 RGBA PICO-8 cart PNG whose first
    /// `0x8000` decoded bytes equal `mem`. Encodes each byte as the real
    /// PICO-8 layout: (A<<6)|(R<<4)|(G<<2)|B in the low 2 bits per channel.
    fn make_cart_png(mem: &[u8]) -> Vec<u8> {
        let mut rgba = vec![0u8; PNG_PIXELS * 4];
        for i in 0..PNG_PIXELS {
            let byte = mem.get(i).copied().unwrap_or(0);
            rgba[i * 4] = (byte >> 4) & 3; // R
            rgba[i * 4 + 1] = (byte >> 2) & 3; // G
            rgba[i * 4 + 2] = byte & 3; // B
            rgba[i * 4 + 3] = (byte >> 6) & 3; // A
        }
        crate::encode_rgba_png(PNG_WIDTH, PNG_HEIGHT, &rgba)
    }

    #[test]
    fn test_rejects_wrong_dimensions() {
        // A 128x128 image is not a real PICO-8 cart.
        let png_bytes = crate::encode_rgba_png(128, 128, &vec![0u8; 128 * 128 * 4]);
        assert!(parse_p8_png(&png_bytes).is_err());
    }

    #[test]
    fn test_extract_byte_order() {
        // Verify the (A<<6)|(R<<4)|(G<<2)|B assembly round-trips every value.
        let mut mem = vec![0u8; CART_RAM_SIZE];
        for (i, slot) in mem.iter_mut().take(256).enumerate() {
            *slot = i as u8;
        }
        let png_bytes = make_cart_png(&mem);
        let raw = extract_steganographic_data(&png_bytes).unwrap();
        assert_eq!(raw.len(), CART_RAM_SIZE);
        for (i, &got) in raw.iter().take(256).enumerate() {
            assert_eq!(got, i as u8, "byte {i} did not round-trip");
        }
    }

    #[test]
    fn test_decode_sections_real_layout() {
        let mut mem = vec![0u8; CART_RAM_SIZE];
        // gfx: byte 0 = 0xA5 -> pixels 5, 10
        mem[0x0000] = 0xA5;
        // map upper row at 0x2000
        mem[0x2000] = 0x42;
        // flags at 0x3000
        mem[0x3000] = 0x07;
        // raw (uncompressed) code at 0x4300: ":" not present, plain text + NUL
        let code = b"print(1)\0";
        mem[0x4300..0x4300 + code.len()].copy_from_slice(code);

        let png_bytes = make_cart_png(&mem);
        let cart = parse_p8_png(&png_bytes).unwrap();

        assert_eq!(cart.sprites[0], 5);
        assert_eq!(cart.sprites[1], 10);
        assert_eq!(cart.map[0], 0x42);
        assert_eq!(cart.flags[0], 0x07);
        assert_eq!(cart.code, "print(1)");
    }

    #[test]
    fn test_legacy_decompress_roundtrip() {
        // Build a `:c:\0` stream for a string using only literal table chars
        // and one back-reference, then verify it decompresses correctly.
        // "abcabc": a,b,c literals then back-ref offset 3 length 3.
        let target = "abcabc";
        // table indices: 'a'..'z' start after "\n 0123456789" (12 entries),
        // so 'a' = index 12 -> code 13.
        let a = 13u8;
        let mut stream = Vec::new();
        stream.extend_from_slice(b":c:\0");
        stream.push((target.len() >> 8) as u8); // length MSB
        stream.push((target.len() & 0xFF) as u8); // length LSB
        stream.push(0);
        stream.push(0);
        stream.push(a); // 'a'
        stream.push(a + 1); // 'b'
        stream.push(a + 2); // 'c'
                            // back-ref: offset 3, length 3 -> first byte 0x3C + offset/16,
                            // second byte (offset%16) | ((len-2)<<4)
        let offset = 3usize;
        let len = 3usize;
        stream.push(0x3C + (offset / 16) as u8);
        stream.push(((offset % 16) as u8) | (((len - 2) as u8) << 4));

        let got = decompress_code_legacy(&stream).unwrap();
        assert_eq!(got, target);
    }

    #[test]
    fn test_invalid_png() {
        assert!(parse_p8_png(b"not a png").is_err());
    }
}
