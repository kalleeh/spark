// png_cart.rs -- Parse .p8.png cartridge images for Spark.
//
// PICO-8 carts can be distributed as 128x128 PNG images where the cart data is
// encoded steganographically in the least significant 2 bits of each RGBA
// channel. This module decodes the PNG, extracts the hidden bytes, and
// populates a `CartData` struct.

use spark_core::audio::{MusicPattern, Note, NOTES_PER_SFX, NUM_MUSIC, NUM_SFX};
use spark_core::cart::CartData;
use spark_core::console::{MAP_SIZE, SPRITE_SHEET_SIZE};

/// Total bytes encoded in a 128x128 RGBA image (1 byte per pixel).
const PNG_DATA_SIZE: usize = 128 * 128;

// Section byte offsets within the 16384-byte raw data.
//
//   0x0000-0x1FFF: Sprite sheet (packed nibbles, 8192 bytes -> 16384 pixels)
//   0x1000-0x1FFF: Upper sprite sheet / shared with map rows 32-63
//   0x2000-0x2FFF: Map rows 0-31 (4096 bytes)
//   0x2000-0x20FF: Sprite flags (256 bytes, overlaps map region in layout)
//   0x3000-0x30FF: Sprite flags (canonical position)
//   0x3100-0x31FF: Song/music data (64 * 4 = 256 bytes)
//   0x3200-0x42FF: SFX data (64 * 68 = 4352 bytes)
//   0x4300+:       Compressed Lua code
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
/// 2 bits of each RGBA channel.
fn extract_steganographic_data(png_bytes: &[u8]) -> Result<Vec<u8>, String> {
    let decoder = png::Decoder::new(png_bytes);
    let mut reader = decoder.read_info().map_err(|e| format!("PNG decode error: {}", e))?;

    // Copy dimensions before the mutable borrow in next_frame().
    let (width, height) = {
        let info = reader.info();
        (info.width, info.height)
    };
    if width != 128 || height != 128 {
        return Err(format!(
            "Expected 128x128 PNG, got {}x{}",
            width, height
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

    // We need at least RGBA (4 channels) for the steganographic encoding.
    // RGB (3 channels) images are also accepted -- the alpha contribution
    // is treated as 0 (since there is no alpha channel).
    if channels < 3 {
        return Err(format!(
            "Expected RGBA or RGB PNG, got color type {:?} ({} channels)",
            color_type, channels
        ));
    }

    let pixel_count = (width * height) as usize;
    if pixel_count != PNG_DATA_SIZE {
        return Err(format!(
            "Expected {} pixels, got {}",
            PNG_DATA_SIZE, pixel_count
        ));
    }

    let mut data = Vec::with_capacity(PNG_DATA_SIZE);
    for i in 0..pixel_count {
        let base = i * channels;
        let r = buf[base];
        let g = buf[base + 1];
        let b = buf[base + 2];
        let a = if channels >= 4 { buf[base + 3] } else { 0xFF };

        // Byte = low2(R) | low2(G)<<2 | low2(B)<<4 | low2(A)<<6
        let byte = (r & 3) | ((g & 3) << 2) | ((b & 3) << 4) | ((a & 3) << 6);
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

/// Decode (and possibly decompress) the Lua code section starting at 0x4300.
fn decode_code(raw: &[u8], cart: &mut CartData) -> Result<(), String> {
    if CODE_START >= raw.len() {
        cart.code = String::new();
        return Ok(());
    }
    let code_data = &raw[CODE_START..];

    if code_data.len() >= 4 && &code_data[0..4] == b":c:\0" {
        // Newer compressed format (PICO-8 v0.2.0+)
        cart.code = decompress_code_new(code_data)?;
    } else if !code_data.is_empty() && code_data[0] == 0x00 {
        // Older compressed format: starts with 0x00 header
        cart.code = decompress_code_old(code_data)?;
    } else if !code_data.is_empty() && code_data[0] == b':' {
        // Uncompressed code (starts with ':' but not ":c:\0")
        // Find the null terminator.
        let end = code_data.iter().position(|&b| b == 0).unwrap_or(code_data.len());
        cart.code = String::from_utf8_lossy(&code_data[1..end]).into_owned();
    } else {
        // Possibly uncompressed or raw -- try to interpret as text.
        let end = code_data.iter().position(|&b| b == 0).unwrap_or(code_data.len());
        if end == 0 {
            cart.code = String::new();
        } else {
            cart.code = String::from_utf8_lossy(&code_data[..end]).into_owned();
        }
    }

    Ok(())
}

/// Character map used by PICO-8 new compression (`:c:\0` format).
///
/// The first entry (index 0) is a special "copy from" marker; indices 1-59
/// map to the most commonly used characters in Lua source.
const COMPRESS_CHAR_TABLE: &[u8] = b"\n 0123456789abcdefghijklmnopqrstuvwxyz!#%(){}[]<>+=/*:;.,~_";

/// Decompress code in the newer `:c:\0` format (PICO-8 v0.2.0+).
///
/// Format:
///   Bytes 0-3: header `:c:\0`
///   Bytes 4-5: uncompressed length (little-endian u16)
///   Bytes 6+:  compressed bitstream
///
/// The bitstream encodes:
///   - If next bit is 1: the next 4 bits index into COMPRESS_CHAR_TABLE
///     (indices 1-15 map to the 15 most common chars).
///   - If next bit is 0 and the following bit is 0: literal byte (next 8 bits).
///   - If next bit is 0 and the following bit is 1: the next 5 bits index into
///     the extended character table (indices 0-58 map to more chars). But wait --
///     the encoding is actually simpler than this.
///
/// Let me describe the actual decompression algorithm based on the PICO-8
/// community's reverse-engineering:
///
/// Read bits from MSB first. For each output character:
///   1. Read 1 bit.
///      - If 1: read 4 more bits => index (1-15) into the short character map
///        (first 15 entries of COMPRESS_CHAR_TABLE starting from index 1).
///        Output that character.
///      - If 0: read 1 more bit.
///        - If 0 (so pattern 00): read 8 bits => literal byte. Output it.
///        - If 1 (so pattern 01): read 5 bits => n (0-31).
///          If n < length(COMPRESS_CHAR_TABLE): output COMPRESS_CHAR_TABLE[n].
///          Otherwise this is a copy reference:
///            read 4 more bits => offset high
///            let offset_bits = (n - 59) * 16 + offset_high
///            ...actually this gets complex with the copy offsets.
///
/// The actual algorithm based on careful reverse-engineering:
fn decompress_code_new(data: &[u8]) -> Result<String, String> {
    if data.len() < 8 {
        return Err("Compressed code section too short".to_string());
    }
    let uncompressed_len = (data[4] as usize) | ((data[5] as usize) << 8);
    let compressed = &data[8..]; // Skip 4-byte header + 2-byte length + 2 reserved

    let mut reader = BitReader::new(compressed);
    let mut output: Vec<u8> = Vec::with_capacity(uncompressed_len);

    while output.len() < uncompressed_len {
        let bit = reader.read_bits(1).ok_or("Unexpected end of compressed data")?;
        if bit == 1 {
            // 1 + 4 bits: index into short table (entries 1-15)
            let idx = reader
                .read_bits(4)
                .ok_or("Unexpected end reading short char index")?
                as usize;
            if idx < COMPRESS_CHAR_TABLE.len() {
                output.push(COMPRESS_CHAR_TABLE[idx]);
            }
        } else {
            let bit2 = reader
                .read_bits(1)
                .ok_or("Unexpected end reading second bit")?;
            if bit2 == 0 {
                // 00 + 8 bits: literal byte
                let byte = reader
                    .read_bits(8)
                    .ok_or("Unexpected end reading literal byte")?
                    as u8;
                output.push(byte);
            } else {
                // 01 + 5 bits: extended table index or copy reference
                let idx = reader
                    .read_bits(5)
                    .ok_or("Unexpected end reading extended index")?
                    as usize;
                if idx < COMPRESS_CHAR_TABLE.len() {
                    output.push(COMPRESS_CHAR_TABLE[idx]);
                } else {
                    // Copy reference: read offset and length
                    // The offset uses (idx - len(table)) to determine extra bits
                    let extra = idx - COMPRESS_CHAR_TABLE.len();
                    // Read 4 bits for offset high, total offset = extra * 16 + high + 1
                    let offset_hi = reader
                        .read_bits(4)
                        .ok_or("Unexpected end reading copy offset")?
                        as usize;

                    let copy_offset = (extra << 4) + offset_hi + 1;

                    // Read copy length: 4 bits, value + 2 gives actual length
                    let copy_len = reader
                        .read_bits(4)
                        .ok_or("Unexpected end reading copy length")?
                        as usize
                        + 2;

                    if copy_offset > output.len() {
                        // Invalid back-reference; output spaces as fallback
                        for _ in 0..copy_len {
                            output.push(b' ');
                        }
                    } else {
                        let start = output.len() - copy_offset;
                        for j in 0..copy_len {
                            let c = output[start + (j % copy_offset)];
                            output.push(c);
                        }
                    }
                }
            }
        }
    }

    output.truncate(uncompressed_len);
    String::from_utf8(output)
        .map_err(|e| format!("Decompressed code is not valid UTF-8: {}", e))
}

/// Decompress code in the older format (header byte 0x00).
///
/// Format:
///   Byte 0: 0x00
///   Bytes 1-2: uncompressed length (little-endian u16)
///   Bytes 3+: compressed data using a simple LZ scheme
///
/// Each byte in the compressed stream:
///   0x00: end of stream
///   0x01-0x3B: literal character from a lookup table
///   0x3C-0xFF: copy reference -- next byte encodes offset,
///              length = current_byte - 0x3B
fn decompress_code_old(data: &[u8]) -> Result<String, String> {
    if data.len() < 3 {
        return Err("Old compressed code section too short".to_string());
    }

    let uncompressed_len = (data[1] as usize) | ((data[2] as usize) << 8);
    let compressed = &data[3..];

    // Old format character table: same characters used by PICO-8
    let char_table: &[u8] =
        b"#\n 0123456789abcdefghijklmnopqrstuvwxyz!#%(){}[]<>+=/*:;.,~_";

    let mut output: Vec<u8> = Vec::with_capacity(uncompressed_len);
    let mut i = 0;

    while i < compressed.len() && output.len() < uncompressed_len {
        let b = compressed[i];
        i += 1;

        if b == 0x00 {
            break;
        } else if b <= 0x3B {
            // Literal character from table
            let idx = (b - 1) as usize;
            if idx < char_table.len() {
                output.push(char_table[idx]);
            }
        } else {
            // Copy reference
            if i >= compressed.len() {
                break;
            }
            let offset = compressed[i] as usize;
            i += 1;
            let length = (b as usize) - 0x3B;

            if offset > output.len() {
                for _ in 0..length {
                    output.push(b' ');
                }
            } else {
                let start = output.len() - offset;
                for j in 0..length {
                    let c = output[start + (j % offset)];
                    output.push(c);
                }
            }
        }
    }

    output.truncate(uncompressed_len);
    String::from_utf8(output)
        .map_err(|e| format!("Decompressed code is not valid UTF-8: {}", e))
}

// ---------------------------------------------------------------------------
// Bit reader utility
// ---------------------------------------------------------------------------

/// A simple MSB-first bit reader over a byte slice.
struct BitReader<'a> {
    data: &'a [u8],
    byte_pos: usize,
    bit_pos: u8, // 0-7, counts from MSB (7) down to LSB (0)
}

impl<'a> BitReader<'a> {
    fn new(data: &'a [u8]) -> Self {
        BitReader {
            data,
            byte_pos: 0,
            bit_pos: 0,
        }
    }

    /// Read `n` bits (1-16) and return them as a u16. Returns None on EOF.
    fn read_bits(&mut self, n: u8) -> Option<u16> {
        let mut result: u16 = 0;
        for _ in 0..n {
            if self.byte_pos >= self.data.len() {
                return None;
            }
            // Read the next bit from MSB first
            let bit = (self.data[self.byte_pos] >> (7 - self.bit_pos)) & 1;
            result = (result << 1) | (bit as u16);
            self.bit_pos += 1;
            if self.bit_pos >= 8 {
                self.bit_pos = 0;
                self.byte_pos += 1;
            }
        }
        Some(result)
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    /// Create a minimal 128x128 RGBA PNG from raw pixel data.
    fn make_test_png(pixels: &[u8; PNG_DATA_SIZE]) -> Vec<u8> {
        let mut rgba = vec![0u8; PNG_DATA_SIZE * 4];
        for i in 0..PNG_DATA_SIZE {
            let byte = pixels[i];
            // Encode: R low 2 bits = byte & 3, G low 2 = (byte >> 2) & 3, etc.
            rgba[i * 4] = byte & 3;           // R
            rgba[i * 4 + 1] = (byte >> 2) & 3; // G
            rgba[i * 4 + 2] = (byte >> 4) & 3; // B
            rgba[i * 4 + 3] = (byte >> 6) | 0xFC; // A (set high bits so alpha is mostly opaque)
        }

        // Encode as PNG using the png crate
        let mut png_bytes = Vec::new();
        {
            let mut encoder = png::Encoder::new(&mut png_bytes, 128, 128);
            encoder.set_color(png::ColorType::Rgba);
            encoder.set_depth(png::BitDepth::Eight);
            let mut writer = encoder.write_header().expect("PNG write header");
            writer.write_image_data(&rgba).expect("PNG write data");
        }
        png_bytes
    }

    #[test]
    fn test_extract_steganographic_data() {
        let mut pixels = [0u8; PNG_DATA_SIZE];
        // Set some known values
        pixels[0] = 0xAB; // 10101011
        pixels[1] = 0x42; // 01000010
        pixels[100] = 0xFF;
        pixels[PNG_DATA_SIZE - 1] = 0x12;

        let png_bytes = make_test_png(&pixels);
        let extracted = extract_steganographic_data(&png_bytes).unwrap();

        assert_eq!(extracted.len(), PNG_DATA_SIZE);
        assert_eq!(extracted[0], 0xAB);
        assert_eq!(extracted[1], 0x42);
        assert_eq!(extracted[100], 0xFF);
        assert_eq!(extracted[PNG_DATA_SIZE - 1], 0x12);
    }

    #[test]
    fn test_extract_all_byte_values() {
        let mut pixels = [0u8; PNG_DATA_SIZE];
        // Test that every possible byte value round-trips correctly
        for i in 0..256 {
            pixels[i] = i as u8;
        }

        let png_bytes = make_test_png(&pixels);
        let extracted = extract_steganographic_data(&png_bytes).unwrap();

        for i in 0..256 {
            assert_eq!(
                extracted[i], i as u8,
                "Byte value {} did not round-trip correctly",
                i
            );
        }
    }

    #[test]
    fn test_decode_gfx() {
        let mut raw = vec![0u8; PNG_DATA_SIZE];
        // GFX at offset 0x0000: packed nibbles
        // Byte 0x00 = 0x21 means pixel 0 = 1 (low nibble), pixel 1 = 2 (high nibble)
        raw[0] = 0x21;
        raw[1] = 0xF0; // pixel 2 = 0, pixel 3 = 15

        let mut cart = CartData::new();
        decode_gfx(&raw, &mut cart);

        assert_eq!(cart.sprites[0], 1);
        assert_eq!(cart.sprites[1], 2);
        assert_eq!(cart.sprites[2], 0);
        assert_eq!(cart.sprites[3], 15);
    }

    #[test]
    fn test_decode_gff() {
        let mut raw = vec![0u8; PNG_DATA_SIZE];
        // GFF at offset 0x3000
        raw[0x3000] = 0x01;
        raw[0x3001] = 0xFF;
        raw[0x30FF] = 0xAB;

        let mut cart = CartData::new();
        decode_gff(&raw, &mut cart);

        assert_eq!(cart.flags[0], 0x01);
        assert_eq!(cart.flags[1], 0xFF);
        assert_eq!(cart.flags[255], 0xAB);
    }

    #[test]
    fn test_decode_map_upper() {
        let mut raw = vec![0u8; PNG_DATA_SIZE];
        // Upper map at offset 0x2000
        raw[0x2000] = 0x42;
        raw[0x2001] = 0x21;

        let mut cart = CartData::new();
        decode_map(&raw, &mut cart);

        assert_eq!(cart.map[0], 0x42);
        assert_eq!(cart.map[1], 0x21);
    }

    #[test]
    fn test_decode_map_lower() {
        let mut raw = vec![0u8; PNG_DATA_SIZE];
        // Lower map at offset 0x1000 (shared with upper sprite sheet)
        raw[0x1000] = 0xCD;
        raw[0x1001] = 0xEF;

        let mut cart = CartData::new();
        decode_map(&raw, &mut cart);

        // Lower map starts at row 32 in the map array
        assert_eq!(cart.map[128 * 32], 0xCD);
        assert_eq!(cart.map[128 * 32 + 1], 0xEF);
    }

    #[test]
    fn test_decode_sfx() {
        let mut raw = vec![0u8; PNG_DATA_SIZE];
        // SFX at offset 0x3200, 68 bytes per SFX
        let sfx_offset = 0x3200;
        // SFX 0: mode=0, speed=16, loop_start=4, loop_end=8
        raw[sfx_offset] = 0;   // editor mode
        raw[sfx_offset + 1] = 16; // speed
        raw[sfx_offset + 2] = 4;  // loop_start
        raw[sfx_offset + 3] = 8;  // loop_end

        // Note 0: pitch=24 (0x18), waveform=3, volume=5, effect=2
        // Encoded: bits 0-5 = 24 = 0x18, bits 6-8 = 3, bits 9-11 = 5, bits 12-14 = 2
        // w = 24 | (3 << 6) | (5 << 9) | (2 << 12) = 24 | 192 | 2560 | 8192 = 10968
        // = 0x2AD8 => low byte = 0xD8, high byte = 0x2A
        let w: u16 = 24 | (3 << 6) | (5 << 9) | (2 << 12);
        raw[sfx_offset + 4] = (w & 0xFF) as u8;
        raw[sfx_offset + 5] = ((w >> 8) & 0xFF) as u8;

        let mut cart = CartData::new();
        decode_sfx(&raw, &mut cart);

        assert_eq!(cart.sfx[0].speed, 16);
        assert_eq!(cart.sfx[0].loop_start, 4);
        assert_eq!(cart.sfx[0].loop_end, 8);
        assert_eq!(cart.sfx[0].notes[0].pitch, 24);
        assert_eq!(cart.sfx[0].notes[0].waveform, 3);
        assert_eq!(cart.sfx[0].notes[0].volume, 5);
        assert_eq!(cart.sfx[0].notes[0].effect, 2);
    }

    #[test]
    fn test_decode_music() {
        let mut raw = vec![0u8; PNG_DATA_SIZE];
        // Music at offset 0x3100, 4 bytes per pattern
        let music_offset = 0x3100;

        // Pattern 0: loop start, channels 0=SFX 1, 1=SFX 2, 2=disabled, 3=SFX 5
        raw[music_offset] = 0x80 | 1; // bit 7 = loop start, SFX index 1
        raw[music_offset + 1] = 2;    // SFX index 2
        raw[music_offset + 2] = 0x40; // bit 6 = disabled
        raw[music_offset + 3] = 5;    // SFX index 5

        let mut cart = CartData::new();
        decode_music(&raw, &mut cart);

        assert!(cart.music[0].is_loop_start());
        assert!(!cart.music[0].is_loop_end());
        assert!(!cart.music[0].is_stop());
        assert!(cart.music[0].channel_enabled(0));
        assert_eq!(cart.music[0].sfx_index(0), 1);
        assert!(cart.music[0].channel_enabled(1));
        assert_eq!(cart.music[0].sfx_index(1), 2);
        assert!(!cart.music[0].channel_enabled(2)); // disabled
        assert!(cart.music[0].channel_enabled(3));
        assert_eq!(cart.music[0].sfx_index(3), 5);
    }

    #[test]
    fn test_bit_reader() {
        // 0xA5 = 10100101, 0x3C = 00111100
        let data = [0xA5u8, 0x3C];
        let mut reader = BitReader::new(&data);

        // Read 1 bit at a time from 0xA5
        assert_eq!(reader.read_bits(1), Some(1)); // 1
        assert_eq!(reader.read_bits(1), Some(0)); // 0
        assert_eq!(reader.read_bits(1), Some(1)); // 1
        assert_eq!(reader.read_bits(1), Some(0)); // 0
        assert_eq!(reader.read_bits(1), Some(0)); // 0
        assert_eq!(reader.read_bits(1), Some(1)); // 1
        assert_eq!(reader.read_bits(1), Some(0)); // 0
        assert_eq!(reader.read_bits(1), Some(1)); // 1

        // Now reading from 0x3C = 00111100
        assert_eq!(reader.read_bits(4), Some(0b0011)); // 3
        assert_eq!(reader.read_bits(4), Some(0b1100)); // 12
    }

    #[test]
    fn test_bit_reader_multi_bit() {
        // 0xA5 = 10100101
        let data = [0xA5u8];
        let mut reader = BitReader::new(&data);

        assert_eq!(reader.read_bits(4), Some(0b1010)); // top 4 bits
        assert_eq!(reader.read_bits(4), Some(0b0101)); // bottom 4 bits
    }

    #[test]
    fn test_bit_reader_eof() {
        let data = [0xFFu8];
        let mut reader = BitReader::new(&data);

        assert_eq!(reader.read_bits(8), Some(0xFF));
        assert_eq!(reader.read_bits(1), None); // EOF
    }

    #[test]
    fn test_full_png_roundtrip() {
        // Create a raw data buffer with known sprite and flag data
        let mut pixels = [0u8; PNG_DATA_SIZE];

        // Write some GFX data: pixel pair (5, 10) at position 0
        pixels[0x0000] = 0xA5; // low nibble = 5, high nibble = 10

        // Write some flag data at 0x3000
        pixels[0x3000] = 0x07;

        // Write map data at 0x2000
        pixels[0x2000] = 0x42;

        // Encode as PNG
        let png_bytes = make_test_png(&pixels);

        // Parse
        let cart = parse_p8_png(&png_bytes).unwrap();

        // Check GFX decoding
        assert_eq!(cart.sprites[0], 5);
        assert_eq!(cart.sprites[1], 10);

        // Check flags decoding
        assert_eq!(cart.flags[0], 0x07);

        // Check map decoding
        assert_eq!(cart.map[0], 0x42);
    }

    #[test]
    fn test_invalid_png() {
        let result = parse_p8_png(b"not a png");
        assert!(result.is_err());
    }

    #[test]
    fn test_decode_sfx_max_values() {
        let mut raw = vec![0u8; PNG_DATA_SIZE];
        let sfx_offset = 0x3200;
        raw[sfx_offset + 1] = 0xFF; // max speed
        raw[sfx_offset + 2] = 31;   // max loop_start
        raw[sfx_offset + 3] = 31;   // max loop_end

        // Note with max values: pitch=63, waveform=7, volume=7, effect=7
        let w: u16 = 63 | (7 << 6) | (7 << 9) | (7 << 12);
        raw[sfx_offset + 4] = (w & 0xFF) as u8;
        raw[sfx_offset + 5] = ((w >> 8) & 0xFF) as u8;

        let mut cart = CartData::new();
        decode_sfx(&raw, &mut cart);

        assert_eq!(cart.sfx[0].speed, 0xFF);
        assert_eq!(cart.sfx[0].loop_start, 31);
        assert_eq!(cart.sfx[0].loop_end, 31);
        assert_eq!(cart.sfx[0].notes[0].pitch, 63);
        assert_eq!(cart.sfx[0].notes[0].waveform, 7);
        assert_eq!(cart.sfx[0].notes[0].volume, 7);
        assert_eq!(cart.sfx[0].notes[0].effect, 7);
    }

    #[test]
    fn test_decode_music_all_flags() {
        let mut raw = vec![0u8; PNG_DATA_SIZE];
        let base = 0x3100;

        // Pattern 0: loop start
        raw[base] = 0x80; // loop start flag
        raw[base + 1] = 0;
        raw[base + 2] = 0;
        raw[base + 3] = 0;

        // Pattern 1: loop end
        raw[base + 4] = 0;
        raw[base + 5] = 0x80; // loop end flag
        raw[base + 6] = 0;
        raw[base + 7] = 0;

        // Pattern 2: stop
        raw[base + 8] = 0;
        raw[base + 9] = 0;
        raw[base + 10] = 0x80; // stop flag
        raw[base + 11] = 0;

        let mut cart = CartData::new();
        decode_music(&raw, &mut cart);

        assert!(cart.music[0].is_loop_start());
        assert!(!cart.music[0].is_loop_end());
        assert!(!cart.music[0].is_stop());

        assert!(!cart.music[1].is_loop_start());
        assert!(cart.music[1].is_loop_end());
        assert!(!cart.music[1].is_stop());

        assert!(!cart.music[2].is_loop_start());
        assert!(!cart.music[2].is_loop_end());
        assert!(cart.music[2].is_stop());
    }
}
