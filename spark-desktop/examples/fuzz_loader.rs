//! Deterministic loader fuzzer: throw malformed / random bytes at the cart
//! parsing pipeline — `.p8.png` steganographic decode + code decompression,
//! `.p8` text parsing, and the Lua preprocessor — to ensure they return `Err`
//! or a value, but never panic.
//!
//! Loading an untrusted downloaded cart is exactly where a parser panic would
//! bite, so these must be robust. Debug overflow checks catch arithmetic bugs.
//! Run: `cargo run -p spark-desktop --example fuzz_loader [iters]`.

use spark_desktop_lib::png_cart;

struct Rng(u64);
impl Rng {
    fn next(&mut self) -> u64 {
        let mut x = self.0;
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        self.0 = x;
        x
    }
    fn byte(&mut self) -> u8 {
        (self.next() & 0xFF) as u8
    }
    fn range(&mut self, n: usize) -> usize {
        (self.next() as usize) % n
    }
}

/// A minimal valid 8-bit RGBA PNG of the size the parser expects (160x205),
/// built once; the fuzzer then corrupts copies of it so decoding gets far
/// enough to exercise the steganographic + decompression paths.
fn base_png() -> Vec<u8> {
    // A zeroed 160x205 RGBA image; the fuzzer mutates the encoded bytes.
    spark_desktop_lib::encode_rgba_png(160, 205, &vec![0u8; 160 * 205 * 4])
}

fn main() {
    let iters: u64 = std::env::args()
        .nth(1)
        .and_then(|s| s.parse().ok())
        .unwrap_or(100_000);
    let mut rng = Rng(0xD1B5_4A32_D192_ED03);
    let template = base_png();
    let mut ok = 0u64;

    for _ in 0..iters {
        let mode = rng.range(8);
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| match mode {
            // Target the `\0pxa` decompressor directly: valid header + random
            // bitstream (the riskiest LZ/move-to-front code).
            5 => {
                let len = rng.range(4096) + 4;
                let mut b = vec![0u8, b'p', b'x', b'a'];
                for _ in 4..len {
                    b.push(rng.byte());
                }
                let _ = png_cart::decode_code_region(&b);
            }
            // Target the legacy `:c:\0` decompressor directly.
            6 => {
                let len = rng.range(4096) + 4;
                let mut b = vec![b':', b'c', b':', 0u8];
                for _ in 4..len {
                    b.push(rng.byte());
                }
                let _ = png_cart::decode_code_region(&b);
            }
            // Raw code region (NUL-terminated path) with arbitrary bytes.
            7 => {
                let len = rng.range(4096);
                let b: Vec<u8> = (0..len).map(|_| rng.byte()).collect();
                let _ = png_cart::decode_code_region(&b);
            }
            // Pure random bytes (almost always Err — exercises header parsing).
            0 => {
                let len = rng.range(2048);
                let bytes: Vec<u8> = (0..len).map(|_| rng.byte()).collect();
                let _ = png_cart::parse_p8_png(&bytes);
            }
            // Truncated valid PNG (exercises partial-decode paths).
            1 => {
                let mut b = template.clone();
                let cut = rng.range(b.len().max(1));
                b.truncate(cut);
                let _ = png_cart::parse_p8_png(&b);
            }
            // Valid PNG with random byte flips (corrupt steganographic payload
            // + compression headers).
            2 => {
                let mut b = template.clone();
                let flips = rng.range(200) + 1;
                for _ in 0..flips {
                    let i = rng.range(b.len());
                    b[i] = rng.byte();
                }
                let _ = png_cart::parse_p8_png(&b);
            }
            // Random text through the .p8 cart parser.
            3 => {
                let len = rng.range(4096);
                let bytes: Vec<u8> = (0..len).map(|_| rng.byte()).collect();
                let s = String::from_utf8_lossy(&bytes);
                let _ = spark_core::cart::parse_cart(&s);
            }
            // Random text through the Lua preprocessor (the transform that all
            // the syntax fixes live in).
            _ => {
                let len = rng.range(2048);
                let bytes: Vec<u8> = (0..len).map(|_| rng.byte()).collect();
                let s = String::from_utf8_lossy(&bytes);
                let _ = spark_core::lua_preprocess::preprocess_pico8(&s);
            }
        }));
        match result {
            Ok(()) => ok += 1,
            Err(_) => {
                eprintln!("fuzz_loader: PANIC on mode {mode} — a loader bug to fix");
                std::process::exit(1);
            }
        }
    }

    println!("fuzz_loader: {ok} loads over {iters} iterations — no panic");
}
