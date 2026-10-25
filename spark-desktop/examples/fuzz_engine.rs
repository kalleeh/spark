//! Deterministic engine fuzzer: hammer every Console drawing/memory API with
//! boundary and extreme inputs (negative, huge, i32::MIN/MAX) to surface
//! panics — arithmetic overflow, out-of-bounds, divide-by-zero — that the
//! handful of well-behaved sample carts never trigger.
//!
//! Debug builds have overflow checks on, so an overflowing `as`/`+`/`*` panics
//! and is caught here. Run: `cargo run -p spark-desktop --example fuzz_engine`.
//! A non-zero exit / panic means a real bug to fix.

use spark_core::console::Console;

/// Tiny deterministic xorshift PRNG so runs are reproducible (no rand dep).
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
    /// A "spicy" i32: biased toward boundary values that expose overflow/OOB.
    fn coord(&mut self) -> i32 {
        match self.next() % 16 {
            0 => i32::MIN,
            1 => i32::MAX,
            2 => 0,
            3 => -1,
            4 => 127,
            5 => 128,
            6 => -128,
            7 => 32767,
            8 => -32768,
            9 => i32::MIN / 2,
            10 => i32::MAX / 2,
            _ => (self.next() as i32) % 300 - 150, // mostly near-screen
        }
    }
    fn col(&mut self) -> u8 {
        (self.next() % 18) as u8 // 0-17: includes a couple of out-of-range
    }
    fn small(&mut self, n: u64) -> u64 {
        self.next() % n
    }
    /// A small index biased toward the persistent-data boundary (0..63) plus
    /// out-of-range values, to exercise `dget`/`dset` bounds checks.
    fn spicy_small(&mut self) -> i32 {
        match self.next() % 8 {
            0 => i32::MIN,
            1 => i32::MAX,
            2 => -1,
            3 => 63,
            4 => 64,
            _ => (self.next() % 70) as i32,
        }
    }
}

fn main() {
    let iters: u64 = std::env::args()
        .nth(1)
        .and_then(|s| s.parse().ok())
        .unwrap_or(200_000);

    let mut rng = Rng(0x9E37_79B9_7F4A_7C15); // fixed seed for reproducibility
    let mut con = Console::new();
    let mut calls = 0u64;

    for _ in 0..iters {
        let op = rng.small(24);
        let a = rng.coord();
        let b = rng.coord();
        let c = rng.coord();
        let d = rng.coord();
        let col = rng.col();
        match op {
            0 => {
                con.pset(a, b, Some(col));
            }
            1 => {
                let _ = con.pget(a, b);
            }
            2 => con.line(a, b, c, d, Some(col)),
            3 => con.rect(a, b, c, d, Some(col)),
            4 => con.rectfill(a, b, c, d, Some(col)),
            5 => con.circ(a, b, c, Some(col)),
            6 => con.circfill(a, b, c, Some(col)),
            7 => con.oval(a, b, c, d, Some(col)),
            8 => con.ovalfill(a, b, c, d, Some(col)),
            9 => {
                // spr: sprite index (allow out of range), pos, size, flips.
                let n = (rng.next() % 300) as i32 - 20;
                let w = (rng.small(40) as f64) / 8.0;
                let h = (rng.small(40) as f64) / 8.0;
                con.spr(n, a, b, w, h, rng.small(2) == 0, rng.small(2) == 0);
            }
            10 => con.sspr(
                a,
                b,
                c,
                d,
                rng.coord(),
                rng.coord(),
                rng.coord(),
                rng.coord(),
                rng.small(2) == 0,
                rng.small(2) == 0,
            ),
            11 => con.map_draw(a, b, c, d, rng.coord(), rng.coord(), col),
            12 => {
                con.camera(a, b);
            }
            13 => {
                con.clip_rect(a, b, c, d, b & 1 == 0);
            }
            14 => {
                let _ = con.sget(a, b);
            }
            15 => con.sset(a, b, col),
            16 => {
                let _ = con.mget(a, b);
            }
            17 => con.mset(a, b, col),
            18 => {
                // peek/poke across the full u16 address space.
                let addr = (rng.next() & 0xFFFF) as u16;
                let _ = con.peek(addr);
                con.poke(addr, (rng.next() & 0xFF) as u8);
            }
            19 => {
                // Full 16/32-bit accessors near region boundaries + extremes.
                let addr = (rng.next() & 0xFFFF) as u16;
                let _ = con.peek2(addr);
                let _ = con.peek4(addr);
                con.poke2(addr, (rng.next() & 0xFFFF) as u16);
                con.poke4(addr, rng.next() as u32);
                // peek4/poke4 fixed-point round-trip + persistent data.
                let idx = rng.spicy_small();
                con.dset(idx, (rng.next() as i64 as f64) / 65536.0);
                let _ = con.dget(idx);
            }
            20 => {
                // memcpy / memset / cstore / reload with random ranges
                // (including wrapping and boundary lengths).
                let dst = (rng.next() & 0xFFFF) as u16;
                let src = (rng.next() & 0xFFFF) as u16;
                let len = (rng.next() & 0xFFFF) as u16;
                con.memcpy(dst, src, len);
                con.memset(dst, (rng.next() & 0xFF) as u8, len);
                con.cstore(dst, src, len);
                con.reload(dst, src, len);
            }
            21 => {
                // print with extreme positions + random bytes incl. control codes.
                let mut s = alloc_string(&mut rng);
                if s.is_empty() {
                    s.push('x');
                }
                con.print(&s, Some(a), Some(b), Some(col));
            }
            22 => {
                let _ = con.fget((rng.next() % 300) as usize, Some((rng.next() % 10) as u8));
                con.fset(
                    (rng.next() % 300) as usize,
                    Some((rng.next() % 10) as u8),
                    col,
                );
            }
            23 => {
                // tline: textured line with fractional map coords.
                con.tline(
                    a,
                    b,
                    c,
                    d,
                    (a as f64) / 8.0,
                    (b as f64) / 8.0,
                    (rng.next() as f64 % 4.0) - 2.0,
                    (rng.next() as f64 % 4.0) - 2.0,
                );
            }
            _ => unreachable!(),
        }
        calls += 1;
    }

    println!("fuzz_engine: {calls} API calls over {iters} iterations — no panic");
}

/// Build a short random byte string (1-12 bytes), biased to include P8SCII
/// control codes (1-6) and high bytes that exercise the print state machine.
fn alloc_string(rng: &mut Rng) -> String {
    let len = (rng.small(12) + 1) as usize;
    let mut bytes = Vec::with_capacity(len);
    for _ in 0..len {
        let b = match rng.small(8) {
            0 => (rng.small(6) + 1) as u8, // control code 1-6
            1 => b'^',
            2 => (rng.small(16) as u8) + b'0',
            _ => (rng.small(95) as u8) + 32, // printable ASCII
        };
        bytes.push(b);
    }
    String::from_utf8_lossy(&bytes).into_owned()
}
