//! Deterministic audio fuzzer: fill the SFX/music data tables with arbitrary
//! byte values (as a malformed cart would), trigger random `sfx`/`music`
//! calls, and pump `generate_samples` at varied sample rates — surfacing
//! panics (overflow, out-of-bounds, divide-by-zero, NaN-index) in the synth.
//!
//! `sfx_data`/`music_data` come straight from (untrusted) cart bytes, and
//! `generate_samples` interprets them every frame, so it must be robust.
//! Run: `cargo run -p spark-desktop --example fuzz_audio [iters]`.

use spark_core::audio::{Audio, NOTES_PER_SFX, NUM_MUSIC, NUM_SFX};

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
    /// A "spicy" i32 biased to boundary values.
    fn spicy(&mut self) -> i32 {
        match self.next() % 8 {
            0 => i32::MIN,
            1 => i32::MAX,
            2 => -1,
            3 => 0,
            4 => 63,
            5 => 64,
            _ => (self.next() as i32) % 200 - 50,
        }
    }
}

/// Randomize all SFX/music bytes the way a malformed cart would.
fn scramble(audio: &mut Audio, rng: &mut Rng) {
    for s in 0..NUM_SFX {
        let sfx = &mut audio.sfx_data[s];
        sfx.speed = rng.byte();
        sfx.loop_start = rng.byte();
        sfx.loop_end = rng.byte();
        for n in 0..NOTES_PER_SFX {
            let note = &mut sfx.notes[n];
            note.pitch = rng.byte();
            note.waveform = rng.byte();
            note.volume = rng.byte();
            note.effect = rng.byte();
        }
    }
    for m in 0..NUM_MUSIC {
        let pat = &mut audio.music_data[m];
        for c in 0..4 {
            pat.channels[c] = rng.byte();
        }
    }
}

fn main() {
    let iters: u64 = std::env::args()
        .nth(1)
        .and_then(|s| s.parse().ok())
        .unwrap_or(50_000);
    let mut rng = Rng(0xA17C_9F23_001B_EE45);
    let mut audio = Audio::new();
    let mut buf = vec![0f32; 2048];

    for _ in 0..iters {
        match rng.range(6) {
            0 => scramble(&mut audio, &mut rng),
            1 => audio.sfx(rng.spicy(), rng.spicy(), rng.spicy(), rng.spicy()),
            2 => audio.music(rng.spicy(), rng.spicy(), rng.spicy()),
            3 => {
                // generate at an odd/extreme sample rate into a random-size buf.
                let rates = [0u32, 1, 8000, 22050, 44100, 48000, u32::MAX];
                let rate = rates[rng.range(rates.len())];
                let len = rng.range(buf.len()) + 1;
                audio.generate_samples(rate, &mut buf[..len]);
                // Every produced sample must be finite (no NaN/Inf leaking out).
                for &s in &buf[..len] {
                    assert!(s.is_finite(), "generate_samples produced non-finite sample");
                }
            }
            4 => {
                let _ = audio.sfx_on_channel(rng.range(8));
            }
            _ => audio.generate_samples(44100, &mut buf[..735]),
        }
    }

    println!("fuzz_audio: {iters} iterations — no panic, all samples finite");
}
