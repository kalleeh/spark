/// Lightweight bookkeeping shared between desktop and embedded targets.
pub struct GameState {
    pub frame_count: u32,
    /// PICO-8 PRNG state, word 0. The two words are advanced together by
    /// `update_prng`; `rnd()` derives its result from word 1.
    pub rng0: u32,
    /// PICO-8 PRNG state, word 1.
    pub rng1: u32,
    /// Actual measured frames per second (set by the main loop each frame).
    pub actual_fps: f32,
    /// Target FPS: 30 or 60. Set by the main loop based on _update60 detection.
    pub target_fps: u8,
}

impl GameState {
    pub fn new() -> Self {
        let mut gs = Self {
            frame_count: 0,
            rng0: 0,
            rng1: 0,
            actual_fps: 30.0,
            target_fps: 30,
        };
        // Seed to PICO-8's deterministic zero-seed default. Embedded has no
        // RTC for a time-based seed, and a fixed default makes sequences
        // reproducible; carts that want a specific sequence call srand().
        gs.srand(0.0);
        gs
    }

    /// Advance the PRNG one step. This is PICO-8's exact generator (recovered
    /// identically by zepto8 and fake-08): a two-word rotate-then-add on raw
    /// 32-bit integers, so the sequence is bit-reproducible across platforms
    /// regardless of how the result is later projected to a float.
    #[inline]
    fn update_prng(&mut self) {
        // rotate16(x) == (x >> 16) | (x << 16) == rotate_left(16) for u32.
        self.rng1 = self.rng0.wrapping_add(self.rng1.rotate_left(16));
        self.rng0 = self.rng0.wrapping_add(self.rng1);
    }

    /// Seed the PRNG. Matches PICO-8/`srand`: a zero seed falls back to the
    /// magic `0xdeadbeef`, word 1 is `word0 ^ 0xbead29ba`, then 32 warm-up
    /// iterations are run.
    pub fn srand(&mut self, seed: f64) {
        let bits = crate::fixed::to_bits(seed);
        self.rng0 = if bits != 0 { bits } else { 0xdead_beef };
        self.rng1 = self.rng0 ^ 0xbead_29ba;
        for _ in 0..32 {
            self.update_prng();
        }
    }

    /// `rnd(x)` -- advance the PRNG and return a value in `[0, x)`.
    ///
    /// The modulo is computed on the fixed-point bit patterns exactly as
    /// PICO-8 does, then projected to a float. The integer sequence is
    /// bit-exact with real PICO-8; only the final float projection differs
    /// (and for the common `[0,1)` and small ranges it is exact, since the
    /// values land on the 1/65536 grid that f32/f64 represent exactly).
    pub fn rnd(&mut self, x: f64) -> f64 {
        self.update_prng();
        let b = self.rng1;
        let range = crate::fixed::to_bits(x);
        let result_bits = if range > 0 { b % range } else { 0 };
        // frombits: reinterpret as signed 16.16 and scale back to a real.
        crate::fixed::from_bits(result_bits)
    }
}

impl Default for GameState {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Same seed must always yield the same sequence (the property seeded
    /// procedural-generation carts depend on).
    #[test]
    fn rng_is_deterministic() {
        let mut a = GameState::new();
        a.srand(12345.0);
        let sa: [f64; 10] = core::array::from_fn(|_| a.rnd(1.0));

        let mut b = GameState::new();
        b.srand(12345.0);
        let sb: [f64; 10] = core::array::from_fn(|_| b.rnd(1.0));

        assert_eq!(sa, sb);
    }

    /// rnd(x) must stay within [0, x).
    #[test]
    fn rnd_stays_in_range() {
        let mut gs = GameState::new();
        gs.srand(1.0);
        for _ in 0..1000 {
            let v = gs.rnd(5.0);
            assert!((0.0..5.0).contains(&v), "rnd(5) returned {v} out of range");
        }
    }

    /// Bit-exact reference vectors taken from the recovered PICO-8 PRNG
    /// (cross-verified against fake-08's vm.cpp). srand(0) uses the
    /// deterministic zero-seed default (0xdeadbeef).
    #[test]
    fn rng_matches_pico8_reference() {
        let mut gs = GameState::new();
        gs.srand(0.0);
        let expected = [
            0.51739501953125,
            0.34039306640625,
            0.45916748046875,
            0.6631622314453125,
            0.1117401123046875,
        ];
        for (i, &want) in expected.iter().enumerate() {
            let got = gs.rnd(1.0);
            assert!(
                (got - want).abs() < 1e-12,
                "rnd #{i}: got {got}, want {want}"
            );
        }
    }

    /// srand seeding mid-stream resets the sequence reproducibly.
    #[test]
    fn srand_reseeds() {
        let mut gs = GameState::new();
        gs.srand(42.0);
        let first = [gs.rnd(100.0), gs.rnd(100.0), gs.rnd(100.0)];
        gs.srand(42.0);
        let again = [gs.rnd(100.0), gs.rnd(100.0), gs.rnd(100.0)];
        assert_eq!(first, again);
        // Matches the reference algorithm's srand(42) -> rnd(100) sequence.
        assert!((first[0] - 16.923843383789062).abs() < 1e-9);
    }
}
