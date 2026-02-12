/// Lightweight bookkeeping shared between desktop and embedded targets.
pub struct GameState {
    pub frame_count: u32,
    pub rng_state: u64,
}

impl GameState {
    pub fn new() -> Self {
        Self {
            frame_count: 0,
            rng_state: 0xDEAD_BEEF_CAFE_1234,
        }
    }

    /// Simple xorshift64 PRNG.  Returns a value in 0.0 .. 1.0 (exclusive).
    pub fn next_random(&mut self) -> f64 {
        let mut s = self.rng_state;
        s ^= s << 13;
        s ^= s >> 7;
        s ^= s << 17;
        self.rng_state = s;
        (s as f64) / (u64::MAX as f64)
    }
}
