/// Lightweight bookkeeping shared between desktop and embedded targets.
pub struct GameState {
    pub frame_count: u32,
    pub rng_state: u64,
    /// Actual measured frames per second (set by the main loop each frame).
    pub actual_fps: f32,
    /// Target FPS: 30 or 60. Set by the main loop based on _update60 detection.
    pub target_fps: u8,
}

impl GameState {
    pub fn new() -> Self {
        Self {
            frame_count: 0,
            rng_state: 0xDEAD_BEEF_CAFE_1234,
            actual_fps: 30.0,
            target_fps: 30,
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

impl Default for GameState {
    fn default() -> Self {
        Self::new()
    }
}
