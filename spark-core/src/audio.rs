// audio.rs — PICO-8 compatible audio engine for Spark (no_std)
//
// Implements SFX and music data structures matching PICO-8's format,
// a working state machine for playback tracking (note advancement, looping,
// pattern transitions), and waveform generation primitives.

// ---------------------------------------------------------------------------
// Note / Sfx / MusicPattern data structures
// ---------------------------------------------------------------------------

/// A single note inside a sound effect.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Note {
    /// Pitch value 0-63 (C-0 = 0, C#-0 = 1, ... C-1 = 12, ...).
    pub pitch: u8,
    /// Waveform index 0-7.
    pub waveform: u8,
    /// Volume 0-7.
    pub volume: u8,
    /// Effect 0-7 (0=none, 1=slide, 2=vibrato, 3=drop, 4=fade_in,
    ///   5=fade_out, 6=arpeggio_fast, 7=arpeggio_slow).
    pub effect: u8,
}

impl Note {
    /// A silent note (all fields zero). `const` so cart/audio state can be
    /// built at compile time and placed in a `static` on the firmware.
    pub const fn new() -> Self {
        Note {
            pitch: 0,
            waveform: 0,
            volume: 0,
            effect: 0,
        }
    }
}

/// A sound effect consisting of 32 notes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Sfx {
    pub notes: [Note; 32],
    /// Ticks per note (1-255). PICO-8 default is 16 (~0.5 s/note at 30 fps).
    pub speed: u8,
    /// Loop start note index (0-31). 0 means no loop unless loop_end > 0.
    pub loop_start: u8,
    /// Loop end note index (0-31). 0 means no loop.
    pub loop_end: u8,
}

impl Sfx {
    /// An empty SFX with PICO-8's default speed of 16.
    pub const fn new() -> Self {
        Sfx {
            notes: [Note::new(); 32],
            speed: 16,
            loop_start: 0,
            loop_end: 0,
        }
    }
}

impl Default for Sfx {
    fn default() -> Self {
        Self::new()
    }
}

/// A music pattern referencing up to 4 SFX channels.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MusicPattern {
    /// SFX index for each of the 4 channels. Bit 6 set means "channel disabled".
    pub channels: [u8; 4],
    /// Flags: bit 0 = loop start, bit 1 = loop end, bit 2 = stop at end.
    pub flags: u8,
}

impl MusicPattern {
    /// An empty pattern: all channels disabled (bit 6 set = 0x40).
    pub const fn new() -> Self {
        MusicPattern {
            channels: [0x40; 4],
            flags: 0,
        }
    }
}

impl Default for MusicPattern {
    fn default() -> Self {
        Self::new()
    }
}

impl MusicPattern {
    /// Returns true if the given channel is enabled (bit 6 clear).
    pub fn channel_enabled(&self, ch: usize) -> bool {
        ch < 4 && (self.channels[ch] & 0x40) == 0
    }

    /// Returns the SFX index for a channel (lower 6 bits).
    pub fn sfx_index(&self, ch: usize) -> u8 {
        self.channels[ch] & 0x3F
    }

    pub fn is_loop_start(&self) -> bool {
        self.flags & 1 != 0
    }

    pub fn is_loop_end(&self) -> bool {
        self.flags & 2 != 0
    }

    pub fn is_stop(&self) -> bool {
        self.flags & 4 != 0
    }
}

// ---------------------------------------------------------------------------
// Playback state helpers
// ---------------------------------------------------------------------------

/// Tracks the playback position of a single SFX on one channel.
#[derive(Clone, Debug)]
pub struct SfxPlayer {
    /// Which SFX (0-63) is playing.
    pub sfx_index: u8,
    /// Current note position (0-31). `NOTES_PER_SFX` (32) marks a finished SFX.
    pub note_index: u8,
    /// Oscillator phase accumulator (0.0 .. 1.0), used for waveform generation.
    pub phase: f64,
    /// Starting note offset (for partial playback via `sfx(n, ch, offset, length)`).
    pub offset: u8,
    /// Number of notes to play. 0 means "play all 32".
    pub length: u8,
    /// Timestamp (frame_counter) when playback started — used for stealing
    /// the oldest channel.
    pub start_frame: u32,
    /// Sample position within the current note (for sub-frame audio generation).
    pub sample_pos: u32,
    /// Absolute sample counter since this SFX started (never reset at note
    /// boundaries) — drives continuous time-based effects (vibrato, arpeggio).
    pub abs_sample: u32,
    /// Previous note's pitch and volume, captured at each note boundary, so the
    /// slide effect can interpolate FROM the previous note TO the current one
    /// (matching PICO-8). Initialised to the SFX's first note baseline.
    pub prev_key: u8,
    pub prev_vol: f32,
    /// One-pole noise filter state and PRNG, for the noise waveform (6). PICO-8
    /// noise is a key-filtered random walk with carried state, not stateless.
    pub noise_last_sample: f64,
    pub rng: u32,
    /// Unwrapped cumulative oscillator phase in cycles (zepto8 `phi`), never
    /// wrapped; used by the phaser waveform's detuned second partial.
    pub advance: f64,
    /// True when this channel was started by the music player (as opposed to a
    /// user `sfx()` call). `music(-1)` and a music restart stop only these.
    pub is_music: bool,

    // --- Per-note cache, refreshed at note boundaries (see `refresh_note`) ---
    /// Sample rate the cache below was computed for.
    cached_rate: u32,
    /// Samples per note at `cached_rate` for the SFX's speed (>= 1).
    spn: u32,
    /// Base frequency (Hz) of the current note's pitch.
    base_freq: f64,
    /// Base volume (0.0 .. 1.0) of the current note.
    base_vol: f64,
    /// Frequency of `prev_key` (slide origin).
    prev_freq: f64,
}

impl SfxPlayer {
    /// Start playing `sfx_index` from note `offset` for `length` notes (0 =
    /// all). `first_pitch` seeds the slide origin (the SFX's own first note).
    fn new(
        sfx_index: u8,
        offset: u8,
        length: u8,
        start_frame: u32,
        first_pitch: u8,
        is_music: bool,
    ) -> Self {
        SfxPlayer {
            sfx_index,
            note_index: offset,
            phase: 0.0,
            offset,
            length,
            start_frame,
            sample_pos: 0,
            abs_sample: 0,
            prev_key: first_pitch,
            prev_vol: 0.0,
            noise_last_sample: 0.0,
            rng: 0x2545_F491, // non-zero seed (xorshift32 has 0 as a fixed point)
            advance: 0.0,
            is_music,
            // `cached_rate == 0` is never a valid rate (generate_samples returns
            // early for 0), so it marks the cache as needing a refresh.
            cached_rate: 0,
            spn: 1,
            base_freq: 0.0,
            base_vol: 0.0,
            prev_freq: 0.0,
        }
    }

    /// Recompute the per-note derived values for the current note. Called at
    /// each note boundary (and lazily on the first sample) so the per-sample
    /// hot path does no `pow`/division work. Note data edited mid-note takes
    /// effect at the next note boundary.
    fn refresh_note(&mut self, sfx: &Sfx, sample_rate: u32) {
        self.cached_rate = sample_rate;
        self.spn = Audio::samples_per_note(sfx.speed, sample_rate).max(1);
        if let Some(note) = sfx.notes.get(self.note_index as usize) {
            self.base_freq = Audio::pitch_to_freq(note.pitch);
            self.base_vol = note.volume as f64 / 7.0;
        }
        self.prev_freq = Audio::pitch_to_freq(self.prev_key);
    }
}

/// Tracks music (pattern) playback state.
#[derive(Clone, Copy, Debug)]
pub struct MusicPlayer {
    /// Current pattern index (0-63).
    pub pattern_index: i32,
    /// The pattern index that was identified as loop-start while scanning
    /// forward, so we know where to jump back on loop-end.
    pub loop_back: i32,
    /// Channels reserved for music (bit n = channel n), from `music()`'s
    /// `channel_mask`. `sfx(n, -1, ...)` auto-assignment avoids these.
    pub channel_mask: u8,
}

// ---------------------------------------------------------------------------
// Audio engine
// ---------------------------------------------------------------------------

/// Number of SFX / music pattern slots (matches PICO-8).
pub const NUM_SFX: usize = 64;
pub const NUM_MUSIC: usize = 64;
/// Number of audio channels.
pub const NUM_CHANNELS: usize = 4;
/// Notes per SFX.
pub const NOTES_PER_SFX: usize = 32;
/// Sample rate for generated audio.
pub const SAMPLE_RATE: u32 = 44100;
/// Frames per second (PICO-8 runs at 30 fps by default).
pub const FPS: u32 = 30;

/// Frequency in Hz for each PICO-8 pitch 0-63: `440 * 2^((pitch - 33) / 12)`
/// (reference `key_to_freq`; pitch 0 = C2 = 65.4064 Hz). Precomputed so the
/// per-sample synth path never calls `pow`.
const PITCH_FREQ: [f64; 64] = [
    65.40639132514966,
    69.29565774421802,
    73.41619197935188,
    77.78174593052023,
    82.4068892282175,
    87.30705785825097,
    92.4986056779086,
    97.99885899543733,
    103.82617439498628,
    110.0,
    116.54094037952248,
    123.47082531403103,
    130.8127826502993,
    138.59131548843604,
    146.8323839587038,
    155.56349186104046,
    164.81377845643496,
    174.61411571650194,
    184.9972113558172,
    195.99771799087463,
    207.65234878997256,
    220.0,
    233.08188075904496,
    246.94165062806206,
    261.6255653005986,
    277.1826309768721,
    293.6647679174076,
    311.1269837220809,
    329.6275569128699,
    349.2282314330039,
    369.9944227116344,
    391.99543598174927,
    415.3046975799451,
    440.0,
    466.1637615180899,
    493.8833012561241,
    523.2511306011972,
    554.3652619537442,
    587.3295358348151,
    622.2539674441618,
    659.2551138257398,
    698.4564628660078,
    739.9888454232688,
    783.9908719634985,
    830.6093951598903,
    880.0,
    932.3275230361799,
    987.7666025122483,
    1046.5022612023945,
    1108.7305239074883,
    1174.6590716696303,
    1244.5079348883237,
    1318.5102276514797,
    1396.9129257320155,
    1479.9776908465376,
    1567.981743926997,
    1661.2187903197805,
    1760.0,
    1864.6550460723597,
    1975.533205024496,
    2093.004522404789,
    2217.4610478149766,
    2349.31814333926,
    2489.0158697766474,
];

pub struct Audio {
    /// 64 sound effects.
    pub sfx_data: [Sfx; NUM_SFX],
    /// 64 music patterns.
    pub music_data: [MusicPattern; NUM_MUSIC],

    /// Per-channel SFX playback state. `None` means the channel is free.
    pub sfx_playing: [Option<SfxPlayer>; NUM_CHANNELS],
    /// Music playback state. `None` means music is stopped.
    pub music_playing: Option<MusicPlayer>,

    /// Monotonically increasing frame counter (bumped each `update()`).
    pub frame_counter: u32,
}

impl Audio {
    // -----------------------------------------------------------------------
    // Construction
    // -----------------------------------------------------------------------

    /// `const` so the firmware can keep the audio engine in a `static`
    /// instead of building a ~17 KB temporary on the stack at boot.
    pub const fn new() -> Self {
        Audio {
            sfx_data: [Sfx::new(); NUM_SFX],
            music_data: [MusicPattern::new(); NUM_MUSIC],
            sfx_playing: [None, None, None, None],
            music_playing: None,
            frame_counter: 0,
        }
    }
}

impl Default for Audio {
    fn default() -> Self {
        Self::new()
    }
}

impl Audio {
    // -----------------------------------------------------------------------
    // SFX trigger
    // -----------------------------------------------------------------------

    /// Trigger (or stop) a sound effect.
    ///
    /// * `n`       — SFX index 0-63. -1 = stop `channel`, -2 = stop all.
    /// * `channel` — Channel 0-3 to play on. -1 = auto-assign to the first
    ///   free channel (skipping channels reserved by `music()`'s mask), or
    ///   steal the oldest one.
    /// * `offset`  — Starting note (0-31).
    /// * `length`  — Number of notes to play. 0 or negative = all remaining.
    pub fn sfx(&mut self, n: i32, channel: i32, offset: i32, length: i32) {
        // --- stop requests ---
        if n == -2 {
            // Stop all channels.
            for ch in self.sfx_playing.iter_mut() {
                *ch = None;
            }
            return;
        }
        if n == -1 {
            // Stop the specified channel (if valid).
            if channel >= 0 && (channel as usize) < NUM_CHANNELS {
                self.sfx_playing[channel as usize] = None;
            }
            return;
        }

        // Clamp / validate.
        let sfx_idx = n.clamp(0, (NUM_SFX - 1) as i32) as u8;
        let note_offset = offset.clamp(0, (NOTES_PER_SFX - 1) as i32) as u8;
        let note_length = if length <= 0 {
            0u8
        } else {
            // Clamp so that offset + length never exceeds the note count;
            // otherwise the SFX would play past its real end.
            let max_len = NOTES_PER_SFX as i32 - note_offset as i32;
            length.min(max_len) as u8
        };

        // Determine which channel to use.
        let ch = if channel >= 0 && (channel as usize) < NUM_CHANNELS {
            channel as usize
        } else {
            self.auto_channel()
        };

        let first_pitch = self.sfx_data[sfx_idx as usize].notes[note_offset as usize].pitch;
        self.sfx_playing[ch] = Some(SfxPlayer::new(
            sfx_idx,
            note_offset,
            note_length,
            self.frame_counter,
            first_pitch,
            false,
        ));
    }

    /// Pick a channel for `sfx(n, -1, ...)`: the first free non-reserved
    /// channel, else the oldest non-reserved one. If the music mask reserves
    /// every channel, all channels are candidates.
    fn auto_channel(&self) -> usize {
        let reserved = self.music_playing.map_or(0, |m| m.channel_mask & 0x0F);
        let mask = if reserved == 0x0F { 0 } else { reserved };
        let candidates = (0..NUM_CHANNELS).filter(|&ch| mask & (1 << ch) == 0);

        let mut oldest_ch = 0usize;
        let mut oldest_frame = u32::MAX;
        for ch in candidates {
            match &self.sfx_playing[ch] {
                None => return ch,
                Some(p) if p.start_frame < oldest_frame => {
                    oldest_frame = p.start_frame;
                    oldest_ch = ch;
                }
                Some(_) => {}
            }
        }
        oldest_ch
    }

    // -----------------------------------------------------------------------
    // Music trigger
    // -----------------------------------------------------------------------

    /// Start or stop music playback.
    ///
    /// * `n`            — Pattern index 0-63, or -1 to stop.
    /// * `_fade_len`    — Fade length in milliseconds (ignored for MVP).
    /// * `channel_mask` — Bitmask of channels reserved for music (bit n =
    ///   channel n); `sfx(n, -1, ...)` auto-assignment avoids them. 0 = none.
    pub fn music(&mut self, n: i32, _fade_len: i32, channel_mask: i32) {
        // Any channels the current music owns stop; user sfx() channels stay.
        self.stop_music_channels();

        if n < 0 {
            self.music_playing = None;
            return;
        }

        let pattern_idx = n.clamp(0, (NUM_MUSIC - 1) as i32);

        // Scan backwards from `pattern_idx` to find the nearest loop-start
        // flag (or just use pattern_idx itself).
        let mut loop_back = pattern_idx;
        for i in (0..=pattern_idx).rev() {
            if self.music_data[i as usize].is_loop_start() {
                loop_back = i;
                break;
            }
        }

        self.music_playing = Some(MusicPlayer {
            pattern_index: pattern_idx,
            loop_back,
            channel_mask: (channel_mask.clamp(0, 0x0F)) as u8,
        });

        // Load the first pattern's SFX into channels.
        self.load_pattern_into_channels(pattern_idx as usize);
    }

    /// Stop every channel that the music player started.
    fn stop_music_channels(&mut self) {
        for slot in self.sfx_playing.iter_mut() {
            if slot.as_ref().is_some_and(|p| p.is_music) {
                *slot = None;
            }
        }
    }

    /// Load the SFX references from a music pattern into the channels it
    /// enables. Channels the pattern does not use are left alone so a user
    /// `sfx()` playing there is not cut off by a pattern change.
    fn load_pattern_into_channels(&mut self, pattern_idx: usize) {
        if pattern_idx >= NUM_MUSIC {
            return;
        }
        let pattern = self.music_data[pattern_idx];
        for ch in 0..NUM_CHANNELS {
            if pattern.channel_enabled(ch) {
                let sidx = pattern.sfx_index(ch);
                let first_pitch = self.sfx_data[sidx as usize].notes[0].pitch;
                self.sfx_playing[ch] = Some(SfxPlayer::new(
                    sidx,
                    0,
                    0, // play all
                    self.frame_counter,
                    first_pitch,
                    true,
                ));
            }
        }
    }

    // -----------------------------------------------------------------------
    // Per-frame update
    // -----------------------------------------------------------------------

    /// Advance the music player by one frame.
    fn advance_music(&mut self, mplayer: &MusicPlayer) {
        let pattern_idx = mplayer.pattern_index as usize;
        if pattern_idx >= NUM_MUSIC {
            self.music_playing = None;
            return;
        }

        let pattern = self.music_data[pattern_idx];

        // Check whether any enabled channel still has an active music SFX.
        let any_channel_active = (0..NUM_CHANNELS).any(|ch| {
            pattern.channel_enabled(ch) && self.sfx_playing[ch].as_ref().is_some_and(|p| p.is_music)
        });

        if any_channel_active {
            return;
        }

        // --- Pattern transition ---
        let current = mplayer.pattern_index;
        let loop_back = mplayer.loop_back;
        let channel_mask = mplayer.channel_mask;

        if pattern.is_stop() {
            self.music_playing = None;
            return;
        }

        if pattern.is_loop_end() {
            let next = loop_back;
            self.music_playing = Some(MusicPlayer {
                pattern_index: next,
                loop_back,
                channel_mask,
            });
            self.load_pattern_into_channels(next as usize);
            return;
        }

        // Move to next pattern.
        let next = current + 1;
        if next >= NUM_MUSIC as i32 {
            self.music_playing = None;
            return;
        }

        let new_loop_back = if self.music_data[next as usize].is_loop_start() {
            next
        } else {
            loop_back
        };

        self.music_playing = Some(MusicPlayer {
            pattern_index: next,
            loop_back: new_loop_back,
            channel_mask,
        });
        self.load_pattern_into_channels(next as usize);
    }

    // -----------------------------------------------------------------------
    // Audio generation
    // -----------------------------------------------------------------------

    /// Per-channel mix volume to prevent clipping when all 4 channels are active.
    const MIX_VOLUME: f64 = 0.25;

    /// Compute how many audio samples correspond to one note at the given speed.
    ///
    /// PICO-8 advances one note every `183 * speed` samples at its internal
    /// 22050 Hz rate (verified against zepto8 `sfx.cpp` and fake-08
    /// `Audio.cpp`); scaled here to the output `sample_rate`. The previous
    /// `speed/128 s` model ran ~6% fast (344 vs 366 samples/speed at 44100).
    pub fn samples_per_note(speed: u8, sample_rate: u32) -> u32 {
        let s = speed.max(1) as f64;
        ((183.0 * s) * (sample_rate as f64 / 22050.0)) as u32
    }

    /// Generate mono f32 audio samples for one frame into a caller-provided buffer.
    ///
    /// This avoids allocating a new `Vec` every frame, which matters on embedded
    /// targets where ~6 KB allocations at 30 fps cause heap fragmentation.
    ///
    /// `sample_rate` — e.g. 44100.
    /// `buf`         — output buffer; every element is overwritten with a sample
    ///                 in the range [-1.0, 1.0].
    pub fn generate_samples(&mut self, sample_rate: u32, buf: &mut [f32]) {
        // A zero sample rate would make the per-sample phase increment
        // (`freq / sample_rate`) infinite, poisoning the waveform with NaN/Inf.
        // Real audio is never 0 Hz; emit silence for a degenerate rate.
        if sample_rate == 0 {
            buf.fill(0.0);
            return;
        }
        let inv_rate = 1.0 / sample_rate as f64;
        for sample in &mut *buf {
            let mut mix: f64 = 0.0;

            for ch in 0..NUM_CHANNELS {
                if let Some(ref mut player) = self.sfx_playing[ch] {
                    let sfx_idx = player.sfx_index as usize;
                    if sfx_idx >= NUM_SFX {
                        continue;
                    }
                    let sfx = &self.sfx_data[sfx_idx];

                    // Determine current note
                    let ni = player.note_index as usize;
                    if ni >= NOTES_PER_SFX {
                        continue;
                    }
                    let note = sfx.notes[ni];

                    if player.cached_rate != sample_rate {
                        player.refresh_note(sfx, sample_rate);
                    }
                    let spn = player.spn;

                    if note.volume > 0 {
                        let progress = player.sample_pos as f64 / spn as f64;
                        // Absolute seconds since the SFX started (drives the
                        // continuous-time vibrato and arpeggio).
                        let secs = player.abs_sample as f64 * inv_rate;

                        let (freq, vol) = Self::apply_effect(
                            note.effect,
                            player.base_freq,
                            player.base_vol,
                            progress,
                            secs,
                            player.prev_freq,
                            player.prev_vol as f64,
                            ni,
                            sfx.speed,
                            &sfx.notes,
                        );

                        // Generate the waveform sample. Noise (6) and phaser (7)
                        // are stateful and produced here; the rest are stateless.
                        let wf = note.waveform & 0x07;
                        let sample = if wf == 6 {
                            // One-pole filtered random walk, key-dependent gain
                            // (zepto8 INST_NOISE). xorshift32 PRNG in [-1, 1].
                            player.rng ^= player.rng << 13;
                            player.rng ^= player.rng >> 17;
                            player.rng ^= player.rng << 5;
                            let r = (player.rng as f64 / u32::MAX as f64) * 2.0 - 1.0;
                            const TSCALE: f64 = 8.858923; // 22050 / key_to_freq(63)
                                                          // scale = (freq / 22050) * TSCALE, sample-rate-independent.
                            let scale = (freq / 22050.0) * TSCALE;
                            let new_sample = (player.noise_last_sample + scale * r) / (1.0 + scale);
                            player.noise_last_sample = new_sample;
                            let factor = 1.0 - note.pitch as f64 / 63.0;
                            new_sample * 1.5 * (1.0 + factor * factor)
                        } else if wf == 7 {
                            // Sum of two detuned triangles (zepto8 INST_PHASER),
                            // using the unwrapped `advance` for the second partial.
                            let t = player.phase;
                            let mut ret = 2.0 - libm::fabs(8.0 * t - 4.0);
                            let a2 = player.advance * 109.0 / 110.0;
                            let t2 = a2 - libm::floor(a2);
                            ret += 1.0 - libm::fabs(4.0 * t2 - 2.0);
                            ret / 6.0
                        } else {
                            Self::waveform_sample(note.waveform, player.phase)
                        };
                        mix += sample * vol * Self::MIX_VOLUME;

                        // Advance phase (wrapped) and the unwrapped accumulator.
                        let phase_inc = freq * inv_rate;
                        player.phase += phase_inc;
                        if player.phase >= 1.0 {
                            player.phase -= libm::floor(player.phase);
                        }
                        player.advance += phase_inc;
                    }

                    // Advance sample positions. `abs_sample` is never reset on a
                    // note boundary so time-based effects stay continuous.
                    player.sample_pos += 1;
                    player.abs_sample = player.abs_sample.wrapping_add(1);
                    if player.sample_pos >= spn {
                        player.sample_pos = 0;
                        // Capture the note that just finished as the slide origin
                        // for the next note.
                        player.prev_key = note.pitch;
                        player.prev_vol = note.volume as f32 / 7.0;
                        player.note_index += 1;

                        let end_note = if player.length > 0 {
                            player.offset + player.length
                        } else {
                            NOTES_PER_SFX as u8
                        };

                        // Check loop first (loop_end takes priority)
                        let has_loop = sfx.loop_end > 0
                            && sfx.loop_end > sfx.loop_start
                            && sfx.loop_start >= player.offset;
                        if has_loop && player.note_index >= sfx.loop_end {
                            player.note_index = sfx.loop_start;
                        } else if player.note_index >= end_note {
                            // No loop or not at loop boundary — SFX is done
                            player.note_index = NOTES_PER_SFX as u8;
                        }
                        player.refresh_note(sfx, sample_rate);
                    }
                }
            }

            // `f64::clamp` propagates NaN, so guard explicitly: a non-finite
            // mix (from any future synth edge case) must never reach the audio
            // hardware as NaN/Inf — emit silence for that sample instead.
            *sample = if mix.is_finite() {
                mix.clamp(-1.0, 1.0) as f32
            } else {
                0.0
            };
        }

        // Remove finished channels
        for ch in 0..NUM_CHANNELS {
            if let Some(ref player) = self.sfx_playing[ch] {
                if player.note_index as usize >= NOTES_PER_SFX {
                    self.sfx_playing[ch] = None;
                }
            }
        }

        // Advance the per-frame state machine. `generate_samples` produces
        // exactly one frame of audio and is the single authority for playback
        // progression (both desktop and embedded call only this method), so it
        // is the one place that drives the frame clock and advances music
        // patterns once the current pattern's channels have all finished.
        self.frame_counter = self.frame_counter.wrapping_add(1);
        if let Some(mplayer) = self.music_playing {
            self.advance_music(&mplayer);
        }
    }

    /// Apply a PICO-8 note effect to the base frequency and volume, returning
    /// `(modified_freq, modified_vol)`. Semantics match zepto8 `sfx.cpp` /
    /// fake-08 `Audio.cpp`:
    ///
    /// * `progress` — position within the current note, 0.0..1.0.
    /// * `secs` — absolute seconds elapsed since the SFX started (drives the
    ///   time-based vibrato and arpeggio; speed-independent in the reference).
    /// * `prev_freq` / `prev_vol` — the previous note's pitch/volume, for slide.
    /// * `note_id` / `speed` / `sfx_notes` — for arpeggio, which cycles through
    ///   the SFX's own four-note group rather than a fixed triad.
    #[allow(clippy::too_many_arguments)]
    fn apply_effect(
        effect: u8,
        base_freq: f64,
        base_vol: f64,
        progress: f64,
        secs: f64,
        prev_freq: f64,
        prev_vol: f64,
        note_id: usize,
        speed: u8,
        sfx_notes: &[Note; NOTES_PER_SFX],
    ) -> (f64, f64) {
        // Ratio of one semitone up, for the vibrato pitch mix.
        const SEMITONE: f64 = 1.059_463_094_359_295; // 2^(1/12)
        match effect {
            // 0 — None.
            0 => (base_freq, base_vol),
            // 1 — Slide: interpolate FROM the previous note's pitch/volume TO
            // this note's over the note (reference uses prev_key/prev_vol).
            1 => {
                let freq = prev_freq + (base_freq - prev_freq) * progress;
                let vol = if prev_vol > 0.0 {
                    prev_vol + (base_vol - prev_vol) * progress
                } else {
                    base_vol
                };
                (freq, vol)
            }
            // 2 — Vibrato: 7.5 Hz triangle, half-semitone depth. The reference
            // mixes linearly between freq and freq*2^(1/12) with t in
            // [-0.25, 0.25]: t = |frac(7.5*secs) - 0.5| - 0.25.
            2 => {
                let t = libm::fabs(libm::fmod(7.5 * secs, 1.0) - 0.5) - 0.25;
                let freq = base_freq + (base_freq * SEMITONE - base_freq) * t;
                (freq, base_vol)
            }
            // 3 — Drop: pitch falls to zero across the note.
            3 => (base_freq * (1.0 - progress), base_vol),
            // 4 — Fade in.
            4 => (base_freq, base_vol * progress),
            // 5 — Fade out.
            5 => (base_freq, base_vol * (1.0 - progress)),
            // 6 / 7 — Arpeggio (fast / slow): cycle through the current
            // four-note group of the SFX. Step count m and rate per the
            // reference; n indexes into the group (note_id & !3) | (n & 3).
            6 | 7 => {
                let fast = effect == 6;
                let m: i64 = (if speed <= 8 { 32 } else { 16 }) / (if fast { 4 } else { 8 });
                let n = (m as f64 * 7.5 * secs) as i64;
                let arp_idx = (((note_id as i64) & !3) | (n & 3)) as usize;
                let key = sfx_notes[arp_idx & (NOTES_PER_SFX - 1)].pitch;
                (Self::pitch_to_freq(key), base_vol)
            }
            _ => (base_freq, base_vol),
        }
    }

    /// Convert a PICO-8 pitch value (0-63) to a frequency in Hz.
    ///
    /// Pitch 0 corresponds to C-2 in PICO-8, which is ~65.41 Hz. Each
    /// increment is one semitone (equal temperament). Out-of-range pitches
    /// (only possible via direct struct mutation; the cart format is 6-bit)
    /// clamp to 63.
    pub fn pitch_to_freq(pitch: u8) -> f64 {
        PITCH_FREQ[(pitch as usize).min(PITCH_FREQ.len() - 1)]
    }

    /// Generate one sample for the given waveform at `phase`, which must
    /// already be wrapped into `0.0 ..< 1.0` (the synth keeps it so).
    ///
    /// Shapes and amplitudes match zepto8 `synth.cpp` (which were measured from
    /// real PICO-8 WAV exports), so each instrument has its own peak level
    /// rather than a uniform ±1.0. A compensating master gain in
    /// [`Self::generate_samples`] keeps overall loudness comparable.
    ///
    /// Waveforms 6 (noise) and 7 (phaser) are stateful — they need the player's
    /// carried filter state / unwrapped phase — so they are produced inline in
    /// `generate_samples` and return 0.0 here.
    pub fn waveform_sample(waveform: u8, phase: f64) -> f64 {
        let t = phase;
        match waveform & 0x07 {
            // 0 — Triangle: 1 - |4t - 2|, scaled to ±0.5.
            0 => (1.0 - libm::fabs(4.0 * t - 2.0)) * 0.5,
            // 1 — Tilted saw: ramp up to t=0.875 then steeply down, ±0.5.
            1 => {
                let a = 0.875;
                let r = if t < a {
                    2.0 * t / a - 1.0
                } else {
                    2.0 * (1.0 - t) / (1.0 - a) - 1.0
                };
                r * 0.5
            }
            // 2 — Saw: drop in the MIDDLE of the period (t<0.5 ? t : t-1), ×0.653.
            2 => {
                let r = if t < 0.5 { t } else { t - 1.0 };
                0.653 * r
            }
            // 3 — Square (50% duty), ±0.25.
            3 => {
                if t < 0.5 {
                    0.25
                } else {
                    -0.25
                }
            }
            // 4 — Pulse (~31.6% duty), ±0.25.
            4 => {
                if t < 0.316 {
                    0.25
                } else {
                    -0.25
                }
            }
            // 5 — Organ: two triangle segments, /9 (zepto8 INST_ORGAN).
            5 => {
                let ret = if t < 0.5 {
                    3.0 - libm::fabs(24.0 * t - 6.0)
                } else {
                    1.0 - libm::fabs(16.0 * t - 12.0)
                };
                ret / 9.0
            }
            // 6 (noise) and 7 (phaser) are stateful — handled in generate_samples.
            _ => 0.0,
        }
    }

    // -----------------------------------------------------------------------
    // Query helpers (useful for Lua `stat()` calls)
    // -----------------------------------------------------------------------

    /// Returns the SFX index currently playing on `channel`, or -1 if none.
    pub fn sfx_on_channel(&self, channel: usize) -> i32 {
        if channel < NUM_CHANNELS {
            self.sfx_playing[channel]
                .as_ref()
                .map_or(-1, |p| p.sfx_index as i32)
        } else {
            -1
        }
    }

    /// Returns the current note index on `channel`, or -1 if not playing.
    pub fn note_on_channel(&self, channel: usize) -> i32 {
        if channel < NUM_CHANNELS {
            self.sfx_playing[channel]
                .as_ref()
                .map_or(-1, |p| p.note_index as i32)
        } else {
            -1
        }
    }

    /// Returns the current music pattern index, or -1 if music is stopped.
    pub fn current_music_pattern(&self) -> i32 {
        self.music_playing.as_ref().map_or(-1, |m| m.pattern_index)
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;
    use alloc::vec::Vec;

    #[test]
    fn test_generate_samples_always_finite() {
        // Regression (audio fuzzer): malformed SFX data and/or a degenerate
        // sample rate must never produce NaN/Inf samples (which would glitch
        // the audio hardware).
        let mut audio = Audio::new();
        // Fill SFX 0 with extreme/garbage values and play it.
        for note in audio.sfx_data[0].notes.iter_mut() {
            note.pitch = 255;
            note.waveform = 255;
            note.volume = 255;
            note.effect = 255;
        }
        audio.sfx_data[0].speed = 0;
        audio.sfx(0, 0, 0, 0);

        let mut buf = vec![0f32; 512];
        // Degenerate sample rate (would otherwise divide by zero -> Inf phase).
        audio.generate_samples(0, &mut buf);
        assert!(buf.iter().all(|s| s.is_finite()));
        // Normal and extreme rates.
        for &rate in &[1u32, 44100, u32::MAX] {
            audio.sfx(0, 0, 0, 0);
            audio.generate_samples(rate, &mut buf);
            assert!(
                buf.iter().all(|s| s.is_finite()),
                "non-finite sample at rate {rate}"
            );
        }
    }

    #[test]
    fn test_new_audio() {
        let audio = Audio::new();
        assert_eq!(audio.sfx_data.len(), NUM_SFX);
        assert_eq!(audio.music_data.len(), NUM_MUSIC);
        assert!(audio.sfx_playing.iter().all(|c| c.is_none()));
        assert!(audio.music_playing.is_none());
        assert_eq!(audio.frame_counter, 0);
    }

    #[test]
    fn test_pitch_to_freq() {
        let f0 = Audio::pitch_to_freq(0);
        assert!((f0 - 65.41).abs() < 0.01);

        let f12 = Audio::pitch_to_freq(12);
        assert!((f12 - 130.81).abs() < 0.1);

        let f24 = Audio::pitch_to_freq(24);
        assert!((f24 - 261.63).abs() < 0.5);
    }

    #[test]
    fn test_waveform_samples() {
        // Amplitudes match zepto8's measured-from-PICO-8 levels (not ±1.0).
        // Triangle: ±0.5, peak at t=0.5, min at t=0/1.
        assert!((Audio::waveform_sample(0, 0.0) - (-0.5)).abs() < 1e-9);
        assert!((Audio::waveform_sample(0, 0.25) - 0.0).abs() < 1e-9);
        assert!((Audio::waveform_sample(0, 0.5) - 0.5).abs() < 1e-9);

        // Saw: drop in the MIDDLE of the period, ×0.653 (range ~±0.326).
        assert!((Audio::waveform_sample(2, 0.0) - 0.0).abs() < 1e-9);
        assert!((Audio::waveform_sample(2, 0.25) - 0.653 * 0.25).abs() < 1e-9);
        assert!((Audio::waveform_sample(2, 0.5) - 0.653 * (-0.5)).abs() < 1e-9);

        // Square: ±0.25, 50% duty.
        assert!((Audio::waveform_sample(3, 0.25) - 0.25).abs() < 1e-9);
        assert!((Audio::waveform_sample(3, 0.75) - (-0.25)).abs() < 1e-9);

        // Pulse: ±0.25, ~31.6% duty (0.1 high, 0.5 low).
        assert!((Audio::waveform_sample(4, 0.1) - 0.25).abs() < 1e-9);
        assert!((Audio::waveform_sample(4, 0.5) - (-0.25)).abs() < 1e-9);

        // Organ: two-triangle formula, /9. At t=0: (3-6)/9 = -1/3.
        assert!((Audio::waveform_sample(5, 0.0) - (-1.0 / 3.0)).abs() < 1e-9);

        // Noise (6) and phaser (7) are stateful; the stateless helper returns 0.
        assert_eq!(Audio::waveform_sample(6, 0.3), 0.0);
        assert_eq!(Audio::waveform_sample(7, 0.3), 0.0);
    }

    #[test]
    fn test_sfx_trigger_and_stop() {
        let mut audio = Audio::new();
        audio.sfx(5, 0, 0, 0);
        assert!(audio.sfx_playing[0].is_some());
        assert_eq!(audio.sfx_playing[0].as_ref().unwrap().sfx_index, 5);

        audio.sfx(-1, 0, 0, 0);
        assert!(audio.sfx_playing[0].is_none());
    }

    #[test]
    fn test_sfx_auto_assign() {
        let mut audio = Audio::new();
        audio.sfx(1, -1, 0, 0);
        audio.sfx(2, -1, 0, 0);
        audio.sfx(3, -1, 0, 0);
        audio.sfx(4, -1, 0, 0);

        for ch in 0..4 {
            assert!(audio.sfx_playing[ch].is_some());
        }

        audio.sfx(5, -1, 0, 0);
        assert_eq!(audio.sfx_playing[0].as_ref().unwrap().sfx_index, 5);
    }

    #[test]
    fn test_sfx_stop_all() {
        let mut audio = Audio::new();
        audio.sfx(1, 0, 0, 0);
        audio.sfx(2, 1, 0, 0);

        audio.sfx(-2, 0, 0, 0);
        assert!(audio.sfx_playing.iter().all(|c| c.is_none()));
    }

    #[test]
    fn test_sfx_generate_advances_note_index() {
        let mut audio = Audio::new();
        audio.sfx_data[0].notes[0].volume = 5;
        audio.sfx_data[0].notes[0].pitch = 24;
        audio.sfx_data[0].speed = 8;

        audio.sfx(0, 0, 0, 0);
        assert_eq!(audio.sfx_playing[0].as_ref().unwrap().note_index, 0);

        // generate_samples is the single playback authority: rendering one
        // note's worth of samples must advance note_index by exactly one.
        let spn = Audio::samples_per_note(8, 44100).max(1) as usize;
        let mut samples = vec![0.0f32; spn];
        audio.generate_samples(44100, &mut samples);

        let ni = audio.sfx_playing[0].as_ref().unwrap().note_index;
        assert_eq!(ni, 1, "expected note_index 1, got {ni}");
    }

    #[test]
    fn test_sfx_finishes() {
        let mut audio = Audio::new();
        audio.sfx_data[0].speed = 1;

        audio.sfx(0, 0, 0, 0);

        // Render enough samples to play through all 32 notes; the channel
        // should then have been cleared by generate_samples.
        let spn = Audio::samples_per_note(1, 44100).max(1) as usize;
        let mut samples = vec![0.0f32; spn * (NOTES_PER_SFX + 1)];
        audio.generate_samples(44100, &mut samples);

        assert!(audio.sfx_playing[0].is_none());
    }

    #[test]
    fn test_music_pattern() {
        let pat = MusicPattern {
            channels: [0, 1, 0x40 | 2, 3],
            flags: 0,
        };
        assert!(pat.channel_enabled(0));
        assert!(pat.channel_enabled(1));
        assert!(!pat.channel_enabled(2));
        assert!(pat.channel_enabled(3));
        assert_eq!(pat.sfx_index(0), 0);
        assert_eq!(pat.sfx_index(2), 2);
    }

    #[test]
    fn test_music_start_stop() {
        let mut audio = Audio::new();
        audio.music_data[0] = MusicPattern {
            channels: [0, 1, 0x40, 0x40],
            flags: 0,
        };

        audio.music(0, 0, 0);
        assert!(audio.music_playing.is_some());
        assert_eq!(audio.music_playing.as_ref().unwrap().pattern_index, 0);
        assert!(audio.sfx_playing[0].is_some());
        assert!(audio.sfx_playing[1].is_some());
        assert!(audio.sfx_playing[2].is_none());
        assert!(audio.sfx_playing[3].is_none());

        audio.music(-1, 0, 0);
        assert!(audio.music_playing.is_none());
    }

    #[test]
    fn test_music_advances_via_generate_samples() {
        // Regression test: generate_samples() is the single playback authority,
        // so it must advance music patterns on its own (the embedded target
        // never calls any other per-frame method). Previously music only
        // progressed inside update(), which the pico build never invoked.
        let mut audio = Audio::new();
        audio.sfx_data[0].speed = 1; // short SFX so the channel finishes fast
                                     // Pattern 0: channel 0 plays sfx 0; others disabled.
        audio.music_data[0] = MusicPattern {
            channels: [0, 0x40, 0x40, 0x40],
            flags: 0,
        };
        // Pattern 1: channel 0 plays sfx 1; others disabled.
        audio.music_data[1] = MusicPattern {
            channels: [1, 0x40, 0x40, 0x40],
            flags: 0,
        };

        audio.music(0, 0, 0);
        assert_eq!(audio.current_music_pattern(), 0);

        // Render enough samples to play pattern 0's SFX to completion; the
        // single trailing generate_samples frame then advances to pattern 1.
        let spn = Audio::samples_per_note(1, 44100).max(1) as usize;
        let mut samples = vec![0.0f32; spn * (NOTES_PER_SFX + 1)];
        audio.generate_samples(44100, &mut samples);

        assert_eq!(
            audio.current_music_pattern(),
            1,
            "music should advance to the next pattern via generate_samples alone"
        );
    }

    #[test]
    fn test_query_helpers() {
        let mut audio = Audio::new();

        assert_eq!(audio.sfx_on_channel(0), -1);
        assert_eq!(audio.note_on_channel(0), -1);
        assert_eq!(audio.current_music_pattern(), -1);

        audio.sfx(10, 2, 0, 0);
        assert_eq!(audio.sfx_on_channel(2), 10);
        assert_eq!(audio.note_on_channel(2), 0);
    }

    // -----------------------------------------------------------------------
    // generate_samples tests
    // -----------------------------------------------------------------------

    fn audio_with_sfx0() -> Audio {
        let mut audio = Audio::new();
        for i in 0..NOTES_PER_SFX {
            audio.sfx_data[0].notes[i] = Note {
                pitch: 24,
                waveform: 0,
                volume: 5,
                effect: 0,
            };
        }
        audio.sfx_data[0].speed = 16;
        audio
    }

    #[test]
    fn test_generate_samples_silence_when_nothing_playing() {
        let mut audio = Audio::new();
        let mut samples = vec![1.0f32; 1470]; // pre-fill with non-zero
        audio.generate_samples(44100, &mut samples);
        assert!(samples.iter().all(|&s| s == 0.0));
    }

    #[test]
    fn test_generate_samples_produces_nonzero_with_sfx() {
        let mut audio = audio_with_sfx0();
        audio.sfx(0, 0, 0, 0);
        let mut samples = vec![0.0f32; 1470];
        audio.generate_samples(44100, &mut samples);
        let has_nonzero = samples.iter().any(|&s| s != 0.0);
        assert!(has_nonzero, "Expected non-zero samples when SFX is playing");
    }

    #[test]
    fn test_generate_samples_all_waveforms_produce_output() {
        for waveform_id in 0..8u8 {
            let mut audio = Audio::new();
            for i in 0..NOTES_PER_SFX {
                audio.sfx_data[0].notes[i] = Note {
                    pitch: 24,
                    waveform: waveform_id,
                    volume: 7,
                    effect: 0,
                };
            }
            audio.sfx_data[0].speed = 16;
            audio.sfx(0, 0, 0, 0);
            let mut samples = vec![0.0f32; 1470];
            audio.generate_samples(44100, &mut samples);
            let has_nonzero = samples.iter().any(|&s| s != 0.0);
            assert!(has_nonzero, "Waveform {} produced all zeros", waveform_id);
        }
    }

    #[test]
    fn test_generate_samples_volume_scaling() {
        let mut audio_loud = audio_with_sfx0();
        for i in 0..NOTES_PER_SFX {
            audio_loud.sfx_data[0].notes[i].volume = 7;
        }
        audio_loud.sfx(0, 0, 0, 0);
        let mut loud_samples = vec![0.0f32; 1470];
        audio_loud.generate_samples(44100, &mut loud_samples);

        let mut audio_quiet = audio_with_sfx0();
        for i in 0..NOTES_PER_SFX {
            audio_quiet.sfx_data[0].notes[i].volume = 1;
        }
        audio_quiet.sfx(0, 0, 0, 0);
        let mut quiet_samples = vec![0.0f32; 1470];
        audio_quiet.generate_samples(44100, &mut quiet_samples);

        let rms_loud: f64 = libm::sqrt(
            loud_samples
                .iter()
                .map(|&s| (s as f64) * (s as f64))
                .sum::<f64>()
                / loud_samples.len() as f64,
        );
        let rms_quiet: f64 = libm::sqrt(
            quiet_samples
                .iter()
                .map(|&s| (s as f64) * (s as f64))
                .sum::<f64>()
                / quiet_samples.len() as f64,
        );
        assert!(
            rms_loud > rms_quiet,
            "Loud RMS ({}) should be greater than quiet RMS ({})",
            rms_loud,
            rms_quiet
        );
    }

    #[test]
    fn test_generate_samples_zero_volume_is_silent() {
        let mut audio = Audio::new();
        audio.sfx(0, 0, 0, 0);
        let mut samples = vec![0.0f32; 1470];
        audio.generate_samples(44100, &mut samples);
        assert!(
            samples.iter().all(|&s| s == 0.0),
            "Volume-0 should be silent"
        );
    }

    #[test]
    fn test_sfx_completion_marks_channel_inactive() {
        let mut audio = Audio::new();
        for i in 0..2 {
            audio.sfx_data[0].notes[i] = Note {
                pitch: 24,
                waveform: 0,
                volume: 5,
                effect: 0,
            };
        }
        audio.sfx_data[0].speed = 1;
        audio.sfx(0, 0, 0, 2);
        // At speed 1, each note ~ 344 samples. 2 notes ~ 688. Generate 2000.
        let mut buf = vec![0.0f32; 2000];
        audio.generate_samples(44100, &mut buf);
        assert!(
            audio.sfx_playing[0].is_none(),
            "Channel should be inactive after SFX completes"
        );
    }

    #[test]
    fn test_mixing_multiple_channels() {
        let mut audio = audio_with_sfx0();
        for i in 0..NOTES_PER_SFX {
            audio.sfx_data[1].notes[i] = Note {
                pitch: 36,
                waveform: 0,
                volume: 5,
                effect: 0,
            };
        }
        audio.sfx_data[1].speed = 16;
        audio.sfx(0, 0, 0, 0);
        audio.sfx(1, 1, 0, 0);
        let mut samples = vec![0.0f32; 1470];
        audio.generate_samples(44100, &mut samples);
        assert!(samples.iter().any(|&s| s != 0.0));
        assert!(
            samples.iter().all(|&s| (-1.0..=1.0).contains(&s)),
            "Samples should be in [-1, 1]"
        );
    }

    #[test]
    fn test_effect_fade_in() {
        let mut audio = Audio::new();
        audio.sfx_data[0].notes[0] = Note {
            pitch: 24,
            waveform: 3,
            volume: 7,
            effect: 4, // fade in
        };
        audio.sfx_data[0].speed = 32;
        audio.sfx(0, 0, 0, 1);
        let mut samples = vec![0.0f32; 1470];
        audio.generate_samples(44100, &mut samples);

        let early_rms: f64 = libm::sqrt(
            samples[0..100]
                .iter()
                .map(|&s| (s as f64) * (s as f64))
                .sum::<f64>()
                / 100.0,
        );
        let late_rms: f64 = libm::sqrt(
            samples[1370..1470]
                .iter()
                .map(|&s| (s as f64) * (s as f64))
                .sum::<f64>()
                / 100.0,
        );
        assert!(
            late_rms > early_rms,
            "Fade-in: late ({}) > early ({})",
            late_rms,
            early_rms
        );
    }

    #[test]
    fn test_effect_fade_out() {
        let mut audio = Audio::new();
        audio.sfx_data[0].notes[0] = Note {
            pitch: 24,
            waveform: 3,
            volume: 7,
            effect: 5, // fade out
        };
        audio.sfx_data[0].speed = 32;
        audio.sfx(0, 0, 0, 1);
        let mut samples = vec![0.0f32; 1470];
        audio.generate_samples(44100, &mut samples);

        let early_rms: f64 = libm::sqrt(
            samples[0..100]
                .iter()
                .map(|&s| (s as f64) * (s as f64))
                .sum::<f64>()
                / 100.0,
        );
        let late_rms: f64 = libm::sqrt(
            samples[1370..1470]
                .iter()
                .map(|&s| (s as f64) * (s as f64))
                .sum::<f64>()
                / 100.0,
        );
        assert!(
            early_rms > late_rms,
            "Fade-out: early ({}) > late ({})",
            early_rms,
            late_rms
        );
    }

    #[test]
    fn test_effect_slide() {
        let mut audio = Audio::new();
        audio.sfx_data[0].notes[0] = Note {
            pitch: 24,
            waveform: 0,
            volume: 7,
            effect: 1,
        };
        audio.sfx_data[0].notes[1] = Note {
            pitch: 36,
            waveform: 0,
            volume: 7,
            effect: 0,
        };
        audio.sfx_data[0].speed = 64;
        audio.sfx(0, 0, 0, 2);
        let mut samples = vec![0.0f32; 1470];
        audio.generate_samples(44100, &mut samples);
        assert!(
            samples.iter().any(|&s| s != 0.0),
            "Slide should produce sound"
        );
    }

    #[test]
    fn test_effect_drop() {
        let mut audio = Audio::new();
        audio.sfx_data[0].notes[0] = Note {
            pitch: 24,
            waveform: 0,
            volume: 7,
            effect: 3,
        };
        audio.sfx_data[0].speed = 32;
        audio.sfx(0, 0, 0, 1);
        let mut samples = vec![0.0f32; 1470];
        audio.generate_samples(44100, &mut samples);
        assert!(samples.iter().any(|&s| s != 0.0));
    }

    #[test]
    fn test_effect_vibrato() {
        let mut audio = Audio::new();
        audio.sfx_data[0].notes[0] = Note {
            pitch: 24,
            waveform: 0,
            volume: 7,
            effect: 2,
        };
        audio.sfx_data[0].speed = 32;
        audio.sfx(0, 0, 0, 1);
        let mut samples = vec![0.0f32; 1470];
        audio.generate_samples(44100, &mut samples);
        assert!(samples.iter().any(|&s| s != 0.0));
    }

    #[test]
    fn test_effect_arpeggio_fast() {
        let mut audio = Audio::new();
        audio.sfx_data[0].notes[0] = Note {
            pitch: 24,
            waveform: 0,
            volume: 7,
            effect: 6,
        };
        audio.sfx_data[0].speed = 32;
        audio.sfx(0, 0, 0, 1);
        let mut samples = vec![0.0f32; 1470];
        audio.generate_samples(44100, &mut samples);
        assert!(samples.iter().any(|&s| s != 0.0));
    }

    #[test]
    fn test_effect_arpeggio_slow() {
        let mut audio = Audio::new();
        audio.sfx_data[0].notes[0] = Note {
            pitch: 24,
            waveform: 0,
            volume: 7,
            effect: 7,
        };
        audio.sfx_data[0].speed = 32;
        audio.sfx(0, 0, 0, 1);
        let mut samples = vec![0.0f32; 1470];
        audio.generate_samples(44100, &mut samples);
        assert!(samples.iter().any(|&s| s != 0.0));
    }

    #[test]
    fn test_samples_per_note_calculation() {
        // 183 samples per speed-unit at 22050 Hz, scaled to 44100 (×2).
        assert_eq!(Audio::samples_per_note(1, 44100), 366);
        assert_eq!(Audio::samples_per_note(16, 44100), 5856);
        assert_eq!(Audio::samples_per_note(0, 44100), 366); // 0 treated as 1
    }

    #[test]
    fn test_phase_stays_in_range() {
        let mut audio = audio_with_sfx0();
        audio.sfx(0, 0, 0, 0);
        let mut buf = vec![0.0f32; 44100];
        audio.generate_samples(44100, &mut buf);
        if let Some(ref player) = audio.sfx_playing[0] {
            assert!(
                player.phase >= 0.0 && player.phase < 1.0,
                "Phase: {}",
                player.phase
            );
        }
    }

    #[test]
    fn test_sfx_loop_in_generate_samples() {
        let mut audio = audio_with_sfx0();
        audio.sfx_data[0].speed = 1;
        audio.sfx_data[0].loop_start = 2;
        audio.sfx_data[0].loop_end = 4;
        audio.sfx(0, 0, 0, 0);
        let mut buf = vec![0.0f32; 15000];
        audio.generate_samples(44100, &mut buf);
        assert!(
            audio.sfx_playing[0].is_some(),
            "Looping SFX should stay active"
        );
        if let Some(ref player) = audio.sfx_playing[0] {
            let ni = player.note_index as usize;
            assert!(
                (2..4).contains(&ni),
                "Note index {} should be in [2, 4)",
                ni
            );
        }
    }

    #[test]
    fn test_generate_samples_output_in_range() {
        let mut audio = audio_with_sfx0();
        for i in 0..NOTES_PER_SFX {
            audio.sfx_data[0].notes[i].volume = 7;
        }
        audio.sfx(0, 0, 0, 0);
        audio.sfx(0, 1, 0, 0);
        audio.sfx(0, 2, 0, 0);
        audio.sfx(0, 3, 0, 0);
        let mut samples = vec![0.0f32; 4410];
        audio.generate_samples(44100, &mut samples);
        for (i, &s) in samples.iter().enumerate() {
            assert!(
                (-1.0..=1.0).contains(&s),
                "Sample {} out of range: {}",
                i,
                s
            );
        }
    }

    /// Build an Audio exercising every waveform and effect across two
    /// channels, used by the render-snapshot regression test.
    fn snapshot_audio() -> Audio {
        let mut audio = Audio::new();
        for i in 0..NOTES_PER_SFX {
            audio.sfx_data[0].notes[i] = Note {
                pitch: (10 + i * 3) as u8 % 64,
                waveform: (i % 8) as u8,
                volume: (1 + i % 7) as u8,
                effect: (i % 8) as u8,
            };
            audio.sfx_data[1].notes[i] = Note {
                pitch: (60 - i) as u8,
                waveform: ((i + 3) % 8) as u8,
                volume: 7 - (i % 7) as u8,
                effect: ((i * 5) % 8) as u8,
            };
        }
        audio.sfx_data[0].speed = 3;
        audio.sfx_data[1].speed = 5;
        audio.sfx_data[1].loop_start = 4;
        audio.sfx_data[1].loop_end = 9;
        audio.sfx(0, 0, 0, 0);
        audio.sfx(1, 1, 2, 0);
        audio
    }

    #[test]
    fn test_render_snapshot_matches_reference() {
        // Snapshot of the synth output captured from the pre-refactor
        // implementation (f64 phase, libm::pow pitch). Guards the hot-path
        // rewrite (pitch table, per-note cached state) against drift.
        let mut audio = snapshot_audio();
        let mut buf = vec![0.0f32; 4000];
        audio.generate_samples(22050, &mut buf);
        let sum: f64 = buf.iter().map(|&s| s as f64).sum();
        let abs_sum: f64 = buf.iter().map(|&s| (s as f64).abs()).sum();
        let picks = [0usize, 1, 50, 333, 777, 1234, 1999, 2500, 3333, 3999];
        let vals: Vec<f32> = picks.iter().map(|&i| buf[i]).collect();
        // Captured 2026-09 from the pre-refactor implementation.
        const REF_SUM: f64 = -16.428213233;
        const REF_ABS_SUM: f64 = 152.707879509;
        const REF_VALS: [f32; 10] = [
            -0.077380955,
            -0.036135823,
            0.041969594,
            -0.052701574,
            0.0041078567,
            0.07281761,
            0.035533134,
            0.006743248,
            -0.021615554,
            0.021968335,
        ];
        assert!((sum - REF_SUM).abs() < 1e-4, "sum {sum} vs {REF_SUM}");
        assert!(
            (abs_sum - REF_ABS_SUM).abs() < 1e-4,
            "abs_sum {abs_sum} vs {REF_ABS_SUM}"
        );
        for (k, (&got, &want)) in vals.iter().zip(REF_VALS.iter()).enumerate() {
            assert!(
                (got - want).abs() < 1e-6,
                "sample {} (index {}): got {got}, want {want}",
                k,
                picks[k]
            );
        }
    }

    #[test]
    fn test_music_triggers_sfx_and_produces_audio() {
        let mut audio = audio_with_sfx0();
        for i in 0..NOTES_PER_SFX {
            audio.sfx_data[1].notes[i] = Note {
                pitch: 36,
                waveform: 2,
                volume: 4,
                effect: 0,
            };
        }
        audio.sfx_data[1].speed = 16;
        audio.music_data[0] = MusicPattern {
            channels: [0, 1, 0x40, 0x40],
            flags: 0,
        };
        audio.music(0, 0, 0);
        assert!(audio.sfx_playing[0].is_some());
        assert!(audio.sfx_playing[1].is_some());
        let mut samples = vec![0.0f32; 1470];
        audio.generate_samples(44100, &mut samples);
        assert!(
            samples.iter().any(|&s| s != 0.0),
            "Music should produce audio"
        );
    }

    #[test]
    fn test_pitch_table_matches_formula() {
        // The const table must equal 440 * 2^((pitch - 33) / 12) for every
        // pitch, and out-of-range pitches clamp to the top entry.
        for p in 0..64u8 {
            let want = 440.0 * libm::pow(2.0_f64, (p as f64 - 33.0) / 12.0);
            let got = Audio::pitch_to_freq(p);
            assert!((got - want).abs() < 1e-9, "pitch {p}: {got} vs {want}");
        }
        assert_eq!(Audio::pitch_to_freq(200), Audio::pitch_to_freq(63));
    }

    #[test]
    fn test_pattern_change_keeps_user_sfx_on_unused_channel() {
        // Regression: loading a pattern used to clear every channel the
        // pattern did not enable, cutting off user sfx() calls at each
        // pattern transition. Only enabled channels may be touched.
        let mut audio = Audio::new();
        audio.sfx_data[0].speed = 1; // short, so pattern 0 ends quickly
        audio.sfx_data[5].speed = 255; // long user sfx
        audio.music_data[0] = MusicPattern {
            channels: [0, 0x40, 0x40, 0x40],
            flags: 0,
        };
        audio.music_data[1] = MusicPattern {
            channels: [0, 0x40, 0x40, 0x40],
            flags: 0,
        };
        audio.music(0, 0, 0);
        audio.sfx(5, 3, 0, 0); // user sound on channel 3

        let spn = Audio::samples_per_note(1, 44100).max(1) as usize;
        let mut samples = vec![0.0f32; spn * (NOTES_PER_SFX + 1)];
        audio.generate_samples(44100, &mut samples);

        assert_eq!(audio.current_music_pattern(), 1, "pattern should advance");
        assert_eq!(
            audio.sfx_on_channel(3),
            5,
            "user sfx on an unused channel must survive the pattern change"
        );
        assert!(!audio.sfx_playing[3].as_ref().unwrap().is_music);
    }

    #[test]
    fn test_music_channel_mask_reserves_channels_from_auto_sfx() {
        let mut audio = Audio::new();
        audio.music_data[0] = MusicPattern {
            channels: [0x40, 0x40, 0x40, 0x40],
            flags: 0,
        };
        // Reserve channels 0 and 1 for music.
        audio.music(0, 0, 0b0011);
        assert_eq!(audio.music_playing.unwrap().channel_mask, 0b0011);

        audio.sfx(1, -1, 0, 0);
        audio.sfx(2, -1, 0, 0);
        assert!(audio.sfx_playing[0].is_none());
        assert!(audio.sfx_playing[1].is_none());
        assert_eq!(audio.sfx_on_channel(2), 1);
        assert_eq!(audio.sfx_on_channel(3), 2);

        // Both free channels taken: steal the oldest NON-reserved one.
        audio.frame_counter += 1;
        audio.sfx(3, -1, 0, 0);
        assert!(audio.sfx_playing[0].is_none());
        assert!(audio.sfx_playing[1].is_none());
        assert_eq!(audio.sfx_on_channel(2), 3);

        // Explicit channel requests ignore the mask.
        audio.sfx(4, 0, 0, 0);
        assert_eq!(audio.sfx_on_channel(0), 4);

        // Reserving everything must not make auto-assign silently drop sounds.
        let mut all = Audio::new();
        all.music(0, 0, 0x0F);
        all.sfx(1, -1, 0, 0);
        assert!(all.sfx_playing.iter().any(|p| p.is_some()));
    }

    #[test]
    fn test_music_stop_silences_only_music_channels() {
        let mut audio = Audio::new();
        audio.music_data[0] = MusicPattern {
            channels: [0, 1, 0x40, 0x40],
            flags: 0,
        };
        audio.music(0, 0, 0);
        audio.sfx(7, 2, 0, 0);
        assert!(audio.sfx_playing[0].as_ref().unwrap().is_music);
        assert!(!audio.sfx_playing[2].as_ref().unwrap().is_music);

        audio.music(-1, 0, 0);
        assert!(audio.music_playing.is_none());
        assert!(audio.sfx_playing[0].is_none(), "music channel 0 stopped");
        assert!(audio.sfx_playing[1].is_none(), "music channel 1 stopped");
        assert_eq!(audio.sfx_on_channel(2), 7, "user sfx keeps playing");
    }
}
