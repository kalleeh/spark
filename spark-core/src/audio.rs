// audio.rs — PICO-8 compatible audio engine for Spark (no_std)
//
// Implements SFX and music data structures matching PICO-8's format,
// a working state machine for playback tracking (note advancement, looping,
// pattern transitions), and waveform generation primitives.

use core::f64::consts::TAU;

// ---------------------------------------------------------------------------
// Waveform enum
// ---------------------------------------------------------------------------

/// PICO-8 waveform types (0-7).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Waveform {
    Triangle = 0,
    TiltedSaw = 1,
    Saw = 2,
    Square = 3,
    Pulse = 4,
    Organ = 5,
    Noise = 6,
    Phaser = 7,
}

impl Waveform {
    pub fn from_u8(v: u8) -> Self {
        match v & 0x07 {
            0 => Waveform::Triangle,
            1 => Waveform::TiltedSaw,
            2 => Waveform::Saw,
            3 => Waveform::Square,
            4 => Waveform::Pulse,
            5 => Waveform::Organ,
            6 => Waveform::Noise,
            7 => Waveform::Phaser,
            _ => unreachable!(),
        }
    }
}

// ---------------------------------------------------------------------------
// Note / Sfx / MusicPattern data structures
// ---------------------------------------------------------------------------

/// A single note inside a sound effect.
#[derive(Clone, Copy, Debug)]
pub struct Note {
    /// Pitch value 0-63 (C-0 = 0, C#-0 = 1, ... C-1 = 12, ...).
    pub pitch: u8,
    /// Waveform index 0-7.
    pub waveform: u8,
    /// Volume 0-7.
    pub volume: u8,
    /// Effect 0-7 (0=none, 1=slide, 2=vibrato, 3=drop, 4=fade_in,
    ///              5=fade_out, 6=arpeggio_fast, 7=arpeggio_slow).
    pub effect: u8,
}

impl Default for Note {
    fn default() -> Self {
        Note {
            pitch: 0,
            waveform: 0,
            volume: 0,
            effect: 0,
        }
    }
}

/// A sound effect consisting of 32 notes.
#[derive(Clone, Copy, Debug)]
pub struct Sfx {
    pub notes: [Note; 32],
    /// Ticks per note (1-255). PICO-8 default is 16 (~0.5 s/note at 30 fps).
    pub speed: u8,
    /// Loop start note index (0-31). 0 means no loop unless loop_end > 0.
    pub loop_start: u8,
    /// Loop end note index (0-31). 0 means no loop.
    pub loop_end: u8,
}

impl Default for Sfx {
    fn default() -> Self {
        Sfx {
            notes: [Note::default(); 32],
            speed: 16,
            loop_start: 0,
            loop_end: 0,
        }
    }
}

/// A music pattern referencing up to 4 SFX channels.
#[derive(Clone, Copy, Debug)]
pub struct MusicPattern {
    /// SFX index for each of the 4 channels. Bit 6 set means "channel disabled".
    pub channels: [u8; 4],
    /// Flags: bit 0 = loop start, bit 1 = loop end, bit 2 = stop at end.
    pub flags: u8,
}

impl Default for MusicPattern {
    fn default() -> Self {
        MusicPattern {
            // All channels disabled by default (bit 6 set = 0x40).
            channels: [0x40; 4],
            flags: 0,
        }
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
    /// Current note position — fractional so we can interpolate / advance
    /// smoothly depending on speed.
    pub note_index: f32,
    /// Absolute tick counter since this SFX started.
    pub tick: u32,
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
}

/// Tracks music (pattern) playback state.
#[derive(Clone, Copy, Debug)]
pub struct MusicPlayer {
    /// Current pattern index (0-63).
    pub pattern_index: i32,
    /// Tick counter within the current pattern.
    pub tick: u32,
    /// The pattern index that was identified as loop-start while scanning
    /// forward, so we know where to jump back on loop-end.
    pub loop_back: i32,
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

    pub fn new() -> Self {
        Audio {
            sfx_data: [Sfx::default(); NUM_SFX],
            music_data: [MusicPattern::default(); NUM_MUSIC],
            sfx_playing: [None, None, None, None],
            music_playing: None,
            frame_counter: 0,
        }
    }

    // -----------------------------------------------------------------------
    // SFX trigger
    // -----------------------------------------------------------------------

    /// Trigger (or stop) a sound effect.
    ///
    /// * `n`       — SFX index 0-63. -1 = stop `channel`, -2 = stop all.
    /// * `channel` — Channel 0-3 to play on. -1 = auto-assign to the first
    ///               free channel, or steal the oldest one.
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
        let note_length = if length <= 0 { 0u8 } else { (length as u8).min(32) };

        // Determine which channel to use.
        let ch = if channel >= 0 && (channel as usize) < NUM_CHANNELS {
            channel as usize
        } else {
            // Auto-assign: prefer a free channel.
            self.find_free_channel()
                .unwrap_or_else(|| self.find_oldest_channel())
        };

        self.sfx_playing[ch] = Some(SfxPlayer {
            sfx_index: sfx_idx,
            note_index: note_offset as f32,
            tick: 0,
            phase: 0.0,
            offset: note_offset,
            length: note_length,
            start_frame: self.frame_counter,
            sample_pos: 0,
        });
    }

    /// Find the first channel that isn't playing anything.
    fn find_free_channel(&self) -> Option<usize> {
        self.sfx_playing
            .iter()
            .position(|p| p.is_none())
    }

    /// Find the channel whose SFX started earliest (oldest).
    fn find_oldest_channel(&self) -> usize {
        let mut oldest_ch = 0usize;
        let mut oldest_frame = u32::MAX;
        for (i, slot) in self.sfx_playing.iter().enumerate() {
            let frame = slot.as_ref().map_or(0, |p| p.start_frame);
            if frame < oldest_frame {
                oldest_frame = frame;
                oldest_ch = i;
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
    /// * `_channel_mask` — Bitmask of channels to use (ignored for MVP; 0 = all).
    pub fn music(&mut self, n: i32, _fade_len: i32, _channel_mask: i32) {
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
            tick: 0,
            loop_back,
        });

        // Load the first pattern's SFX into channels.
        self.load_pattern_into_channels(pattern_idx as usize);
    }

    /// Load the SFX references from a music pattern into the 4 channels.
    fn load_pattern_into_channels(&mut self, pattern_idx: usize) {
        if pattern_idx >= NUM_MUSIC {
            return;
        }
        let pattern = self.music_data[pattern_idx];
        for ch in 0..NUM_CHANNELS {
            if pattern.channel_enabled(ch) {
                let sidx = pattern.sfx_index(ch);
                self.sfx_playing[ch] = Some(SfxPlayer {
                    sfx_index: sidx,
                    note_index: 0.0,
                    tick: 0,
                    phase: 0.0,
                    offset: 0,
                    length: 0, // play all
                    start_frame: self.frame_counter,
                    sample_pos: 0,
                });
            } else {
                self.sfx_playing[ch] = None;
            }
        }
    }

    // -----------------------------------------------------------------------
    // Per-frame update
    // -----------------------------------------------------------------------

    /// Advance the audio state machine by one frame.
    /// Should be called once per frame from the main loop.
    pub fn update(&mut self) {
        self.frame_counter = self.frame_counter.wrapping_add(1);

        // --- Advance each active SFX channel ---
        for ch in 0..NUM_CHANNELS {
            if let Some(mut player) = self.sfx_playing[ch].take() {
                let finished = self.advance_sfx_player(&mut player);
                if !finished {
                    self.sfx_playing[ch] = Some(player);
                }
            }
        }

        // --- Advance music ---
        // Copy the small MusicPlayer struct (12 bytes) to avoid a borrow
        // conflict: advance_music needs &mut self but also reads the player.
        if let Some(mplayer) = self.music_playing {
            self.advance_music(&mplayer);
        }
    }

    /// Advance a single SFX player by one frame.
    /// Returns `true` if the SFX has finished and should be removed.
    fn advance_sfx_player(&self, player: &mut SfxPlayer) -> bool {
        let sfx_idx = player.sfx_index as usize;
        if sfx_idx >= NUM_SFX {
            return true;
        }
        let sfx = &self.sfx_data[sfx_idx];

        let ticks_per_frame: u32 = 4;
        player.tick += ticks_per_frame;

        let speed = sfx.speed.max(1) as f32;
        player.note_index = player.tick as f32 / speed;

        // Determine the end note.
        let start = player.offset as f32;
        let end = if player.length > 0 {
            start + player.length as f32
        } else {
            NOTES_PER_SFX as f32
        };

        // Check if we've passed the end.
        if player.note_index >= end {
            // Handle looping.
            let has_loop = sfx.loop_end > 0 && sfx.loop_end > sfx.loop_start;
            if has_loop
                && player.note_index >= sfx.loop_end as f32
                && (sfx.loop_start as f32) >= start
            {
                // Jump back to loop_start.
                let loop_len = (sfx.loop_end - sfx.loop_start) as f32;
                if loop_len > 0.0 {
                    // Wrap within the loop region.
                    let overshoot = player.note_index - sfx.loop_end as f32;
                    player.note_index = sfx.loop_start as f32 + (overshoot % loop_len);
                    player.tick = (player.note_index * speed) as u32;
                } else {
                    return true;
                }
                false
            } else {
                // No loop — SFX is done.
                true
            }
        } else {
            // Advance oscillator phase for waveform generation.
            let current_note_idx =
                (player.note_index as usize).min(NOTES_PER_SFX - 1);
            let note = &sfx.notes[current_note_idx];
            if note.volume > 0 {
                let freq = Self::pitch_to_freq(note.pitch);
                // Phase increment per tick (at 120 Hz tick rate).
                let dt = 1.0 / 120.0;
                let phase_inc = freq * dt * ticks_per_frame as f64;
                player.phase = (player.phase + phase_inc) % 1.0;
            } else {
                player.phase = 0.0;
            }
            false
        }
    }

    /// Advance the music player by one frame.
    fn advance_music(&mut self, mplayer: &MusicPlayer) {
        let pattern_idx = mplayer.pattern_index as usize;
        if pattern_idx >= NUM_MUSIC {
            self.music_playing = None;
            return;
        }

        let pattern = self.music_data[pattern_idx];

        // Check whether any enabled channel still has an active SFX.
        let any_channel_active = (0..NUM_CHANNELS)
            .any(|ch| pattern.channel_enabled(ch) && self.sfx_playing[ch].is_some());

        if any_channel_active {
            return;
        }

        // --- Pattern transition ---
        let current = mplayer.pattern_index;
        let loop_back = mplayer.loop_back;

        if pattern.is_stop() {
            self.music_playing = None;
            return;
        }

        if pattern.is_loop_end() {
            let next = loop_back;
            self.music_playing = Some(MusicPlayer {
                pattern_index: next,
                tick: 0,
                loop_back,
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
            tick: 0,
            loop_back: new_loop_back,
        });
        self.load_pattern_into_channels(next as usize);
    }

    // -----------------------------------------------------------------------
    // Audio generation
    // -----------------------------------------------------------------------

    /// PICO-8 SFX tick rate: 128 ticks per second.
    const SFX_TICK_RATE: f64 = 128.0;

    /// Per-channel mix volume to prevent clipping when all 4 channels are active.
    const MIX_VOLUME: f64 = 0.25;

    /// Compute how many audio samples correspond to one note at the given speed.
    pub fn samples_per_note(speed: u8, sample_rate: u32) -> u32 {
        let s = speed.max(1) as f64;
        ((s / Self::SFX_TICK_RATE) * sample_rate as f64) as u32
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
        for s in 0..buf.len() {
            let mut mix: f64 = 0.0;

            for ch in 0..NUM_CHANNELS {
                if let Some(ref mut player) = self.sfx_playing[ch] {
                    let sfx_idx = player.sfx_index as usize;
                    if sfx_idx >= NUM_SFX {
                        continue;
                    }
                    let sfx = &self.sfx_data[sfx_idx];
                    let spn = Self::samples_per_note(sfx.speed, sample_rate).max(1);

                    // Determine current note
                    let ni = player.note_index as usize;
                    if ni >= NOTES_PER_SFX {
                        continue;
                    }
                    let note = sfx.notes[ni];
                    let next_note = if ni + 1 < NOTES_PER_SFX {
                        Some(sfx.notes[ni + 1])
                    } else {
                        None
                    };

                    if note.volume > 0 {
                        let base_freq = Self::pitch_to_freq(note.pitch);
                        let base_vol = note.volume as f64 / 7.0;
                        let progress = player.sample_pos as f64 / spn as f64;

                        // Apply effects
                        let (freq, vol) = Self::apply_effect(
                            note.effect,
                            base_freq,
                            base_vol,
                            progress,
                            next_note.map(|n| Self::pitch_to_freq(n.pitch)),
                            player.sample_pos,
                        );

                        // Generate waveform sample
                        let sample = Self::waveform_sample(note.waveform, player.phase);
                        mix += sample * vol * Self::MIX_VOLUME;

                        // Advance phase
                        let phase_inc = freq / sample_rate as f64;
                        player.phase += phase_inc;
                        if player.phase >= 1.0 {
                            player.phase -= libm::floor(player.phase);
                        }
                    }

                    // Advance sample position and handle note boundaries
                    player.sample_pos += 1;
                    if player.sample_pos >= spn {
                        player.sample_pos = 0;
                        player.note_index += 1.0;
                        player.tick += sfx.speed.max(1) as u32;

                        let end_note = if player.length > 0 {
                            player.offset as f32 + player.length as f32
                        } else {
                            NOTES_PER_SFX as f32
                        };

                        // Check loop first (loop_end takes priority)
                        let has_loop = sfx.loop_end > 0
                            && sfx.loop_end > sfx.loop_start
                            && (sfx.loop_start as f32) >= player.offset as f32;
                        if has_loop && player.note_index >= sfx.loop_end as f32 {
                            player.note_index = sfx.loop_start as f32;
                        } else if player.note_index >= end_note {
                            // No loop or not at loop boundary — SFX is done
                            player.note_index = NOTES_PER_SFX as f32;
                        }
                    }
                }
            }

            buf[s] = mix.clamp(-1.0, 1.0) as f32;
        }

        // Remove finished channels
        for ch in 0..NUM_CHANNELS {
            if let Some(ref player) = self.sfx_playing[ch] {
                if player.note_index as usize >= NOTES_PER_SFX {
                    self.sfx_playing[ch] = None;
                }
            }
        }
    }

    /// Apply a PICO-8 effect to the base frequency and volume.
    ///
    /// Returns `(modified_freq, modified_vol)`.
    fn apply_effect(
        effect: u8,
        base_freq: f64,
        base_vol: f64,
        progress: f64,    // 0.0..1.0 within current note
        next_freq: Option<f64>,
        sample_pos: u32,
    ) -> (f64, f64) {
        match effect {
            0 => (base_freq, base_vol), // None
            1 => {
                // Slide — interpolate toward the next note's pitch
                let target = next_freq.unwrap_or(base_freq);
                let freq = base_freq + (target - base_freq) * progress;
                (freq, base_vol)
            }
            2 => {
                // Vibrato — oscillate pitch at ~4 Hz
                let vib = libm::sin(sample_pos as f64 * 4.0 * core::f64::consts::TAU / 44100.0);
                let semitone_ratio = libm::pow(2.0_f64, vib / 12.0);
                (base_freq * semitone_ratio, base_vol)
            }
            3 => {
                // Drop — pitch drops to zero over the note
                let freq = base_freq * (1.0 - progress);
                (freq, base_vol)
            }
            4 => {
                // Fade in — volume ramps from 0 to full
                (base_freq, base_vol * progress)
            }
            5 => {
                // Fade out — volume ramps from full to 0
                (base_freq, base_vol * (1.0 - progress))
            }
            6 => {
                // Arpeggio fast — cycle root/+4/+7 semitones at ~15 Hz
                let step = ((sample_pos as f64 * 15.0 / 44100.0) as u32) % 3;
                let semitones = match step {
                    1 => 4.0,
                    2 => 7.0,
                    _ => 0.0,
                };
                let freq = base_freq * libm::pow(2.0_f64, semitones / 12.0);
                (freq, base_vol)
            }
            7 => {
                // Arpeggio slow — cycle root/+4/+7 at ~7.5 Hz
                let step = ((sample_pos as f64 * 7.5 / 44100.0) as u32) % 3;
                let semitones = match step {
                    1 => 4.0,
                    2 => 7.0,
                    _ => 0.0,
                };
                let freq = base_freq * libm::pow(2.0_f64, semitones / 12.0);
                (freq, base_vol)
            }
            _ => (base_freq, base_vol),
        }
    }

    /// Convert a PICO-8 pitch value (0-63) to a frequency in Hz.
    ///
    /// Pitch 0 corresponds to C-2 in PICO-8, which is ~65.41 Hz.
    /// Each increment is one semitone (equal temperament).
    pub fn pitch_to_freq(pitch: u8) -> f64 {
        65.41 * libm::pow(2.0_f64, pitch as f64 / 12.0)
    }

    /// Generate one sample (-1.0 ... 1.0) for the given waveform at `phase`
    /// (0.0 ... 1.0).
    pub fn waveform_sample(waveform: u8, phase: f64) -> f64 {
        match waveform & 0x07 {
            // 0 — Triangle
            0 => {
                if phase < 0.5 {
                    4.0 * phase - 1.0
                } else {
                    3.0 - 4.0 * phase
                }
            }
            // 1 — Tilted Saw (saw with a flattened top region)
            1 => {
                if phase < 0.875 {
                    2.0 * phase / 0.875 - 1.0
                } else {
                    1.0 - 2.0 * (phase - 0.875) / 0.125
                }
            }
            // 2 — Saw
            2 => 2.0 * phase - 1.0,
            // 3 — Square (50 % duty)
            3 => {
                if phase < 0.5 {
                    1.0
                } else {
                    -1.0
                }
            }
            // 4 — Pulse (25 % duty)
            4 => {
                if phase < 0.25 {
                    1.0
                } else {
                    -1.0
                }
            }
            // 5 — Organ (fundamental + 1st overtone)
            5 => {
                (libm::sin(phase * TAU) + libm::sin(phase * TAU * 2.0)) * 0.5
            }
            // 6 — Noise (deterministic PRNG seeded by quantised phase)
            6 => {
                let seed = (phase * 65536.0) as u32;
                let hash = seed
                    .wrapping_mul(1_103_515_245)
                    .wrapping_add(12345);
                (hash as f64 / u32::MAX as f64) * 2.0 - 1.0
            }
            // 7 — Phaser (FM-like)
            7 => {
                libm::sin(phase * TAU + libm::sin(phase * TAU * 3.0))
            }
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
        self.music_playing
            .as_ref()
            .map_or(-1, |m| m.pattern_index)
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;

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
        assert!((Audio::waveform_sample(0, 0.0) - (-1.0)).abs() < 1e-9);
        assert!((Audio::waveform_sample(0, 0.25) - 0.0).abs() < 1e-9);
        assert!((Audio::waveform_sample(0, 0.5) - 1.0).abs() < 1e-9);

        assert!((Audio::waveform_sample(2, 0.0) - (-1.0)).abs() < 1e-9);
        assert!((Audio::waveform_sample(2, 1.0) - 1.0).abs() < 1e-9);

        assert!((Audio::waveform_sample(3, 0.25) - 1.0).abs() < 1e-9);
        assert!((Audio::waveform_sample(3, 0.75) - (-1.0)).abs() < 1e-9);

        assert!((Audio::waveform_sample(4, 0.1) - 1.0).abs() < 1e-9);
        assert!((Audio::waveform_sample(4, 0.5) - (-1.0)).abs() < 1e-9);

        assert!((Audio::waveform_sample(5, 0.0)).abs() < 1e-9);

        let n1 = Audio::waveform_sample(6, 0.3);
        let n2 = Audio::waveform_sample(6, 0.3);
        assert!((n1 - n2).abs() < 1e-9);
        assert!(n1 >= -1.0 && n1 <= 1.0);
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
    fn test_sfx_update_advances() {
        let mut audio = Audio::new();
        audio.sfx_data[0].notes[0].volume = 5;
        audio.sfx_data[0].notes[0].pitch = 24;
        audio.sfx_data[0].speed = 8;

        audio.sfx(0, 0, 0, 0);
        assert_eq!(audio.sfx_playing[0].as_ref().unwrap().note_index, 0.0);

        audio.update();
        let ni = audio.sfx_playing[0].as_ref().unwrap().note_index;
        assert!((ni - 0.5).abs() < 1e-5);

        audio.update();
        let ni = audio.sfx_playing[0].as_ref().unwrap().note_index;
        assert!((ni - 1.0).abs() < 1e-5);
    }

    #[test]
    fn test_sfx_finishes() {
        let mut audio = Audio::new();
        audio.sfx_data[0].speed = 1;

        audio.sfx(0, 0, 0, 0);

        for _ in 0..10 {
            audio.update();
        }

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
    fn test_query_helpers() {
        let mut audio = Audio::new();

        assert_eq!(audio.sfx_on_channel(0), -1);
        assert_eq!(audio.note_on_channel(0), -1);
        assert_eq!(audio.current_music_pattern(), -1);

        audio.sfx(10, 2, 0, 0);
        assert_eq!(audio.sfx_on_channel(2), 10);
        assert_eq!(audio.note_on_channel(2), 0);
    }

    #[test]
    fn test_waveform_enum_from_u8() {
        assert_eq!(Waveform::from_u8(0), Waveform::Triangle);
        assert_eq!(Waveform::from_u8(3), Waveform::Square);
        assert_eq!(Waveform::from_u8(7), Waveform::Phaser);
        assert_eq!(Waveform::from_u8(8), Waveform::Triangle);
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
            loud_samples.iter().map(|&s| (s as f64) * (s as f64)).sum::<f64>()
                / loud_samples.len() as f64,
        );
        let rms_quiet: f64 = libm::sqrt(
            quiet_samples.iter().map(|&s| (s as f64) * (s as f64)).sum::<f64>()
                / quiet_samples.len() as f64,
        );
        assert!(
            rms_loud > rms_quiet,
            "Loud RMS ({}) should be greater than quiet RMS ({})",
            rms_loud, rms_quiet
        );
    }

    #[test]
    fn test_generate_samples_zero_volume_is_silent() {
        let mut audio = Audio::new();
        audio.sfx(0, 0, 0, 0);
        let mut samples = vec![0.0f32; 1470];
        audio.generate_samples(44100, &mut samples);
        assert!(samples.iter().all(|&s| s == 0.0), "Volume-0 should be silent");
    }

    #[test]
    fn test_sfx_completion_marks_channel_inactive() {
        let mut audio = Audio::new();
        for i in 0..2 {
            audio.sfx_data[0].notes[i] = Note {
                pitch: 24, waveform: 0, volume: 5, effect: 0,
            };
        }
        audio.sfx_data[0].speed = 1;
        audio.sfx(0, 0, 0, 2);
        // At speed 1, each note ~ 344 samples. 2 notes ~ 688. Generate 2000.
        let mut buf = vec![0.0f32; 2000];
        audio.generate_samples(44100, &mut buf);
        assert!(audio.sfx_playing[0].is_none(), "Channel should be inactive after SFX completes");
    }

    #[test]
    fn test_mixing_multiple_channels() {
        let mut audio = audio_with_sfx0();
        for i in 0..NOTES_PER_SFX {
            audio.sfx_data[1].notes[i] = Note {
                pitch: 36, waveform: 0, volume: 5, effect: 0,
            };
        }
        audio.sfx_data[1].speed = 16;
        audio.sfx(0, 0, 0, 0);
        audio.sfx(1, 1, 0, 0);
        let mut samples = vec![0.0f32; 1470];
        audio.generate_samples(44100, &mut samples);
        assert!(samples.iter().any(|&s| s != 0.0));
        assert!(samples.iter().all(|&s| s >= -1.0 && s <= 1.0), "Samples should be in [-1, 1]");
    }

    #[test]
    fn test_effect_fade_in() {
        let mut audio = Audio::new();
        audio.sfx_data[0].notes[0] = Note {
            pitch: 24, waveform: 3, volume: 7, effect: 4, // fade in
        };
        audio.sfx_data[0].speed = 32;
        audio.sfx(0, 0, 0, 1);
        let mut samples = vec![0.0f32; 1470];
        audio.generate_samples(44100, &mut samples);

        let early_rms: f64 = libm::sqrt(
            samples[0..100].iter().map(|&s| (s as f64) * (s as f64)).sum::<f64>() / 100.0,
        );
        let late_rms: f64 = libm::sqrt(
            samples[1370..1470].iter().map(|&s| (s as f64) * (s as f64)).sum::<f64>() / 100.0,
        );
        assert!(late_rms > early_rms, "Fade-in: late ({}) > early ({})", late_rms, early_rms);
    }

    #[test]
    fn test_effect_fade_out() {
        let mut audio = Audio::new();
        audio.sfx_data[0].notes[0] = Note {
            pitch: 24, waveform: 3, volume: 7, effect: 5, // fade out
        };
        audio.sfx_data[0].speed = 32;
        audio.sfx(0, 0, 0, 1);
        let mut samples = vec![0.0f32; 1470];
        audio.generate_samples(44100, &mut samples);

        let early_rms: f64 = libm::sqrt(
            samples[0..100].iter().map(|&s| (s as f64) * (s as f64)).sum::<f64>() / 100.0,
        );
        let late_rms: f64 = libm::sqrt(
            samples[1370..1470].iter().map(|&s| (s as f64) * (s as f64)).sum::<f64>() / 100.0,
        );
        assert!(early_rms > late_rms, "Fade-out: early ({}) > late ({})", early_rms, late_rms);
    }

    #[test]
    fn test_effect_slide() {
        let mut audio = Audio::new();
        audio.sfx_data[0].notes[0] = Note { pitch: 24, waveform: 0, volume: 7, effect: 1 };
        audio.sfx_data[0].notes[1] = Note { pitch: 36, waveform: 0, volume: 7, effect: 0 };
        audio.sfx_data[0].speed = 64;
        audio.sfx(0, 0, 0, 2);
        let mut samples = vec![0.0f32; 1470];
        audio.generate_samples(44100, &mut samples);
        assert!(samples.iter().any(|&s| s != 0.0), "Slide should produce sound");
    }

    #[test]
    fn test_effect_drop() {
        let mut audio = Audio::new();
        audio.sfx_data[0].notes[0] = Note { pitch: 24, waveform: 0, volume: 7, effect: 3 };
        audio.sfx_data[0].speed = 32;
        audio.sfx(0, 0, 0, 1);
        let mut samples = vec![0.0f32; 1470];
        audio.generate_samples(44100, &mut samples);
        assert!(samples.iter().any(|&s| s != 0.0));
    }

    #[test]
    fn test_effect_vibrato() {
        let mut audio = Audio::new();
        audio.sfx_data[0].notes[0] = Note { pitch: 24, waveform: 0, volume: 7, effect: 2 };
        audio.sfx_data[0].speed = 32;
        audio.sfx(0, 0, 0, 1);
        let mut samples = vec![0.0f32; 1470];
        audio.generate_samples(44100, &mut samples);
        assert!(samples.iter().any(|&s| s != 0.0));
    }

    #[test]
    fn test_effect_arpeggio_fast() {
        let mut audio = Audio::new();
        audio.sfx_data[0].notes[0] = Note { pitch: 24, waveform: 0, volume: 7, effect: 6 };
        audio.sfx_data[0].speed = 32;
        audio.sfx(0, 0, 0, 1);
        let mut samples = vec![0.0f32; 1470];
        audio.generate_samples(44100, &mut samples);
        assert!(samples.iter().any(|&s| s != 0.0));
    }

    #[test]
    fn test_effect_arpeggio_slow() {
        let mut audio = Audio::new();
        audio.sfx_data[0].notes[0] = Note { pitch: 24, waveform: 0, volume: 7, effect: 7 };
        audio.sfx_data[0].speed = 32;
        audio.sfx(0, 0, 0, 1);
        let mut samples = vec![0.0f32; 1470];
        audio.generate_samples(44100, &mut samples);
        assert!(samples.iter().any(|&s| s != 0.0));
    }

    #[test]
    fn test_samples_per_note_calculation() {
        assert_eq!(Audio::samples_per_note(1, 44100), 344);
        assert_eq!(Audio::samples_per_note(16, 44100), 5512);
        assert_eq!(Audio::samples_per_note(0, 44100), 344); // 0 treated as 1
    }

    #[test]
    fn test_phase_stays_in_range() {
        let mut audio = audio_with_sfx0();
        audio.sfx(0, 0, 0, 0);
        let mut buf = vec![0.0f32; 44100];
        audio.generate_samples(44100, &mut buf);
        if let Some(ref player) = audio.sfx_playing[0] {
            assert!(player.phase >= 0.0 && player.phase < 1.0, "Phase: {}", player.phase);
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
        assert!(audio.sfx_playing[0].is_some(), "Looping SFX should stay active");
        if let Some(ref player) = audio.sfx_playing[0] {
            let ni = player.note_index as usize;
            assert!(ni >= 2 && ni < 4, "Note index {} should be in [2, 4)", ni);
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
            assert!(s >= -1.0 && s <= 1.0, "Sample {} out of range: {}", i, s);
        }
    }

    #[test]
    fn test_music_triggers_sfx_and_produces_audio() {
        let mut audio = audio_with_sfx0();
        for i in 0..NOTES_PER_SFX {
            audio.sfx_data[1].notes[i] = Note {
                pitch: 36, waveform: 2, volume: 4, effect: 0,
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
        assert!(samples.iter().any(|&s| s != 0.0), "Music should produce audio");
    }
}
