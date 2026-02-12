// audio_output.rs — Desktop audio output bridge for Spark
//
// Approach: Each frame we generate one frame's worth of mono f32 audio samples
// from the Spark audio engine, encode them into a minimal WAV buffer in memory,
// and play them through macroquad's audio system via `load_sound_from_bytes` +
// `play_sound`.
//
// Why WAV-in-memory?
// macroquad 0.4's audio API accepts pre-decoded `Sound` objects loaded from
// file-like byte buffers (WAV, OGG, etc.). There is no raw PCM streaming API,
// no `delete_sound` function, and `Sound` has no `Drop` implementation that
// frees underlying resources. Constructing a tiny WAV each frame is cheap
// (~3 KB for 1470 mono 16-bit samples) and avoids adding any extra crate
// dependencies.
//
// Resource management:
// Since macroquad provides no way to free `Sound` handles, we maintain a small
// ring buffer (default capacity 3) of recently-created sounds. When a new sound
// is pushed, the oldest is stopped and its handle is dropped. While dropping a
// `Sound` handle doesn't release the underlying audio resource in macroquad, the
// ring buffer bounds the number of live handles and ensures we always stop old
// playback promptly. This prevents overlap buildup and limits the rate of
// resource accumulation to a bounded pool.
//
// Latency characteristics:
// At 30 fps with 44100 Hz sample rate, each frame produces ~1470 samples
// (~33 ms of audio). The audio will be one frame behind visuals, which is
// standard for frame-synchronized audio in retro consoles.

use macroquad::audio::{load_sound_from_bytes, play_sound, stop_sound, PlaySoundParams, Sound};

/// Sample rate used for audio generation (matches PICO-8's typical output).
pub const SAMPLE_RATE: u32 = 44100;

/// Target frames per second. PICO-8 runs at 30 fps.
const FPS: u32 = 30;

/// Number of samples to generate per frame at 30fps: SAMPLE_RATE / FPS.
/// At 44100 / 30 this is 1470 samples per frame.
pub const SAMPLES_PER_FRAME: usize = (SAMPLE_RATE / FPS) as usize;

/// Target frames per second for 60fps mode.
const FPS_60: u32 = 60;

/// Number of samples to generate per frame at 60fps: SAMPLE_RATE / FPS_60.
/// At 44100 / 60 this is 735 samples per frame.
pub const SAMPLES_PER_FRAME_60: usize = (SAMPLE_RATE / FPS_60) as usize;

/// Amplitude threshold below which a sample is considered silent.
/// This avoids encoding and playing frames that contain only near-zero noise,
/// which can happen due to floating-point rounding in the audio engine.
/// The value -60 dBFS (~0.001) is well below audible levels for a retro console.
const SILENCE_THRESHOLD: f32 = 0.001;

/// Number of Sound handles to keep in the ring buffer. A value of 3 means we
/// hold the currently-playing sound plus up to 2 previous ones that may still
/// be finishing playback. This provides a small safety margin against timing
/// variations while keeping resource usage bounded.
const SOUND_POOL_SIZE: usize = 3;

/// Manages the frame-by-frame audio output pipeline.
///
/// Internally maintains:
/// - A ring buffer of recent `Sound` handles to bound resource usage
/// - A reusable byte buffer for WAV encoding (avoids per-frame allocation)
/// - A master volume level applied during WAV encoding
///
/// # Usage
///
/// ```ignore
/// let mut audio_output = AudioOutput::new();
/// // Optionally set volume (0.0 = silent, 1.0 = full, default = 1.0)
/// audio_output.set_volume(0.8);
/// // Each frame:
/// audio_output.play_frame_audio(&mut audio_engine).await;
/// // When stopping:
/// audio_output.stop();
/// ```
pub struct AudioOutput {
    /// Ring buffer of recent Sound handles. When full, the oldest is stopped
    /// and evicted before a new one is pushed. This prevents unbounded
    /// accumulation of Sound resources.
    sound_pool: Vec<Sound>,

    /// Index of the next slot to write in `sound_pool`. Wraps around at
    /// `SOUND_POOL_SIZE`.
    pool_index: usize,

    /// Master volume level in the range [0.0, 1.0]. Applied as a multiplier
    /// to all samples during WAV encoding, so it affects the actual PCM data
    /// rather than relying on macroquad's per-sound volume (which would only
    /// apply after decoding).
    volume: f32,

    /// Reusable byte buffer for the WAV encoding. Kept allocated across frames
    /// to avoid repeated heap allocation. Pre-sized for one frame's worth of
    /// audio at construction time.
    wav_buf: Vec<u8>,
}

impl AudioOutput {
    /// Create a new `AudioOutput` with default settings.
    ///
    /// Volume starts at 1.0 (full). No system audio resources are allocated
    /// until `play_frame_audio` is called.
    pub fn new() -> Self {
        // Pre-allocate the WAV buffer: 44-byte header + 2 bytes per sample.
        let wav_capacity = 44 + SAMPLES_PER_FRAME * 2;
        AudioOutput {
            sound_pool: Vec::with_capacity(SOUND_POOL_SIZE),
            pool_index: 0,
            volume: 1.0,
            wav_buf: Vec::with_capacity(wav_capacity),
        }
    }

    /// Set the master volume level.
    ///
    /// `vol` is clamped to the range [0.0, 1.0]. A value of 0.0 produces
    /// silence (frames are skipped entirely), and 1.0 is full volume.
    ///
    /// The volume is applied as a multiplier during WAV encoding, so it
    /// affects the actual PCM sample values written to the buffer.
    #[allow(dead_code)]
    pub fn set_volume(&mut self, vol: f32) {
        self.volume = vol.clamp(0.0, 1.0);
    }

    /// Get the current master volume level (0.0 to 1.0).
    #[allow(dead_code)]
    pub fn volume(&self) -> f32 {
        self.volume
    }

    /// Generate audio for one frame at 30fps and play it.
    ///
    /// Convenience wrapper around `play_frame_audio_with_fps` for the default
    /// 30fps mode (generates `SAMPLES_PER_FRAME` = 1470 samples).
    #[allow(dead_code)]
    pub async fn play_frame_audio(&mut self, audio: &mut spark_core::audio::Audio) {
        self.play_frame_audio_with_fps(audio, false).await;
    }

    /// Generate audio for one frame and play it, with FPS-aware sample count.
    ///
    /// When `is_60fps` is true, generates half the samples per frame (735
    /// instead of 1470) since frames come twice as fast. Otherwise behaves
    /// identically to `play_frame_audio`.
    pub async fn play_frame_audio_with_fps(
        &mut self,
        audio: &mut spark_core::audio::Audio,
        is_60fps: bool,
    ) {
        let num_samples = if is_60fps {
            SAMPLES_PER_FRAME_60
        } else {
            SAMPLES_PER_FRAME
        };

        // Generate one frame of mono f32 samples from the audio engine
        // into a stack-allocated buffer, avoiding per-frame heap allocation.
        let mut samples = [0.0f32; SAMPLES_PER_FRAME];
        let buf = &mut samples[..num_samples];
        audio.generate_samples(SAMPLE_RATE, buf);

        // If volume is effectively zero, treat as silent -- skip all work.
        if self.volume < SILENCE_THRESHOLD {
            return;
        }

        // Check if the entire frame is silent (all samples below threshold).
        if is_silent(buf) {
            return;
        }

        // Encode the f32 samples as a 16-bit mono PCM WAV in our reusable
        // buffer, applying the master volume during encoding.
        encode_wav_16bit_mono(buf, SAMPLE_RATE, self.volume, &mut self.wav_buf);

        // Load the WAV bytes as a macroquad Sound.
        match load_sound_from_bytes(&self.wav_buf).await {
            Ok(sound) => {
                play_sound(
                    &sound,
                    PlaySoundParams {
                        looped: false,
                        volume: 1.0,
                    },
                );
                self.push_sound(sound);
            }
            Err(e) => {
                eprintln!("[spark audio] failed to load sound buffer: {}", e);
            }
        }
    }

    /// Push a new Sound into the ring buffer, stopping and evicting the oldest
    /// sound if the pool is full.
    fn push_sound(&mut self, sound: Sound) {
        if self.sound_pool.len() < SOUND_POOL_SIZE {
            // Pool not yet full: just append.
            self.sound_pool.push(sound);
            self.pool_index = self.sound_pool.len();
        } else {
            // Pool is full: stop the oldest sound and overwrite its slot.
            let idx = self.pool_index % SOUND_POOL_SIZE;
            stop_sound(&self.sound_pool[idx]);
            self.sound_pool[idx] = sound;
            self.pool_index = idx + 1;
        }
    }

    /// Stop all currently-playing audio and release all sound handles.
    ///
    /// Call this when leaving Running mode (e.g., pressing Escape to return
    /// to the editor). This stops every sound in the pool and clears the pool,
    /// ensuring no audio continues playing after the game stops.
    pub fn stop(&mut self) {
        for sound in &self.sound_pool {
            stop_sound(sound);
        }
        self.sound_pool.clear();
        self.pool_index = 0;
    }
}

/// Check whether a buffer of audio samples is effectively silent.
///
/// Returns `true` if every sample's absolute value is below `SILENCE_THRESHOLD`.
/// This is more robust than exact `== 0.0` comparison, as it catches near-zero
/// noise from floating-point arithmetic in the audio engine.
fn is_silent(samples: &[f32]) -> bool {
    samples.iter().all(|&s| s.abs() < SILENCE_THRESHOLD)
}

/// Encode a slice of f32 samples (range -1.0 to 1.0) as a 16-bit mono PCM
/// WAV into the provided buffer, applying a volume multiplier.
///
/// The buffer is cleared and filled with:
///   - 44-byte RIFF/WAV header
///   - 16-bit little-endian PCM sample data (volume-scaled)
///
/// This is a minimal WAV encoder — no chunks beyond "fmt " and "data".
///
/// # Arguments
///
/// * `samples` — Audio samples in the range [-1.0, 1.0]
/// * `sample_rate` — Sample rate in Hz (e.g., 44100)
/// * `volume` — Volume multiplier in the range [0.0, 1.0], applied to each
///   sample before encoding. Values outside this range are clamped after
///   multiplication (the sample is clamped, not the volume parameter).
/// * `buf` — Reusable output buffer; cleared and overwritten each call
fn encode_wav_16bit_mono(samples: &[f32], sample_rate: u32, volume: f32, buf: &mut Vec<u8>) {
    let num_samples = samples.len();
    let bits_per_sample: u16 = 16;
    let num_channels: u16 = 1;
    let byte_rate = sample_rate * (bits_per_sample as u32 / 8) * num_channels as u32;
    let block_align = num_channels * (bits_per_sample / 8);
    let data_size = (num_samples * 2) as u32; // 2 bytes per 16-bit sample
    let file_size = 36 + data_size; // Total file size minus 8 bytes for RIFF header

    buf.clear();
    // Ensure we have enough capacity without repeated reallocation.
    buf.reserve(44 + num_samples * 2);

    // --- RIFF header ---
    buf.extend_from_slice(b"RIFF");
    buf.extend_from_slice(&file_size.to_le_bytes());
    buf.extend_from_slice(b"WAVE");

    // --- fmt sub-chunk ---
    buf.extend_from_slice(b"fmt ");
    buf.extend_from_slice(&16u32.to_le_bytes()); // Sub-chunk size (16 for PCM)
    buf.extend_from_slice(&1u16.to_le_bytes()); // Audio format: 1 = PCM
    buf.extend_from_slice(&num_channels.to_le_bytes());
    buf.extend_from_slice(&sample_rate.to_le_bytes());
    buf.extend_from_slice(&byte_rate.to_le_bytes());
    buf.extend_from_slice(&block_align.to_le_bytes());
    buf.extend_from_slice(&bits_per_sample.to_le_bytes());

    // --- data sub-chunk ---
    buf.extend_from_slice(b"data");
    buf.extend_from_slice(&data_size.to_le_bytes());

    // Convert f32 [-1.0, 1.0] to i16 [-32768, 32767] and write as little-endian.
    // Volume is applied as a multiplier before clamping, so it smoothly scales
    // the output without introducing distortion (beyond what the source already has).
    for &sample in samples {
        let scaled_sample = sample * volume;
        let clamped = scaled_sample.clamp(-1.0, 1.0);
        let pcm = (clamped * 32767.0) as i16;
        buf.extend_from_slice(&pcm.to_le_bytes());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // -----------------------------------------------------------------------
    // WAV encoder: header structure
    // -----------------------------------------------------------------------

    #[test]
    fn test_wav_encoding_header_structure() {
        let samples = vec![0.0f32; 100];
        let mut buf = Vec::new();
        encode_wav_16bit_mono(&samples, 44100, 1.0, &mut buf);

        // Total size: 44 header + 200 data bytes = 244
        assert_eq!(buf.len(), 244);

        // Check RIFF header
        assert_eq!(&buf[0..4], b"RIFF");
        let file_size = u32::from_le_bytes([buf[4], buf[5], buf[6], buf[7]]);
        assert_eq!(file_size, 236); // 244 - 8

        assert_eq!(&buf[8..12], b"WAVE");

        // Check fmt chunk
        assert_eq!(&buf[12..16], b"fmt ");
        let fmt_size = u32::from_le_bytes([buf[16], buf[17], buf[18], buf[19]]);
        assert_eq!(fmt_size, 16);
        let audio_format = u16::from_le_bytes([buf[20], buf[21]]);
        assert_eq!(audio_format, 1); // PCM
        let channels = u16::from_le_bytes([buf[22], buf[23]]);
        assert_eq!(channels, 1); // mono
        let sr = u32::from_le_bytes([buf[24], buf[25], buf[26], buf[27]]);
        assert_eq!(sr, 44100);

        // Check data chunk
        assert_eq!(&buf[36..40], b"data");
        let data_size = u32::from_le_bytes([buf[40], buf[41], buf[42], buf[43]]);
        assert_eq!(data_size, 200); // 100 samples * 2 bytes
    }

    #[test]
    fn test_wav_encoding_sample_values() {
        let samples = vec![1.0f32, -1.0, 0.0, 0.5, -0.5];
        let mut buf = Vec::new();
        encode_wav_16bit_mono(&samples, 44100, 1.0, &mut buf);

        // Read back the 16-bit samples from the data section (offset 44).
        let s0 = i16::from_le_bytes([buf[44], buf[45]]);
        let s1 = i16::from_le_bytes([buf[46], buf[47]]);
        let s2 = i16::from_le_bytes([buf[48], buf[49]]);
        let s3 = i16::from_le_bytes([buf[50], buf[51]]);
        let s4 = i16::from_le_bytes([buf[52], buf[53]]);

        assert_eq!(s0, 32767);  // 1.0
        assert_eq!(s1, -32767); // -1.0 (clamped, so -32767 not -32768)
        assert_eq!(s2, 0);      // 0.0
        assert!((s3 - 16383).abs() <= 1); // 0.5 ~ 16383
        assert!((s4 - (-16383)).abs() <= 1); // -0.5 ~ -16383
    }

    #[test]
    fn test_wav_encoding_clamps_out_of_range() {
        let samples = vec![2.0f32, -3.0];
        let mut buf = Vec::new();
        encode_wav_16bit_mono(&samples, 44100, 1.0, &mut buf);

        let s0 = i16::from_le_bytes([buf[44], buf[45]]);
        let s1 = i16::from_le_bytes([buf[46], buf[47]]);

        assert_eq!(s0, 32767);  // clamped from 2.0
        assert_eq!(s1, -32767); // clamped from -3.0
    }

    #[test]
    fn test_wav_buffer_reuse() {
        let mut buf = Vec::new();

        // First encode
        encode_wav_16bit_mono(&[0.0; 50], 44100, 1.0, &mut buf);
        assert_eq!(buf.len(), 44 + 100);

        // Second encode with different size — should correctly overwrite
        encode_wav_16bit_mono(&[0.0; 100], 44100, 1.0, &mut buf);
        assert_eq!(buf.len(), 44 + 200);
    }

    // -----------------------------------------------------------------------
    // Silence detection
    // -----------------------------------------------------------------------

    #[test]
    fn test_silence_detection_all_zero() {
        let samples = vec![0.0f32; 1470];
        assert!(is_silent(&samples));
    }

    #[test]
    fn test_silence_detection_below_threshold() {
        // Values just below the silence threshold should be considered silent.
        let samples = vec![0.0005f32, -0.0005, 0.0009, -0.0001];
        assert!(is_silent(&samples));
    }

    #[test]
    fn test_silence_detection_above_threshold() {
        // A single sample above the threshold makes the frame non-silent.
        let mut samples = vec![0.0f32; 1000];
        samples[500] = 0.01;
        assert!(!is_silent(&samples));
    }

    #[test]
    fn test_silence_detection_empty() {
        // An empty sample buffer is trivially silent.
        let samples: Vec<f32> = vec![];
        assert!(is_silent(&samples));
    }

    #[test]
    fn test_silence_detection_negative_above_threshold() {
        let samples = vec![-0.5f32];
        assert!(!is_silent(&samples));
    }

    // -----------------------------------------------------------------------
    // Volume scaling
    // -----------------------------------------------------------------------

    #[test]
    fn test_volume_scaling_full() {
        // At volume 1.0, output should match the original encoding.
        let samples = vec![0.5f32, -0.5];
        let mut buf = Vec::new();
        encode_wav_16bit_mono(&samples, 44100, 1.0, &mut buf);

        let s0 = i16::from_le_bytes([buf[44], buf[45]]);
        let s1 = i16::from_le_bytes([buf[46], buf[47]]);

        assert!((s0 - 16383).abs() <= 1);
        assert!((s1 - (-16383)).abs() <= 1);
    }

    #[test]
    fn test_volume_scaling_half() {
        // At volume 0.5, a 1.0 sample should encode as ~0.5 (16383).
        let samples = vec![1.0f32, -1.0];
        let mut buf = Vec::new();
        encode_wav_16bit_mono(&samples, 44100, 0.5, &mut buf);

        let s0 = i16::from_le_bytes([buf[44], buf[45]]);
        let s1 = i16::from_le_bytes([buf[46], buf[47]]);

        assert!((s0 - 16383).abs() <= 1);
        assert!((s1 - (-16383)).abs() <= 1);
    }

    #[test]
    fn test_volume_scaling_zero() {
        // At volume 0.0, all samples should encode as 0.
        let samples = vec![1.0f32, -1.0, 0.5, -0.5];
        let mut buf = Vec::new();
        encode_wav_16bit_mono(&samples, 44100, 0.0, &mut buf);

        for i in 0..4 {
            let offset = 44 + i * 2;
            let s = i16::from_le_bytes([buf[offset], buf[offset + 1]]);
            assert_eq!(s, 0, "sample {} should be 0 at volume 0.0", i);
        }
    }

    #[test]
    fn test_volume_scaling_with_clamp() {
        // Volume > 1.0 could push samples out of range. The encoder should
        // clamp after applying volume, but volume is only in [0, 1] via
        // set_volume. Test that the encoder handles the math correctly even
        // if volume = 1.0 and sample > 1.0.
        let samples = vec![1.5f32];
        let mut buf = Vec::new();
        encode_wav_16bit_mono(&samples, 44100, 1.0, &mut buf);

        let s0 = i16::from_le_bytes([buf[44], buf[45]]);
        assert_eq!(s0, 32767); // clamped
    }

    // -----------------------------------------------------------------------
    // Volume get/set
    // -----------------------------------------------------------------------

    #[test]
    fn test_set_volume_default() {
        let output = AudioOutput::new();
        assert_eq!(output.volume(), 1.0);
    }

    #[test]
    fn test_set_volume_normal() {
        let mut output = AudioOutput::new();
        output.set_volume(0.5);
        assert_eq!(output.volume(), 0.5);
    }

    #[test]
    fn test_set_volume_clamps_high() {
        let mut output = AudioOutput::new();
        output.set_volume(2.0);
        assert_eq!(output.volume(), 1.0);
    }

    #[test]
    fn test_set_volume_clamps_low() {
        let mut output = AudioOutput::new();
        output.set_volume(-1.0);
        assert_eq!(output.volume(), 0.0);
    }

    #[test]
    fn test_set_volume_zero() {
        let mut output = AudioOutput::new();
        output.set_volume(0.0);
        assert_eq!(output.volume(), 0.0);
    }

    // -----------------------------------------------------------------------
    // Buffer reuse and edge cases
    // -----------------------------------------------------------------------

    #[test]
    fn test_wav_encoding_empty_samples() {
        // Edge case: encoding zero samples should produce a valid WAV header
        // with an empty data section.
        let samples: Vec<f32> = vec![];
        let mut buf = Vec::new();
        encode_wav_16bit_mono(&samples, 44100, 1.0, &mut buf);

        // Should be exactly the 44-byte header with 0 data bytes.
        assert_eq!(buf.len(), 44);

        // RIFF header should report file_size = 36 (44 - 8 + 0 data)
        let file_size = u32::from_le_bytes([buf[4], buf[5], buf[6], buf[7]]);
        assert_eq!(file_size, 36);

        // Data size should be 0
        let data_size = u32::from_le_bytes([buf[40], buf[41], buf[42], buf[43]]);
        assert_eq!(data_size, 0);
    }

    #[test]
    fn test_wav_encoding_large_buffer() {
        // Edge case: a very large sample buffer (e.g., 1 second at 44100 Hz).
        let samples = vec![0.25f32; 44100];
        let mut buf = Vec::new();
        encode_wav_16bit_mono(&samples, 44100, 1.0, &mut buf);

        // 44 header + 44100 * 2 = 88244
        assert_eq!(buf.len(), 44 + 44100 * 2);

        // Spot-check a few samples
        let s0 = i16::from_le_bytes([buf[44], buf[45]]);
        let expected = (0.25f32 * 32767.0) as i16;
        assert_eq!(s0, expected);
    }

    #[test]
    fn test_wav_buffer_reuse_shrink() {
        // Verify that encoding a smaller buffer after a larger one works
        // correctly (buf.clear() resets length but keeps capacity).
        let mut buf = Vec::new();

        encode_wav_16bit_mono(&[0.0; 1000], 44100, 1.0, &mut buf);
        assert_eq!(buf.len(), 44 + 2000);
        let cap_after_large = buf.capacity();

        encode_wav_16bit_mono(&[0.0; 10], 44100, 1.0, &mut buf);
        assert_eq!(buf.len(), 44 + 20);
        // Capacity should not have shrunk.
        assert!(buf.capacity() >= cap_after_large);
    }

    #[test]
    fn test_wav_encoding_different_sample_rates() {
        // The encoder should correctly write whatever sample rate is given.
        let samples = vec![0.0f32; 10];
        let mut buf = Vec::new();
        encode_wav_16bit_mono(&samples, 22050, 1.0, &mut buf);

        let sr = u32::from_le_bytes([buf[24], buf[25], buf[26], buf[27]]);
        assert_eq!(sr, 22050);

        let byte_rate = u32::from_le_bytes([buf[28], buf[29], buf[30], buf[31]]);
        assert_eq!(byte_rate, 22050 * 2); // 16-bit mono = 2 bytes per sample
    }

    // -----------------------------------------------------------------------
    // AudioOutput construction
    // -----------------------------------------------------------------------

    #[test]
    fn test_audio_output_new() {
        let output = AudioOutput::new();
        assert_eq!(output.volume(), 1.0);
        assert_eq!(output.sound_pool.len(), 0);
        assert_eq!(output.pool_index, 0);
        // WAV buffer should be pre-allocated but empty.
        assert_eq!(output.wav_buf.len(), 0);
        assert!(output.wav_buf.capacity() >= 44 + SAMPLES_PER_FRAME * 2);
    }
}
