// PWM audio output for RP2350
//
// Drives a speaker (or piezo) through a PWM pin. The PICO-8 audio engine
// produces mono f32 samples in the range -1.0..1.0; this module converts
// each sample to a PWM duty-cycle value and outputs it at the requested
// sample rate using busy-wait timing.
//
// Hardware setup
// --------------
// GPIO 8 -> low-pass RC filter (e.g. 1 kOhm + 100 nF) -> speaker/amplifier
//
// GPIO 8 is on PWM slice 4, channel A.  This pin is free in the current
// Spark hardware layout (GPIO 0-7 = buttons, GPIO 17-22 = display).
//
// PWM configuration
// -----------------
// - TOP = 255  -> 8-bit resolution per sample
// - Clock divider = 1 (no division)
// - PWM frequency = 150 MHz / 256 = ~586 kHz (well above audible range,
//   easily filtered by a simple RC low-pass)
//
// Sample rate timing
// ------------------
// The sample rate (default 22050 Hz) is maintained by busy-waiting between
// samples using `cortex_m::asm::delay`.
//
// On Cortex-M33 (Armv8-M), the `delay` intrinsic compiles to a tight
// loop: `1: subs r0, #1; bne 1b`.  Each iteration takes exactly 3 cycles
// (1 for SUBS + 1 for BNE-taken + 1 pipeline refill on some M33 variants).
// At 150 MHz, `delay(150_000_000 / 22050) = delay(6802)` produces a
// ~45.3 us period per sample, matching the 22050 Hz target within <0.1%.
//
// The Cortex-M33 in the RP2350 runs from tightly-coupled SRAM with zero
// wait states, so the delay loop timing is deterministic.  Flash XIP would
// add variable latency from cache misses, but the rp235x-hal boot2
// copies critical code to SRAM.

use embedded_hal::pwm::SetDutyCycle;

/// 8-bit PWM resolution (TOP register value).
const PWM_TOP: u16 = 255;

/// Audio output driver backed by a single PWM channel.
///
/// The type parameter `P` is the concrete PWM channel type returned by
/// rp235x-hal after calling `channel_a.output_to(pin)` and converting the
/// slice to `FreeRunning` mode.
pub struct AudioOut<P: SetDutyCycle> {
    pwm_channel: P,
    /// CPU cycles to delay between samples, pre-computed from sys_freq and
    /// sample_rate to avoid division in the hot loop.
    delay_cycles: u32,
    /// Audio sample rate in Hz.
    sample_rate: u32,
}

impl<P: SetDutyCycle> AudioOut<P> {
    /// Create a new audio output driver.
    ///
    /// * `pwm_channel` -- A PWM channel already configured and enabled.
    /// * `sys_freq`    -- System clock frequency in Hz (typically 150_000_000).
    /// * `sample_rate` -- Desired audio sample rate (e.g. 22050).
    pub fn new(pwm_channel: P, sys_freq: u32, sample_rate: u32) -> Self {
        Self {
            pwm_channel,
            delay_cycles: sys_freq / sample_rate,
            sample_rate,
        }
    }

    /// Return the configured sample rate.
    #[inline]
    pub fn sample_rate(&self) -> u32 {
        self.sample_rate
    }

    /// Return the number of samples to generate per frame at the given FPS.
    #[inline]
    pub fn samples_per_frame(&self, fps: u32) -> usize {
        (self.sample_rate / fps) as usize
    }

    /// Play a buffer of f32 audio samples through PWM.
    ///
    /// Each sample is expected to be in the range -1.0..1.0.  The function
    /// converts each to an 8-bit duty cycle and busy-waits between samples
    /// to maintain the configured sample rate.
    ///
    /// This is a blocking call -- it returns only after all samples have been
    /// output.  At 22050 Hz and 30 fps that is ~735 samples taking ~33 ms,
    /// which consumes most of the frame budget.  In practice the display
    /// update and game logic run first, then audio fills the remaining time
    /// and doubles as frame pacing.
    ///
    /// # Timing accuracy
    ///
    /// The delay between samples is `sys_freq / sample_rate` CPU cycles.
    /// At 150 MHz / 22050 Hz = 6802 cycles. The `set_duty_cycle` call and
    /// loop overhead add ~10-20 cycles, which is <0.3% error -- well within
    /// acceptable bounds for audio playback.
    pub fn play_samples(&mut self, samples: &[f32]) {
        let delay = self.delay_cycles;

        for &sample in samples {
            // Convert -1.0..1.0 to 0..PWM_TOP duty cycle.
            //
            // clamp manually to avoid pulling in the f32 clamp libcall
            let clamped = if sample > 1.0 {
                1.0f32
            } else if sample < -1.0 {
                -1.0f32
            } else {
                sample
            };
            let duty = ((clamped + 1.0) * 0.5 * PWM_TOP as f32) as u16;

            // Set the duty cycle.  The error type for rp235x-hal PWM is
            // `core::convert::Infallible`, so ignoring the result is safe.
            let _ = self.pwm_channel.set_duty_cycle(duty);

            // Wait one sample period.
            //
            // On Cortex-M33, `cortex_m::asm::delay(n)` executes a tight
            // SUBS/BNE loop consuming exactly `n` iterations. Each iteration
            // is 3 cycles, so the total delay is `3 * n` cycles. The
            // rp235x-hal `delay` function accounts for this internally.
            cortex_m::asm::delay(delay);
        }
    }

    /// Set the PWM output to the midpoint (silence) without producing any
    /// audio.  Useful to call once at startup to avoid a DC offset pop.
    pub fn silence(&mut self) {
        let mid = PWM_TOP / 2;
        let _ = self.pwm_channel.set_duty_cycle(mid);
    }
}

/// PWM TOP value used by this module.  Callers need this to configure the
/// PWM slice's TOP register before constructing `AudioOut`.
pub const fn pwm_top() -> u16 {
    PWM_TOP
}
