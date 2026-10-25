//! PWM audio output for RP2350, driven from a timer interrupt.
//!
//! The PICO-8 audio engine produces mono `f32` samples in `-1.0..1.0`. This
//! module converts them to 8-bit PWM duty values, queues them in a ring
//! buffer, and a 22 050 Hz interrupt pops one per tick into the PWM compare
//! register. The main loop is therefore free to be paced by the system timer
//! (see `main.rs`); audio no longer doubles as a busy-wait frame pacer, which
//! is what made every frame 38-45 ms with an audible gap between frames.
//!
//! Hardware
//! --------
//! * GPIO 8 -> low-pass RC filter (e.g. 1 kOhm + 100 nF) -> speaker/amplifier.
//!   GPIO 8 is PWM slice 4, channel A. Slice 4 is the *carrier*: TOP = 255,
//!   phase-correct, divider 1 -> 150 MHz / 512 = ~293 kHz, far above audio
//!   and trivially filtered.
//! * PWM slice 5 (no pin attached) is the *sample clock*: it free-runs with a
//!   wrap period of `sys_hz / 22050` cycles and raises `PWM_IRQ_WRAP_0` on
//!   every wrap. Its top is `sys_hz / SAMPLE_RATE` (period one cycle longer),
//!   so it ticks at 22 049.1 Hz -- 0.004 % *slower* than the producer. The
//!   ring therefore fills very slowly (~1 sample/s) instead of draining, and
//!   the producer's block-when-full backstop absorbs the drift once every
//!   ~20 min instead of the ISR under-running twice a second.
//!
//! Concurrency invariants
//! ----------------------
//! Single producer / single consumer:
//! * `push_samples` (producer) runs only on the main thread.
//! * `PWM_IRQ_WRAP_0` (consumer) is the only interrupt handler in the
//!   firmware, and it touches **only** the statics in this module -- never
//!   the console, audio engine or Lua state.
//! * `HEAD` is written only by the producer, `TAIL` only by the consumer.
//!   Both are monotonically increasing (wrapping) counters; the ring index is
//!   `counter % RING_LEN` and the fill level is `HEAD - TAIL`, so full
//!   (`fill == RING_LEN`) and empty (`fill == 0`) are distinguishable.
//! * Sample bytes are published with a Release store to `HEAD` after the
//!   ring slot is written, and consumed after an Acquire load of `HEAD`.
//! * The PWM slice handles live in a `critical_section::Mutex` and are only
//!   touched inside the ISR (after `start`).
//!
//! Underrun (ring empty when the ISR fires) holds the last duty value, which
//! is inaudible for a sample or two. Overrun cannot lose samples: the
//! producer waits (`wfi`) for the ISR to free a slot.
//!
//! Board split
//! -----------
//! The PWM/IRQ half (`start`, `push_samples`, the ISR) exists only under the
//! `board-pico2` feature; boards without audio hardware (`Board::audio ==
//! None`) still use `SAMPLE_RATE`, `MAX_SAMPLES_PER_TICK` and `SampleClock`
//! so the synth advances at the right rate.

#[cfg(feature = "board-pico2")]
use core::cell::RefCell;
#[cfg(feature = "board-pico2")]
use core::sync::atomic::{AtomicU8, AtomicUsize, Ordering};

#[cfg(feature = "board-pico2")]
use critical_section::Mutex;
#[cfg(feature = "board-pico2")]
use embedded_hal::pwm::SetDutyCycle;
#[cfg(feature = "board-pico2")]
use rp235x_hal::pac::{interrupt, Interrupt};
#[cfg(feature = "board-pico2")]
use rp235x_hal::pwm::{FreeRunning, Pwm4, Pwm5, Slice};

#[cfg(feature = "board-pico2")]
use crate::board::AudioBackend;

/// Audio sample rate in Hz. Must match what `Audio::generate_samples` is told.
pub const SAMPLE_RATE: u32 = 22050;

/// Upper bound on samples generated per game tick (30 fps is the slowest
/// supported rate: 22050 / 30 = 735). `SampleClock::next` never exceeds it.
pub const MAX_SAMPLES_PER_TICK: usize = SAMPLE_RATE.div_ceil(30) as usize;

/// 8-bit PWM resolution (TOP register value) for the carrier slice.
#[cfg(feature = "board-pico2")]
const PWM_TOP: u16 = 255;

/// Duty value for silence (mid-rail).
#[cfg(feature = "board-pico2")]
const MID_DUTY: u8 = 128;

/// Ring capacity in samples. Power of two, and comfortably more than two
/// 30 fps frames (2 x 735 = 1470) so one late frame never under-runs.
#[cfg(feature = "board-pico2")]
const RING_LEN: usize = 2048;

// Zero-initialised (not MID_DUTY) so it lives in .bss rather than costing 2 KB
// of .data; every slot is written by the producer before the consumer reads it.
#[cfg(feature = "board-pico2")]
static RING: [AtomicU8; RING_LEN] = [const { AtomicU8::new(0) }; RING_LEN];
/// Producer cursor (samples written so far).
#[cfg(feature = "board-pico2")]
static HEAD: AtomicUsize = AtomicUsize::new(0);
/// Consumer cursor (samples played so far).
#[cfg(feature = "board-pico2")]
static TAIL: AtomicUsize = AtomicUsize::new(0);
/// Last duty written to the PWM; repeated on underrun to avoid a step.
#[cfg(feature = "board-pico2")]
static LAST_DUTY: AtomicU8 = AtomicU8::new(MID_DUTY);

/// The two PWM slices, owned by the ISR after `start`.
#[cfg(feature = "board-pico2")]
struct Hw {
    /// Carrier on GPIO 8 (channel A).
    carrier: Slice<Pwm4, FreeRunning>,
    /// Sample-rate tick source; its wrap interrupt drives everything.
    tick: Slice<Pwm5, FreeRunning>,
}

#[cfg(feature = "board-pico2")]
static HW: Mutex<RefCell<Option<Hw>>> = Mutex::new(RefCell::new(None));

/// Configure both PWM slices and start the sample-rate interrupt.
///
/// `hw.carrier.channel_a` must already be routed to the audio pin
/// (`output_to`, done by `board::init`). `sys_hz` is the system clock
/// (150 MHz on a stock Pico 2).
#[cfg(feature = "board-pico2")]
pub fn start(hw: AudioBackend, sys_hz: u32) {
    let AudioBackend {
        mut carrier,
        mut tick,
    } = hw;
    carrier.set_ph_correct();
    carrier.set_top(PWM_TOP);
    carrier.set_div_int(1);
    carrier.set_div_frac(0);
    // Mid-rail before enabling so there is no DC pop at power-up.
    let _ = carrier.channel_a.set_duty_cycle(MID_DUTY as u16);
    carrier.enable();

    // Wrap period is TOP + 1 cycles; see the module docs for why we round
    // the period *up* (slightly slow) rather than to nearest.
    let top = (sys_hz / SAMPLE_RATE).min(u16::MAX as u32) as u16;
    tick.set_top(top);
    tick.set_div_int(1);
    tick.set_div_frac(0);
    tick.clear_interrupt();
    tick.enable_interrupt0();
    tick.enable();

    critical_section::with(|cs| {
        HW.borrow(cs).replace(Some(Hw { carrier, tick }));
    });

    // Safety: the handler below is the only user of the PWM slices and of the
    // ring statics' consumer side; unmasking after `HW` is populated means it
    // never observes `None` in steady state.
    unsafe {
        cortex_m::peripheral::NVIC::unmask(Interrupt::PWM_IRQ_WRAP_0);
    }
}

/// Convert one `-1.0..1.0` sample to a PWM duty value.
#[cfg(feature = "board-pico2")]
#[inline]
fn to_duty(sample: f32) -> u8 {
    // Manual clamp avoids pulling in the f32 clamp libcall; NaN falls through
    // both comparisons and the `as u8` cast then saturates to 0 -- acceptable
    // for a value that should never occur.
    let s = if sample > 1.0 {
        1.0
    } else if sample < -1.0 {
        -1.0
    } else {
        sample
    };
    ((s + 1.0) * (PWM_TOP as f32 * 0.5)) as u8
}

/// Queue a block of samples for playback.
///
/// Blocks (sleeping with `wfi`, woken by the 22 kHz tick) while the ring is
/// full, so no sample is ever dropped. Only call after [`start`]; before it
/// there is no consumer and a full ring would wait forever.
#[cfg(feature = "board-pico2")]
pub fn push_samples(samples: &[f32]) {
    let mut head = HEAD.load(Ordering::Relaxed);
    for &sample in samples {
        // Wait for at least one free slot.
        while head.wrapping_sub(TAIL.load(Ordering::Acquire)) >= RING_LEN {
            cortex_m::asm::wfi();
        }
        RING[head % RING_LEN].store(to_duty(sample), Ordering::Relaxed);
        head = head.wrapping_add(1);
        HEAD.store(head, Ordering::Release);
    }
}

/// Splits `SAMPLE_RATE` samples/second evenly across game ticks at `fps`,
/// carrying the remainder so the long-run average is exact (60 fps is not an
/// integer number of samples per tick: 367.5).
pub struct SampleClock {
    fps: u32,
    rem: u32,
}

impl SampleClock {
    pub fn new(fps: u32) -> Self {
        Self {
            fps: fps.max(1),
            rem: 0,
        }
    }

    /// Number of samples to generate for the next tick (never more than
    /// [`MAX_SAMPLES_PER_TICK`] for fps >= 30).
    pub fn next(&mut self) -> usize {
        let total = SAMPLE_RATE + self.rem;
        self.rem = total % self.fps;
        ((total / self.fps) as usize).min(MAX_SAMPLES_PER_TICK)
    }
}

/// 22 050 Hz sample tick: pop one duty value into the carrier PWM.
#[cfg(feature = "board-pico2")]
#[interrupt]
fn PWM_IRQ_WRAP_0() {
    critical_section::with(|cs| {
        let mut hw = HW.borrow_ref_mut(cs);
        let Some(hw) = hw.as_mut() else {
            return;
        };
        hw.tick.clear_interrupt();

        let tail = TAIL.load(Ordering::Relaxed);
        let head = HEAD.load(Ordering::Acquire);
        let duty = if head == tail {
            // Underrun: hold the previous level.
            LAST_DUTY.load(Ordering::Relaxed)
        } else {
            let d = RING[tail % RING_LEN].load(Ordering::Relaxed);
            TAIL.store(tail.wrapping_add(1), Ordering::Release);
            LAST_DUTY.store(d, Ordering::Relaxed);
            d
        };
        // Infallible on rp235x-hal.
        let _ = hw.carrier.channel_a.set_duty_cycle(duty as u16);
    });
}
