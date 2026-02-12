#![no_std]
#![no_main]

extern crate alloc;

use core::mem::MaybeUninit;
use embedded_alloc::LlffHeap as Heap;
use embedded_hal::digital::OutputPin;
use fugit::RateExtU32;
use panic_halt as _;
use rp235x_hal::Clock;

mod audio_out;
mod cart_baked;
mod display;
mod input;
mod lua_ffi;

// ---------------------------------------------------------------------------
// Global allocator -- 200 KB heap for Lua + general allocations
// ---------------------------------------------------------------------------
//
// Memory budget (520 KB total SRAM):
//   Console (static)     ~65 KB
//   Audio   (static)      ~9 KB
//   Heap (Lua + alloc)  200 KB
//   Stack                 ~8 KB (cortex-m-rt default)
//   BSS/data/other      ~238 KB remaining
//
// Lua typically uses 30-60 KB for a PICO-8 cart. The 200 KB budget leaves
// ample headroom for complex carts and avoids out-of-memory during
// `luaL_openlibs`.
#[global_allocator]
static HEAP: Heap = Heap::empty();

const HEAP_SIZE: usize = 200 * 1024;
static mut HEAP_MEM: [u8; HEAP_SIZE] = [0u8; HEAP_SIZE];

// ---------------------------------------------------------------------------
// Static storage for large structs -- avoids stack overflow
// ---------------------------------------------------------------------------
//
// Console (~65 KB) and Audio (~9 KB) far exceed the Cortex-M33 stack
// (typically 8 KB). Placing them in statics keeps them in the .bss segment,
// which lives in SRAM but not on the stack. We use MaybeUninit because
// cortex-m-rt zeroes .bss for us, and we initialize them explicitly in main.
//
// Safety: These are only accessed from the single-threaded main loop.
// The MaybeUninit is initialized exactly once before any reads.
static mut CONSOLE: MaybeUninit<spark_core::console::Console> = MaybeUninit::uninit();
static mut AUDIO: MaybeUninit<spark_core::audio::Audio> = MaybeUninit::uninit();

// ---------------------------------------------------------------------------
// Rust-side malloc/free for the C compat shim
// ---------------------------------------------------------------------------
#[no_mangle]
pub unsafe extern "C" fn rust_malloc(size: usize) -> *mut u8 {
    let layout = core::alloc::Layout::from_size_align_unchecked(size, 8);
    alloc::alloc::alloc(layout)
}

#[no_mangle]
pub unsafe extern "C" fn rust_realloc(ptr: *mut u8, old_size: usize, new_size: usize) -> *mut u8 {
    let layout = core::alloc::Layout::from_size_align_unchecked(old_size, 8);
    alloc::alloc::realloc(ptr, layout, new_size)
}

#[no_mangle]
pub unsafe extern "C" fn rust_free(ptr: *mut u8, size: usize) {
    if !ptr.is_null() && size > 0 {
        let layout = core::alloc::Layout::from_size_align_unchecked(size, 8);
        alloc::alloc::dealloc(ptr, layout);
    }
}

#[no_mangle]
pub unsafe extern "C" fn rust_calloc(nmemb: usize, size: usize) -> *mut u8 {
    // Guard against multiplication overflow (a real concern on 32-bit).
    let total = match nmemb.checked_mul(size) {
        Some(t) => t,
        None => return core::ptr::null_mut(),
    };
    if total == 0 {
        return core::ptr::null_mut();
    }
    let layout = core::alloc::Layout::from_size_align_unchecked(total, 8);
    alloc::alloc::alloc_zeroed(layout)
}

// ---------------------------------------------------------------------------
// RP2350 boot info block
// ---------------------------------------------------------------------------
#[link_section = ".start_block"]
#[used]
pub static IMAGE_DEF: rp235x_hal::block::ImageDef = rp235x_hal::block::ImageDef::secure_exe();

// ---------------------------------------------------------------------------
// Entry point
// ---------------------------------------------------------------------------
#[cortex_m_rt::entry]
fn main() -> ! {
    // Initialise the heap allocator.
    //
    // Safety: HEAP_MEM is a static mut only accessed here before any
    // allocations occur. The heap is initialized exactly once.
    unsafe {
        HEAP.init(
            core::ptr::addr_of_mut!(HEAP_MEM) as usize,
            HEAP_SIZE,
        );
    }

    // ------------------------------------------------------------------
    // Clocks & peripherals
    // ------------------------------------------------------------------
    let mut pac = rp235x_hal::pac::Peripherals::take().unwrap();
    let mut watchdog = rp235x_hal::Watchdog::new(pac.WATCHDOG);

    let clocks = rp235x_hal::clocks::init_clocks_and_plls(
        12_000_000u32, // XOSC crystal frequency
        pac.XOSC,
        pac.CLOCKS,
        pac.PLL_SYS,
        pac.PLL_USB,
        &mut pac.RESETS,
        &mut watchdog,
    )
    .unwrap();

    // ------------------------------------------------------------------
    // SIO & GPIO pins
    // ------------------------------------------------------------------
    let sio = rp235x_hal::Sio::new(pac.SIO);
    let pins = rp235x_hal::gpio::Pins::new(
        pac.IO_BANK0,
        pac.PADS_BANK0,
        sio.gpio_bank0,
        &mut pac.RESETS,
    );

    // ------------------------------------------------------------------
    // SPI0 for ST7789 display
    // ------------------------------------------------------------------
    let sck_pin = pins.gpio18.into_function::<rp235x_hal::gpio::FunctionSpi>();
    let mosi_pin = pins.gpio19.into_function::<rp235x_hal::gpio::FunctionSpi>();
    let cs_pin = pins.gpio17.into_push_pull_output();
    let dc_pin = pins.gpio20.into_push_pull_output();
    let rst_pin = pins.gpio21.into_push_pull_output();
    let mut bl_pin = pins.gpio22.into_push_pull_output();

    // Turn on backlight
    let _ = bl_pin.set_high();

    // rp235x-hal SPI::new takes (TX/MOSI, SCK) order
    let spi_device = rp235x_hal::spi::Spi::<_, _, _, 8>::new(
        pac.SPI0,
        (mosi_pin, sck_pin),
    );

    let spi = spi_device.init(
        &mut pac.RESETS,
        clocks.peripheral_clock.freq(),
        62_500_000u32.Hz(),
        embedded_hal::spi::MODE_3,
    );

    let spi_bus = embedded_hal_bus::spi::ExclusiveDevice::new_no_delay(spi, cs_pin).unwrap();

    // ------------------------------------------------------------------
    // Button inputs (active-low with pull-up)
    // ------------------------------------------------------------------
    let mut pin0 = pins.gpio0.into_pull_up_input(); // Left
    let mut pin1 = pins.gpio1.into_pull_up_input(); // Right
    let mut pin2 = pins.gpio2.into_pull_up_input(); // Up
    let mut pin3 = pins.gpio3.into_pull_up_input(); // Down
    let mut pin4 = pins.gpio4.into_pull_up_input(); // O button
    let mut pin5 = pins.gpio5.into_pull_up_input(); // X button
    let mut pin6 = pins.gpio6.into_pull_up_input(); // Start
    let mut pin7 = pins.gpio7.into_pull_up_input(); // Select

    // ------------------------------------------------------------------
    // PWM audio output on GPIO 8 (PWM slice 4, channel A)
    // ------------------------------------------------------------------
    // GPIO 8 is the first free pin after the 8 button inputs (GPIO 0-7).
    // Connect GPIO 8 through a low-pass RC filter (1 kOhm + 100 nF) to
    // a speaker or small amplifier.
    let pwm_slices = rp235x_hal::pwm::Slices::new(pac.PWM, &mut pac.RESETS);
    let mut pwm4 = pwm_slices.pwm4;
    pwm4.set_ph_correct();
    pwm4.set_top(audio_out::pwm_top());
    pwm4.set_div_int(1);
    pwm4.set_div_frac(0);
    pwm4.enable();

    let _audio_pin = pwm4.channel_a.output_to(pins.gpio8);

    const AUDIO_SAMPLE_RATE: u32 = 22050;
    const AUDIO_FPS: u32 = 30;
    let sys_freq = clocks.system_clock.freq().to_Hz();
    let mut audio_out = audio_out::AudioOut::new(pwm4.channel_a, sys_freq, AUDIO_SAMPLE_RATE);
    audio_out.silence();

    // ------------------------------------------------------------------
    // ST7789 display init
    // ------------------------------------------------------------------
    let mut timer = rp235x_hal::Timer::new_timer0(pac.TIMER0, &mut pac.RESETS, &clocks);
    let mut display = display::Display::new(spi_bus, dc_pin, rst_pin, &mut timer).unwrap();

    // ------------------------------------------------------------------
    // Game state -- Console and Audio live in statics to avoid stack overflow
    // ------------------------------------------------------------------
    //
    // Console (~65 KB) and Audio (~9 KB) would overflow the Cortex-M33
    // stack (typically 8 KB). By initializing them in-place in static
    // storage, we avoid both stack temporaries and heap fragmentation.
    //
    // Safety: CONSOLE and AUDIO are static muts accessed only from this
    // single-threaded main loop. Each is written exactly once here before
    // any reads, so the MaybeUninit is always initialized before use.
    //
    // We use `addr_of_mut!` + pointer writes to avoid creating mutable
    // references to `static mut`, which triggers Rust 2024 lints about
    // aliasing. The pointer-based pattern is the recommended approach.
    let console: &'static mut spark_core::console::Console = unsafe {
        let ptr = core::ptr::addr_of_mut!(CONSOLE);
        (*ptr).write(spark_core::console::Console::new())
    };
    let audio: &'static mut spark_core::audio::Audio = unsafe {
        let ptr = core::ptr::addr_of_mut!(AUDIO);
        (*ptr).write(spark_core::audio::Audio::new())
    };
    let mut game_state = spark_core::game_state::GameState::new();
    let mut input_state = crate::input::Input::new();

    // ------------------------------------------------------------------
    // Parse the baked-in cart
    // ------------------------------------------------------------------
    let cart = spark_core::cart::parse_cart(cart_baked::CART_SOURCE);
    console.sprites = cart.sprites;
    console.map = cart.map;
    console.flags = cart.flags;
    audio.sfx_data = cart.sfx;
    audio.music_data = cart.music;

    // ------------------------------------------------------------------
    // Preprocess & load Lua code
    // ------------------------------------------------------------------
    let code = spark_core::lua_preprocess::preprocess_pico8(&cart.code);

    // Safety: The Lua state and global pointers are used only from this
    // single-threaded main loop. The references to console, audio, and
    // game_state remain valid for the entire program lifetime.
    unsafe {
        let l = lua_ffi::new_lua_state();
        lua_ffi::luaL_openlibs(l);
        lua_ffi::register_api(l);
        lua_ffi::set_state(console, audio, &mut game_state);
        lua_ffi::load_and_run(l, &code);
        lua_ffi::call_global(l, b"_init\0");

        // ------------------------------------------------------------------
        // Main loop (~30 fps)
        // ------------------------------------------------------------------
        //
        // Frame structure:
        //   1. Read GPIO buttons (with debounce) -- ~0 ms
        //   2. Run Lua _update + _draw           -- typically 1-5 ms
        //   3. Push framebuffer to SPI display    -- ~0.5 ms at 62.5 MHz
        //   4. Generate + play audio via PWM      -- ~33 ms (frame pacing)
        //
        // The audio busy-wait in step 4 serves as the frame timer. At
        // 22050 Hz / 30 fps = 735 samples, each taking 1/22050 s, the
        // total audio time is ~33.3 ms, which matches the 30 fps target.
        loop {
            // Read buttons (active-low: pressed == pin low).
            // The Input struct provides 2-frame debouncing: a button must
            // be in the same state for 2 consecutive reads to be accepted.
            // At 30 fps each frame is ~33 ms, so the effective debounce
            // window is 33-66 ms, which filters typical mechanical bounce
            // (5-20 ms) without adding perceptible input lag.
            let raw = [
                input::read_pin_active_low(&mut pin0),
                input::read_pin_active_low(&mut pin1),
                input::read_pin_active_low(&mut pin2),
                input::read_pin_active_low(&mut pin3),
                input::read_pin_active_low(&mut pin4),
                input::read_pin_active_low(&mut pin5),
                input::read_pin_active_low(&mut pin6),
                input::read_pin_active_low(&mut pin7),
            ];
            input_state.update(raw);

            // Pad 8 hardware buttons to 12 for Console (player 2 unused).
            let st = input_state.state();
            let padded = [
                st[0], st[1], st[2], st[3], st[4], st[5], st[6], st[7],
                false, false, false, false,
            ];
            console.update_input(padded);

            // Tick game logic and draw
            lua_ffi::set_state(console, audio, &mut game_state);
            lua_ffi::call_global(l, b"_update\0");
            lua_ffi::call_global(l, b"_draw\0");

            game_state.frame_count += 1;

            // Push framebuffer to display.
            // The 128x128 screen is converted from 4-bit palette indices
            // to RGB565 using a const lookup table (zero computation per
            // pixel) and streamed to the ST7789 via SPI at 62.5 MHz.
            // Transfer time: 128*128*2 bytes / 62.5 Mbps ~ 0.5 ms.
            display.update(&console.screen);

            // Generate audio for this frame and play through PWM.
            //
            // At 22050 Hz / 30 fps = 735 samples per frame (~1.5 KB as f32).
            // The busy-wait inside play_samples doubles as frame pacing:
            // 735 samples * (1/22050) s/sample = 33.3 ms, which matches
            // the 30 fps target.
            //
            // Note: if game logic + display takes longer than expected,
            // the audio still plays all 735 samples, so the frame will
            // slightly exceed 33.3 ms. This is acceptable -- the game
            // slows down gracefully rather than dropping audio.
            let num_samples = audio_out.samples_per_frame(AUDIO_FPS);
            // Stack buffer for audio samples -- avoids per-frame heap allocation.
            // 735 f32 samples = 2940 bytes, well within stack budget.
            let mut samples = [0.0f32; 735];
            audio.generate_samples(AUDIO_SAMPLE_RATE, &mut samples[..num_samples]);
            audio_out.play_samples(&samples[..num_samples]);
        }
    }
}
