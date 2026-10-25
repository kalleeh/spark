#![no_std]
#![no_main]

extern crate alloc;

use alloc::boxed::Box;
use core::ffi::CStr;
use core::mem::MaybeUninit;
use core::sync::atomic::{AtomicBool, Ordering};
use embedded_alloc::LlffHeap as Heap;

mod audio_out;
mod board;
mod cart_baked;
mod cmath;
mod display;
mod fatal;
mod input;
mod lua_ffi;

// ---------------------------------------------------------------------------
// Allocators -- SRAM heap (global) + optional PSRAM heap (Lua only)
// ---------------------------------------------------------------------------
//
// Memory budget (520 KB total SRAM; figures from `arm-none-eabi-size` on the
// `pico` profile -- re-measure after touching any of these):
//
//   .data               ~75 KB   Console (~66 KB) + Audio (~9 KB): `const`-
//                                constructed statics, copied from flash at
//                                reset (lua_ffi::CONSOLE_MEM / AUDIO_MEM);
//                                plus the PSRAM direct-mode routines (Tufty)
//   Heap (HEAP_SIZE)   pico2: 392 KB   all of it for Lua, apart from
//                                     GameState + the display handle (<1 KB)
//                      tufty: 288 KB   Rust allocations only -- Lua lives in
//                                     the 8 MB PSRAM heap (below)
//   .bss misc           ~2 KB   audio ring (pico2) / LCD DMA rows (tufty)
//   Stack              pico2: ~51 KB   everything above; grows down; guarded
//                      tufty: ~155 KB  by MSPLIM (an overflow faults instead
//                                     of corrupting the heap)
//
// Stack sizing: `main`'s own frame is a few KB (the per-tick sample buffer
// is ~3 KB). The largest boot-time transient is `load_baked_cart` (~17 KB,
// released before Lua runs). Steady state is dominated by nested
// Lua<->C calls (LUAI_MAXCCALLS = 200); PICO-8 carts rarely nest more than
// a handful. If the stack ever proves tight on hardware, shrink HEAP_SIZE.
//
// Boot-time SRAM heap peak (both boards): `lua_preprocess::preprocess_pico8`
// starts its output at `String::with_capacity(code.len())` and typical carts
// grow it by ~5 % (starfall: 10203 -> 10762 bytes), which trips one Vec
// doubling; during that realloc the old (1x) and new (2x) buffers coexist
// with the source (1x), so the peak is ~4x the cart's code section:
// ~256 KB for the 64 KB PICO-8 maximum, ~40 KB for the baked cart. The
// parsed `CartData` (~33 KB) is dropped *before* preprocessing so it does
// not add to that. On the Tufty 288 KB covers a maximum-size cart with a
// little headroom while still leaving ~3x the Pico 2's stack; on the Pico 2
// the same transient fits comfortably inside the 392 KB Lua heap because
// Lua has not allocated anything yet.
//
// Lua's collector is tuned to the heap it lives in (lua_ffi::tune_gc);
// `stat(0)` reports live Lua-heap-used KB (see api_stat) so the budget can
// be verified empirically on-device rather than estimated.
#[global_allocator]
static HEAP: Heap = Heap::empty();

#[cfg(feature = "board-pico2")]
const HEAP_SIZE: usize = 392 * 1024;
#[cfg(feature = "board-tufty2350")]
const HEAP_SIZE: usize = 288 * 1024;
static mut HEAP_MEM: [MaybeUninit<u8>; HEAP_SIZE] = [MaybeUninit::uninit(); HEAP_SIZE];

/// Second heap over the board's external RAM window (Tufty 2350 PSRAM),
/// initialised in `main` when `Board::psram` is `Some`. It serves *only*
/// Lua (`lua_ffi::new_lua_state` is handed a reference); the global
/// allocator stays `HEAP` so Rust code never depends on PSRAM being present.
static PSRAM_HEAP: Heap = Heap::empty();
/// Set once `PSRAM_HEAP` is initialised; decides which heap Lua gets.
static LUA_IN_PSRAM: AtomicBool = AtomicBool::new(false);

/// The heap Lua allocates from: PSRAM when the board brought some up and it
/// passed its self-test, otherwise the SRAM global heap (Pico 2, or a Tufty
/// whose PSRAM failed).
fn lua_heap() -> &'static Heap {
    if LUA_IN_PSRAM.load(Ordering::Relaxed) {
        &PSRAM_HEAP
    } else {
        &HEAP
    }
}

/// Live bytes currently allocated by Lua. Used by `stat(0)`.
pub fn lua_heap_used() -> usize {
    lua_heap().used()
}

/// Parse the baked-in cart into a heap box.
///
/// `CartData::new()` is `const`, but the compiler still stages its non-zero
/// part (the 64 SFX + 64 music defaults, ~17 KB) on the stack before the
/// heap write. Kept out of line so that temporary lives in this short boot
/// frame instead of `main`'s, which is alive for the whole program.
#[inline(never)]
fn load_baked_cart() -> Box<spark_core::cart::CartData> {
    let mut cart: Box<spark_core::cart::CartData> = {
        let mut slot = Box::<spark_core::cart::CartData>::new_uninit();
        // Safety: writing a fully initialised value into the uninit slot.
        unsafe {
            slot.as_mut_ptr().write(spark_core::cart::CartData::new());
            slot.assume_init()
        }
    };
    spark_core::cart::parse_cart_into(&mut cart, cart_baked::CART_SOURCE);
    cart
}

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

extern "C" {
    /// Lowest address the stack may grow down to (provided by cortex-m-rt's
    /// link.x as the end of statically allocated RAM).
    static _stack_end: u32;
}

/// Show a fatal Lua error and stop, or continue.
fn check(result: Result<(), alloc::string::String>) {
    if let Err(msg) = result {
        fatal::halt(&msg);
    }
}

// ---------------------------------------------------------------------------
// Entry point
// ---------------------------------------------------------------------------
#[cortex_m_rt::entry]
fn main() -> ! {
    // Hardware stack guard. The stack lives above .bss with nothing between
    // it and the heap array; an overflow used to silently corrupt the heap.
    // With MSPLIM set, an overflow raises a fault (which locks up on
    // exception entry -- a frozen screen -- rather than corrupting memory).
    // Safety: `_stack_end` is a linker symbol; only its address is used.
    unsafe {
        cortex_m::register::msplim::write(core::ptr::addr_of!(_stack_end) as u32);
    }

    // Initialise the heap allocator.
    // Safety: HEAP_MEM is only ever handed to the allocator, exactly once.
    unsafe {
        HEAP.init(core::ptr::addr_of_mut!(HEAP_MEM) as usize, HEAP_SIZE);
    }

    // ------------------------------------------------------------------
    // Board: clocks, LCD (registered with display::install), buttons, audio
    // ------------------------------------------------------------------
    // All pin numbers and peripheral wiring live in `board::*`; this file
    // only sees the `Board` handle.
    let pac = rp235x_hal::pac::Peripherals::take().unwrap();
    let board::Board {
        sys_hz,
        timer,
        input: buttons,
        audio,
        psram,
    } = board::init(pac);

    // External RAM (Tufty PSRAM, already self-tested by the board) becomes
    // the Lua heap. `None` -- no PSRAM on this board, or it failed -- leaves
    // Lua in the SRAM heap, so the firmware still boots either way.
    if let Some(region) = psram {
        // Safety: the region is a mapped, writable window that nothing else
        // uses, and it is handed to this allocator exactly once.
        unsafe {
            PSRAM_HEAP.init(region.base, region.len);
        }
        LUA_IN_PSRAM.store(true, Ordering::Relaxed);
    }

    // ------------------------------------------------------------------
    // Engine state -- heap-allocated once (see lua_ffi::init_state)
    // ------------------------------------------------------------------
    // Safety: boot-time, single core, before any accessor is used.
    unsafe {
        lua_ffi::init_state();
    }
    let mut input_state = input::Input::new();

    // ------------------------------------------------------------------
    // Parse the baked-in cart, hand its data to the engine, load the code
    // ------------------------------------------------------------------
    // `CartData` is ~33 KB. `CartData::new()` is `const`, so it is written
    // straight into a heap box (a memcpy from flash, no stack temporary) and
    // parsed in place; the box is freed before the source is preprocessed
    // (the SRAM heap's boot-time peak, see the budget above) and long before
    // Lua starts allocating.
    let l = unsafe {
        let mut cart = load_baked_cart();

        {
            let con = lua_ffi::console();
            con.sprites.copy_from_slice(&cart.sprites);
            con.map.copy_from_slice(&cart.map);
            con.flags.copy_from_slice(&cart.flags);
            // `reload()`/`cstore()` operate on the cart as loaded.
            con.snapshot_cart_rom();
        }
        {
            let au = lua_ffi::audio();
            au.sfx_data.copy_from_slice(&cart.sfx);
            au.music_data.copy_from_slice(&cart.music);
        }

        // Keep only the code section; the rest of the cart is in the engine.
        let source = core::mem::take(&mut cart.code);
        drop(cart);
        let code = spark_core::lua_preprocess::preprocess_pico8(&source);
        drop(source);

        let l = lua_ffi::new_lua_state(lua_heap());
        // Collector settings for the heap Lua lives in (see
        // lua_ffi::tune_gc), applied before the cart allocates anything.
        lua_ffi::tune_gc(l, !LUA_IN_PSRAM.load(Ordering::Relaxed));
        lua_ffi::luaL_openlibs(l);
        check(lua_ffi::register_api(l));
        // Compile by length (no NUL-terminated copy), free the source, then
        // run the main chunk so its allocations don't compete with it.
        check(lua_ffi::load_code(l, &code, c"cart"));
        drop(code);
        check(lua_ffi::run_top(l));
        // Load-time garbage (compiler state, the prelude's temporaries) is
        // dead now; collect it once so the first frames start from live data.
        lua_ffi::full_gc(l);
        l
    };

    // ------------------------------------------------------------------
    // Frame rate: _update60 -> 60 fps, otherwise _update -> 30 fps
    // ------------------------------------------------------------------
    let update_fn: &CStr = if unsafe { lua_ffi::has_global_function(l, c"_update60") } {
        c"_update60"
    } else {
        c"_update"
    };
    let fps: u32 = if update_fn == c"_update60" { 60 } else { 30 };
    unsafe {
        lua_ffi::game_state().target_fps = fps as u8;
    }
    let frame_us: u64 = 1_000_000 / fps as u64;

    // Start audio only now: the ISR consumes from an empty ring (holding
    // mid-rail) until the first frame is pushed. Boards without audio
    // hardware still run the synth each tick (below) so music/sfx state and
    // `stat()` advance; the samples are simply discarded.
    let has_audio = audio.is_some();
    #[cfg(feature = "board-pico2")]
    if let Some(a) = audio {
        audio_out::start(a, sys_hz);
    }
    // Only the PWM sample clock needs the system clock rate.
    #[cfg(not(feature = "board-pico2"))]
    let _ = sys_hz;
    let mut sample_clock = audio_out::SampleClock::new(fps);
    let mut samples = [0.0f32; audio_out::MAX_SAMPLES_PER_TICK];

    check(unsafe { lua_ffi::call_global(l, c"_init") });

    // ------------------------------------------------------------------
    // Main loop
    // ------------------------------------------------------------------
    //
    // Per frame (33.3 ms at 30 fps, 16.7 ms at 60):
    //   1. Read buttons (debounced)                ~0 ms
    //   2. Run Lua _update/_update60 + _draw       typically 1-5 ms
    //   3. Push framebuffer to the LCD             ~4-5 ms (pico2: 32 KB SPI)
    //   4. Generate one tick of audio into the ring ~1 ms
    //   5. Sleep until the frame deadline (timer)   remainder
    //
    // Pacing uses the 1 MHz system timer. If a frame overruns by more than a
    // whole period the deadline is re-based rather than "caught up", so a
    // hiccup causes one slow frame instead of a burst of fast ones. Audio is
    // decoupled: the ring buffer holds ~93 ms, so a late frame never causes
    // an audible gap.
    let mut deadline = timer.get_counter().ticks() + frame_us;
    let mut last_frame_start = timer.get_counter().ticks();
    loop {
        // Read buttons (the board already inverted active-low wiring).
        input_state.update(buttons.read());

        // Pad 6 hardware buttons to 12 for Console (player 2 unused).
        let st = input_state.state();
        let padded = [
            st[0], st[1], st[2], st[3], st[4], st[5], false, false, false, false, false, false,
        ];

        // Safety (all blocks below): single core, no ISR touches the engine
        // statics, and each accessor reference is dropped before the next
        // call into Lua (which re-derives its own).
        unsafe {
            lua_ffi::console().update_input(padded);
            check(lua_ffi::call_global(l, update_fn));
            check(lua_ffi::call_global(l, c"_draw"));
            lua_ffi::game_state().frame_count += 1;
        }

        // Present the frame (screen palette applied by the sink).
        unsafe {
            let con = lua_ffi::console();
            if let Some(sink) = display::sink() {
                sink.show(&con.screen, &con.screen_pal);
            }
        }

        // One tick of audio (735 samples at 30 fps, 367/368 at 60). Always
        // generated so the synth state advances; pushed to the ring only when
        // there is PWM hardware. `push_samples` blocks only if the ring is
        // full, which the pacer below normally prevents.
        let n = sample_clock.next();
        unsafe {
            lua_ffi::audio().generate_samples(audio_out::SAMPLE_RATE, &mut samples[..n]);
        }
        #[cfg(feature = "board-pico2")]
        if has_audio {
            audio_out::push_samples(&samples[..n]);
        }

        // Frame pacing.
        let now = timer.get_counter().ticks();
        if now >= deadline + frame_us {
            // More than a full frame late: re-base instead of catching up.
            deadline = now;
        } else {
            while timer.get_counter().ticks() < deadline {
                if has_audio {
                    // Woken every 45 us by the audio tick; fine-grained enough.
                    cortex_m::asm::wfi();
                } else {
                    // No interrupt source to wake us: spin on the timer.
                    cortex_m::asm::nop();
                }
            }
        }
        deadline += frame_us;

        // Measured fps for stat(8).
        let start = timer.get_counter().ticks();
        let elapsed = start.saturating_sub(last_frame_start).max(1);
        last_frame_start = start;
        unsafe {
            lua_ffi::game_state().actual_fps = 1_000_000.0 / elapsed as f32;
        }
    }
}
