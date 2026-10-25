//! Library surface of the desktop target, exposing the window-free pieces
//! (Lua API layer, cart runtime and PNG cart parsing) so headless tooling and
//! examples can drive carts without pulling in macroquad. The interactive app
//! lives in the `spark` binary (main.rs); the windowed modules are not part of
//! this lib.

pub mod lua_api;
pub mod png_cart;
pub mod runtime;

use spark_core::audio::Audio;
use spark_core::cart::CartData;
use spark_core::console::{Console, SCREEN_H, SCREEN_W};

pub use lua_api::{
    call_draw, call_update, call_update60, create_init_coroutine, create_lua, has_update60,
    load_code, resume_cart_thread,
};
pub use runtime::{CartRuntime, Tick};

/// Load a cart from a file path, detecting the format by extension.
///
/// Files ending in `.p8.png` are treated as PNG-encoded carts (steganographic
/// data). Everything else is treated as a `.p8` text cart.
pub fn load_cart_from_file(path: &str) -> Result<CartData, String> {
    if path.ends_with(".p8.png") {
        let bytes = std::fs::read(path).map_err(|e| format!("Failed to read file: {}", e))?;
        png_cart::parse_p8_png(&bytes)
    } else {
        let source =
            std::fs::read_to_string(path).map_err(|e| format!("Failed to read file: {}", e))?;
        Ok(spark_core::cart::parse_cart(&source))
    }
}

/// Copy a cart's ROM sections (sprites, map, flags, sfx, music) into the
/// engine state and snapshot them so `reload()`/`cstore()` see the cart as
/// loaded.
pub fn install_cart(console: &mut Console, audio: &mut Audio, cart: &CartData) {
    console.sprites = cart.sprites;
    console.map = cart.map;
    console.flags = cart.flags;
    console.snapshot_cart_rom();
    audio.sfx_data = cart.sfx;
    audio.music_data = cart.music;
}

/// Number of non-black (color index != 0) pixels on the console screen.
pub fn lit_pixels(console: &Console) -> usize {
    console.screen.iter().filter(|&&p| p != 0).count()
}

/// Encode an 8-bit RGBA buffer as a PNG. `rgba.len()` must be `w * h * 4`.
pub fn encode_rgba_png(width: u32, height: u32, rgba: &[u8]) -> Vec<u8> {
    debug_assert_eq!(rgba.len(), (width * height * 4) as usize);
    let mut bytes = Vec::new();
    {
        let mut enc = png::Encoder::new(&mut bytes, width, height);
        enc.set_color(png::ColorType::Rgba);
        enc.set_depth(png::BitDepth::Eight);
        let mut w = enc.write_header().expect("png header");
        w.write_image_data(rgba).expect("png image data");
    }
    bytes
}

/// Encode a 128x128 RGBA screen buffer (see `Console::screen_to_rgba`) as PNG.
pub fn encode_screen_png(rgba: &[u8]) -> Vec<u8> {
    encode_rgba_png(SCREEN_W as u32, SCREEN_H as u32, rgba)
}
