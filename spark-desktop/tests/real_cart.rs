//! Integration test: a real PICO-8 `.p8.png` cart (downloaded from the BBS,
//! 160x205) must parse, load, run `_init` + frames, and draw -- exercising the
//! whole pipeline (PNG steganography, code decompression, preprocessor, Lua).

use spark_desktop_lib::{CartRuntime, Tick};

#[test]
fn real_bbs_cart_loads_and_runs() {
    let path = concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/fixtures/real_cart.p8.png"
    );
    let cart = spark_desktop_lib::load_cart_from_file(path)
        .expect("real .p8.png cart should parse (160x205 format)");

    // Decompression must yield real Lua source, not garbage / embedded NULs.
    assert!(!cart.code.is_empty(), "cart code should be non-empty");
    assert!(
        !cart.code.contains('\0'),
        "decompressed code must not contain NUL"
    );

    // Start (compiles the code and runs _init) then drive a few frames; none
    // should raise a Lua error.
    let mut rt = CartRuntime::start(&cart, "", "").expect("cart should compile and _init");
    for frame in 0..30 {
        assert_eq!(
            rt.tick(),
            Tick::Continue,
            "frame {frame} should run cleanly"
        );
    }

    // The cart should have drawn something.
    assert!(
        spark_desktop_lib::lit_pixels(&rt.console()) > 0,
        "cart should draw to the screen"
    );
}
