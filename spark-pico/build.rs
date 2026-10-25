use std::path::PathBuf;

fn main() {
    // ------------------------------------------------------------------
    // Linker wiring
    // ------------------------------------------------------------------
    // Copy memory.x next to the build products so `-Tlink.x` (cortex-m-rt's
    // script, which `INCLUDE`s memory.x) can find it.
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let memory_x = std::fs::read("memory.x").expect("failed to read memory.x");
    std::fs::write(format!("{out_dir}/memory.x"), memory_x).unwrap();
    println!("cargo:rustc-link-search={out_dir}");

    // Emit the link arguments from here rather than relying on
    // `.cargo/config.toml` rustflags: cargo only reads that file when the
    // current directory is inside `spark-pico/`, so a build launched from the
    // workspace root used to produce an ELF that failed to link (undefined
    // `__sidata`/`__bi_entries_start` -- link.x was never used). Build-script
    // link args apply regardless of cwd.
    //
    // `--nmagic` disables page alignment of sections, which cortex-m-rt
    // requires so the vector table lands at the very start of FLASH.
    println!("cargo:rustc-link-arg-bins=--nmagic");
    println!("cargo:rustc-link-arg-bins=-Tlink.x");

    // Once any `rerun-if-changed` is emitted cargo stops re-running the script
    // on every package change, so list everything the script depends on.
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=memory.x");
    // A directory: cargo scans it recursively, so edits to any vendored Lua C
    // or compat header trigger a rebuild of the C library.
    println!("cargo:rerun-if-changed=lua");

    // ------------------------------------------------------------------
    // Vendored Lua 5.4 + bare-metal libc shim
    // ------------------------------------------------------------------
    let lua_dir = PathBuf::from("lua");
    let compat_dir = lua_dir.join("compat");

    // Collect all .c files from the lua/ directory (top-level only)
    let mut c_files: Vec<PathBuf> = std::fs::read_dir(&lua_dir)
        .expect("failed to read lua/ directory")
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let path = entry.path();
            if path.extension().is_some_and(|ext| ext == "c") {
                Some(path)
            } else {
                None
            }
        })
        .collect();
    // Deterministic object order (read_dir order is filesystem-dependent).
    c_files.sort();

    // Also compile the compat shim
    c_files.push(compat_dir.join("luacompat.c"));

    cc::Build::new()
        .files(&c_files)
        // -nostdinc: don't search system include paths at all
        .flag("-nostdinc")
        // Our compat shim headers provide everything (stddef, stdarg, stdint,
        // limits, float, errno, stdio, stdlib, string, math, etc.)
        .include(&compat_dir)
        .include(&lua_dir)
        // Use C89-compatible subset for safer embedded compilation
        .define("LUA_USE_C89", None)
        // 32-bit integers and single-precision floats (hardware FPU on the
        // Cortex-M33). `lua_ffi.rs` declares lua_Number = f32 / lua_Integer =
        // i32 and `compat/luacompat.c` _Static_asserts the sizes, so a
        // mismatch is a build error rather than a silent ABI corruption.
        .define("LUA_32BITS", None)
        // Reduced Lua stack limit for constrained embedded RAM
        .define("LUAI_MAXSTACK", "2000")
        // Warnings stay ON (cc's default -Wall -Wextra): a redefinition like
        // the unguarded LUA_32BITS that hid the double/float ABI mismatch, or
        // the self-recursive math shims (-Winfinite-recursion), must show up
        // in the build log. Both Lua 5.4 and compat/luacompat.c are currently
        // warning-clean under these flags; keep them that way.
        .compile("lua54");
}
