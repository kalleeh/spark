use std::path::PathBuf;

fn main() {
    // Tell the linker where to find memory.x
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let memory_x = std::fs::read("memory.x").expect("failed to read memory.x");
    std::fs::write(format!("{out_dir}/memory.x"), memory_x).unwrap();
    println!("cargo:rustc-link-search={out_dir}");
    println!("cargo:rerun-if-changed=memory.x");

    let lua_dir = PathBuf::from("lua");
    let compat_dir = lua_dir.join("compat");

    // Collect all .c files from the lua/ directory (top-level only)
    let mut c_files: Vec<PathBuf> = std::fs::read_dir(&lua_dir)
        .expect("failed to read lua/ directory")
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let path = entry.path();
            if path.extension().map_or(false, |ext| ext == "c") {
                Some(path)
            } else {
                None
            }
        })
        .collect();

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
        // Use 32-bit integers and floats to save memory on Cortex-M33
        // (hardware single-precision FPU)
        .define("LUA_32BITS", None)
        // Reduced Lua stack limit for constrained embedded RAM
        .define("LUAI_MAXSTACK", "2000")
        .warnings(false)
        .compile("lua54");
}
