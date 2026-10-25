//! API parity between the two binding layers.
//!
//! Spark wires the PICO-8 API to Lua twice: `spark-desktop/src/lua_api.rs`
//! (mlua, host) and `spark-pico/src/lua_ffi.rs` (hand-written C FFI, embedded).
//! The Pico layer can't be compiled or run on the host (it links vendored Lua C
//! with ARM inline asm), so we can't exercise it directly in a test. What we
//! *can* do is guarantee the two layers expose the **same set of API names** —
//! the divergences that have actually bitten us (`fget`, `tonum`) were a
//! function present/behaving differently on one side. This test parses both
//! source files and compares the registered global names.
//!
//! The tricky *numeric* semantics are already unified in
//! `spark_core::api_ops` (shared, unit-tested once); this test guards the
//! *surface* so neither layer can quietly gain or lose a function.

use std::collections::BTreeSet;

/// Names registered by the desktop layer but intentionally NOT mirrored on the
/// Pico (or vice versa), with the reason. Keep this list short and justified.
fn desktop_only() -> BTreeSet<&'static str> {
    // (none today — both layers expose the same surface)
    BTreeSet::new()
}

fn pico_only() -> BTreeSet<&'static str> {
    // (none today)
    BTreeSet::new()
}

/// Extract the globals registered by the desktop mlua layer.
/// Pattern: `globals.set(\n  "name",` or `bind_binop!("name", ...)` /
/// `bind_shift!("name", ...)`.
fn desktop_api_names(src: &str) -> BTreeSet<String> {
    let mut names = BTreeSet::new();

    // globals.set("name", ...) — the name string literal is the first arg,
    // possibly on the next line. We scan for `globals.set(` then take the next
    // quoted string.
    collect_after_marker(src, "globals.set(", &mut names);

    // The bitwise binds use macros: bind_binop!("name", ...) / bind_shift!.
    collect_after_marker(src, "bind_binop!(", &mut names);
    collect_after_marker(src, "bind_shift!(", &mut names);

    // Some APIs are defined as Lua glue strings (e.g. `flip`): `"function name(`
    // — only the ones that begin a string literal, to avoid catching cart
    // callbacks (`function _update60() end`) defined in the test module.
    collect_glue_function_defs(src, &mut names);

    names
}

/// Scan for `function name(` definitions that begin a Lua-glue string literal
/// (`"function name(`). Used by both layers' Lua glue (flip, sub, all, …).
fn collect_glue_function_defs(src: &str, out: &mut BTreeSet<String>) {
    for marker in ["\"function ", "c\"function "] {
        let mut rest = src;
        while let Some(pos) = rest.find(marker) {
            rest = &rest[pos + marker.len()..];
            let name: String = rest
                .chars()
                .take_while(|c| c.is_ascii_alphanumeric() || *c == '_')
                .collect();
            if !name.is_empty() && rest[name.len()..].trim_start().starts_with('(') {
                out.insert(name);
            }
        }
    }
}

/// Extract the globals registered by the Pico FFI layer.
/// Pattern: `lua_register(L, c"name".as_ptr(), ...)` plus the small set of
/// helpers defined as Lua source strings (`function name(`).
fn pico_api_names(src: &str) -> BTreeSet<String> {
    let mut names = BTreeSet::new();

    // lua_register(L, c"name".as_ptr(), api_fn);
    let mut rest = src;
    while let Some(pos) = rest.find("lua_register(") {
        rest = &rest[pos + "lua_register(".len()..];
        // find the c"..." literal
        if let Some(cq) = rest.find("c\"") {
            let after = &rest[cq + 2..];
            if let Some(end) = after.find('"') {
                names.insert(after[..end].to_string());
            }
        }
    }

    // Helpers registered by loading Lua source: `c"function name(` strings
    // (del/all/foreach/sub/tostr/tonum/chr/ord/split/deli/flip).
    collect_glue_function_defs(src, &mut names);

    names
}

/// Find every occurrence of `marker` and insert the next double-quoted string
/// literal that follows it (skipping whitespace/newlines).
fn collect_after_marker(src: &str, marker: &str, out: &mut BTreeSet<String>) {
    let mut rest = src;
    while let Some(pos) = rest.find(marker) {
        rest = &rest[pos + marker.len()..];
        if let Some(q) = rest.find('"') {
            // Make sure there's only whitespace/newline/comma between marker and
            // the quote (so we don't grab an unrelated later string).
            let gap = &rest[..q];
            if gap.chars().all(|c| c.is_whitespace() || c == ',') {
                let after = &rest[q + 1..];
                if let Some(end) = after.find('"') {
                    out.insert(after[..end].to_string());
                }
            }
        }
    }
}

fn read(rel: &str) -> String {
    let path = format!("{}/{}", env!("CARGO_MANIFEST_DIR"), rel);
    let src = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {path}: {e}"));
    // Only consider the implementation, not the in-file `#[cfg(test)]` module
    // (its test carts define `function _init()` etc. that aren't API globals).
    match src.find("#[cfg(test)]") {
        Some(pos) => src[..pos].to_string(),
        None => src,
    }
}

#[test]
fn binding_layers_expose_the_same_api_surface() {
    let desktop_src = read("src/lua_api.rs");
    let pico_src = read("../spark-pico/src/lua_ffi.rs");

    let mut desktop = desktop_api_names(&desktop_src);
    let mut pico = pico_api_names(&pico_src);

    // Defensive: drop any empty name a parser edge-case may have produced.
    desktop.remove("");
    pico.remove("");

    // Apply intentional allowances.
    for n in desktop_only() {
        desktop.remove(n);
    }
    for n in pico_only() {
        pico.remove(n);
    }

    // Sanity: we actually parsed something.
    assert!(
        desktop.len() > 50,
        "parsed too few desktop API names ({}) — parser likely broke",
        desktop.len()
    );
    assert!(
        pico.len() > 50,
        "parsed too few pico API names ({}) — parser likely broke",
        pico.len()
    );

    let missing_on_pico: Vec<&String> = desktop.difference(&pico).collect();
    let missing_on_desktop: Vec<&String> = pico.difference(&desktop).collect();

    assert!(
        missing_on_pico.is_empty() && missing_on_desktop.is_empty(),
        "binding layers diverge.\n  registered on desktop but not pico: {missing_on_pico:?}\n  \
         registered on pico but not desktop: {missing_on_desktop:?}\n  \
         (if intentional, record it in desktop_only()/pico_only() with a reason)"
    );
}
