//! C math symbols for the vendored Lua C library.
//!
//! The firmware links no libc/libm. `lua/compat/luacompat.c` used to define
//! these as `double sin(double x) { return __builtin_sin(x); }`, which the
//! compiler lowers straight back into a call to `sin` -- every one of them
//! recursed until the stack overflowed (GCC: `-Winfinite-recursion`). Because
//! the library is built with `LUA_USE_C89`, `l_mathop(op)` expands to the
//! double-precision `op`, so Lua's `//`, `%`, `^`, float->integer coercion and
//! `math.*` all end up here.
//!
//! The set below is exactly what the compiled Lua objects reference (found
//! with `arm-none-eabi-nm -u` on them). `libm` is already a dependency of
//! this crate; on the FPv5-SP core these `f64` routines are soft-float, which
//! matches what the C build was always going to produce.
//!
//! Keep this list in sync with `lua/compat/luacompat.h` prototypes. Adding a
//! symbol that `compiler_builtins` also exports strongly would be a link
//! error, so the list is deliberately minimal.

#[no_mangle]
pub extern "C" fn floor(x: f64) -> f64 {
    libm::floor(x)
}

#[no_mangle]
pub extern "C" fn ceil(x: f64) -> f64 {
    libm::ceil(x)
}

#[no_mangle]
pub extern "C" fn sqrt(x: f64) -> f64 {
    libm::sqrt(x)
}

#[no_mangle]
pub extern "C" fn sin(x: f64) -> f64 {
    libm::sin(x)
}

#[no_mangle]
pub extern "C" fn cos(x: f64) -> f64 {
    libm::cos(x)
}

#[no_mangle]
pub extern "C" fn tan(x: f64) -> f64 {
    libm::tan(x)
}

#[no_mangle]
pub extern "C" fn asin(x: f64) -> f64 {
    libm::asin(x)
}

#[no_mangle]
pub extern "C" fn acos(x: f64) -> f64 {
    libm::acos(x)
}

#[no_mangle]
pub extern "C" fn atan2(y: f64, x: f64) -> f64 {
    libm::atan2(y, x)
}

#[no_mangle]
pub extern "C" fn pow(x: f64, y: f64) -> f64 {
    libm::pow(x, y)
}

#[no_mangle]
pub extern "C" fn fmod(x: f64, y: f64) -> f64 {
    libm::fmod(x, y)
}

#[no_mangle]
pub extern "C" fn exp(x: f64) -> f64 {
    libm::exp(x)
}

#[no_mangle]
pub extern "C" fn log(x: f64) -> f64 {
    libm::log(x)
}

#[no_mangle]
pub extern "C" fn log10(x: f64) -> f64 {
    libm::log10(x)
}

#[no_mangle]
pub extern "C" fn ldexp(x: f64, n: i32) -> f64 {
    libm::ldexp(x, n)
}

/// # Safety
/// `exp` must be null or point to writable `int` storage (C `frexp` contract).
#[no_mangle]
pub unsafe extern "C" fn frexp(x: f64, exp: *mut i32) -> f64 {
    let (mantissa, e) = libm::frexp(x);
    if !exp.is_null() {
        *exp = e;
    }
    mantissa
}
