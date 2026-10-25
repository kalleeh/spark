use alloc::borrow::Cow;
use alloc::collections::BTreeSet;
use alloc::format;
use alloc::string::{String, ToString};
use alloc::vec::Vec;

/// Pre-process PICO-8 Lua syntax into standard Lua 5.4.
///
/// Handles:
///  - Backslash line continuation: `\` at end of line joins with next line
///  - Compound assignment operators: +=, -=, *=, /=, %=, ..=
///  - Not-equal shorthand: != -> ~=
///  - Print shorthand: ?expr -> print(expr)
///  - Single-line if: if (cond) stmt  (with no matching then)
///  - Single-line if/else: if (cond) stmt1 else stmt2
///  - Multi-line string literals: `[[ ]]`, `[=[ ]=]`, etc.
///  - Multi-line comments: `--[[ ]]`, `--[=[ ]=]`, etc.
///
/// All transforms are string-literal-aware and comment-aware.
pub fn preprocess_pico8(code: &str) -> String {
    preprocess_pico8_with_includes(code, &|_| None)
}

/// Preprocess PICO-8 code with support for `#include` directives.
///
/// `resolve_include` is called with the filename from `#include filename`.
/// It should return the file contents as a String, or None if not found.
/// Included files are themselves preprocessed (recursive).
///
/// # Include format
///
/// PICO-8 uses `#include filename.lua` (no quotes needed, but quotes also work):
/// ```text
/// #include utils.lua
/// #include "helpers.lua"
/// #include path/to/file.lua
/// ```
///
/// Cycle detection prevents infinite recursion (A includes B, B includes A).
/// Recursion depth is limited to 16 levels.
pub fn preprocess_pico8_with_includes<F>(code: &str, resolve_include: &F) -> String
where
    F: Fn(&str) -> Option<String>,
{
    let mut seen = BTreeSet::new();
    preprocess_with_includes_inner(code, resolve_include, &mut seen, 0)
}

/// Maximum recursion depth for `#include` directives.
const INCLUDE_MAX_DEPTH: usize = 16;

/// Inner recursive function for include processing.
///
/// Memory: the include and continuation phases return the input *borrowed*
/// when they have nothing to do, so the common case holds only the source and
/// the output (not four full copies).
fn preprocess_with_includes_inner<F>(
    code: &str,
    resolve_include: &F,
    seen: &mut BTreeSet<String>,
    depth: usize,
) -> String
where
    F: Fn(&str) -> Option<String>,
{
    // Phase 0: Resolve #include directives before any other processing.
    let code_with_includes = resolve_includes(code, resolve_include, seen, depth);

    // Phase 1: Join backslash-continued lines
    let joined = join_backslash_lines(&code_with_includes);

    let mut output = String::with_capacity(joined.len());

    // Track multi-line string/comment state across lines.
    // When Some(n), we are inside a `[=*[` block where n is the number of `=` signs.
    let mut multiline_depth: Option<usize> = None;

    for line in joined.lines() {
        // If we are currently inside a multi-line string/comment, look for the closing delimiter.
        if let Some(eq_count) = multiline_depth {
            if let Some(close_end) = find_multiline_close(line, eq_count) {
                // The closing delimiter ends at `close_end` (byte index, exclusive).
                // Everything up to `close_end` is still inside the multi-line block.
                // The remainder after it should be processed normally.
                multiline_depth = None;
                let inside_part = &line[..close_end];
                let rest = &line[close_end..];
                output.push_str(inside_part);
                // Process the remainder of the line normally (it may contain code).
                if !rest.is_empty() {
                    output.push_str(&process_line_normal(rest));
                }
                output.push('\n');
            } else {
                // Still inside multi-line block; pass through unchanged.
                output.push_str(line);
                output.push('\n');
            }
            continue;
        }

        // Not inside a multi-line block. Check if this line opens one.
        if let Some((open_byte, eq_count)) = find_multiline_open(line) {
            // The opening delimiter starts at byte `open_byte`.
            // Process the part before the opening delimiter normally.
            let before = &line[..open_byte];
            let from_open = &line[open_byte..];

            // Calculate the byte length of the opening delimiter (`[` + `=`*n + `[`)
            let delim_len = 2 + eq_count;
            let after_open = &from_open[delim_len..];

            // Check if the closing delimiter also appears on this same line.
            if let Some(close_end) = find_multiline_close(after_open, eq_count) {
                // Entire multi-line block is on this single line.
                let inside_and_close = &after_open[..close_end];
                let rest = &after_open[close_end..];

                // Process the part before the open delimiter.
                output.push_str(&process_line_normal(before));
                // Emit the opening delimiter + content + closing delimiter as-is.
                output.push_str(&from_open[..delim_len]);
                output.push_str(inside_and_close);
                // Process any remainder after the closing delimiter.
                if !rest.is_empty() {
                    output.push_str(&process_line_normal(rest));
                }
                output.push('\n');
            } else {
                // Multi-line block opens but does not close on this line.
                multiline_depth = Some(eq_count);
                output.push_str(&process_line_normal(before));
                output.push_str(from_open);
                output.push('\n');
            }
            continue;
        }

        // No multi-line block involvement; process normally.
        output.push_str(&process_line_normal(line));
        output.push('\n');
    }
    output
}

/// Resolve `#include` directives in the source code.
///
/// Scans line-by-line, respecting string literals and comments. When a line
/// (outside strings/comments) matches `#include <filename>`, the resolver is
/// called and the line is replaced with the (recursively preprocessed) content.
/// Returns the source borrowed when it contains no `#include` at all.
fn resolve_includes<'a, F>(
    code: &'a str,
    resolve_include: &F,
    seen: &mut BTreeSet<String>,
    depth: usize,
) -> Cow<'a, str>
where
    F: Fn(&str) -> Option<String>,
{
    if !code.contains("#include") {
        return Cow::Borrowed(code);
    }

    let mut output = String::with_capacity(code.len());
    let mut multiline_depth: Option<usize> = None;

    for line in code.lines() {
        // If inside a multi-line string/comment, pass through and track close.
        if let Some(eq_count) = multiline_depth {
            if find_multiline_close(line, eq_count).is_some() {
                multiline_depth = None;
            }
            output.push_str(line);
            output.push('\n');
            continue;
        }

        // Check if this line opens a multi-line string/comment.
        if let Some((open_byte, eq_count)) = find_multiline_open(line) {
            // Check if it also closes on this line.
            let delim_len = 2 + eq_count;
            let after_open = &line[open_byte + delim_len..];
            if find_multiline_close(after_open, eq_count).is_none() {
                multiline_depth = Some(eq_count);
            }
            output.push_str(line);
            output.push('\n');
            continue;
        }

        // Check for #include directive on this line.
        if let Some(filename) = parse_include_directive(line) {
            if depth >= INCLUDE_MAX_DEPTH {
                // Depth limit exceeded; emit a comment.
                output.push_str("-- #include depth limit exceeded: ");
                output.push_str(filename);
                output.push('\n');
                continue;
            }

            if seen.contains(filename) {
                // Cycle detected; emit a comment.
                output.push_str("-- #include cycle detected: ");
                output.push_str(filename);
                output.push('\n');
                continue;
            }

            if let Some(contents) = resolve_include(filename) {
                // Mark as seen for cycle detection.
                seen.insert(filename.to_string());
                // Recursively process the included content.
                let processed =
                    preprocess_with_includes_inner(&contents, resolve_include, seen, depth + 1);
                output.push_str(&processed);
                // Remove from seen after processing so the same file can be
                // included from independent branches (but not from itself).
                seen.remove(filename);
            } else {
                // Resolver returned None; emit a comment.
                output.push_str("-- #include not found: ");
                output.push_str(filename);
                output.push('\n');
            }
            continue;
        }

        // Regular line; pass through.
        output.push_str(line);
        output.push('\n');
    }

    Cow::Owned(output)
}

/// Parse a `#include` directive from a line, returning the filename if matched.
///
/// Recognized formats:
/// - `#include filename.lua`
/// - `#include "filename.lua"`
/// - `#include <filename.lua>`
/// - Leading whitespace is allowed.
///
/// Because the directive must be the first thing on the line, it can never be
/// inside a string literal or comment.
fn parse_include_directive(line: &str) -> Option<&str> {
    let trimmed = line.trim();

    // Extract the part after `#include`
    let after_include = trimmed.strip_prefix("#include")?;

    // Must be followed by whitespace
    if !after_include.starts_with(|c: char| c.is_ascii_whitespace()) {
        return None;
    }

    let filename_part = after_include.trim();

    if filename_part.is_empty() {
        return None;
    }

    // Strip optional quotes or angle brackets
    let filename = if (filename_part.starts_with('"') && filename_part.ends_with('"'))
        || (filename_part.starts_with('<') && filename_part.ends_with('>'))
    {
        &filename_part[1..filename_part.len() - 1]
    } else {
        filename_part
    };

    let filename = filename.trim();
    if filename.is_empty() {
        return None;
    }

    Some(filename)
}

// ---------------------------------------------------------------------------
// Shared lexical scanner
// ---------------------------------------------------------------------------

/// Classification of a byte range of a single line.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum SpanKind {
    /// Plain code: transforms apply here.
    Code,
    /// A complete `"..."` / `'...'` literal, delimiters included.
    Str,
    /// A string literal that runs to the end of the line without closing.
    UnterminatedStr,
    /// A `--` comment: from the dashes to the end of the line.
    Comment,
}

/// A byte range `[start, end)` of a line with its classification. Boundaries
/// always fall on ASCII bytes (quotes, `--`, line end) so they are valid
/// `str` slice indices even when the line contains multibyte UTF-8.
#[derive(Clone, Copy, Debug)]
struct Span {
    start: usize,
    end: usize,
    kind: SpanKind,
}

/// The one string/comment state machine shared by every per-line pass.
///
/// Rules (PICO-8 single-line lexing):
///  - `'…'` and `"…"` literals; a backslash escapes the next byte.
///  - `--` outside a string starts a comment that runs to the end of line.
///  - Long brackets (`[[…]]`) are handled by the multi-line driver before a
///    line reaches the per-line passes, so they are not tracked here
///    (`find_multiline_open` detects them on top of the code spans).
struct Spans<'a> {
    bytes: &'a [u8],
    pos: usize,
}

impl<'a> Spans<'a> {
    fn new(line: &'a str) -> Self {
        Spans {
            bytes: line.as_bytes(),
            pos: 0,
        }
    }

    /// Start scanning at `start`, which must be a code-level byte position.
    fn from(line: &'a str, start: usize) -> Self {
        Spans {
            bytes: line.as_bytes(),
            pos: start,
        }
    }

    /// Only the code spans.
    fn code(self) -> impl Iterator<Item = Span> + 'a {
        self.filter(|s| s.kind == SpanKind::Code)
    }
}

impl Iterator for Spans<'_> {
    type Item = Span;

    fn next(&mut self) -> Option<Span> {
        let b = self.bytes;
        let n = b.len();
        let start = self.pos;
        if start >= n {
            return None;
        }
        match b[start] {
            q @ (b'"' | b'\'') => {
                let mut i = start + 1;
                while i < n {
                    if b[i] == b'\\' && i + 1 < n {
                        i += 2;
                        continue;
                    }
                    if b[i] == q {
                        self.pos = i + 1;
                        return Some(Span {
                            start,
                            end: i + 1,
                            kind: SpanKind::Str,
                        });
                    }
                    i += 1;
                }
                self.pos = n;
                Some(Span {
                    start,
                    end: n,
                    kind: SpanKind::UnterminatedStr,
                })
            }
            b'-' if start + 1 < n && b[start + 1] == b'-' => {
                self.pos = n;
                Some(Span {
                    start,
                    end: n,
                    kind: SpanKind::Comment,
                })
            }
            _ => {
                let mut i = start + 1;
                while i < n {
                    match b[i] {
                        b'"' | b'\'' => break,
                        b'-' if i + 1 < n && b[i + 1] == b'-' => break,
                        _ => i += 1,
                    }
                }
                self.pos = i;
                Some(Span {
                    start,
                    end: i,
                    kind: SpanKind::Code,
                })
            }
        }
    }
}

/// Byte offset of the first occurrence of `needle` at code level, or `None`.
fn find_in_code(line: &str, needle: &str) -> Option<usize> {
    Spans::new(line)
        .code()
        .find_map(|s| line[s.start..s.end].find(needle).map(|k| s.start + k))
}

/// Does any code span contain byte `b`?
fn code_contains_byte(line: &str, b: u8) -> bool {
    let bytes = line.as_bytes();
    Spans::new(line)
        .code()
        .any(|s| bytes[s.start..s.end].contains(&b))
}

// ---------------------------------------------------------------------------
// Per-line passes
// ---------------------------------------------------------------------------

/// Convert a PICO-8 `//` single-line comment to a Lua `--` comment.
///
/// The first `//` found at code level (before any `--`, which already starts
/// a comment) is replaced with `--` and the rest of the line is kept verbatim
/// as comment text. If no such `//` exists the line is returned unchanged.
fn convert_double_slash_comment(line: &str) -> Cow<'_, str> {
    if !line.contains("//") {
        return Cow::Borrowed(line);
    }
    match find_in_code(line, "//") {
        Some(k) => {
            let mut out = String::with_capacity(line.len());
            out.push_str(&line[..k]);
            out.push_str("--");
            out.push_str(&line[k + 2..]);
            Cow::Owned(out)
        }
        None => Cow::Borrowed(line),
    }
}

/// Convert PICO-8's `\` integer-division operator to Lua's `//`.
///
/// String- and comment-aware. Leaves `\=` untouched (the compound-assignment
/// pass turns `x \= n` into `x = x // (n)`), and leaves `\` inside string
/// literals (escape sequences) and after a `--` comment alone. Any remaining
/// end-of-line continuation `\` has already been removed by
/// [`join_backslash_lines`] before this runs.
fn convert_backslash_idiv(line: &str) -> Cow<'_, str> {
    if !line.contains('\\') || !code_contains_byte(line, b'\\') {
        return Cow::Borrowed(line);
    }
    let bytes = line.as_bytes();
    let mut out = String::with_capacity(line.len() + 4);
    let mut copied = 0usize;
    for span in Spans::new(line).code() {
        let mut i = span.start;
        while i < span.end {
            // `\=` is compound integer-division-assign; leave for that pass.
            if bytes[i] == b'\\' && !(i + 1 < bytes.len() && bytes[i + 1] == b'=') {
                out.push_str(&line[copied..i]);
                out.push_str("//");
                copied = i + 1;
            }
            i += 1;
        }
    }
    out.push_str(&line[copied..]);
    Cow::Owned(out)
}

/// Convert PICO-8's custom string-escape sequences to standard Lua escapes
/// inside string literals. PICO-8 (z8lua llex.c) maps these P8SCII control
/// escapes that Lua 5.4 rejects:
///   `\*`->\1  `\#`->\2  `\-`->\3  `\|`->\4  `\+`->\5  `\^`->\6
/// Only the interior of `"..."`/`'...'` literals is rewritten; a `\\` escapes
/// the backslash so `\\*` is left as a literal backslash + `*`.
fn convert_string_escapes(line: &str) -> Cow<'_, str> {
    // Cheap reject: a backslash is required for any custom escape.
    if !line.contains('\\') {
        return Cow::Borrowed(line);
    }
    // Map to a fixed 2-digit hex escape (`\xNN`) rather than a decimal
    // escape: a decimal `\1` would merge with a following digit (`"\*4"` must
    // be char 1 + '4', not char 14), whereas `\x01` is exactly two hex digits
    // and cannot absorb more.
    fn mapped(next: u8) -> Option<&'static str> {
        Some(match next {
            b'*' => "\\x01",
            b'#' => "\\x02",
            b'-' => "\\x03",
            b'|' => "\\x04",
            b'+' => "\\x05",
            b'^' => "\\x06",
            _ => return None,
        })
    }
    let bytes = line.as_bytes();
    let mut out: Option<String> = None;
    let mut copied = 0usize;
    for span in Spans::new(line) {
        if !matches!(span.kind, SpanKind::Str | SpanKind::UnterminatedStr) {
            continue;
        }
        // Interior only: skip the opening quote.
        let mut i = span.start + 1;
        while i < span.end {
            if bytes[i] == b'\\' && i + 1 < bytes.len() {
                if let Some(esc) = mapped(bytes[i + 1]) {
                    let out = out.get_or_insert_with(|| String::with_capacity(line.len() + 8));
                    out.push_str(&line[copied..i]);
                    out.push_str(esc);
                    copied = i + 2;
                }
                // Any other escape (incl. `\\`, `\"`, `\n`, `\x`, `\ddd`) is
                // kept verbatim so its own meaning is preserved.
                i += 2;
                continue;
            }
            i += 1;
        }
    }
    match out {
        Some(mut out) => {
            out.push_str(&line[copied..]);
            Cow::Owned(out)
        }
        None => Cow::Borrowed(line),
    }
}

/// Convert PICO-8 binary number literals (`0b1010`, `0B11`, `0b0101.1`) to
/// their decimal value, which Lua 5.4 accepts. String- and comment-aware. A
/// `0b` is only treated as a literal when it begins a number token (the byte
/// before it is not an identifier char or `.`), so `x0b1` / `a.0b` are left
/// alone. Bails on a malformed literal by leaving it unchanged.
fn convert_binary_literals(line: &str) -> Cow<'_, str> {
    // Cheap reject: needs a '0' followed by b/B somewhere.
    if !line.contains("0b") && !line.contains("0B") {
        return Cow::Borrowed(line);
    }
    let bytes = line.as_bytes();
    let mut out: Option<String> = None;
    let mut copied = 0usize;
    for span in Spans::new(line).code() {
        let mut i = span.start;
        while i + 1 < span.end {
            // A binary literal: `0` then `b`/`B`, at a number-token boundary.
            if bytes[i] == b'0'
                && (bytes[i + 1] == b'b' || bytes[i + 1] == b'B')
                && !preceded_by_ident_byte(bytes, i)
            {
                // Collect binary digits and an optional fractional part.
                let start_digits = i + 2;
                let mut j = start_digits;
                while j < span.end && matches!(bytes[j], b'0' | b'1' | b'.') {
                    j += 1;
                }
                let lit = &line[start_digits..j];
                // Parse via the shared PICO-8 number parser (handles the fraction).
                if !lit.is_empty() {
                    if let Some(v) = crate::api_ops::parse_pico8_number(&format!("0b{lit}")) {
                        let out = out.get_or_insert_with(|| String::with_capacity(line.len()));
                        out.push_str(&line[copied..i]);
                        // Emit a plain decimal. Integers stay integer-looking.
                        if v == (v as i64) as f64 {
                            out.push_str(&format!("{}", v as i64));
                        } else {
                            out.push_str(&format!("{v}"));
                        }
                        copied = j;
                        i = j;
                        continue;
                    }
                }
                // Malformed: leave the original characters as-is.
            }
            i += 1;
        }
    }
    match out {
        Some(mut out) => {
            out.push_str(&line[copied..]);
            Cow::Owned(out)
        }
        None => Cow::Borrowed(line),
    }
}

/// True if the byte before index `i` is part of an identifier or a number
/// (so a `0b` there is NOT the start of a fresh numeric literal).
fn preceded_by_ident_byte(bytes: &[u8], i: usize) -> bool {
    if i == 0 {
        return false;
    }
    let c = bytes[i - 1];
    c.is_ascii_alphanumeric() || c == b'_' || c == b'.'
}

/// PICO-8's six button glyphs are predefined globals (z8lua bios / fake-08
/// `p8GlobalLuaFunctions.h`): ⬅️=0 ➡️=1 ⬆️=2 ⬇️=3 🅾️=4 ❎=5. Listed without
/// the U+FE0F variation selector, which carts may or may not include.
const BUTTON_GLYPHS: &[(&str, &str)] = &[
    ("\u{2b05}", "0"),  // ⬅️
    ("\u{27a1}", "1"),  // ➡️
    ("\u{2b06}", "2"),  // ⬆️
    ("\u{2b07}", "3"),  // ⬇️
    ("\u{1f17e}", "4"), // 🅾️
    ("\u{274e}", "5"),  // ❎
];

/// Replace PICO-8 button glyphs used as identifiers (`btn(⬅️)`) with their
/// numeric literal, since Lua 5.4 cannot lex them. Code spans only — glyphs
/// inside strings (`print("⬅️")`) and comments are untouched. A glyph glued
/// to identifier characters (`x⬅️`) is left alone rather than being fused
/// into a different token.
fn convert_button_glyphs(line: &str) -> Cow<'_, str> {
    if line.is_ascii() {
        return Cow::Borrowed(line);
    }
    const VS16: &str = "\u{fe0f}";
    let bytes = line.as_bytes();
    let is_ident = |b: u8| b.is_ascii_alphanumeric() || b == b'_';
    let mut out: Option<String> = None;
    let mut copied = 0usize;
    for span in Spans::new(line).code() {
        let mut i = span.start;
        while i < span.end {
            if bytes[i] < 0x80 {
                i += 1;
                continue;
            }
            let rest = &line[i..span.end];
            let hit = BUTTON_GLYPHS
                .iter()
                .find(|(glyph, _)| rest.starts_with(glyph));
            let Some((glyph, literal)) = hit else {
                // Some other multibyte char: step over it whole.
                i += rest.chars().next().map_or(1, char::len_utf8);
                continue;
            };
            let mut end = i + glyph.len();
            if line[end..span.end].starts_with(VS16) {
                end += VS16.len();
            }
            let glued =
                (i > 0 && is_ident(bytes[i - 1])) || (end < bytes.len() && is_ident(bytes[end]));
            if !glued {
                let out = out.get_or_insert_with(|| String::with_capacity(line.len()));
                out.push_str(&line[copied..i]);
                out.push_str(literal);
                copied = end;
            }
            i = end;
        }
    }
    match out {
        Some(mut out) => {
            out.push_str(&line[copied..]);
            Cow::Owned(out)
        }
        None => Cow::Borrowed(line),
    }
}

/// PICO-8 infix operators with no Lua 5.4 equivalent, mapped to API functions.
/// Native Lua ops (`<<`, `>>`, `&`, `|`) are deliberately absent — they pass
/// through. Listed longest-first so the matcher can't mistake `>>>` for `>>`.
/// The number is the PICO-8 operator precedence (z8lua lparser.c priority[]):
/// `^^` is 5, the shifts/rotates are 7.
const INFIX_BITOPS: &[(&str, &str, u8)] = &[
    (">>>", "lshr", 7),
    ("<<>", "rotl", 7),
    (">><", "rotr", 7),
    ("^^", "bxor", 5),
];

/// Concatenate a token range into one owned string.
fn join_toks(toks: &[Tok<'_>]) -> String {
    let mut out = String::with_capacity(toks.iter().map(|t| t.text.len()).sum());
    for tk in toks {
        out.push_str(&tk.text);
    }
    out
}

/// Replace `toks[range]` with a single synthesized `Group` token `text`. A
/// call expression is an atom, exactly what re-tokenizing the rebuilt line
/// would have produced for operand-delimiting purposes, so no re-lex is needed.
fn splice_group(toks: &mut Vec<Tok<'_>>, range: core::ops::Range<usize>, text: String) {
    toks.splice(
        range,
        core::iter::once(Tok {
            text: Cow::Owned(text),
            kind: TokKind::Group,
        }),
    );
}

/// Rewrite PICO-8-only infix bitwise operators (`>>>`, `<<>`, `>><`, `^^`) into
/// API function calls, e.g. `a >>> b` -> `lshr(a, b)`.
///
/// Correctness over cleverness: operands are delimited by scanning outward
/// until a token whose precedence is **lower** than the operator's (or a
/// statement/grouping boundary). If an operand cannot be confidently delimited,
/// the line is returned **unchanged** — a visible Lua syntax error is far
/// better than silently miscomputing. Processes one operator occurrence per
/// pass, highest precedence first, until none remain.
fn convert_infix_bitops(line: &str) -> Cow<'_, str> {
    // Fast path: none of the target operators present.
    if !INFIX_BITOPS.iter().any(|(op, _, _)| line.contains(op)) {
        return Cow::Borrowed(line);
    }

    let mut toks = match tokenize_lua_line(line) {
        Some(t) => t,
        None => return Cow::Borrowed(line), // tokenizer bailed (unterminated string, etc.)
    };

    // Repeatedly rewrite the highest-precedence target operator until none are
    // left. Each rewrite collapses the operands + operator into one Group token.
    let mut changed = false;
    let mut guard = 0;
    loop {
        guard += 1;
        if guard > 64 {
            return Cow::Borrowed(line); // pathological; bail to unchanged
        }
        // Find the target operator token to rewrite: highest precedence first,
        // leftmost at that precedence.
        let mut target: Option<(usize, &'static str, u8)> = None;
        for (ti, tk) in toks.iter().enumerate() {
            if let TokKind::Op = tk.kind {
                if let Some((_, func, prec)) =
                    INFIX_BITOPS.iter().find(|(op, _, _)| *op == &*tk.text)
                {
                    let better = match target {
                        None => true,
                        Some((_, _, p)) => *prec > p,
                    };
                    if better {
                        target = Some((ti, func, *prec));
                    }
                }
            }
        }
        let (op_idx, func, prec) = match target {
            Some(t) => t,
            None => {
                return if changed {
                    Cow::Owned(join_toks(&toks))
                } else {
                    Cow::Borrowed(line)
                };
            }
        };

        // Delimit operands by precedence.
        let left = match operand_span_left(&toks, op_idx, prec) {
            Some(r) => r,
            None => return Cow::Borrowed(line),
        };
        let right = match operand_span_right(&toks, op_idx, prec) {
            Some(r) => r,
            None => return Cow::Borrowed(line),
        };

        // Rebuild: "func(" + left.. + ", " + right.. + ")"
        let mut call = String::new();
        call.push_str(func);
        call.push('(');
        call.push_str(&join_toks(&toks[left.0..op_idx]));
        call.push_str(", ");
        call.push_str(&join_toks(&toks[op_idx + 1..right.1]));
        call.push(')');
        splice_group(&mut toks, left.0..right.1, call);
        changed = true;
    }
}

/// Rewrite PICO-8 unary peek operators into function calls:
/// `@addr` -> `peek(addr)`, `%addr` -> `peek2(addr)`, `$addr` -> `peek4(addr)`.
///
/// These are unary prefix operators with priority 12 (z8lua), so they bind a
/// single high-precedence term (an atom plus postfix indexing/calls and any
/// leading sign), NOT a full expression: `@a+1` is `peek(a)+1`, `@(a+1)` is
/// `peek((a+1))`. `%` is only peek2 in prefix position — as a binary operator
/// it is modulo, which we leave alone.
///
/// Innermost-first (rightmost operator) so nested `@@a` -> `peek(peek(a))`
/// builds correctly. Bails to the unchanged line if an operand can't be found.
fn convert_unary_peek(line: &str) -> Cow<'_, str> {
    if !line.contains('@') && !line.contains('$') && !line.contains('%') {
        return Cow::Borrowed(line);
    }
    let mut toks = match tokenize_lua_line(line) {
        Some(t) => t,
        None => return Cow::Borrowed(line),
    };
    let mut changed = false;
    let mut guard = 0;
    loop {
        guard += 1;
        if guard > 64 {
            return Cow::Borrowed(line);
        }

        // Find the RIGHTMOST peek operator that is in prefix position, so nested
        // peeks resolve inside-out.
        let mut found: Option<(usize, &'static str)> = None;
        for (ti, tk) in toks.iter().enumerate() {
            if tk.kind != TokKind::Op {
                continue;
            }
            let func = match &*tk.text {
                "@" => "peek",
                "$" => "peek4",
                "%" if is_prefix_position(&toks, ti) => "peek2",
                _ => continue,
            };
            found = Some((ti, func)); // keep last (rightmost)
        }
        let (op_idx, func) = match found {
            Some(f) => f,
            None => {
                return if changed {
                    Cow::Owned(join_toks(&toks))
                } else {
                    Cow::Borrowed(line)
                };
            }
        };

        // The operand is the unary term to the right (priority 12).
        let span = match unary_operand_span(&toks, op_idx) {
            Some(s) => s,
            None => return Cow::Borrowed(line),
        };

        let mut call = String::new();
        call.push_str(func);
        call.push('(');
        call.push_str(&join_toks(&toks[op_idx + 1..span]));
        call.push(')');
        splice_group(&mut toks, op_idx..span, call);
        changed = true;
    }
}

/// True if the operator token at `idx` is in prefix (unary) position: the
/// previous non-space token is absent, an operator, an open boundary, or a
/// keyword — i.e. not a value that `%` could be a binary modulo of.
fn is_prefix_position(toks: &[Tok<'_>], idx: usize) -> bool {
    let mut j = idx;
    while j > 0 {
        let t = &toks[j - 1];
        match t.kind {
            TokKind::Space => {
                j -= 1;
            }
            TokKind::Op => return true,
            TokKind::Comment => return true,
            TokKind::Word => return is_boundary_keyword(&t.text),
            // A value (string / parenthesized group / number) precedes: binary.
            TokKind::Str | TokKind::Group => return false,
        }
    }
    true // start of line
}

/// Span `(op_idx, end)` of a unary operand: optional leading sign/peek tokens
/// then one atom (Word/Str/Group) plus trailing postfix groups (`[...]`,
/// `(...)`, and `.field` which tokenizes as part of the Word). Returns the end
/// index (exclusive). `None` if no atom follows.
fn unary_operand_span(toks: &[Tok<'_>], op_idx: usize) -> Option<usize> {
    let n = toks.len();
    let mut j = op_idx + 1;
    // Skip whitespace.
    while j < n && toks[j].kind == TokKind::Space {
        j += 1;
    }
    // Allow stacked unary prefix operators: @, $, %, -, ~, #, not.
    while j < n {
        let t = &toks[j];
        let is_unary = match t.kind {
            TokKind::Op => matches!(&*t.text, "@" | "$" | "%" | "-" | "~" | "#"),
            TokKind::Word => t.text == "not",
            _ => false,
        };
        if is_unary {
            j += 1;
            while j < n && toks[j].kind == TokKind::Space {
                j += 1;
            }
        } else {
            break;
        }
    }
    // The atom.
    if j >= n {
        return None;
    }
    match toks[j].kind {
        TokKind::Word | TokKind::Str | TokKind::Group => {
            j += 1;
        }
        _ => return None,
    }
    // Trailing postfix groups: indexing `[...]` or calls `(...)`. (`.field`
    // attaches to the Word token already.)
    while j < n && toks[j].kind == TokKind::Group {
        j += 1;
    }
    Some(j)
}

/// A lexical token of a single Lua line (no multi-line constructs — those are
/// handled by the caller). Whitespace is its own token so the original spacing
/// round-trips when tokens are concatenated. Text borrows from the line except
/// for groups synthesized by a rewrite.
struct Tok<'a> {
    text: Cow<'a, str>,
    kind: TokKind,
}

#[derive(PartialEq)]
enum TokKind {
    /// Identifier, number, or keyword.
    Word,
    /// String literal (including quotes).
    Str,
    /// An operator or punctuation run (we keep multi-char ops whole).
    Op,
    /// A balanced `(...)`, `[...]`, or `{...}` group (kept as one token).
    Group,
    /// Whitespace.
    Space,
    /// A `--` comment and everything after it (verbatim, never an operand).
    Comment,
}

/// Tokenize a single line for infix-operator rewriting. Returns `None` if the
/// line has an unterminated string or unbalanced grouping (caller leaves the
/// line unchanged). A `--` comment and everything after it is captured as one
/// trailing `Comment` token so it is preserved verbatim.
fn tokenize_lua_line(line: &str) -> Option<Vec<Tok<'_>>> {
    let bytes = line.as_bytes();
    let n = bytes.len();
    let mut toks: Vec<Tok<'_>> = Vec::new();
    let mut i = 0;
    fn push<'a>(toks: &mut Vec<Tok<'a>>, line: &'a str, start: usize, end: usize, kind: TokKind) {
        toks.push(Tok {
            text: Cow::Borrowed(&line[start..end]),
            kind,
        });
    }
    while i < n {
        let c = bytes[i];
        // Comment: capture the rest verbatim.
        if c == b'-' && i + 1 < n && bytes[i + 1] == b'-' {
            push(&mut toks, line, i, n, TokKind::Comment);
            return Some(toks);
        }
        // Whitespace run.
        if c == b' ' || c == b'\t' {
            let start = i;
            while i < n && (bytes[i] == b' ' || bytes[i] == b'\t') {
                i += 1;
            }
            push(&mut toks, line, start, i, TokKind::Space);
            continue;
        }
        // String literal.
        if c == b'"' || c == b'\'' {
            let span = Spans::from(line, i).next()?;
            if span.kind != SpanKind::Str {
                return None;
            }
            push(&mut toks, line, span.start, span.end, TokKind::Str);
            i = span.end;
            continue;
        }
        // Balanced group.
        if c == b'(' || c == b'[' || c == b'{' {
            let open = c;
            let close = match c {
                b'(' => b')',
                b'[' => b']',
                _ => b'}',
            };
            let start = i;
            let mut depth = 0i32;
            let mut end = None;
            'scan: for span in Spans::from(line, i).code() {
                for (off, &b) in bytes[span.start..span.end].iter().enumerate() {
                    if b == open {
                        depth += 1;
                    } else if b == close {
                        depth -= 1;
                        if depth == 0 {
                            end = Some(span.start + off + 1);
                            break 'scan;
                        }
                    }
                }
            }
            let end = end?; // unbalanced — bail
            push(&mut toks, line, start, end, TokKind::Group);
            i = end;
            continue;
        }
        if c == b')' || c == b']' || c == b'}' {
            // Unbalanced close at top level — bail.
            return None;
        }
        // Word: identifier / number (numbers may contain '.', 'x', hex digits).
        if c.is_ascii_alphanumeric() || c == b'_' || c == b'.' {
            let start = i;
            while i < n
                && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_' || bytes[i] == b'.')
            {
                i += 1;
            }
            push(&mut toks, line, start, i, TokKind::Word);
            continue;
        }
        // Operator/punctuation run. Match the longest known multi-char operator
        // first so `>>>`, `<<>`, `>><`, `^^`, `..`, `==`, etc. stay whole. A
        // non-ASCII character becomes a single (whole-char) Op token.
        let len = match_operator(&bytes[i..]).map_or_else(
            || line[i..].chars().next().map_or(1, char::len_utf8),
            str::len,
        );
        push(&mut toks, line, i, i + len, TokKind::Op);
        i += len;
    }
    Some(toks)
}

/// Match the longest known operator at the start of `rest`.
fn match_operator(rest: &[u8]) -> Option<&'static str> {
    const OPS: &[&str] = &[
        ">><", ">>>", "<<>", "<<", ">>", "..", "==", "~=", "<=", ">=", "^^", "//", "+", "-", "*",
        "/", "%", "^", "&", "|", "~", "<", ">", "=", ",", ";", "#", ":", "@", "$",
    ];
    OPS.iter()
        .copied()
        .find(|op| rest.starts_with(op.as_bytes()))
}

/// Precedence of an operator token for operand-delimiting purposes, using the
/// z8lua priority table. Returns `None` for non-operators / unknown tokens.
/// Lower number = binds looser.
fn op_precedence(text: &str) -> Option<u8> {
    Some(match text {
        "or" => 1,
        "and" => 2,
        "<" | ">" | "<=" | ">=" | "~=" | "==" => 3,
        ".." => 9, // right-assoc; treated as higher than our ops (5/7)
        "|" => 4,
        "^^" => 5,
        "&" => 6,
        "<<" | ">>" | ">>>" | "<<>" | ">><" => 7,
        "+" | "-" => 10,
        "*" | "/" | "%" | "\\" | "//" => 11,
        "^" => 12,
        // Statement/expression boundaries bind looser than anything.
        "=" | "," | ";" | ":" => 0,
        _ => return None,
    })
}

/// Find the left operand span `[start, op_idx)` for the operator at `op_idx`.
/// Scans left over tokens that are part of the operand, stopping before any
/// binary operator whose precedence is `<= prec` (looser-or-equal binds the
/// operand away) or a boundary. Returns `(start_idx, op_idx)`.
fn operand_span_left(toks: &[Tok<'_>], op_idx: usize, prec: u8) -> Option<(usize, usize)> {
    let mut start = op_idx;
    let mut j = op_idx;
    while j > 0 {
        let t = &toks[j - 1];
        match t.kind {
            TokKind::Space => {
                j -= 1;
            }
            TokKind::Word if is_boundary_keyword(&t.text) => break,
            TokKind::Word | TokKind::Str | TokKind::Group => {
                start = j - 1;
                j -= 1;
            }
            TokKind::Op => {
                // A boundary keyword/op: stop. Unary minus handling: if this op
                // is immediately an operand-leading sign we still stop (the
                // operand began after it).
                match op_precedence(&t.text) {
                    Some(p) if p > prec => {
                        // Higher-precedence operator binds into our operand.
                        start = j - 1;
                        j -= 1;
                    }
                    _ => break,
                }
            }
            TokKind::Comment => break,
        }
    }
    if start == op_idx {
        return None; // empty operand
    }
    Some((start, op_idx))
}

/// Find the right operand span `(op_idx, end)` for the operator at `op_idx`.
fn operand_span_right(toks: &[Tok<'_>], op_idx: usize, prec: u8) -> Option<(usize, usize)> {
    let mut end = op_idx + 1;
    let mut j = op_idx + 1;
    let n = toks.len();
    // Allow a leading unary +/-/not/# on the right operand.
    while j < n {
        let t = &toks[j];
        match t.kind {
            TokKind::Space => {
                j += 1;
                end = j;
            }
            TokKind::Word if is_boundary_keyword(&t.text) => break,
            TokKind::Word | TokKind::Str | TokKind::Group => {
                j += 1;
                end = j;
            }
            TokKind::Op => match op_precedence(&t.text) {
                Some(p) if p > prec => {
                    j += 1;
                    end = j;
                }
                _ => {
                    // Unary sign directly after the operator is part of operand.
                    if (t.text == "-" || t.text == "#" || t.text == "~")
                        && j == op_idx + 1 + leading_space(toks, op_idx + 1)
                    {
                        j += 1;
                        end = j;
                    } else {
                        break;
                    }
                }
            },
            TokKind::Comment => break,
        }
    }
    if end == op_idx + 1 {
        return None;
    }
    Some((op_idx + 1, end))
}

/// Lua keywords that act as operand boundaries when they appear as a `Word`
/// token (they terminate or separate expressions). `and`/`or`/`not` are NOT
/// here — `and`/`or` are handled via precedence, and `not`/`nil`/`true`/`false`
/// are valid operand atoms.
fn is_boundary_keyword(text: &str) -> bool {
    matches!(
        text,
        "return"
            | "then"
            | "do"
            | "end"
            | "else"
            | "elseif"
            | "if"
            | "while"
            | "for"
            | "repeat"
            | "until"
            | "function"
            | "local"
            | "break"
            | "in"
            | "goto"
    )
}

/// Count leading Space tokens starting at `from`.
fn leading_space(toks: &[Tok<'_>], from: usize) -> usize {
    let mut k = 0;
    while from + k < toks.len() && toks[from + k].kind == TokKind::Space {
        k += 1;
    }
    k
}

/// Run one per-line pass over `cur`, keeping the original borrow alive when
/// the pass has nothing to change (so unchanged lines never allocate).
fn apply<'a>(cur: Cow<'a, str>, pass: fn(&str) -> Cow<'_, str>) -> Cow<'a, str> {
    match cur {
        Cow::Borrowed(s) => pass(s),
        Cow::Owned(s) => {
            let changed = match pass(&s) {
                Cow::Borrowed(_) => None,
                Cow::Owned(o) => Some(o),
            };
            Cow::Owned(changed.unwrap_or(s))
        }
    }
}

/// Process a single line with all PICO-8 transforms (no multi-line string
/// state). Returns the line borrowed when nothing needed rewriting.
fn process_line_normal(line: &str) -> Cow<'_, str> {
    const PASSES: &[fn(&str) -> Cow<'_, str>] = &[
        // Button glyph globals (`btn(⬅️)`) -> numeric literals, since Lua 5.4
        // cannot lex the glyphs. First, so later token-level passes see atoms.
        convert_button_glyphs,
        // PICO-8 custom string escapes (`\*`,`\#`,`\-`,`\|`,`\+`,`\^`) -> hex
        // escapes Lua 5.4 accepts. Done first; it only rewrites string interiors.
        convert_string_escapes,
        // PICO-8's Lua treats `//` as a single-line comment (z8lua llex.c: "'/' or
        // '/=' or '//' (short comment)"). Standard Lua 5.4 parses `//` as integer
        // division, so convert a `//` comment to `--` before any other transform.
        // String-literal-aware; stops at an existing `--` comment.
        convert_double_slash_comment,
        // PICO-8 uses `\` as the integer-division operator (z8lua lparser.c: `'\\'
        // -> OPR_IDIV`); standard Lua 5.4 uses `//`. Convert bare `\` (not `\=`,
        // which the compound-assign pass handles) to `//`. Runs AFTER the comment
        // conversion so the produced `//` is division, not a comment.
        convert_backslash_idiv,
        // PICO-8 unary peek operators `@`/`%`/`$` -> peek/peek2/peek4 calls. Done
        // before the infix bitop pass so a peek inside a bitop operand (e.g.
        // `@a >>> @b`) is already a function-call atom.
        convert_unary_peek,
        // PICO-8-only infix bitwise ops (`>>>`, `<<>`, `>><`, `^^`) -> API calls.
        // Native Lua ops (`<<`, `>>`, `&`, `|`) pass through. Precedence-aware;
        // bails to unchanged if operands can't be confidently delimited.
        convert_infix_bitops,
        // PICO-8 accepts binary number literals (`0b1010`, with optional fraction);
        // Lua 5.4's lexer rejects them. Convert to decimal. (Hex literals already
        // work natively in Lua.)
        convert_binary_literals,
        // `?expr`, shorthand `if`, compound assignment and `!=`.
        process_line_tail,
    ];
    let mut cur = Cow::Borrowed(line);
    for pass in PASSES {
        cur = apply(cur, *pass);
    }
    cur
}

/// The statement-level transforms (`?expr`, shorthand `if`, compound
/// assignment / `!=`) applied after the token-level passes.
fn process_line_tail(line: &str) -> Cow<'_, str> {
    let trimmed = line.trim();

    // ?expr  ->  print(expr)
    if let Some(stripped) = trimmed.strip_prefix('?') {
        let indent = &line[..line.len() - line.trim_start().len()];
        let expr = stripped.trim();
        let mut out = String::with_capacity(indent.len() + expr.len() + 7);
        out.push_str(indent);
        out.push_str("print(");
        out.push_str(expr);
        out.push(')');
        return Cow::Owned(out);
    }

    // Single-line if shorthand: `if (cond) stmt` where there's no `then`
    if let Some(processed) = try_shorthand_if(trimmed, line) {
        // Also run operator transforms on the result (e.g., body may have +=)
        return apply(Cow::Owned(processed), process_operators);
    }

    // Compound assignment and != replacement
    process_operators(line)
}

/// Scan a line for a multi-line string opening delimiter `[=*[` or multi-line comment
/// opening `--[=*[`, respecting regular string literals.
///
/// Returns `Some((byte_offset, eq_count))` where `byte_offset` is the byte position of the
/// `[` (or `--` for comments) and `eq_count` is the number of `=` signs. Returns `None` if
/// no opening delimiter is found outside of regular strings.
fn find_multiline_open(line: &str) -> Option<(usize, usize)> {
    let bytes = line.as_bytes();
    for span in Spans::new(line) {
        match span.kind {
            SpanKind::Str | SpanKind::UnterminatedStr => {}
            SpanKind::Comment => {
                // `--[=*[` is a multi-line comment; the offset is the start of
                // `--` so the dashes are part of the pass-through portion.
                // Otherwise the rest of the line is a plain comment.
                return try_parse_long_bracket_open(bytes, span.start + 2)
                    .map(|eq_count| (span.start, eq_count));
            }
            SpanKind::Code => {
                for i in span.start..span.end {
                    if bytes[i] == b'[' {
                        if let Some(eq_count) = try_parse_long_bracket_open(bytes, i) {
                            return Some((i, eq_count));
                        }
                    }
                }
            }
        }
    }
    None
}

/// Try to parse a long bracket opening at position `start` in `bytes`.
/// `bytes[start]` must be `[`. If the pattern `[=*[` is found, returns `Some(eq_count)`.
fn try_parse_long_bracket_open(bytes: &[u8], start: usize) -> Option<usize> {
    if start >= bytes.len() || bytes[start] != b'[' {
        return None;
    }
    let mut j = start + 1;
    let mut eq_count = 0;
    while j < bytes.len() && bytes[j] == b'=' {
        eq_count += 1;
        j += 1;
    }
    if j < bytes.len() && bytes[j] == b'[' {
        Some(eq_count)
    } else {
        None
    }
}

/// Scan a string for the closing long bracket `]=*]` with exactly `eq_count` equals signs.
///
/// Returns `Some(byte_offset)` where `byte_offset` is the byte position just past the end
/// of the closing delimiter (exclusive). Returns `None` if not found.
fn find_multiline_close(s: &str, eq_count: usize) -> Option<usize> {
    let bytes = s.as_bytes();
    let len = bytes.len();
    let mut i = 0;

    // Build the closing pattern: `]` + `=`*eq_count + `]`
    while i < len {
        if bytes[i] == b']' {
            // Check if this is the start of a closing long bracket
            let mut j = i + 1;
            let mut found_eq = 0;
            while j < len && bytes[j] == b'=' {
                found_eq += 1;
                j += 1;
            }
            if found_eq == eq_count && j < len && bytes[j] == b']' {
                // Found the matching close delimiter. Return position past the end.
                return Some(j + 1);
            }
        }
        i += 1;
    }

    None
}

/// Join lines that end with `\` (PICO-8 line continuation).
///
/// A backslash at the end of a line (ignoring trailing whitespace) causes
/// the current line and the next line to be joined with a space.
/// The backslash must not be inside a string literal or comment.
/// Returns the source borrowed when no line has a continuation.
fn join_backslash_lines(code: &str) -> Cow<'_, str> {
    if !code.lines().any(line_ends_with_continuation) {
        return Cow::Borrowed(code);
    }
    let mut result = String::with_capacity(code.len());
    let mut lines = code.lines();
    while let Some(line) = lines.next() {
        if !line_ends_with_continuation(line) {
            result.push_str(line);
            result.push('\n');
            continue;
        }
        let mut combined = String::from(line);
        while line_ends_with_continuation(&combined) {
            // Remove the trailing backslash and any whitespace before it
            let trimmed_end = combined.trim_end();
            let new_len = trimmed_end.len() - 1; // remove the '\'
            combined.truncate(new_len);
            let trimmed_len = combined.trim_end().len();
            combined.truncate(trimmed_len);
            match lines.next() {
                Some(next) => {
                    combined.push(' ');
                    combined.push_str(next.trim_start());
                }
                None => break,
            }
        }
        result.push_str(&combined);
        result.push('\n');
    }
    Cow::Owned(result)
}

/// Check if a line ends with a `\` continuation character outside of strings and comments.
fn line_ends_with_continuation(line: &str) -> bool {
    let trimmed = line.trim_end();
    if !trimmed.ends_with('\\') {
        return false;
    }
    // The final `\` counts only if it sits at code level: an unterminated
    // string or a comment running to the end of the line swallows it.
    Spans::new(trimmed)
        .last()
        .is_some_and(|s| s.kind == SpanKind::Code)
}

/// Try to convert a PICO-8 single-line `if (cond) stmt` shorthand.
/// Also handles `if (cond) stmt1 else stmt2`.
/// Returns Some(converted) if the pattern matches, None otherwise.
fn try_shorthand_if(trimmed: &str, original: &str) -> Option<String> {
    if !trimmed.starts_with("if") {
        return None;
    }
    let after_if = trimmed[2..].trim_start();
    if !after_if.starts_with('(') {
        return None;
    }
    // Already has "then" keyword -> it's normal Lua, don't transform
    if find_keyword_outside_strings(trimmed, "then").is_some() {
        return None;
    }
    // Find matching close paren (string-aware); byte offset.
    let close_idx = find_matching_paren(after_if, 0)?;
    let cond = &after_if[1..close_idx];
    let rest_full = after_if[close_idx + 1..].trim();
    if rest_full.is_empty() {
        return None;
    }

    // A trailing line comment (e.g. `if(x)return--bye`) must be moved AFTER
    // the synthesized `end`, otherwise the comment swallows the `end`.
    let (rest, comment) = split_trailing_comment(rest_full);
    let rest = rest.trim();
    let comment_suffix = if comment.is_empty() {
        String::new()
    } else {
        format!(" {comment}")
    };

    let indent = &original[..original.len() - original.trim_start().len()];

    // Check if rest contains an `else` keyword outside strings
    if let Some(else_pos) = find_keyword_outside_strings(rest, "else") {
        let stmt1 = rest[..else_pos].trim();
        let after_else = rest[else_pos + 4..].trim();
        if !stmt1.is_empty() && !after_else.is_empty() {
            return Some(format!(
                "{indent}if {cond} then {stmt1} else {after_else} end{comment_suffix}"
            ));
        }
    }

    Some(format!("{indent}if {cond} then {rest} end{comment_suffix}"))
}

/// Split a trailing `--` line comment off `s`, ignoring `--` inside strings.
/// Returns `(code_part, comment_part)` where `comment_part` is empty if there
/// is no trailing comment. Long-bracket comments (`--[[`) are left in the code
/// part (they are block comments, not line comments, and are rare on one line)
/// and scanning continues after them.
fn split_trailing_comment(s: &str) -> (&str, &str) {
    let mut pos = 0;
    while let Some(span) = Spans::from(s, pos).find(|sp| sp.kind == SpanKind::Comment) {
        if try_parse_long_bracket_open(s.as_bytes(), span.start + 2).is_none() {
            return (&s[..span.start], &s[span.start..]);
        }
        pos = span.start + 2;
    }
    (s, "")
}

/// Find the matching close parenthesis starting from `start` in `s`.
/// `s[start]` must be `(` at code level. Returns the BYTE offset of the
/// matching `)`. String-literal-aware.
fn find_matching_paren(s: &str, start: usize) -> Option<usize> {
    let bytes = s.as_bytes();
    let mut depth = 0i32;
    for span in Spans::from(s, start).code() {
        for (off, &b) in bytes[span.start..span.end].iter().enumerate() {
            match b {
                b'(' => depth += 1,
                b')' => {
                    depth -= 1;
                    if depth == 0 {
                        return Some(span.start + off);
                    }
                }
                _ => {}
            }
        }
    }
    None
}

/// Find the byte position of keyword `kw` outside of string literals and
/// comments. A keyword must be bounded by non-identifier bytes (or line
/// start/end).
fn find_keyword_outside_strings(line: &str, kw: &str) -> Option<usize> {
    let bytes = line.as_bytes();
    let is_ident = |b: u8| b.is_ascii_alphanumeric() || b == b'_';
    for span in Spans::new(line) {
        match span.kind {
            SpanKind::Comment => return None, // rest is comment
            SpanKind::Str | SpanKind::UnterminatedStr => continue,
            SpanKind::Code => {}
        }
        let mut from = span.start;
        while let Some(rel) = line[from..span.end].find(kw) {
            let at = from + rel;
            let end = at + kw.len();
            let left_ok = at == 0 || !is_ident(bytes[at - 1]);
            let right_ok = end >= bytes.len() || !is_ident(bytes[end]);
            if left_ok && right_ok {
                return Some(at);
            }
            from = at + 1;
        }
    }
    None
}

/// Replace compound assignments (+=, -=, *=, /=, %=, ..=) and != with standard Lua.
fn process_operators(line: &str) -> Cow<'_, str> {
    // Fast path: if the line has no `=` or `!`, no operators can match.
    if !line.contains('=') && !line.contains('!') {
        return Cow::Borrowed(line);
    }

    let mut result = Cow::Borrowed(line);

    // Function-form compound assignments first, longest operator first so that
    // e.g. `>>>=` matches before `>>=`, `<<>=` before `<<=`, `^^=` before `^=`.
    // PICO-8's bitwise/shift ops map to function calls, not Lua operators:
    //   var <<= e   ->  var = shl(var, (e))   (z8lua TK_SHLE etc.)
    for (op, func) in &[
        (">><=", "rotr"), // TK_ROTRE
        (">>>=", "lshr"), // TK_LSHRE
        ("<<>=", "rotl"), // TK_ROTLE
        ("<<=", "shl"),   // TK_SHLE
        (">>=", "shr"),   // TK_SHRE
        ("^^=", "bxor"),  // TK_BXORE
        ("&=", "band"),   // TK_BANDE
        ("|=", "bor"),    // TK_BORE
    ] {
        if let Some(transformed) = compound_assign(&result, op, |var, expr| {
            format!("{var} = {func}({var}, ({expr}))")
        }) {
            result = Cow::Owned(transformed);
        }
    }

    // Replace != with ~= (but not inside strings). Done after the shift ops so
    // it can't interfere with `>>=`/`<<=` matching.
    if let Cow::Owned(replaced) = replace_outside_strings(&result, "!=", "~=") {
        result = Cow::Owned(replaced);
    }

    // Infix-operator compound assignments: var op= expr  ->  var = var op (expr)
    // Important: ..= must come before the others to avoid partial matches; `^=`
    // is pow (Lua has `^`), distinct from the `^^=` (bxor) handled above.
    for (op, lua_op) in &[
        ("..=", ".."),
        ("+=", "+"),
        ("-=", "-"),
        ("*=", "*"),
        ("/=", "/"),
        ("%=", "%"),
        ("^=", "^"),
        // PICO-8 `\=` is integer-division-assign -> Lua `//`.
        ("\\=", "//"),
    ] {
        if let Some(transformed) = compound_assign(&result, op, |var, expr| {
            format!("{var} = {var} {lua_op} ({expr})")
        }) {
            result = Cow::Owned(transformed);
        }
    }

    result
}

/// Replace occurrences of `pattern` with `replacement`, but only outside of
/// string literals and comments. `pattern` must not contain a quote or `--`.
/// Returns the line borrowed when there is nothing to replace.
fn replace_outside_strings<'a>(line: &'a str, pattern: &str, replacement: &str) -> Cow<'a, str> {
    let mut out: Option<String> = None;
    let mut copied = 0usize;
    for span in Spans::new(line).code() {
        let mut from = span.start;
        while let Some(rel) = line[from..span.end].find(pattern) {
            let at = from + rel;
            let out = out.get_or_insert_with(|| String::with_capacity(line.len()));
            out.push_str(&line[copied..at]);
            out.push_str(replacement);
            copied = at + pattern.len();
            from = copied;
        }
    }
    match out {
        Some(mut out) => {
            out.push_str(&line[copied..]);
            Cow::Owned(out)
        }
        None => Cow::Borrowed(line),
    }
}

/// Rewrite every `var <op> expr` compound assignment on the line, where `op`
/// is a compound operator (e.g. `+=`, `>>>=`) and `emit(var, expr)` produces
/// the replacement statement. String- and comment-aware. Returns `None` if
/// nothing was rewritten.
fn compound_assign(line: &str, op: &str, emit: impl Fn(&str, &str) -> String) -> Option<String> {
    if !line.contains(op) {
        return None;
    }

    let mut result = String::with_capacity(line.len() + 16);
    // `result` mirrors `line[..copied]` (with rewrites applied).
    let mut copied = 0usize;
    let mut changed = false;
    let mut spans = Spans::new(line);

    while let Some(span) = spans.next() {
        if span.kind != SpanKind::Code {
            continue;
        }
        let mut from = span.start;
        while let Some(rel) = line[from..span.end].find(op) {
            let at = from + rel;
            result.push_str(&line[copied..at]);
            copied = at;
            if let Some((var, prefix_end)) = extract_lvalue_backwards(&result) {
                let expr_start = at + op.len();
                let (expr, expr_end) = extract_rhs_expr(line, expr_start);
                if !var.is_empty() && !expr.is_empty() {
                    let stmt = emit(var, &expr);
                    result.truncate(prefix_end);
                    result.push_str(&stmt);
                    copied = expr_end;
                    changed = true;
                    // The RHS may have crossed string spans; resume the span
                    // scan at its (code-level) end.
                    spans = Spans::from(line, expr_end);
                    break;
                }
            }
            // No rewrite here: keep scanning one byte further on so an
            // overlapping match (e.g. `..=` inside `...=`) is still found.
            from = at + 1;
        }
    }

    if changed {
        result.push_str(&line[copied..]);
        Some(result)
    } else {
        None
    }
}

/// Look backwards from the end of `built` to extract an l-value (variable name).
/// Returns `(lvalue, byte_offset_in_built_where_lvalue_starts)`.
fn extract_lvalue_backwards(built: &str) -> Option<(&str, usize)> {
    let bytes = built.as_bytes();
    let mut end = bytes.len();

    // Skip trailing whitespace
    while end > 0 && (bytes[end - 1] == b' ' || bytes[end - 1] == b'\t') {
        end -= 1;
    }
    if end == 0 {
        return None;
    }

    // Scan backwards through the l-value. Only ASCII bytes are consumed
    // outside brackets, so `start` always lands on a char boundary.
    let mut start = end;
    let mut bracket_depth = 0i32;
    while start > 0 {
        let ch = bytes[start - 1];
        if ch == b']' {
            bracket_depth += 1;
            start -= 1;
        } else if ch == b'[' {
            if bracket_depth > 0 {
                bracket_depth -= 1;
                start -= 1;
            } else {
                break;
            }
        } else if bracket_depth > 0
            || ch.is_ascii_alphanumeric()
            || ch == b'_'
            || ch == b'.'
            || ch == b':'
        {
            start -= 1;
        } else {
            break;
        }
    }

    if start == end {
        return None;
    }

    let first = bytes[start];
    if !first.is_ascii_alphabetic() && first != b'_' {
        return None;
    }

    Some((&built[start..end], start))
}

/// Check if position `i` in `bytes` is at a keyword boundary that ends a
/// statement: `end`, `else`, or `elseif`, preceded by whitespace (or at the start).
fn starts_with_keyword_boundary(bytes: &[u8], i: usize) -> bool {
    let remaining = &bytes[i..];
    let preceded_by_ws = i == 0 || bytes[i - 1] == b' ' || bytes[i - 1] == b'\t';

    // Keywords that terminate a preceding expression / start a new statement.
    // NOTE: `and`/`or`/`not` are deliberately excluded — they are part of the
    // expression. `then`/`do`/`until`/`return`/`break` follow an expression and
    // begin new syntax, so the compound-assignment RHS must stop before them.
    const BOUNDARY_KWS: &[&[u8]] = &[
        b"end", b"elseif", // before "else" so longer matches win
        b"else", b"then", b"do", b"return", b"until", b"break",
    ];

    // The keyword must be a whole word: followed by a non-identifier byte
    // (or end-of-line) so we don't match `endx` or `returns`.
    let word_boundary_after = |after: usize| -> bool {
        after >= remaining.len() || {
            let c = remaining[after];
            !(c.is_ascii_alphanumeric() || c == b'_')
        }
    };

    for kw in BOUNDARY_KWS {
        // Match with one leading whitespace char.
        if remaining.len() > kw.len()
            && (remaining[0] == b' ' || remaining[0] == b'\t')
            && remaining[1..].starts_with(kw)
            && word_boundary_after(1 + kw.len())
        {
            return true;
        }

        // Match at position if preceded by whitespace.
        if preceded_by_ws && remaining.starts_with(kw) && word_boundary_after(kw.len()) {
            return true;
        }
    }

    false
}

/// Extract the right-hand-side expression after a compound operator.
/// Stops at: `end`, `else`, `elseif`, `)`, `;`, end of line, or comment.
/// `start` must be a code-level byte offset. Returns
/// `(expression_string, byte_index_past_expression)`; the index is always a
/// code-level position (or the line length).
fn extract_rhs_expr(line: &str, start: usize) -> (String, usize) {
    let bytes = line.as_bytes();
    let mut i = start;
    // Skip leading whitespace
    while i < bytes.len() && (bytes[i] == b' ' || bytes[i] == b'\t') {
        i += 1;
    }

    let expr_start = i;
    let mut paren_depth = 0i32;

    'outer: for span in Spans::from(line, i) {
        match span.kind {
            SpanKind::Comment => {
                i = span.start;
                break;
            }
            SpanKind::Str | SpanKind::UnterminatedStr => {
                i = span.end;
                continue;
            }
            SpanKind::Code => {}
        }
        i = span.start;
        while i < span.end {
            let b = bytes[i];
            // A statement separator ends the RHS expression (e.g. `t /= d;`).
            if paren_depth == 0 && b == b';' {
                break 'outer;
            }
            if b == b'(' {
                paren_depth += 1;
            } else if b == b')' {
                if paren_depth > 0 {
                    paren_depth -= 1;
                } else {
                    break 'outer;
                }
            }

            // Check for Lua keywords that end a statement (only at paren_depth 0)
            if paren_depth == 0 && starts_with_keyword_boundary(bytes, i) {
                break 'outer;
            }

            i += 1;
        }
    }

    (line[expr_start..i].trim().to_string(), i)
}

#[cfg(test)]
mod tests {
    use super::*;

    // -------------------------------------------------------
    // `//` single-line comments (PICO-8 treats `//` as a comment, NOT integer
    // division — z8lua llex.c). Real BBS carts use `pal(1,0) //0`.
    // -------------------------------------------------------

    #[test]
    fn test_double_slash_trailing_comment() {
        let out = preprocess_pico8("pal(1,0) //0\n");
        assert_eq!(out.trim(), "pal(1,0) --0");
    }

    #[test]
    fn test_double_slash_full_line_comment() {
        let out = preprocess_pico8("//a comment\n");
        assert_eq!(out.trim(), "--a comment");
    }

    #[test]
    fn test_double_slash_inside_string_untouched() {
        // `//` inside a string literal must NOT become a comment.
        let out = preprocess_pico8("x=\"http://a\"\n");
        assert_eq!(out.trim(), "x=\"http://a\"");
    }

    #[test]
    fn test_double_slash_after_dash_comment_untouched() {
        // An existing `--` comment owns the rest of the line; `//` within it
        // stays literal comment text.
        let out = preprocess_pico8("x=1 -- see // note\n");
        assert_eq!(out.trim(), "x=1 -- see // note");
    }

    #[test]
    fn test_double_slash_does_not_break_compound_ops() {
        // Code before the `//` still gets normal transforms applied. (The
        // compound-op transform may drop the space before the comment; the
        // result is still valid Lua with the comment intact.)
        let out = preprocess_pico8("x+=1 //inc\n");
        assert_eq!(out.trim(), "x = x + (1)--inc");
    }

    // -------------------------------------------------------
    // `\` integer division (PICO-8) -> `//` (Lua 5.4). Integer division in
    // PICO-8 is `\`, NOT `//` (which is a comment). z8lua lparser.c maps `\`
    // to OPR_IDIV.
    // -------------------------------------------------------

    #[test]
    fn test_backslash_integer_division() {
        assert_eq!(preprocess_pico8("x=7\\2\n").trim(), "x=7//2");
        assert_eq!(preprocess_pico8("y = a \\ b\n").trim(), "y = a // b");
    }

    #[test]
    fn test_backslash_compound_assign() {
        assert_eq!(preprocess_pico8("x\\=3\n").trim(), "x = x // (3)");
    }

    #[test]
    fn test_backslash_inside_string_untouched() {
        // A backslash escape inside a string must NOT become `//`.
        assert_eq!(preprocess_pico8("s=\"a\\nb\"\n").trim(), "s=\"a\\nb\"");
    }

    #[test]
    fn test_backslash_line_continuation_still_joins() {
        // A trailing `\` is line continuation (joined before idiv conversion),
        // not integer division.
        let out = preprocess_pico8("x=1+\\\n2\n");
        assert!(
            out.contains("x=1+2") || out.contains("x=1+ 2"),
            "got: {out:?}"
        );
        assert!(
            !out.contains("//"),
            "continuation must not become idiv: {out:?}"
        );
    }

    #[test]
    fn test_backslash_idiv_evaluates() {
        let out = preprocess_pico8("function f() return 17\\5 end\n");
        assert!(out.contains("17//5"), "got: {out}");
    }

    // -------------------------------------------------------
    // PICO-8 custom string escapes (\* \# \- \| \+ \^) -> \xNN. Lua 5.4 rejects
    // the raw forms; the P8SCII control bytes 1-6 must be preserved.
    // -------------------------------------------------------

    #[test]
    fn test_string_escapes_mapped() {
        assert_eq!(preprocess_pico8("s=\"\\*\"\n").trim(), "s=\"\\x01\"");
        assert_eq!(preprocess_pico8("s=\"\\#\"\n").trim(), "s=\"\\x02\"");
        assert_eq!(preprocess_pico8("s=\"\\-\"\n").trim(), "s=\"\\x03\"");
        assert_eq!(preprocess_pico8("s=\"\\|\"\n").trim(), "s=\"\\x04\"");
        assert_eq!(preprocess_pico8("s=\"\\+\"\n").trim(), "s=\"\\x05\"");
        assert_eq!(preprocess_pico8("s=\"\\^\"\n").trim(), "s=\"\\x06\"");
    }

    #[test]
    fn test_string_escape_trailing_digit_not_merged() {
        // `\*4` must be char 1 + '4' (two bytes), NOT a single `\14`.
        // Hex escape `\x01` is exactly 2 digits and won't absorb the '4'.
        assert_eq!(preprocess_pico8("s=\"\\*4\"\n").trim(), "s=\"\\x014\"");
    }

    #[test]
    fn test_string_escape_other_escapes_untouched() {
        // Standard escapes and an escaped backslash are preserved.
        assert_eq!(preprocess_pico8("s=\"\\n\\t\"\n").trim(), "s=\"\\n\\t\"");
        assert_eq!(preprocess_pico8("s=\"\\\\*\"\n").trim(), "s=\"\\\\*\"");
        // A `*` outside a string is untouched (multiplication).
        assert_eq!(preprocess_pico8("x=a*b\n").trim(), "x=a*b");
    }

    // -------------------------------------------------------
    // Binary number literals (0b1010) -> decimal. PICO-8's lexer accepts them;
    // Lua 5.4's does not. (Hex literals already work in Lua.)
    // -------------------------------------------------------

    #[test]
    fn test_binary_literals() {
        assert_eq!(preprocess_pico8("x=0b1010\n").trim(), "x=10");
        assert_eq!(preprocess_pico8("x=0B11\n").trim(), "x=3");
        assert_eq!(preprocess_pico8("x=0b0\n").trim(), "x=0");
        // fractional binary.
        assert_eq!(preprocess_pico8("x=0b0101.1\n").trim(), "x=5.5");
        // multiple on one line.
        assert_eq!(preprocess_pico8("x=0b1+0b10\n").trim(), "x=1+2");
    }

    #[test]
    fn test_binary_literals_boundaries() {
        // Hex passes through (native Lua).
        assert_eq!(preprocess_pico8("x=0xff\n").trim(), "x=0xff");
        // Inside a string: untouched.
        assert_eq!(preprocess_pico8("s=\"0b11\"\n").trim(), "s=\"0b11\"");
        // Part of an identifier: not a literal.
        assert_eq!(preprocess_pico8("x=a0b1\n").trim(), "x=a0b1");
    }

    // -------------------------------------------------------
    // Unary peek operators: @ -> peek, % -> peek2 (prefix), $ -> peek4.
    // Priority 12: binds a single atom + postfix, not a full expression.
    // -------------------------------------------------------

    #[test]
    fn test_unary_peek_basic() {
        assert_eq!(preprocess_pico8("x=@0x5f00\n").trim(), "x=peek(0x5f00)");
        assert_eq!(preprocess_pico8("x=$0x4300\n").trim(), "x=peek4(0x4300)");
        assert_eq!(preprocess_pico8("x=%100\n").trim(), "x=peek2(100)");
    }

    #[test]
    fn test_unary_peek_binding_and_nesting() {
        // Unary (prio 12) binds tighter than `+` (prio 10).
        assert_eq!(preprocess_pico8("x=@a+1\n").trim(), "x=peek(a)+1");
        // Parenthesized operand.
        assert_eq!(preprocess_pico8("x=@(a+1)\n").trim(), "x=peek((a+1))");
        // Postfix indexing attaches to the operand.
        assert_eq!(preprocess_pico8("x=@a[2]\n").trim(), "x=peek(a[2])");
        // Nested peeks resolve inside-out.
        assert_eq!(preprocess_pico8("x=@@a\n").trim(), "x=peek(peek(a))");
    }

    #[test]
    fn test_percent_modulo_not_mistaken_for_peek2() {
        // `%` after a value is binary modulo, NOT peek2.
        assert_eq!(preprocess_pico8("x=5%2\n").trim(), "x=5%2");
        assert_eq!(preprocess_pico8("x=a%b\n").trim(), "x=a%b");
        assert_eq!(preprocess_pico8("x=(a)%2\n").trim(), "x=(a)%2");
    }

    #[test]
    fn test_unary_peek_in_string_untouched() {
        assert_eq!(
            preprocess_pico8("s=\"@a $b %c\"\n").trim(),
            "s=\"@a $b %c\""
        );
    }

    // -------------------------------------------------------
    // Infix PICO-8-only bitwise operators (>>> <<> >>< ^^) -> API calls.
    // Native Lua ops (<< >> & |) pass through. Precedence-aware.
    // -------------------------------------------------------

    #[test]
    fn test_infix_bitops_basic() {
        assert_eq!(preprocess_pico8("x=8>>>1\n").trim(), "x=lshr(8, 1)");
        assert_eq!(preprocess_pico8("x=5^^3\n").trim(), "x=bxor(5, 3)");
        assert_eq!(preprocess_pico8("x=8<<>4\n").trim(), "x=rotl(8, 4)");
        assert_eq!(preprocess_pico8("x=8>><1\n").trim(), "x=rotr(8, 1)");
    }

    #[test]
    fn test_infix_bitops_native_pass_through() {
        // `<<`, `>>`, `&`, `|` are native Lua 5.4 — must NOT be transformed.
        assert_eq!(preprocess_pico8("x=8>>1\n").trim(), "x=8>>1");
        assert_eq!(preprocess_pico8("x=8<<1\n").trim(), "x=8<<1");
        assert_eq!(preprocess_pico8("x=8&4\n").trim(), "x=8&4");
        assert_eq!(preprocess_pico8("x=8|4\n").trim(), "x=8|4");
    }

    #[test]
    fn test_infix_bitops_precedence() {
        // `+` (prec 10) binds tighter than `>>>` (prec 7): operand is `a+b`.
        assert_eq!(preprocess_pico8("x=a+b>>>c\n").trim(), "x=lshr(a+b, c)");
        assert_eq!(preprocess_pico8("x=a>>>b+c\n").trim(), "x=lshr(a, b+c)");
        // chained left-assoc.
        assert_eq!(
            preprocess_pico8("x=a^^b^^c\n").trim(),
            "x=bxor(bxor(a, b), c)"
        );
    }

    #[test]
    fn test_infix_bitops_stop_at_keywords_and_strings() {
        assert_eq!(
            preprocess_pico8("return 8>>>1\n").trim(),
            "return lshr(8, 1)"
        );
        // string contents untouched.
        assert_eq!(preprocess_pico8("s=\"a>>>b\"\n").trim(), "s=\"a>>>b\"");
        // function/return boundary respected.
        assert_eq!(
            preprocess_pico8("function f() return 8>>>1 end\n").trim(),
            "function f() return lshr(8, 1 )end"
        );
    }

    // -------------------------------------------------------
    // Bitwise / shift compound assignments (PICO-8 -> function-call form).
    // z8lua: <<= >>= >>>= <<>= >><= &= |= ^^= map to shl/shr/lshr/rotl/rotr/
    // band/bor/bxor; ^= is pow.
    // -------------------------------------------------------

    #[test]
    fn test_shift_compound_assigns() {
        assert_eq!(preprocess_pico8("x<<=1\n").trim(), "x = shl(x, (1))");
        assert_eq!(preprocess_pico8("x>>=1\n").trim(), "x = shr(x, (1))");
        assert_eq!(preprocess_pico8("x>>>=1\n").trim(), "x = lshr(x, (1))");
        assert_eq!(preprocess_pico8("x<<>=4\n").trim(), "x = rotl(x, (4))");
        assert_eq!(preprocess_pico8("x>><=1\n").trim(), "x = rotr(x, (1))");
    }

    #[test]
    fn test_bitwise_compound_assigns() {
        assert_eq!(preprocess_pico8("x&=2\n").trim(), "x = band(x, (2))");
        assert_eq!(preprocess_pico8("x|=2\n").trim(), "x = bor(x, (2))");
        assert_eq!(preprocess_pico8("x^^=3\n").trim(), "x = bxor(x, (3))");
    }

    #[test]
    fn test_pow_compound_assign_is_infix() {
        // `^=` is exponent-assign (Lua has `^`), distinct from `^^=` (bxor).
        assert_eq!(preprocess_pico8("x^=3\n").trim(), "x = x ^ (3)");
    }

    #[test]
    fn test_compound_assign_rhs_stops_at_keywords() {
        // RHS must not swallow a following statement keyword.
        assert_eq!(
            preprocess_pico8("x+=2 return x\n").trim(),
            "x = x + (2) return x"
        );
        assert_eq!(
            preprocess_pico8("x<<=2 return x\n").trim(),
            "x = shl(x, (2)) return x"
        );
        // ...but `and`/`or` ARE part of the expression.
        assert_eq!(preprocess_pico8("a+=b and c\n").trim(), "a = a + (b and c)");
    }

    #[test]
    fn test_comparisons_not_mistaken_for_compound() {
        // `<=`, `>=`, `==` must pass through untouched.
        assert_eq!(
            preprocess_pico8("if x<=2 then end\n").trim(),
            "if x<=2 then end"
        );
        assert_eq!(
            preprocess_pico8("if x>=2 then end\n").trim(),
            "if x>=2 then end"
        );
        assert_eq!(
            preprocess_pico8("if x==2 then end\n").trim(),
            "if x==2 then end"
        );
    }

    // -------------------------------------------------------
    // Compound assignment operators
    // -------------------------------------------------------

    #[test]
    fn test_plus_equals() {
        let out = preprocess_pico8("x+=1\n");
        assert_eq!(out.trim(), "x = x + (1)");
    }

    #[test]
    fn test_compound_assign_with_multibyte_utf8_no_panic() {
        // Regression (loader fuzzer): a multibyte UTF-8 char before a compound
        // operator made `extract_lvalue_backwards` return a char index that
        // `String::truncate` rejected as a non-char-boundary, panicking. The
        // l-value offset must be a byte offset. These must not panic and must
        // still transform the trailing compound assignment.
        let out = preprocess_pico8("-- héllo ⬅️\nx+=1\n");
        assert!(out.contains("x = x + (1)"), "got: {out:?}");
        // Multibyte content on the same line as the operator.
        let out2 = preprocess_pico8(" €=0 x+=2\n");
        assert!(out2.contains("x = x + (2)"), "got: {out2:?}");
        // A multibyte identifier-ish prefix directly before the operator just
        // must not panic (it isn't a valid l-value, so it's left alone).
        let _ = preprocess_pico8("café+=1\n");
    }

    #[test]
    fn test_shorthand_if_with_multibyte_utf8_no_panic() {
        // Regression: `find_matching_paren` returned a CHAR index that
        // `try_shorthand_if` used as a BYTE slice index, panicking on any
        // multibyte char before the closing paren (PICO-8 button glyphs are
        // common here). All index-returning scanners must yield byte offsets.
        let out = preprocess_pico8("if (btn(\u{2b05}\u{fe0f})) x-=1\n");
        assert_eq!(out.trim(), "if btn(0) then x = x - (1) end", "got: {out:?}");
        let out = preprocess_pico8("if (s==\"\u{e9}\u{e9}\") x=1\n");
        assert_eq!(
            out.trim(),
            "if s==\"\u{e9}\u{e9}\" then x=1 end",
            "got: {out:?}"
        );
        // Multibyte in the body and in a trailing comment, with else.
        let out = preprocess_pico8("if (a) b=\"\u{e9}\" else c=\"\u{2b05}\" -- \u{e9}\n");
        assert_eq!(
            out.trim(),
            "if a then b=\"\u{e9}\" else c=\"\u{2b05}\" end -- \u{e9}",
            "got: {out:?}"
        );
        // Multibyte before every other index-returning scanner: keyword
        // search, `!=` replacement, peek/bitop tokenizer, binary literals.
        let out = preprocess_pico8("if (s!=\"\u{e9}\") x=@a else y=0b11>>>b -- \u{e9}\n");
        assert_eq!(
            out.trim(),
            // (the space before the comment is part of the right operand,
            // as it always has been)
            "if s~=\"\u{e9}\" then x=peek(a) else y=lshr(3, b ) end -- \u{e9}",
            "got: {out:?}"
        );
        // A non-ASCII char in code position is not a Lua identifier; the
        // token-level passes leave such a line alone rather than guessing.
        assert_eq!(
            preprocess_pico8("x=@\u{e9}\n").trim(),
            "x=@\u{e9}",
            "non-identifier operand must be left unchanged"
        );
    }

    #[test]
    fn test_button_glyphs_become_literals() {
        // PICO-8 predefines ⬅️=0 ➡️=1 ⬆️=2 ⬇️=3 🅾️=4 ❎=5 as globals; Lua 5.4
        // cannot lex the glyphs, so they become numeric literals in code.
        let out = preprocess_pico8("if (btn(\u{2b05}\u{fe0f})) x-=1\n");
        assert_eq!(out.trim(), "if btn(0) then x = x - (1) end", "got: {out:?}");
        let out = preprocess_pico8("if btn(\u{274e}) then x=1 end\n");
        assert_eq!(out.trim(), "if btn(5) then x=1 end", "got: {out:?}");
        // All six, with and without the U+FE0F variation selector.
        let out = preprocess_pico8(
            "t={\u{2b05},\u{27a1}\u{fe0f},\u{2b06},\u{2b07}\u{fe0f},\u{1f17e}\u{fe0f},\u{274e}}\n",
        );
        assert_eq!(out.trim(), "t={0,1,2,3,4,5}", "got: {out:?}");
        // `btnp` and expression positions.
        let out = preprocess_pico8("x = btnp(\u{1f17e}\u{fe0f}) and \u{2b06}\u{fe0f}+1\n");
        assert_eq!(out.trim(), "x = btnp(4) and 2+1", "got: {out:?}");
    }

    #[test]
    fn test_button_glyphs_untouched_in_strings_and_comments() {
        let src = "print(\"\u{2b05}\u{fe0f}\")\n";
        assert_eq!(
            preprocess_pico8(src),
            src,
            "string interior must be untouched"
        );
        let src = "print('\u{274e}') -- press \u{274e}\n";
        assert_eq!(preprocess_pico8(src), src, "string + comment untouched");
        // Code glyph next to a string glyph: only the code one changes.
        let out = preprocess_pico8("if (btn(\u{274e})) print(\"\u{274e}\")\n");
        assert_eq!(
            out.trim(),
            "if btn(5) then print(\"\u{274e}\") end",
            "got: {out:?}"
        );
        // A glyph fused to identifier characters is a different identifier in
        // PICO-8; do not turn it into a number.
        let src = "x\u{2b05}\u{fe0f}=1\n";
        assert_eq!(preprocess_pico8(src), src);
        let src = "\u{274e}x=1\n";
        assert_eq!(preprocess_pico8(src), src);
        // Other non-ASCII code chars are left alone.
        let src = "\u{e9}=1\n";
        assert_eq!(preprocess_pico8(src), src);
    }

    #[test]
    fn test_minus_equals() {
        let out = preprocess_pico8("x-=5\n");
        assert_eq!(out.trim(), "x = x - (5)");
    }

    #[test]
    fn test_mul_equals() {
        let out = preprocess_pico8("x*=0.92\n");
        assert_eq!(out.trim(), "x = x * (0.92)");
    }

    #[test]
    fn test_div_equals() {
        let out = preprocess_pico8("x/=2\n");
        assert_eq!(out.trim(), "x = x / (2)");
    }

    #[test]
    fn test_mod_equals() {
        let out = preprocess_pico8("x%=10\n");
        assert_eq!(out.trim(), "x = x % (10)");
    }

    #[test]
    fn test_compound_assign_with_trailing_semicolon() {
        // Regression: `t /= d;` must not capture the `;` inside the parens
        // (real BBS carts write statements with trailing semicolons).
        let out = preprocess_pico8("t /= d;\n");
        assert_eq!(out.trim(), "t = t / (d);");
    }

    #[test]
    fn test_shorthand_if_with_trailing_comment() {
        // Regression: a `--comment` after the statement must be moved AFTER the
        // synthesized `end`, not left to swallow it.
        let out = preprocess_pico8("if(x)return--bye\n");
        assert_eq!(out.trim(), "if x then return end --bye");
    }

    #[test]
    fn test_concat_equals() {
        let out = preprocess_pico8("s..=\"hello\"\n");
        assert_eq!(out.trim(), "s = s .. (\"hello\")");
    }

    #[test]
    fn test_concat_equals_with_spaces() {
        let out = preprocess_pico8("msg ..= \" world\"\n");
        assert_eq!(out.trim(), "msg = msg .. (\" world\")");
    }

    #[test]
    fn test_compound_assign_table_member() {
        let out = preprocess_pico8("p.x+=mx*p.speed\n");
        assert_eq!(out.trim(), "p.x = p.x + (mx*p.speed)");
    }

    #[test]
    fn test_multiple_compound_on_same_line_in_code() {
        // Realistic: two statements separated by a semicolon-like pattern won't occur,
        // but compound in a for init might. Test simple case.
        let out = preprocess_pico8(" bg_scroll+=spd\n");
        assert_eq!(out.trim(), "bg_scroll = bg_scroll + (spd)");
    }

    // -------------------------------------------------------
    // != -> ~=
    // -------------------------------------------------------

    #[test]
    fn test_not_equal() {
        let out = preprocess_pico8("if a!=b then end\n");
        assert_eq!(out.trim(), "if a~=b then end");
    }

    #[test]
    fn test_not_equal_in_string_preserved() {
        let out = preprocess_pico8("x = \"a != b\"\n");
        assert_eq!(out.trim(), "x = \"a != b\"");
    }

    #[test]
    fn test_not_equal_multiple() {
        let out = preprocess_pico8("if a!=b and c!=d then end\n");
        assert_eq!(out.trim(), "if a~=b and c~=d then end");
    }

    // -------------------------------------------------------
    // Print shorthand: ?expr -> print(expr)
    // -------------------------------------------------------

    #[test]
    fn test_print_shorthand_simple() {
        let out = preprocess_pico8("?\"hello\"\n");
        assert_eq!(out.trim(), "print(\"hello\")");
    }

    #[test]
    fn test_print_shorthand_with_args() {
        let out = preprocess_pico8("? \"hello\", 10, 20\n");
        assert_eq!(out.trim(), "print(\"hello\", 10, 20)");
    }

    #[test]
    fn test_print_shorthand_expression() {
        let out = preprocess_pico8("?x+1\n");
        assert_eq!(out.trim(), "print(x+1)");
    }

    #[test]
    fn test_print_shorthand_with_indent() {
        let out = preprocess_pico8("  ?\"hi\"\n");
        assert_eq!(out.trim(), "print(\"hi\")");
        // Also check the indent is preserved
        let out = preprocess_pico8("  ?\"hi\"\n");
        assert!(out.starts_with("  print("));
    }

    // -------------------------------------------------------
    // Single-line if shorthand
    // -------------------------------------------------------

    #[test]
    fn test_shorthand_if_basic() {
        let out = preprocess_pico8("if (x>0) print(x)\n");
        assert_eq!(out.trim(), "if x>0 then print(x) end");
    }

    #[test]
    fn test_shorthand_if_with_else() {
        let out = preprocess_pico8("if (x>0) x=1 else x=0\n");
        assert_eq!(out.trim(), "if x>0 then x=1 else x=0 end");
    }

    #[test]
    fn test_shorthand_if_nested_parens() {
        let out = preprocess_pico8("if (a and (b or c)) x=1\n");
        assert_eq!(out.trim(), "if a and (b or c) then x=1 end");
    }

    #[test]
    fn test_shorthand_if_deeply_nested_parens() {
        let out = preprocess_pico8("if ((a>0) and (b<10 or (c==5))) do_thing()\n");
        assert_eq!(
            out.trim(),
            "if (a>0) and (b<10 or (c==5)) then do_thing() end"
        );
    }

    #[test]
    fn test_normal_if_not_transformed() {
        let input = "if x>0 then print(x) end\n";
        let out = preprocess_pico8(input);
        assert_eq!(out.trim(), "if x>0 then print(x) end");
    }

    #[test]
    fn test_shorthand_if_with_string_containing_parens() {
        let out = preprocess_pico8("if (x>0) print(\"hello (world)\")\n");
        assert_eq!(out.trim(), "if x>0 then print(\"hello (world)\") end");
    }

    #[test]
    fn test_shorthand_if_preserves_indent() {
        let out = preprocess_pico8("  if (x>0) x=1\n");
        assert!(out.starts_with("  if"));
        assert_eq!(out.trim(), "if x>0 then x=1 end");
    }

    #[test]
    fn test_shorthand_if_no_body_does_not_transform() {
        // `if (cond)` with nothing after close paren should NOT transform
        let out = preprocess_pico8("if (x>0)\n");
        // Should pass through to process_operators, no shorthand transform
        assert_eq!(out.trim(), "if (x>0)");
    }

    // -------------------------------------------------------
    // Backslash line continuation
    // -------------------------------------------------------

    #[test]
    fn test_backslash_continuation() {
        let input = "x = 1 + \\\n2 + 3\n";
        let out = preprocess_pico8(input);
        assert_eq!(out.trim(), "x = 1 + 2 + 3");
    }

    #[test]
    fn test_backslash_continuation_multiple() {
        let input = "x = a + \\\nb + \\\nc\n";
        let out = preprocess_pico8(input);
        assert_eq!(out.trim(), "x = a + b + c");
    }

    #[test]
    fn test_backslash_in_string_not_continuation() {
        let input = "x = \"hello\\\\\"\ny=1\n";
        let out = preprocess_pico8(input);
        // The backslash is inside a string, so lines should NOT be joined
        assert!(out.contains("y=1"));
    }

    #[test]
    fn test_backslash_in_comment_not_continuation() {
        let input = "x=1 -- comment \\\ny=2\n";
        let out = preprocess_pico8(input);
        // Backslash is in a comment, so lines should NOT be joined
        assert!(out.contains("y=2"));
    }

    // -------------------------------------------------------
    // String literal protection
    // -------------------------------------------------------

    #[test]
    fn test_operators_in_double_quoted_string() {
        let out = preprocess_pico8("x = \"a += b != c\"\n");
        assert_eq!(out.trim(), "x = \"a += b != c\"");
    }

    #[test]
    fn test_operators_in_single_quoted_string() {
        let out = preprocess_pico8("x = 'a += b != c'\n");
        assert_eq!(out.trim(), "x = 'a += b != c'");
    }

    #[test]
    fn test_compound_assign_after_string() {
        let out = preprocess_pico8("s = \"hello\" x+=1\n");
        // This line has a string followed by a compound assign
        assert!(out.contains("x = x + (1)"));
        assert!(out.contains("\"hello\""));
    }

    #[test]
    fn test_escaped_quote_in_string() {
        let out = preprocess_pico8("x = \"she said \\\"hi\\\"\" y+=1\n");
        assert!(out.contains("y = y + (1)"));
        assert!(out.contains("\\\"hi\\\""));
    }

    // -------------------------------------------------------
    // Comment preservation
    // -------------------------------------------------------

    #[test]
    fn test_comment_preserved() {
        let out = preprocess_pico8("x=1 -- this is a comment\n");
        assert_eq!(out.trim(), "x=1 -- this is a comment");
    }

    #[test]
    fn test_operators_in_comment_not_transformed() {
        let out = preprocess_pico8("x=1 -- x+=2 != 3\n");
        assert_eq!(out.trim(), "x=1 -- x+=2 != 3");
    }

    #[test]
    fn test_double_dash_is_comment_not_operator() {
        // Ensure -- is treated as comment, not as minus-minus
        let out = preprocess_pico8("-- this is a comment\n");
        assert_eq!(out.trim(), "-- this is a comment");
    }

    // -------------------------------------------------------
    // Floor division // (Lua 5.4 native)
    // -------------------------------------------------------

    #[test]
    fn test_floor_division_passthrough() {
        // In PICO-8's Lua, `//` is a SHORT COMMENT, not floor division
        // (z8lua llex.c: "'/' or '/=' or '//' (short comment)"; integer
        // division is `\`). So `x = a // b` comments out `// b`.
        let out = preprocess_pico8("x = a // b\n");
        assert_eq!(out.trim(), "x = a -- b");
    }

    #[test]
    fn test_floor_division_is_comment() {
        // PICO-8 treats `//` as a short comment (verified vs z8lua). The `// 3`
        // becomes a `-- 3` comment; the following line is unaffected.
        let out = preprocess_pico8("x = 10 // 3\ny=2\n");
        assert!(out.contains("x = 10 -- 3"), "got: {out}");
        assert!(out.contains("y=2"));
    }

    // -------------------------------------------------------
    // Length operator #
    // -------------------------------------------------------

    #[test]
    fn test_length_operator_passthrough() {
        let out = preprocess_pico8("n = #tbl\n");
        assert_eq!(out.trim(), "n = #tbl");
    }

    #[test]
    fn test_length_in_expression() {
        let out = preprocess_pico8("print(127-#sc_str*4,1,7)\n");
        assert_eq!(out.trim(), "print(127-#sc_str*4,1,7)");
    }

    // -------------------------------------------------------
    // Edge cases
    // -------------------------------------------------------

    #[test]
    fn test_empty_line() {
        let out = preprocess_pico8("\n");
        assert_eq!(out, "\n");
    }

    #[test]
    fn test_whitespace_only_line() {
        let out = preprocess_pico8("   \n");
        assert_eq!(out.trim(), "");
    }

    #[test]
    fn test_multiple_transforms_same_line() {
        // A line with != and then a compound assign
        let out = preprocess_pico8("if a!=0 then x+=1 end\n");
        assert!(out.contains("~="));
        assert!(out.contains("x = x + (1)"));
    }

    #[test]
    fn test_no_false_compound_on_comparison() {
        // `<=` should not be confused with a compound assign
        let out = preprocess_pico8("if x<=5 then end\n");
        assert_eq!(out.trim(), "if x<=5 then end");
    }

    #[test]
    fn test_no_false_compound_on_gte() {
        // `>=` should not be confused with a compound assign
        let out = preprocess_pico8("if x>=5 then end\n");
        assert_eq!(out.trim(), "if x>=5 then end");
    }

    #[test]
    fn test_no_false_compound_on_equals() {
        // `==` should pass through
        let out = preprocess_pico8("if x==5 then end\n");
        assert_eq!(out.trim(), "if x==5 then end");
    }

    // -------------------------------------------------------
    // Real-world PICO-8 code snippets
    // -------------------------------------------------------

    #[test]
    fn test_real_world_bg_scroll() {
        let input = " bg_scroll+=spd\n if bg_scroll>=256 then bg_scroll-=256 end\n";
        let out = preprocess_pico8(input);
        assert!(out.contains("bg_scroll = bg_scroll + (spd)"));
        assert!(out.contains("bg_scroll = bg_scroll - (256)"));
    }

    #[test]
    fn test_real_world_particle_friction() {
        let input = "  p.dx*=0.92\n  p.dy*=0.92\n  p.life-=1\n  p.r=p.r*0.95\n";
        let out = preprocess_pico8(input);
        assert!(out.contains("p.dx = p.dx * (0.92)"));
        assert!(out.contains("p.dy = p.dy * (0.92)"));
        assert!(out.contains("p.life = p.life - (1)"));
        // p.r=p.r*0.95 is a regular assign, should not be transformed
        assert!(out.contains("p.r=p.r*0.95"));
    }

    #[test]
    fn test_real_world_score_display() {
        let input = " local sc_str=\"\"..score\n print(sc_str,127-#sc_str*4,1,7)\n";
        let out = preprocess_pico8(input);
        assert!(out.contains("local sc_str=\"\"..score"));
        assert!(out.contains("#sc_str"));
    }

    #[test]
    fn test_real_world_combo_display() {
        let input = "  print(combo..\"x\",56,1,cc)\n";
        let out = preprocess_pico8(input);
        assert!(out.contains("combo..\"x\""));
    }

    #[test]
    fn test_real_world_fire_timer() {
        let input = " if btn(4) and p.fire_timer<=0 then\n  p.fire_timer=p.fire_rate\n end\n";
        let out = preprocess_pico8(input);
        // Should not transform <= into a compound assign
        assert!(out.contains("p.fire_timer<=0"));
    }

    #[test]
    fn test_shorthand_if_with_compound_in_body() {
        let out = preprocess_pico8("if (flash>0) flash-=1\n");
        assert_eq!(out.trim(), "if flash>0 then flash = flash - (1) end");
    }

    #[test]
    fn test_shorthand_if_else_with_assignments() {
        let out = preprocess_pico8("if (x>0) a=1 else a=2\n");
        assert_eq!(out.trim(), "if x>0 then a=1 else a=2 end");
    }

    #[test]
    fn test_then_in_string_does_not_prevent_shorthand_if() {
        // The word "then" appears inside a string, should still trigger shorthand if
        let out = preprocess_pico8("if (x>0) print(\"then what\")\n");
        assert_eq!(out.trim(), "if x>0 then print(\"then what\") end");
    }

    #[test]
    fn test_concat_equals_real_world() {
        let input = "local msg=\"score: \"\nmsg..=score\n?msg\n";
        let out = preprocess_pico8(input);
        assert!(out.contains("msg = msg .. (score)"));
        assert!(out.contains("print(msg)"));
    }

    #[test]
    fn test_backslash_continuation_with_compound() {
        let input = "x = very_long + \\\nother_var\nx+=1\n";
        let out = preprocess_pico8(input);
        assert!(out.contains("x = very_long + other_var"));
        assert!(out.contains("x = x + (1)"));
    }

    #[test]
    fn test_div_equals_not_confused_with_double_slash_comment() {
        // /= is divide-assign; // starts a short comment in PICO-8.
        let out1 = preprocess_pico8("x/=2\n");
        assert_eq!(out1.trim(), "x = x / (2)");

        let out2 = preprocess_pico8("x = y // 2\n");
        assert_eq!(out2.trim(), "x = y -- 2");
    }

    #[test]
    fn test_pico8_cart_header_comment() {
        // `//` starts a PICO-8 short comment; the first `//` converts to `--`
        // and the rest (including the `//` in the URL) becomes comment text.
        let out = preprocess_pico8("pico-8 cartridge // http://www.pico-8.com\n");
        assert_eq!(out.trim(), "pico-8 cartridge -- http://www.pico-8.com");
    }

    #[test]
    fn test_shorthand_if_with_function_call() {
        let out = preprocess_pico8("if (btnp(4) or btnp(5)) reset_game()\n");
        assert_eq!(out.trim(), "if btnp(4) or btnp(5) then reset_game() end");
    }

    #[test]
    fn test_shorthand_if_else_with_function_calls() {
        let out = preprocess_pico8("if (x>0) do_a() else do_b()\n");
        assert_eq!(out.trim(), "if x>0 then do_a() else do_b() end");
    }

    // -------------------------------------------------------
    // Multi-line string literals
    // -------------------------------------------------------

    #[test]
    fn test_multiline_string_basic_passes_through() {
        let input = "x = [[\nhello\nworld\n]]\n";
        let out = preprocess_pico8(input);
        assert!(out.contains("x = [["));
        assert!(out.contains("hello"));
        assert!(out.contains("world"));
        assert!(out.contains("]]"));
    }

    #[test]
    fn test_multiline_string_no_transforms_inside() {
        // Operators like +=, !=, and ?expr inside multi-line strings must NOT be transformed.
        let input = "s = [[\nx += 1\na != b\n?expr\n]]\n";
        let out = preprocess_pico8(input);
        assert!(
            out.contains("x += 1"),
            "'+=' inside multiline string should not be transformed"
        );
        assert!(
            out.contains("a != b"),
            "'!=' inside multiline string should not be transformed"
        );
        assert!(
            out.contains("?expr"),
            "'?expr' inside multiline string should not be transformed"
        );
        // Make sure it does NOT contain the transformed versions
        assert!(
            !out.contains("print(expr)"),
            "?expr should not become print(expr) inside [[]]"
        );
        assert!(!out.contains("~="), "!= should not become ~= inside [[]]");
    }

    #[test]
    fn test_multiline_string_open_close_same_line() {
        let input = "x = [[hello]]\n";
        let out = preprocess_pico8(input);
        assert_eq!(out.trim(), "x = [[hello]]");
    }

    #[test]
    fn test_multiline_string_with_one_equals() {
        let input = "x = [=[\nhello\n]=]\n";
        let out = preprocess_pico8(input);
        assert!(out.contains("[=["));
        assert!(out.contains("hello"));
        assert!(out.contains("]=]"));
    }

    #[test]
    fn test_multiline_string_with_two_equals() {
        let input = "x = [==[\nhello\na += 1\n]==]\n";
        let out = preprocess_pico8(input);
        assert!(out.contains("[==["));
        assert!(
            out.contains("a += 1"),
            "+= inside [==[]==] should not be transformed"
        );
        assert!(out.contains("]==]"));
    }

    #[test]
    fn test_multiline_open_inside_regular_string_not_triggered() {
        // `[[` inside a regular string should NOT start a multi-line block.
        let input = "x = \"[[\"\ny+=1\n";
        let out = preprocess_pico8(input);
        assert!(
            out.contains("\"[[\""),
            "string containing [[ should be preserved"
        );
        assert!(
            out.contains("y = y + (1)"),
            "+= on next line should be transformed"
        );
    }

    #[test]
    fn test_multiline_comment_suppresses_transforms() {
        // --[[ starts a multi-line comment in Lua.
        let input = "--[[\nx += 1\na != b\n]]\ny+=1\n";
        let out = preprocess_pico8(input);
        assert!(
            out.contains("x += 1"),
            "+= inside --[[]] comment should not be transformed"
        );
        assert!(
            out.contains("a != b"),
            "!= inside --[[]] comment should not be transformed"
        );
        // The line after the comment should be processed normally.
        assert!(
            out.contains("y = y + (1)"),
            "+= after comment should be transformed"
        );
    }

    #[test]
    fn test_multiline_comment_same_line() {
        let input = "--[[this is a comment]]\nx+=1\n";
        let out = preprocess_pico8(input);
        assert!(out.contains("--[[this is a comment]]"));
        assert!(out.contains("x = x + (1)"));
    }

    #[test]
    fn test_multiline_string_with_operators_that_look_like_transforms() {
        // Content that looks like various PICO-8 shorthand but is inside a multi-line string.
        let input = "data = [[\nif (x>0) do_thing()\nscore += 100\nresult != nil\n]]\n";
        let out = preprocess_pico8(input);
        // None of these should be transformed
        assert!(
            out.contains("if (x>0) do_thing()"),
            "shorthand if inside [[]] should not be transformed"
        );
        assert!(
            out.contains("score += 100"),
            "+= inside [[]] should not be transformed"
        );
        assert!(
            out.contains("result != nil"),
            "!= inside [[]] should not be transformed"
        );
    }

    #[test]
    fn test_line_after_multiline_string_processed_normally() {
        let input = "s = [[\nhello\n]]\nx+=1\n";
        let out = preprocess_pico8(input);
        assert!(out.contains("hello"));
        assert!(
            out.contains("x = x + (1)"),
            "+= after ]] should be transformed"
        );
    }

    #[test]
    fn test_multiline_string_real_world_data() {
        // Simulating map data or sprite data stored in a multi-line string.
        let input = "map_data = [[\n0000000000000000\n1111111111111111\n2222222222222222\n]]\nfor row in all(parse(map_data)) do\n  count+=1\nend\n";
        let out = preprocess_pico8(input);
        assert!(out.contains("0000000000000000"));
        assert!(out.contains("1111111111111111"));
        assert!(
            out.contains("count = count + (1)"),
            "+= after multiline string should be transformed"
        );
    }

    #[test]
    fn test_multiline_string_nested_brackets() {
        // Brackets inside multi-line strings should not confuse the parser.
        let input = "x = [[\n[1] [2] [3]\n]]\n";
        let out = preprocess_pico8(input);
        assert!(out.contains("[1] [2] [3]"));
        assert!(out.contains("]]"));
    }

    #[test]
    fn test_empty_multiline_string() {
        let input = "x = [[]]\n";
        let out = preprocess_pico8(input);
        assert_eq!(out.trim(), "x = [[]]");
    }

    #[test]
    fn test_multiline_string_code_before_open() {
        // Code before the opening `[[` on a prior line should be processed normally.
        let input = "x+=1\ns=[[\nhello\n]]\n";
        let out = preprocess_pico8(input);
        assert!(
            out.contains("x = x + (1)"),
            "+= before [[ should be transformed"
        );
        assert!(out.contains("s=[["));
        assert!(out.contains("hello"));
    }

    #[test]
    fn test_multiline_string_code_after_close() {
        // Code after the closing `]]` on the same line should be processed normally.
        let input = "s = [[\nhello\n]] x+=1\n";
        let out = preprocess_pico8(input);
        assert!(out.contains("hello"));
        assert!(
            out.contains("x = x + (1)"),
            "+= after ]] on same line should be transformed"
        );
    }

    #[test]
    fn test_multiline_comment_with_equals() {
        // --[=[ ... ]=] multi-line comment with equals signs.
        let input = "--[=[\nx += 1\n]=]\ny+=1\n";
        let out = preprocess_pico8(input);
        assert!(
            out.contains("x += 1"),
            "+= inside --[=[]=] should not be transformed"
        );
        assert!(
            out.contains("y = y + (1)"),
            "+= after ]=] should be transformed"
        );
    }

    #[test]
    fn test_multiline_mismatched_equals_not_closed() {
        // [=[ should not be closed by ]] (wrong number of equals).
        let input = "x = [=[\nhello\n]]\nstill inside\n]=]\ny+=1\n";
        let out = preprocess_pico8(input);
        // ]] should not close [=[ -- "still inside" is still part of the string
        assert!(out.contains("still inside"));
        // y+=1 is after the proper ]=] close, so it should be transformed
        assert!(
            out.contains("y = y + (1)"),
            "+= after ]=] should be transformed"
        );
    }

    // -------------------------------------------------------
    // #include directive tests
    // -------------------------------------------------------

    #[test]
    fn test_include_basic_replaces_line() {
        let resolver = |name: &str| -> Option<String> {
            if name == "utils.lua" {
                Some("function util() end\n".to_string())
            } else {
                None
            }
        };
        let input = "#include utils.lua\nx=1\n";
        let out = preprocess_pico8_with_includes(input, &resolver);
        assert!(
            out.contains("function util() end"),
            "included content should be present"
        );
        assert!(out.contains("x=1"), "code after include should be present");
        assert!(
            !out.contains("#include"),
            "#include directive should be removed"
        );
    }

    #[test]
    fn test_include_resolver_returns_none() {
        let resolver = |_: &str| -> Option<String> { None };
        let input = "#include missing.lua\nx=1\n";
        let out = preprocess_pico8_with_includes(input, &resolver);
        assert!(
            out.contains("-- #include not found: missing.lua"),
            "should emit not-found comment"
        );
        assert!(out.contains("x=1"));
    }

    #[test]
    fn test_include_recursive() {
        let resolver = |name: &str| -> Option<String> {
            match name {
                "a.lua" => Some("#include b.lua\nfunction a() end\n".to_string()),
                "b.lua" => Some("function b() end\n".to_string()),
                _ => None,
            }
        };
        let input = "#include a.lua\n";
        let out = preprocess_pico8_with_includes(input, &resolver);
        assert!(
            out.contains("function a() end"),
            "a.lua content should be present"
        );
        assert!(
            out.contains("function b() end"),
            "b.lua content (included by a.lua) should be present"
        );
    }

    #[test]
    fn test_include_cycle_detection() {
        let resolver = |name: &str| -> Option<String> {
            match name {
                "a.lua" => Some("#include b.lua\nfunction a() end\n".to_string()),
                "b.lua" => Some("#include a.lua\nfunction b() end\n".to_string()),
                _ => None,
            }
        };
        let input = "#include a.lua\n";
        let out = preprocess_pico8_with_includes(input, &resolver);
        assert!(
            out.contains("function a() end"),
            "a.lua content should be present"
        );
        assert!(
            out.contains("function b() end"),
            "b.lua content should be present"
        );
        assert!(
            out.contains("-- #include cycle detected: a.lua"),
            "cycle should be detected"
        );
    }

    #[test]
    fn test_include_depth_limit() {
        // Create a chain that exceeds the depth limit.
        let resolver = |name: &str| -> Option<String> {
            // Each file includes the next: file_0.lua -> file_1.lua -> ...
            if let Some(rest) = name.strip_prefix("file_") {
                if let Some(num_str) = rest.strip_suffix(".lua") {
                    if let Ok(n) = num_str.parse::<usize>() {
                        let next = format!("#include file_{}.lua\n", n + 1);
                        return Some(next);
                    }
                }
            }
            None
        };
        let input = "#include file_0.lua\n";
        let out = preprocess_pico8_with_includes(input, &resolver);
        assert!(
            out.contains("-- #include depth limit exceeded"),
            "depth limit should trigger"
        );
    }

    #[test]
    fn test_include_inside_string_not_processed() {
        let resolver = |_: &str| -> Option<String> { Some("SHOULD NOT APPEAR\n".to_string()) };
        let input = "x = \"#include utils.lua\"\n";
        let out = preprocess_pico8_with_includes(input, &resolver);
        assert!(
            out.contains("\"#include utils.lua\""),
            "include inside string should be preserved"
        );
        assert!(!out.contains("SHOULD NOT APPEAR"));
    }

    #[test]
    fn test_include_inside_multiline_string_not_processed() {
        let resolver = |_: &str| -> Option<String> { Some("SHOULD NOT APPEAR\n".to_string()) };
        let input = "x = [[\n#include utils.lua\n]]\n";
        let out = preprocess_pico8_with_includes(input, &resolver);
        assert!(
            out.contains("#include utils.lua"),
            "include inside multiline string should be preserved"
        );
        assert!(!out.contains("SHOULD NOT APPEAR"));
    }

    #[test]
    fn test_include_inside_comment_not_processed() {
        let resolver = |_: &str| -> Option<String> { Some("SHOULD NOT APPEAR\n".to_string()) };
        let input = "-- #include utils.lua\n";
        let out = preprocess_pico8_with_includes(input, &resolver);
        assert!(
            out.contains("-- #include utils.lua"),
            "include inside comment should be preserved"
        );
        assert!(!out.contains("SHOULD NOT APPEAR"));
    }

    #[test]
    fn test_include_content_is_preprocessed() {
        let resolver = |name: &str| -> Option<String> {
            if name == "ops.lua" {
                Some("x+=1\nif a!=b then end\n".to_string())
            } else {
                None
            }
        };
        let input = "#include ops.lua\n";
        let out = preprocess_pico8_with_includes(input, &resolver);
        assert!(
            out.contains("x = x + (1)"),
            "+= in included file should be transformed"
        );
        assert!(
            out.contains("~="),
            "!= in included file should be transformed to ~="
        );
    }

    #[test]
    fn test_include_with_leading_whitespace() {
        let resolver = |name: &str| -> Option<String> {
            if name == "utils.lua" {
                Some("function util() end\n".to_string())
            } else {
                None
            }
        };
        let input = "  #include utils.lua\n";
        let out = preprocess_pico8_with_includes(input, &resolver);
        assert!(
            out.contains("function util() end"),
            "indented include should be resolved"
        );
    }

    #[test]
    fn test_include_with_quotes() {
        let resolver = |name: &str| -> Option<String> {
            if name == "helpers.lua" {
                Some("function help() end\n".to_string())
            } else {
                None
            }
        };
        let input = "#include \"helpers.lua\"\n";
        let out = preprocess_pico8_with_includes(input, &resolver);
        assert!(
            out.contains("function help() end"),
            "quoted include should be resolved"
        );
    }

    #[test]
    fn test_preprocess_pico8_backward_compatible() {
        // The original function should still work without includes.
        let input = "x+=1\nif a!=b then end\n?\"hello\"\n";
        let out = preprocess_pico8(input);
        assert!(out.contains("x = x + (1)"));
        assert!(out.contains("~="));
        assert!(out.contains("print(\"hello\")"));
    }
}
