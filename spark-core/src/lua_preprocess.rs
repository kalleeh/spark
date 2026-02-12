use alloc::collections::BTreeSet;
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use alloc::format;

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
                    let processed_rest = process_line_normal(rest);
                    output.push_str(&processed_rest);
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
                let processed_before = process_line_normal(before);
                output.push_str(&processed_before);
                // Emit the opening delimiter + content + closing delimiter as-is.
                output.push_str(&from_open[..delim_len]);
                output.push_str(inside_and_close);
                // Process any remainder after the closing delimiter.
                if !rest.is_empty() {
                    let processed_rest = process_line_normal(rest);
                    output.push_str(&processed_rest);
                }
                output.push('\n');
            } else {
                // Multi-line block opens but does not close on this line.
                multiline_depth = Some(eq_count);
                let processed_before = process_line_normal(before);
                output.push_str(&processed_before);
                output.push_str(from_open);
                output.push('\n');
            }
            continue;
        }

        // No multi-line block involvement; process normally.
        let processed = process_line_normal(line);
        output.push_str(&processed);
        output.push('\n');
    }
    output
}

/// Resolve `#include` directives in the source code.
///
/// Scans line-by-line, respecting string literals and comments. When a line
/// (outside strings/comments) matches `#include <filename>`, the resolver is
/// called and the line is replaced with the (recursively preprocessed) content.
fn resolve_includes<F>(
    code: &str,
    resolve_include: &F,
    seen: &mut BTreeSet<String>,
    depth: usize,
) -> String
where
    F: Fn(&str) -> Option<String>,
{
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
                output.push_str(&filename);
                output.push('\n');
                continue;
            }

            if seen.contains(&filename) {
                // Cycle detected; emit a comment.
                output.push_str("-- #include cycle detected: ");
                output.push_str(&filename);
                output.push('\n');
                continue;
            }

            if let Some(contents) = resolve_include(&filename) {
                // Mark as seen for cycle detection.
                seen.insert(filename.clone());
                // Recursively process the included content.
                let processed =
                    preprocess_with_includes_inner(&contents, resolve_include, seen, depth + 1);
                output.push_str(&processed);
                // Remove from seen after processing so the same file can be
                // included from independent branches (but not from itself).
                seen.remove(&filename);
            } else {
                // Resolver returned None; emit a comment.
                output.push_str("-- #include not found: ");
                output.push_str(&filename);
                output.push('\n');
            }
            continue;
        }

        // Regular line; pass through.
        output.push_str(line);
        output.push('\n');
    }

    output
}

/// Parse a `#include` directive from a line, returning the filename if matched.
///
/// Recognized formats:
/// - `#include filename.lua`
/// - `#include "filename.lua"`
/// - `#include <filename.lua>`
/// - Leading whitespace is allowed.
/// - The `#include` must not be inside a string literal or comment on the line.
fn parse_include_directive(line: &str) -> Option<String> {
    let trimmed = line.trim();

    // Quick check: must start with #include
    if !trimmed.starts_with("#include") {
        return None;
    }

    // Verify it's not inside a string or comment by checking that the `#`
    // is at the code level. Walk the line up to the `#` position.
    let hash_pos = line.find('#')?;
    let prefix = &line[..hash_pos];
    if is_inside_string_or_comment(prefix) {
        return None;
    }

    // Extract the part after `#include`
    let after_include = &trimmed["#include".len()..];

    // Must be followed by whitespace
    if after_include.is_empty() || !after_include.starts_with(|c: char| c.is_ascii_whitespace()) {
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

    Some(filename.to_string())
}

/// Check if the end of `prefix` is inside a string literal or after a comment start.
fn is_inside_string_or_comment(prefix: &str) -> bool {
    let mut in_string: Option<char> = None;
    let chars: Vec<char> = prefix.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        let ch = chars[i];

        if let Some(delim) = in_string {
            if ch == '\\' && i + 1 < chars.len() {
                i += 2;
                continue;
            }
            if ch == delim {
                in_string = None;
            }
            i += 1;
            continue;
        }

        if ch == '"' || ch == '\'' {
            in_string = Some(ch);
            i += 1;
            continue;
        }

        if ch == '-' && i + 1 < chars.len() && chars[i + 1] == '-' {
            // Rest is a comment
            return true;
        }

        i += 1;
    }

    in_string.is_some()
}

/// Process a single line with all PICO-8 transforms (no multi-line string state).
fn process_line_normal(line: &str) -> String {
    let trimmed = line.trim();

    // ?expr  ->  print(expr)
    if trimmed.starts_with('?') {
        let indent = &line[..line.len() - line.trim_start().len()];
        let expr = trimmed[1..].trim();
        let mut out = String::new();
        out.push_str(indent);
        out.push_str("print(");
        out.push_str(expr);
        out.push(')');
        return out;
    }

    // Single-line if shorthand: `if (cond) stmt` where there's no `then`
    if let Some(processed) = try_shorthand_if(trimmed, line) {
        // Also run operator transforms on the result (e.g., body may have +=)
        return process_operators(&processed);
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
    let len = bytes.len();
    let mut in_string: Option<u8> = None;
    let mut i = 0;

    while i < len {
        let b = bytes[i];

        // Track regular string state
        if let Some(delim) = in_string {
            if b == b'\\' && i + 1 < len {
                // Escaped character inside string, skip next
                i += 2;
                continue;
            }
            if b == delim {
                in_string = None;
            }
            i += 1;
            continue;
        }

        // Start of a regular string
        if b == b'"' || b == b'\'' {
            in_string = Some(b);
            i += 1;
            continue;
        }

        // Check for comment `--` possibly followed by `[=*[`
        if b == b'-' && i + 1 < len && bytes[i + 1] == b'-' {
            let comment_start = i;
            let after_dashes = i + 2;
            // Check if this is a multi-line comment: --[=*[
            if after_dashes < len && bytes[after_dashes] == b'[' {
                if let Some(eq_count) = try_parse_long_bracket_open(bytes, after_dashes) {
                    // Multi-line comment opening found.
                    // The byte offset is the start of `--` so that the `--` prefix is
                    // included in the "pass-through" portion.
                    return Some((comment_start, eq_count));
                }
            }
            // Regular single-line comment -- rest of line is a comment, no multi-line open.
            return None;
        }

        // Check for long bracket string `[=*[`
        if b == b'[' {
            if let Some(eq_count) = try_parse_long_bracket_open(bytes, i) {
                return Some((i, eq_count));
            }
        }

        i += 1;
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
fn join_backslash_lines(code: &str) -> String {
    let lines: Vec<&str> = code.lines().collect();
    let mut result = String::with_capacity(code.len());
    let mut i = 0;
    while i < lines.len() {
        let mut combined = String::from(lines[i]);
        while i < lines.len() && line_ends_with_continuation(&combined) {
            // Remove the trailing backslash and any whitespace before it
            let trimmed_end = combined.trim_end();
            let new_len = trimmed_end.len() - 1; // remove the '\'
            combined.truncate(new_len);
            let trimmed_len = combined.trim_end().len();
            combined.truncate(trimmed_len);
            i += 1;
            if i < lines.len() {
                combined.push(' ');
                combined.push_str(lines[i].trim_start());
            }
        }
        result.push_str(&combined);
        result.push('\n');
        i += 1;
    }
    result
}

/// Check if a line ends with a `\` continuation character outside of strings and comments.
fn line_ends_with_continuation(line: &str) -> bool {
    let trimmed = line.trim_end();
    if !trimmed.ends_with('\\') {
        return false;
    }

    // Walk the line tracking string/comment state to see if the final `\` is outside strings/comments
    let chars: Vec<char> = trimmed.chars().collect();
    let mut in_string: Option<char> = None;
    let mut last_outside_backslash = false;

    let mut i = 0;
    while i < chars.len() {
        let ch = chars[i];

        if let Some(delim) = in_string {
            if ch == '\\' && i + 1 < chars.len() {
                // Escaped character inside string, skip next
                i += 2;
                continue;
            }
            if ch == delim {
                in_string = None;
            }
            last_outside_backslash = false;
            i += 1;
            continue;
        }

        // Check for comment start
        if ch == '-' && i + 1 < chars.len() && chars[i + 1] == '-' {
            // Rest of line is a comment; backslash in a comment is not continuation
            return false;
        }

        if ch == '"' || ch == '\'' {
            in_string = Some(ch);
            last_outside_backslash = false;
            i += 1;
            continue;
        }

        last_outside_backslash = ch == '\\' && i == chars.len() - 1;
        i += 1;
    }

    last_outside_backslash
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
    if contains_keyword_outside_strings(trimmed, "then") {
        return None;
    }
    // Find matching close paren (string-aware)
    let close_idx = find_matching_paren(after_if, 0)?;
    let cond = &after_if[1..close_idx];
    let rest = after_if[close_idx + 1..].trim();
    if rest.is_empty() {
        return None;
    }

    let indent = &original[..original.len() - original.trim_start().len()];

    // Check if rest contains an `else` keyword outside strings
    if let Some(else_pos) = find_keyword_outside_strings(rest, "else") {
        let stmt1 = rest[..else_pos].trim();
        let after_else = rest[else_pos + 4..].trim();
        if !stmt1.is_empty() && !after_else.is_empty() {
            return Some(format!("{indent}if {cond} then {stmt1} else {after_else} end"));
        }
    }

    Some(format!("{indent}if {cond} then {rest} end"))
}

/// Find the matching close parenthesis starting from `start` in `s`.
/// `s[start]` must be `(`. Returns the index of the matching `)`.
/// String-literal-aware.
fn find_matching_paren(s: &str, start: usize) -> Option<usize> {
    let chars: Vec<char> = s.chars().collect();
    let mut depth = 0;
    let mut in_string: Option<char> = None;
    let mut i = start;
    while i < chars.len() {
        let ch = chars[i];
        if let Some(delim) = in_string {
            if ch == '\\' && i + 1 < chars.len() {
                i += 2;
                continue;
            }
            if ch == delim {
                in_string = None;
            }
            i += 1;
            continue;
        }
        if ch == '"' || ch == '\'' {
            in_string = Some(ch);
            i += 1;
            continue;
        }
        if ch == '(' {
            depth += 1;
        } else if ch == ')' {
            depth -= 1;
            if depth == 0 {
                return Some(i);
            }
        }
        i += 1;
    }
    None
}

/// Check if `line` contains the keyword `kw` outside of string literals.
/// A keyword must be bounded by non-alphanumeric/non-underscore characters.
fn contains_keyword_outside_strings(line: &str, kw: &str) -> bool {
    find_keyword_outside_strings(line, kw).is_some()
}

/// Find the byte position of keyword `kw` outside of string literals.
/// A keyword must be bounded by non-identifier characters (or line start/end).
fn find_keyword_outside_strings(line: &str, kw: &str) -> Option<usize> {
    let chars: Vec<char> = line.chars().collect();
    let kw_chars: Vec<char> = kw.chars().collect();
    let mut in_string: Option<char> = None;
    let mut i = 0;
    // We also track byte offsets for returning positions
    let mut byte_offset = 0;

    while i < chars.len() {
        let ch = chars[i];
        if let Some(delim) = in_string {
            if ch == '\\' && i + 1 < chars.len() {
                byte_offset += ch.len_utf8() + chars[i + 1].len_utf8();
                i += 2;
                continue;
            }
            if ch == delim {
                in_string = None;
            }
            byte_offset += ch.len_utf8();
            i += 1;
            continue;
        }
        if ch == '"' || ch == '\'' {
            in_string = Some(ch);
            byte_offset += ch.len_utf8();
            i += 1;
            continue;
        }
        // Check for comment
        if ch == '-' && i + 1 < chars.len() && chars[i + 1] == '-' {
            return None; // rest is comment
        }
        // Check for keyword match
        if i + kw_chars.len() <= chars.len() && chars[i..i + kw_chars.len()] == kw_chars[..] {
            // Check left boundary
            let left_ok = if i == 0 {
                true
            } else {
                let prev = chars[i - 1];
                !prev.is_ascii_alphanumeric() && prev != '_'
            };
            // Check right boundary
            let right_ok = if i + kw_chars.len() >= chars.len() {
                true
            } else {
                let next = chars[i + kw_chars.len()];
                !next.is_ascii_alphanumeric() && next != '_'
            };
            if left_ok && right_ok {
                return Some(byte_offset);
            }
        }
        byte_offset += ch.len_utf8();
        i += 1;
    }
    None
}

/// Replace compound assignments (+=, -=, *=, /=, %=, ..=) and != with standard Lua.
fn process_operators(line: &str) -> String {
    // Fast path: if the line has no `=` or `!`, no operators can match.
    if !line.contains('=') && !line.contains('!') {
        return line.to_string();
    }

    let mut result = line.to_string();

    // Replace != with ~= (but not inside strings)
    result = replace_outside_strings(&result, "!=", "~=");

    // Compound assignments: var op= expr  ->  var = var op (expr)
    // Important: ..= must come before the others to avoid partial matches
    for (op, lua_op) in &[
        ("..=", ".."),
        ("+=", "+"),
        ("-=", "-"),
        ("*=", "*"),
        ("/=", "/"),
        ("%=", "%"),
    ] {
        if let Some(transformed) = try_compound_assign(&result, op, lua_op) {
            result = transformed;
        }
    }

    result
}

/// Replace occurrences of `pattern` with `replacement`, but only outside of string literals
/// and comments.
fn replace_outside_strings(line: &str, pattern: &str, replacement: &str) -> String {
    let mut result = String::with_capacity(line.len());
    let mut in_string: Option<char> = None;
    let chars: Vec<char> = line.chars().collect();
    let pat_chars: Vec<char> = pattern.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        if let Some(delim) = in_string {
            result.push(chars[i]);
            if chars[i] == '\\' && i + 1 < chars.len() {
                // Escaped character inside string - push next char and skip
                i += 1;
                result.push(chars[i]);
                i += 1;
                continue;
            }
            if chars[i] == delim {
                in_string = None;
            }
            i += 1;
        } else if chars[i] == '"' || chars[i] == '\'' {
            in_string = Some(chars[i]);
            result.push(chars[i]);
            i += 1;
        } else if chars[i] == '-' && i + 1 < chars.len() && chars[i + 1] == '-' {
            // Rest of line is a comment
            for j in i..chars.len() {
                result.push(chars[j]);
            }
            return result;
        } else if i + pat_chars.len() <= chars.len()
            && chars[i..i + pat_chars.len()] == pat_chars[..]
        {
            result.push_str(replacement);
            i += pat_chars.len();
        } else {
            result.push(chars[i]);
            i += 1;
        }
    }
    result
}

/// Replace ALL compound assignments on a line.
fn try_compound_assign(line: &str, op: &str, lua_op: &str) -> Option<String> {
    if !line.contains(op) {
        return None;
    }

    let chars: Vec<char> = line.chars().collect();
    let op_chars: Vec<char> = op.chars().collect();
    let mut in_string: Option<char> = None;
    let mut result = String::new();
    let mut i = 0;
    let mut changed = false;

    while i < chars.len() {
        // Track string state
        if let Some(delim) = in_string {
            result.push(chars[i]);
            if chars[i] == '\\' && i + 1 < chars.len() {
                // Escaped character in string
                i += 1;
                result.push(chars[i]);
                i += 1;
                continue;
            }
            if chars[i] == delim {
                in_string = None;
            }
            i += 1;
            continue;
        }
        if chars[i] == '"' || chars[i] == '\'' {
            in_string = Some(chars[i]);
            result.push(chars[i]);
            i += 1;
            continue;
        }
        if chars[i] == '-' && i + 1 < chars.len() && chars[i + 1] == '-' {
            // Rest is a comment, emit as-is
            while i < chars.len() {
                result.push(chars[i]);
                i += 1;
            }
            break;
        }

        // Check for compound operator match
        if i + op_chars.len() <= chars.len()
            && chars[i..i + op_chars.len()] == op_chars[..]
        {
            if let Some((var, prefix_end)) = extract_lvalue_backwards(&result) {
                let expr_start = i + op_chars.len();
                let (expr, expr_end) = extract_rhs_expr(&chars, expr_start);

                if !var.is_empty() && !expr.is_empty() {
                    result.truncate(prefix_end);
                    result.push_str(&format!("{var} = {var} {lua_op} ({expr})"));
                    i = expr_end;
                    changed = true;
                    continue;
                }
            }
        }

        result.push(chars[i]);
        i += 1;
    }

    if changed { Some(result) } else { None }
}

/// Look backwards from the end of `built` to extract an l-value (variable name).
/// Returns (lvalue_string, index_in_built_where_lvalue_starts).
fn extract_lvalue_backwards(built: &str) -> Option<(String, usize)> {
    let chars: Vec<char> = built.chars().collect();
    let mut end = chars.len();

    // Skip trailing whitespace
    while end > 0 && (chars[end - 1] == ' ' || chars[end - 1] == '\t') {
        end -= 1;
    }
    if end == 0 {
        return None;
    }

    // Scan backwards through the l-value
    let mut start = end;
    let mut bracket_depth = 0i32;
    while start > 0 {
        let ch = chars[start - 1];
        if ch == ']' {
            bracket_depth += 1;
            start -= 1;
        } else if ch == '[' {
            if bracket_depth > 0 {
                bracket_depth -= 1;
                start -= 1;
            } else {
                break;
            }
        } else if bracket_depth > 0 {
            start -= 1;
        } else if ch.is_ascii_alphanumeric() || ch == '_' || ch == '.' || ch == ':' {
            start -= 1;
        } else {
            break;
        }
    }

    if start == end {
        return None;
    }

    let var: String = chars[start..end].iter().collect();

    let first = var.chars().next()?;
    if !first.is_ascii_alphabetic() && first != '_' {
        return None;
    }

    Some((var, start))
}

/// Check if position `i` in `chars` is at a keyword boundary that ends a
/// statement: `end`, `else`, or `elseif`, preceded by whitespace (or at the start).
fn starts_with_keyword_boundary(chars: &[char], i: usize) -> bool {
    let remaining = &chars[i..];
    let preceded_by_ws = i == 0 || chars[i - 1] == ' ' || chars[i - 1] == '\t';

    // Check for " end", " else", " elseif", "\tend", "\telse", "\telseif"
    for kw in &["end", "else", "elseif"] {
        let kw_chars: &[char] = match *kw {
            "end" => &['e', 'n', 'd'],
            "else" => &['e', 'l', 's', 'e'],
            "elseif" => &['e', 'l', 's', 'e', 'i', 'f'],
            _ => continue,
        };

        // Match with leading whitespace
        if remaining.len() > kw_chars.len()
            && (remaining[0] == ' ' || remaining[0] == '\t')
            && remaining[1..].starts_with(kw_chars)
        {
            return true;
        }

        // Match at position if preceded by whitespace
        if preceded_by_ws && remaining.starts_with(kw_chars) {
            return true;
        }
    }

    false
}

/// Extract the right-hand-side expression after a compound operator.
/// Stops at: `end`, `else`, `elseif`, `)`, end of line, or comment.
/// Returns (expression_string, index_past_expression).
fn extract_rhs_expr(chars: &[char], start: usize) -> (String, usize) {
    let mut i = start;
    // Skip leading whitespace
    while i < chars.len() && (chars[i] == ' ' || chars[i] == '\t') {
        i += 1;
    }

    let expr_start = i;
    let mut paren_depth = 0i32;
    let mut in_string: Option<char> = None;

    while i < chars.len() {
        if let Some(delim) = in_string {
            if chars[i] == '\\' && i + 1 < chars.len() {
                // Escaped character in string
                i += 2;
                continue;
            }
            if chars[i] == delim {
                in_string = None;
            }
            i += 1;
            continue;
        }
        if chars[i] == '"' || chars[i] == '\'' {
            in_string = Some(chars[i]);
            i += 1;
            continue;
        }
        if chars[i] == '-' && i + 1 < chars.len() && chars[i + 1] == '-' {
            break; // comment
        }
        if chars[i] == '(' {
            paren_depth += 1;
        } else if chars[i] == ')' {
            if paren_depth > 0 {
                paren_depth -= 1;
            } else {
                break;
            }
        }

        // Check for Lua keywords that end a statement (only at paren_depth 0)
        if paren_depth == 0 && starts_with_keyword_boundary(chars, i) {
            let expr: String = chars[expr_start..i].iter().collect();
            return (expr.trim().to_string(), i);
        }

        i += 1;
    }

    let expr: String = chars[expr_start..i].iter().collect();
    (expr.trim().to_string(), i)
}

#[cfg(test)]
mod tests {
    use super::*;

    // -------------------------------------------------------
    // Compound assignment operators
    // -------------------------------------------------------

    #[test]
    fn test_plus_equals() {
        let out = preprocess_pico8("x+=1\n");
        assert_eq!(out.trim(), "x = x + (1)");
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
        assert_eq!(out.trim(), "if (a>0) and (b<10 or (c==5)) then do_thing() end");
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
        // `//` is valid Lua 5.4 floor division; ensure it's not mangled
        let out = preprocess_pico8("x = a // b\n");
        assert_eq!(out.trim(), "x = a // b");
    }

    #[test]
    fn test_floor_division_not_comment() {
        // // should NOT be treated as a comment (PICO-8 uses -- for comments)
        let out = preprocess_pico8("x = 10 // 3\ny=2\n");
        assert!(out.contains("x = 10 // 3"));
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
    fn test_div_equals_not_confused_with_floor_div() {
        // /= is divide-assign, // is floor division
        let out1 = preprocess_pico8("x/=2\n");
        assert_eq!(out1.trim(), "x = x / (2)");

        let out2 = preprocess_pico8("x = y // 2\n");
        assert_eq!(out2.trim(), "x = y // 2");
    }

    #[test]
    fn test_pico8_cart_header_comment() {
        // The first line of a .p8 cart often has `//` in a comment context,
        // but it's actually just text. Ensure it passes through.
        let out = preprocess_pico8("pico-8 cartridge // http://www.pico-8.com\n");
        assert_eq!(out.trim(), "pico-8 cartridge // http://www.pico-8.com");
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
        assert!(out.contains("x += 1"), "'+=' inside multiline string should not be transformed");
        assert!(out.contains("a != b"), "'!=' inside multiline string should not be transformed");
        assert!(out.contains("?expr"), "'?expr' inside multiline string should not be transformed");
        // Make sure it does NOT contain the transformed versions
        assert!(!out.contains("print(expr)"), "?expr should not become print(expr) inside [[]]");
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
        assert!(out.contains("a += 1"), "+= inside [==[]==] should not be transformed");
        assert!(out.contains("]==]"));
    }

    #[test]
    fn test_multiline_open_inside_regular_string_not_triggered() {
        // `[[` inside a regular string should NOT start a multi-line block.
        let input = "x = \"[[\"\ny+=1\n";
        let out = preprocess_pico8(input);
        assert!(out.contains("\"[[\""), "string containing [[ should be preserved");
        assert!(out.contains("y = y + (1)"), "+= on next line should be transformed");
    }

    #[test]
    fn test_multiline_comment_suppresses_transforms() {
        // --[[ starts a multi-line comment in Lua.
        let input = "--[[\nx += 1\na != b\n]]\ny+=1\n";
        let out = preprocess_pico8(input);
        assert!(out.contains("x += 1"), "+= inside --[[]] comment should not be transformed");
        assert!(out.contains("a != b"), "!= inside --[[]] comment should not be transformed");
        // The line after the comment should be processed normally.
        assert!(out.contains("y = y + (1)"), "+= after comment should be transformed");
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
        assert!(out.contains("if (x>0) do_thing()"), "shorthand if inside [[]] should not be transformed");
        assert!(out.contains("score += 100"), "+= inside [[]] should not be transformed");
        assert!(out.contains("result != nil"), "!= inside [[]] should not be transformed");
    }

    #[test]
    fn test_line_after_multiline_string_processed_normally() {
        let input = "s = [[\nhello\n]]\nx+=1\n";
        let out = preprocess_pico8(input);
        assert!(out.contains("hello"));
        assert!(out.contains("x = x + (1)"), "+= after ]] should be transformed");
    }

    #[test]
    fn test_multiline_string_real_world_data() {
        // Simulating map data or sprite data stored in a multi-line string.
        let input = "map_data = [[\n0000000000000000\n1111111111111111\n2222222222222222\n]]\nfor row in all(parse(map_data)) do\n  count+=1\nend\n";
        let out = preprocess_pico8(input);
        assert!(out.contains("0000000000000000"));
        assert!(out.contains("1111111111111111"));
        assert!(out.contains("count = count + (1)"), "+= after multiline string should be transformed");
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
        assert!(out.contains("x = x + (1)"), "+= before [[ should be transformed");
        assert!(out.contains("s=[["));
        assert!(out.contains("hello"));
    }

    #[test]
    fn test_multiline_string_code_after_close() {
        // Code after the closing `]]` on the same line should be processed normally.
        let input = "s = [[\nhello\n]] x+=1\n";
        let out = preprocess_pico8(input);
        assert!(out.contains("hello"));
        assert!(out.contains("x = x + (1)"), "+= after ]] on same line should be transformed");
    }

    #[test]
    fn test_multiline_comment_with_equals() {
        // --[=[ ... ]=] multi-line comment with equals signs.
        let input = "--[=[\nx += 1\n]=]\ny+=1\n";
        let out = preprocess_pico8(input);
        assert!(out.contains("x += 1"), "+= inside --[=[]=] should not be transformed");
        assert!(out.contains("y = y + (1)"), "+= after ]=] should be transformed");
    }

    #[test]
    fn test_multiline_mismatched_equals_not_closed() {
        // [=[ should not be closed by ]] (wrong number of equals).
        let input = "x = [=[\nhello\n]]\nstill inside\n]=]\ny+=1\n";
        let out = preprocess_pico8(input);
        // ]] should not close [=[ -- "still inside" is still part of the string
        assert!(out.contains("still inside"));
        // y+=1 is after the proper ]=] close, so it should be transformed
        assert!(out.contains("y = y + (1)"), "+= after ]=] should be transformed");
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
        assert!(out.contains("function util() end"), "included content should be present");
        assert!(out.contains("x=1"), "code after include should be present");
        assert!(!out.contains("#include"), "#include directive should be removed");
    }

    #[test]
    fn test_include_resolver_returns_none() {
        let resolver = |_: &str| -> Option<String> { None };
        let input = "#include missing.lua\nx=1\n";
        let out = preprocess_pico8_with_includes(input, &resolver);
        assert!(out.contains("-- #include not found: missing.lua"), "should emit not-found comment");
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
        assert!(out.contains("function a() end"), "a.lua content should be present");
        assert!(out.contains("function b() end"), "b.lua content (included by a.lua) should be present");
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
        assert!(out.contains("function a() end"), "a.lua content should be present");
        assert!(out.contains("function b() end"), "b.lua content should be present");
        assert!(out.contains("-- #include cycle detected: a.lua"), "cycle should be detected");
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
        assert!(out.contains("-- #include depth limit exceeded"), "depth limit should trigger");
    }

    #[test]
    fn test_include_inside_string_not_processed() {
        let resolver = |_: &str| -> Option<String> {
            Some("SHOULD NOT APPEAR\n".to_string())
        };
        let input = "x = \"#include utils.lua\"\n";
        let out = preprocess_pico8_with_includes(input, &resolver);
        assert!(out.contains("\"#include utils.lua\""), "include inside string should be preserved");
        assert!(!out.contains("SHOULD NOT APPEAR"));
    }

    #[test]
    fn test_include_inside_multiline_string_not_processed() {
        let resolver = |_: &str| -> Option<String> {
            Some("SHOULD NOT APPEAR\n".to_string())
        };
        let input = "x = [[\n#include utils.lua\n]]\n";
        let out = preprocess_pico8_with_includes(input, &resolver);
        assert!(out.contains("#include utils.lua"), "include inside multiline string should be preserved");
        assert!(!out.contains("SHOULD NOT APPEAR"));
    }

    #[test]
    fn test_include_inside_comment_not_processed() {
        let resolver = |_: &str| -> Option<String> {
            Some("SHOULD NOT APPEAR\n".to_string())
        };
        let input = "-- #include utils.lua\n";
        let out = preprocess_pico8_with_includes(input, &resolver);
        assert!(out.contains("-- #include utils.lua"), "include inside comment should be preserved");
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
        assert!(out.contains("x = x + (1)"), "+= in included file should be transformed");
        assert!(out.contains("~="), "!= in included file should be transformed to ~=");
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
        assert!(out.contains("function util() end"), "indented include should be resolved");
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
        assert!(out.contains("function help() end"), "quoted include should be resolved");
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
