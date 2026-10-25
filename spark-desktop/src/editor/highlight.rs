//! Lua syntax highlighting tokenizer and PICO-8 token counting.

// ---------------------------------------------------------------------------
// Syntax highlighting
// ---------------------------------------------------------------------------

/// Token categories for syntax highlighting.
#[derive(Debug, PartialEq, Clone, Copy)]
pub(super) enum TokenKind {
    Default,   // color 6 (light gray)
    Comment,   // color 13 (dark gray/blue)
    StringLit, // color 4 (brown)
    Number,    // color 14 (pink/mauve)
    Keyword,   // color 12 (light blue)
    ApiFunc,   // color 9 (orange)
}

/// A span of characters on one line sharing the same highlight color.
#[derive(Debug, PartialEq)]
pub(super) struct Token {
    pub(super) kind: TokenKind,
    pub(super) start: usize,
    pub(super) end: usize, // exclusive byte offset within the line
}

/// Lua keywords recognized by the highlighter.
const LUA_KEYWORDS: &[&str] = &[
    "if", "then", "else", "elseif", "end", "for", "while", "do", "repeat", "until", "function",
    "return", "local", "and", "or", "not", "in", "true", "false", "nil", "break", "goto",
];

/// PICO-8 API function names recognized by the highlighter.
const PICO8_API: &[&str] = &[
    "cls", "pset", "print", "spr", "map", "sfx", "music", "btn", "btnp", "rnd", "sin", "cos",
    "atan2", "sqrt", "abs", "flr", "ceil", "min", "max", "mid", "sgn", "band", "bor", "bxor",
    "bnot", "shl", "shr", "sub", "add", "del", "count", "foreach", "all", "tostr", "tonum", "peek",
    "poke", "memcpy", "memset", "stat", "camera", "clip", "pal", "palt", "color", "circ",
    "circfill", "rect", "rectfill", "line", "sget", "sset", "fget", "fset", "mget", "mset", "sspr",
];

/// Return true if `c` can appear in a Lua identifier.
fn is_ident_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

/// Tokenize a single line for syntax highlighting.
/// Returns a list of non-overlapping, contiguous tokens covering the line.
pub(super) fn tokenize_line(line: &str) -> Vec<Token> {
    let bytes = line.as_bytes();
    let len = bytes.len();
    let mut tokens: Vec<Token> = Vec::new();
    let mut i: usize = 0;

    while i < len {
        // --- Comment: -- to end of line ---
        if i + 1 < len && bytes[i] == b'-' && bytes[i + 1] == b'-' {
            tokens.push(Token {
                kind: TokenKind::Comment,
                start: i,
                end: len,
            });
            i = len;
            continue;
        }

        // --- String literal: "..." or '...' ---
        if bytes[i] == b'"' || bytes[i] == b'\'' {
            let quote = bytes[i];
            let start = i;
            i += 1;
            while i < len && bytes[i] != quote {
                if bytes[i] == b'\\' && i + 1 < len {
                    i += 1; // skip escaped char
                }
                i += 1;
            }
            if i < len {
                i += 1; // skip closing quote
            }
            tokens.push(Token {
                kind: TokenKind::StringLit,
                start,
                end: i,
            });
            continue;
        }

        // --- Number: digit sequences, hex 0x... ---
        if bytes[i].is_ascii_digit()
            || (bytes[i] == b'.' && i + 1 < len && bytes[i + 1].is_ascii_digit())
        {
            let start = i;
            if bytes[i] == b'0' && i + 1 < len && (bytes[i + 1] == b'x' || bytes[i + 1] == b'X') {
                // hex number
                i += 2;
                while i < len && bytes[i].is_ascii_hexdigit() {
                    i += 1;
                }
            } else {
                // decimal, possibly with dot
                while i < len && (bytes[i].is_ascii_digit() || bytes[i] == b'.') {
                    i += 1;
                }
            }
            tokens.push(Token {
                kind: TokenKind::Number,
                start,
                end: i,
            });
            continue;
        }

        // --- Identifier (keyword / API / default) ---
        if bytes[i].is_ascii_alphabetic() || bytes[i] == b'_' {
            let start = i;
            while i < len && is_ident_char(bytes[i] as char) {
                i += 1;
            }
            let word = &line[start..i];
            let kind = if LUA_KEYWORDS.contains(&word) {
                TokenKind::Keyword
            } else if PICO8_API.contains(&word) {
                TokenKind::ApiFunc
            } else {
                TokenKind::Default
            };
            tokens.push(Token {
                kind,
                start,
                end: i,
            });
            continue;
        }

        // --- Default: single character (punctuation, whitespace, etc.) ---
        let start = i;
        i += 1;
        // Merge consecutive default characters
        while i < len
            && bytes[i] != b'-'
            && bytes[i] != b'"'
            && bytes[i] != b'\''
            && !bytes[i].is_ascii_alphanumeric()
            && bytes[i] != b'_'
            && !(bytes[i] == b'.' && i + 1 < len && bytes[i + 1].is_ascii_digit())
        {
            i += 1;
        }
        tokens.push(Token {
            kind: TokenKind::Default,
            start,
            end: i,
        });
    }

    tokens
}

/// Map a TokenKind to a PICO-8 color index.
pub(super) fn token_color(kind: TokenKind) -> u8 {
    match kind {
        TokenKind::Default => 6,
        TokenKind::Comment => 13,
        TokenKind::StringLit => 4,
        TokenKind::Number => 14,
        TokenKind::Keyword => 12,
        TokenKind::ApiFunc => 9,
    }
}

// ---------------------------------------------------------------------------
// Token counting
// ---------------------------------------------------------------------------

/// Keywords and operators that count as free punctuation in PICO-8 (0 tokens).
/// Parentheses, commas, semicolons, and dots are free.
const FREE_PUNCTUATION: &[u8] = b"(),.;:[]{}";

/// Approximate the PICO-8 token count for `code`.
/// This uses the syntax highlighting tokenizer per line and counts tokens
/// according to PICO-8 rules:
/// - Comments and whitespace = 0 tokens
/// - Each keyword, identifier, number, string literal = 1 token
/// - Each operator = 1 token
/// - Free punctuation (parentheses, commas, semicolons, colons, dots, brackets) = 0 tokens
pub(super) fn count_tokens(code: &str) -> usize {
    let mut count: usize = 0;
    for line in code.split('\n') {
        let tokens = tokenize_line(line);
        for tok in &tokens {
            match tok.kind {
                TokenKind::Comment => {} // free
                TokenKind::Keyword
                | TokenKind::ApiFunc
                | TokenKind::Number
                | TokenKind::StringLit => {
                    count += 1;
                }
                TokenKind::Default => {
                    // Count identifiers and operators within Default tokens.
                    // Whitespace and free punctuation are free.
                    let text = &line[tok.start..tok.end];
                    for ch in text.chars() {
                        if ch.is_whitespace() {
                            continue;
                        }
                        if FREE_PUNCTUATION.contains(&(ch as u8)) {
                            continue;
                        }
                        // Operators and other non-free punctuation each cost 1 token
                        if !ch.is_ascii_alphanumeric() && ch != '_' {
                            count += 1;
                            continue;
                        }
                        // If we encounter an alphanumeric char in a Default token,
                        // it's an identifier (already counted as one Default span).
                        // We count the whole span as 1 token and break.
                        count += 1;
                        break;
                    }
                }
            }
        }
    }
    count
}

#[cfg(test)]
mod tests {
    use super::*;

    // ===================================================================
    // Syntax highlighting tokenizer
    // ===================================================================

    #[test]
    fn tokenize_comment() {
        let tokens = tokenize_line("-- this is a comment");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Comment);
        assert_eq!(tokens[0].start, 0);
        assert_eq!(tokens[0].end, 20);
    }

    #[test]
    fn tokenize_inline_comment() {
        let tokens = tokenize_line("x=1 -- comment");
        // Should have tokens for: x, =, 1, space, comment
        let comment = tokens.iter().find(|t| t.kind == TokenKind::Comment);
        assert!(comment.is_some());
        let c = comment.unwrap();
        assert_eq!(&"x=1 -- comment"[c.start..c.end], "-- comment");
    }

    #[test]
    fn tokenize_string_double_quotes() {
        let tokens = tokenize_line("\"hello world\"");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::StringLit);
    }

    #[test]
    fn tokenize_string_single_quotes() {
        let tokens = tokenize_line("'test'");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::StringLit);
    }

    #[test]
    fn tokenize_string_with_escape() {
        let tokens = tokenize_line("\"he\\\"llo\"");
        // Should be a single string token
        let strings: Vec<_> = tokens
            .iter()
            .filter(|t| t.kind == TokenKind::StringLit)
            .collect();
        assert_eq!(strings.len(), 1);
    }

    #[test]
    fn tokenize_number_decimal() {
        let tokens = tokenize_line("42");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Number);
    }

    #[test]
    fn tokenize_number_hex() {
        let tokens = tokenize_line("0xFF");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Number);
    }

    #[test]
    fn tokenize_number_with_decimal_point() {
        let tokens = tokenize_line("3.14");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Number);
    }

    #[test]
    fn tokenize_keyword_if() {
        let tokens = tokenize_line("if x then");
        let kw: Vec<_> = tokens
            .iter()
            .filter(|t| t.kind == TokenKind::Keyword)
            .collect();
        assert_eq!(kw.len(), 2); // "if" and "then"
    }

    #[test]
    fn tokenize_keyword_function() {
        let tokens = tokenize_line("function foo()");
        let kw: Vec<_> = tokens
            .iter()
            .filter(|t| t.kind == TokenKind::Keyword)
            .collect();
        assert_eq!(kw.len(), 1);
        assert_eq!(&"function foo()"[kw[0].start..kw[0].end], "function");
    }

    #[test]
    fn tokenize_all_keyword_types() {
        for kw in LUA_KEYWORDS {
            let tokens = tokenize_line(kw);
            assert_eq!(
                tokens[0].kind,
                TokenKind::Keyword,
                "Expected keyword for: {}",
                kw
            );
        }
    }

    #[test]
    fn tokenize_api_function() {
        let tokens = tokenize_line("cls(1)");
        let api: Vec<_> = tokens
            .iter()
            .filter(|t| t.kind == TokenKind::ApiFunc)
            .collect();
        assert_eq!(api.len(), 1);
        assert_eq!(&"cls(1)"[api[0].start..api[0].end], "cls");
    }

    #[test]
    fn tokenize_api_pset() {
        let tokens = tokenize_line("pset(10,20,7)");
        let api: Vec<_> = tokens
            .iter()
            .filter(|t| t.kind == TokenKind::ApiFunc)
            .collect();
        assert_eq!(api.len(), 1);
        assert_eq!(&"pset(10,20,7)"[api[0].start..api[0].end], "pset");
    }

    #[test]
    fn tokenize_identifier_not_keyword() {
        let tokens = tokenize_line("foobar");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Default);
    }

    #[test]
    fn tokenize_empty_line() {
        let tokens = tokenize_line("");
        assert_eq!(tokens.len(), 0);
    }

    #[test]
    fn tokenize_mixed_line() {
        // "local x = 42 -- init"
        let line = "local x = 42 -- init";
        let tokens = tokenize_line(line);
        let kinds: Vec<TokenKind> = tokens.iter().map(|t| t.kind).collect();
        // Expected: Keyword("local"), Default(" "), Default("x"), Default(" = "),
        //           Number("42"), Default(" "), Comment("-- init")
        assert!(kinds.contains(&TokenKind::Keyword));
        assert!(kinds.contains(&TokenKind::Number));
        assert!(kinds.contains(&TokenKind::Comment));
    }

    #[test]
    fn tokenize_contiguous_coverage() {
        // Ensure tokens cover the entire line without gaps
        let line = "if x>0 then cls(1) end -- ok";
        let tokens = tokenize_line(line);
        assert!(!tokens.is_empty());
        assert_eq!(tokens[0].start, 0);
        for i in 1..tokens.len() {
            assert_eq!(
                tokens[i].start,
                tokens[i - 1].end,
                "Gap between token {} and {} at positions {}-{}",
                i - 1,
                i,
                tokens[i - 1].end,
                tokens[i].start
            );
        }
        assert_eq!(tokens.last().unwrap().end, line.len());
    }

    #[test]
    fn token_color_mapping() {
        assert_eq!(token_color(TokenKind::Default), 6);
        assert_eq!(token_color(TokenKind::Comment), 13);
        assert_eq!(token_color(TokenKind::StringLit), 4);
        assert_eq!(token_color(TokenKind::Number), 14);
        assert_eq!(token_color(TokenKind::Keyword), 12);
        assert_eq!(token_color(TokenKind::ApiFunc), 9);
    }

    // ===================================================================
    // Token count
    // ===================================================================

    #[test]
    fn token_count_simple_code() {
        // "x = 1" => x(1) =(1) 1(1) = 3 tokens
        let count = count_tokens("x = 1");
        assert_eq!(count, 3);
    }

    #[test]
    fn token_count_with_comment() {
        // Comments should not count as tokens
        let count = count_tokens("x = 1 -- set x");
        assert_eq!(count, 3); // same as without comment
    }

    #[test]
    fn token_count_with_keywords() {
        // "if x then" => if(1) x(1) then(1) = 3 tokens
        let count = count_tokens("if x then");
        assert_eq!(count, 3);
    }

    #[test]
    fn token_count_with_string() {
        // '"hello"' => 1 string token
        let count = count_tokens("\"hello\"");
        assert_eq!(count, 1);
    }

    #[test]
    fn token_count_free_punctuation() {
        // "f(x,y)" => f(1) ( free ) x(1) , free  y(1) ) free = 3 tokens
        let count = count_tokens("f(x,y)");
        assert_eq!(count, 3);
    }

    #[test]
    fn token_count_operators() {
        // "a + b * c" => a(1) +(1) b(1) *(1) c(1) = 5 tokens
        let count = count_tokens("a + b * c");
        assert_eq!(count, 5);
    }

    #[test]
    fn token_count_empty_code() {
        let count = count_tokens("");
        assert_eq!(count, 0);
    }

    #[test]
    fn token_count_only_comments() {
        let count = count_tokens("-- just a comment\n-- another");
        assert_eq!(count, 0);
    }

    #[test]
    fn token_count_multiline() {
        let code = "local x = 1\nlocal y = 2";
        let count = count_tokens(code);
        // local(1) x(1) =(1) 1(1) = 4 per line, 8 total
        assert_eq!(count, 8);
    }
}
