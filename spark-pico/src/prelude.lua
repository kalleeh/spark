-- PICO-8 helpers that are simplest to express in Lua itself: table value
-- comparison, insert-position handling and deletion-safe iteration.
--
-- Loaded once at boot by `register_api` (src/lua_ffi.rs) via include_str!.
-- Every `function name(` defined here must be listed in PRELUDE_DEFS in
-- lua_ffi.rs: the firmware checks at boot that each one exists, and the
-- desktop/pico parity test reads that list. Numeric-heavy helpers (tostr,
-- tonum, split) are implemented in Rust on top of spark_core::api_ops so
-- both targets share one implementation.

-- del(t, v): remove the first element equal to v, shifting the rest down.
-- Returns the removed value (nil if absent).
function del(t, v)
  if t == nil then return end
  for i = 1, #t do
    if t[i] == v then
      table.remove(t, i)
      return v
    end
  end
end

-- add(c, x, [i]): insert at 1-based i (clamped to [1, #c+1]), else append.
-- Returns x.
function add(c, x, i)
  if c == nil then return end
  if i then
    i = i // 1
    if i < 1 then i = 1 elseif i > #c + 1 then i = #c + 1 end
    table.insert(c, i, x)
  else
    c[#c + 1] = x
  end
  return x
end

-- deli(t, [i]): delete the element at index i (default: last) and shift the
-- remaining ones down. Returns the removed value.
function deli(t, i)
  if t == nil then return nil end
  i = i or #t
  if i < 1 or i > #t then return nil end
  return table.remove(t, i)
end

-- count(c, [v]): number of non-nil elements in [1, #c], or of elements == v.
function count(c, v)
  if c == nil then return 0 end
  local n = 0
  if v == nil then
    for i = 1, #c do if c[i] ~= nil then n = n + 1 end end
  else
    for i = 1, #c do if c[i] == v then n = n + 1 end end
  end
  return n
end

-- all(t): deletion-safe iterator. Re-reads #t each step, skips nil holes, and
-- only advances past the current element if it is unchanged, so a del() that
-- shifts a new element into the slot isn't skipped.
function all(t)
  if t == nil or #t == 0 then return function() end end
  local i, prev = 1, nil
  return function()
    if t[i] == prev then i = i + 1 end
    while i <= #t and t[i] == nil do i = i + 1 end
    prev = t[i]
    return prev
  end
end

-- foreach(t, f): built on the same deletion-safe traversal so a callback can
-- del() the current element without skipping its successor.
function foreach(t, f)
  if t == nil then return end
  for v in all(t) do f(v) end
end

-- sub(s, i, [j]): string.sub alias.
function sub(s, i, j)
  if j then return string.sub(s, i, j) end
  return string.sub(s, i)
end

-- chr(a, b, c, ...): build a string from raw bytes (multi-arg form).
function chr(...)
  return string.char(...)
end

-- ord(s, [i], [n]): return n byte values starting at 1-based index i.
function ord(s, i, n)
  if s == nil then return nil end
  s = tostring(s)
  i = i or 1
  n = n or 1
  if i < 1 or i > #s or n < 1 then return end
  local j = i + n - 1
  if j > #s then j = #s end
  return string.byte(s, i, j)
end
