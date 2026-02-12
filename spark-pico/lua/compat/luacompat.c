/*
 * luacompat.c - Bare-metal C library implementations for Lua 5.4
 *
 * Provides real implementations of string, memory, number-parsing,
 * and formatting functions.  Memory allocation delegates to Rust-provided
 * extern functions.
 */

#include "luacompat.h"

/* ======================================================================== */
/*  Rust allocator bridge                                                   */
/* ======================================================================== */

extern void *rust_malloc(size_t size);
extern void *rust_realloc(void *ptr, size_t old_size, size_t new_size);
extern void  rust_free(void *ptr, size_t size);
extern void *rust_calloc(size_t nmemb, size_t size);

/*
 * We track allocation sizes in a small header prepended to every block.
 * This lets free() know the size to pass to rust_free(), and realloc()
 * know the old size for rust_realloc().
 *
 * Layout: [ size_t alloc_size ] [ user data ... ]
 *         ^                     ^
 *         raw pointer           pointer returned to caller
 */

#define ALLOC_HEADER sizeof(size_t)

static inline size_t *_hdr(void *ptr) {
    return (size_t *)((char *)ptr - ALLOC_HEADER);
}

void *malloc(size_t size) {
    if (size == 0) size = 1;
    void *raw = rust_malloc(size + ALLOC_HEADER);
    if (!raw) return NULL;
    *(size_t *)raw = size;
    return (char *)raw + ALLOC_HEADER;
}

void *calloc(size_t nmemb, size_t size) {
    size_t total = nmemb * size;
    void *p = malloc(total);
    if (p) memset(p, 0, total);
    return p;
}

void *realloc(void *ptr, size_t new_size) {
    if (!ptr) return malloc(new_size);
    if (new_size == 0) { free(ptr); return NULL; }
    size_t old_size = *_hdr(ptr);
    void *raw_old = (char *)ptr - ALLOC_HEADER;
    void *raw_new = rust_realloc(raw_old, old_size + ALLOC_HEADER,
                                  new_size + ALLOC_HEADER);
    if (!raw_new) return NULL;
    *(size_t *)raw_new = new_size;
    return (char *)raw_new + ALLOC_HEADER;
}

void free(void *ptr) {
    if (!ptr) return;
    size_t sz = *_hdr(ptr);
    rust_free((char *)ptr - ALLOC_HEADER, sz + ALLOC_HEADER);
}

/* ======================================================================== */
/*  errno                                                                   */
/* ======================================================================== */

int errno = 0;

/* ======================================================================== */
/*  assert                                                                  */
/* ======================================================================== */

void _assert_fail(const char *expr, const char *file, int line) {
    (void)expr; (void)file; (void)line;
    abort();
}

/* ======================================================================== */
/*  abort / exit / atexit                                                   */
/* ======================================================================== */

void abort(void) {
    while (1) {
        __asm__ volatile("bkpt #0");
    }
}

static void (*_atexit_funcs[8])(void);
static int _atexit_count = 0;

int atexit(void (*func)(void)) {
    if (_atexit_count >= 8) return -1;
    _atexit_funcs[_atexit_count++] = func;
    return 0;
}

void exit(int status) {
    (void)status;
    for (int i = _atexit_count - 1; i >= 0; i--)
        _atexit_funcs[i]();
    abort();
}

/* ======================================================================== */
/*  string / memory functions                                               */
/* ======================================================================== */

void *memcpy(void *dest, const void *src, size_t n) {
    unsigned char *d = (unsigned char *)dest;
    const unsigned char *s = (const unsigned char *)src;
    while (n--) *d++ = *s++;
    return dest;
}

void *memmove(void *dest, const void *src, size_t n) {
    unsigned char *d = (unsigned char *)dest;
    const unsigned char *s = (const unsigned char *)src;
    if (d < s) {
        while (n--) *d++ = *s++;
    } else {
        d += n; s += n;
        while (n--) *--d = *--s;
    }
    return dest;
}

void *memset(void *s, int c, size_t n) {
    unsigned char *p = (unsigned char *)s;
    while (n--) *p++ = (unsigned char)c;
    return s;
}

int memcmp(const void *s1, const void *s2, size_t n) {
    const unsigned char *a = (const unsigned char *)s1;
    const unsigned char *b = (const unsigned char *)s2;
    while (n--) {
        if (*a != *b) return *a - *b;
        a++; b++;
    }
    return 0;
}

void *memchr(const void *s, int c, size_t n) {
    const unsigned char *p = (const unsigned char *)s;
    unsigned char ch = (unsigned char)c;
    while (n--) {
        if (*p == ch) return (void *)p;
        p++;
    }
    return NULL;
}

size_t strlen(const char *s) {
    const char *p = s;
    while (*p) p++;
    return (size_t)(p - s);
}

char *strcpy(char *dest, const char *src) {
    char *d = dest;
    while ((*d++ = *src++))
        ;
    return dest;
}

char *strncpy(char *dest, const char *src, size_t n) {
    char *d = dest;
    while (n && (*d++ = *src++)) n--;
    while (n--) *d++ = '\0';
    return dest;
}

int strcmp(const char *s1, const char *s2) {
    while (*s1 && *s1 == *s2) { s1++; s2++; }
    return (unsigned char)*s1 - (unsigned char)*s2;
}

int strcoll(const char *s1, const char *s2) {
    /* No locale support on bare-metal; just use strcmp */
    return strcmp(s1, s2);
}

int strncmp(const char *s1, const char *s2, size_t n) {
    while (n && *s1 && *s1 == *s2) { s1++; s2++; n--; }
    if (n == 0) return 0;
    return (unsigned char)*s1 - (unsigned char)*s2;
}

char *strcat(char *dest, const char *src) {
    char *d = dest;
    while (*d) d++;
    while ((*d++ = *src++))
        ;
    return dest;
}

char *strncat(char *dest, const char *src, size_t n) {
    char *d = dest;
    while (*d) d++;
    while (n-- && (*d = *src++)) d++;
    *d = '\0';
    return dest;
}

char *strchr(const char *s, int c) {
    char ch = (char)c;
    while (*s) {
        if (*s == ch) return (char *)s;
        s++;
    }
    return ch == '\0' ? (char *)s : NULL;
}

char *strrchr(const char *s, int c) {
    char ch = (char)c;
    const char *last = NULL;
    while (*s) {
        if (*s == ch) last = s;
        s++;
    }
    if (ch == '\0') return (char *)s;
    return (char *)last;
}

char *strstr(const char *haystack, const char *needle) {
    if (!*needle) return (char *)haystack;
    for (; *haystack; haystack++) {
        const char *h = haystack, *n = needle;
        while (*h && *n && *h == *n) { h++; n++; }
        if (!*n) return (char *)haystack;
    }
    return NULL;
}

size_t strspn(const char *s, const char *accept) {
    const char *p = s;
    while (*p) {
        const char *a = accept;
        int found = 0;
        while (*a) { if (*p == *a++) { found = 1; break; } }
        if (!found) break;
        p++;
    }
    return (size_t)(p - s);
}

size_t strcspn(const char *s, const char *reject) {
    const char *p = s;
    while (*p) {
        const char *r = reject;
        while (*r) { if (*p == *r++) return (size_t)(p - s); }
        p++;
    }
    return (size_t)(p - s);
}

char *strpbrk(const char *s, const char *accept) {
    while (*s) {
        const char *a = accept;
        while (*a) {
            if (*s == *a++) return (char *)s;
        }
        s++;
    }
    return NULL;
}

static char _strerror_buf[32];

char *strerror(int errnum) {
    switch (errnum) {
        case 0:      strcpy(_strerror_buf, "Success"); break;
        case EDOM:   strcpy(_strerror_buf, "Math arg"); break;
        case ERANGE: strcpy(_strerror_buf, "Out of range"); break;
        default:     strcpy(_strerror_buf, "Unknown error"); break;
    }
    return _strerror_buf;
}

/* ======================================================================== */
/*  atoi / abs                                                              */
/* ======================================================================== */

int atoi(const char *s) {
    return (int)strtol(s, NULL, 10);
}

long atol(const char *s) {
    return strtol(s, NULL, 10);
}

int abs(int x) {
    return x < 0 ? -x : x;
}

long labs(long x) {
    return x < 0 ? -x : x;
}

/* ======================================================================== */
/*  getenv / system                                                         */
/* ======================================================================== */

char *getenv(const char *name) {
    (void)name;
    return NULL;
}

int system(const char *command) {
    (void)command;
    return -1;
}

/* ======================================================================== */
/*  qsort (simple Shell sort -- sufficient for Lua table.sort)              */
/* ======================================================================== */

void qsort(void *base, size_t nmemb, size_t size,
            int (*compar)(const void *, const void *)) {
    /* Shell sort: simple, in-place, no recursion, small code size */
    char *arr = (char *)base;
    /* Use a small temp buffer on the stack for swaps */
    char tmp[64];  /* supports elements up to 64 bytes (Lua uses pointers) */
    if (size > sizeof(tmp)) {
        /* Fallback: bubble sort for large elements (shouldn't happen in Lua) */
        for (size_t i = 0; i < nmemb; i++) {
            for (size_t j = i + 1; j < nmemb; j++) {
                if (compar(arr + i * size, arr + j * size) > 0) {
                    for (size_t k = 0; k < size; k++) {
                        char c = arr[i * size + k];
                        arr[i * size + k] = arr[j * size + k];
                        arr[j * size + k] = c;
                    }
                }
            }
        }
        return;
    }
    for (size_t gap = nmemb / 2; gap > 0; gap /= 2) {
        for (size_t i = gap; i < nmemb; i++) {
            memcpy(tmp, arr + i * size, size);
            size_t j = i;
            while (j >= gap && compar(arr + (j - gap) * size, tmp) > 0) {
                memcpy(arr + j * size, arr + (j - gap) * size, size);
                j -= gap;
            }
            memcpy(arr + j * size, tmp, size);
        }
    }
}

/* ======================================================================== */
/*  Number parsing: strtol, strtoul, strtoll, strtoull                      */
/* ======================================================================== */

static inline int _digit_val(char c, int base) {
    int v;
    if (c >= '0' && c <= '9')      v = c - '0';
    else if (c >= 'a' && c <= 'z') v = c - 'a' + 10;
    else if (c >= 'A' && c <= 'Z') v = c - 'A' + 10;
    else return -1;
    return v < base ? v : -1;
}

unsigned long long strtoull(const char *nptr, char **endptr, int base) {
    const char *s = nptr;
    /* skip whitespace */
    while (isspace((unsigned char)*s)) s++;

    /* optional sign */
    int neg = 0;
    if (*s == '+') s++;
    else if (*s == '-') { neg = 1; s++; }

    /* detect base */
    if (base == 0) {
        if (*s == '0') {
            s++;
            if (*s == 'x' || *s == 'X') { base = 16; s++; }
            else { base = 8; }
        } else {
            base = 10;
        }
    } else if (base == 16) {
        if (s[0] == '0' && (s[1] == 'x' || s[1] == 'X'))
            s += 2;
    }

    unsigned long long result = 0;
    int any = 0;
    int overflow = 0;

    while (1) {
        int d = _digit_val(*s, base);
        if (d < 0) break;
        any = 1;
        unsigned long long prev = result;
        result = result * (unsigned long long)base + (unsigned long long)d;
        if (result / (unsigned long long)base < prev) overflow = 1;
        s++;
    }

    if (!any) {
        if (endptr) *endptr = (char *)nptr;
        return 0;
    }
    if (overflow) {
        errno = ERANGE;
        if (endptr) *endptr = (char *)s;
        return (unsigned long long)(-1);  /* ULLONG_MAX */
    }
    if (endptr) *endptr = (char *)s;
    return neg ? (unsigned long long)(-(long long)result) : result;
}

long long strtoll(const char *nptr, char **endptr, int base) {
    const char *s = nptr;
    while (isspace((unsigned char)*s)) s++;

    int neg = 0;
    if (*s == '+') s++;
    else if (*s == '-') { neg = 1; s++; }

    /* Let strtoull do the heavy lifting, but we need to pass the
       rest of the string without the sign */
    /* Reconstruct: just call our own parsing */
    if (base == 0) {
        if (*s == '0') {
            if (s[1] == 'x' || s[1] == 'X') base = 16;
            else base = 8;
        } else {
            base = 10;
        }
    }
    if (base == 16 && s[0] == '0' && (s[1] == 'x' || s[1] == 'X'))
        s += 2;

    unsigned long long result = 0;
    int any = 0;

    while (1) {
        int d = _digit_val(*s, base);
        if (d < 0) break;
        any = 1;
        result = result * (unsigned long long)base + (unsigned long long)d;
        s++;
    }

    if (!any) {
        if (endptr) *endptr = (char *)nptr;
        return 0;
    }
    if (endptr) *endptr = (char *)s;

    long long sresult;
    if (neg) {
        sresult = -(long long)result;
        /* Check underflow: if result was larger than can be negated */
        if (result > (unsigned long long)LLONG_MAX + 1ULL) {
            errno = ERANGE;
            return LLONG_MIN;
        }
    } else {
        if (result > (unsigned long long)LLONG_MAX) {
            errno = ERANGE;
            return LLONG_MAX;
        }
        sresult = (long long)result;
    }
    return sresult;
}

long strtol(const char *nptr, char **endptr, int base) {
    long long v = strtoll(nptr, endptr, base);
    if (v > LONG_MAX)  { errno = ERANGE; return LONG_MAX; }
    if (v < LONG_MIN)  { errno = ERANGE; return LONG_MIN; }
    return (long)v;
}

unsigned long strtoul(const char *nptr, char **endptr, int base) {
    unsigned long long v = strtoull(nptr, endptr, base);
    if (v > ULONG_MAX) { errno = ERANGE; return ULONG_MAX; }
    return (unsigned long)v;
}

/* ======================================================================== */
/*  strtod / strtof  - real implementation for Lua number parsing           */
/*                                                                          */
/*  Handles: [+/-] digits [. digits] [(e|E) [+/-] digits]                  */
/*           [+/-] 0x hex [. hex] [(p|P) [+/-] digits]                     */
/*           inf, infinity, nan                                             */
/* ======================================================================== */

double strtod(const char *nptr, char **endptr) {
    const char *s = nptr;

    /* skip leading whitespace */
    while (isspace((unsigned char)*s)) s++;

    /* sign */
    int neg = 0;
    if (*s == '+') s++;
    else if (*s == '-') { neg = 1; s++; }

    /* inf / nan */
    if ((s[0] == 'i' || s[0] == 'I') &&
        (s[1] == 'n' || s[1] == 'N') &&
        (s[2] == 'f' || s[2] == 'F')) {
        s += 3;
        if ((s[0] == 'i' || s[0] == 'I') &&
            (s[1] == 'n' || s[1] == 'N') &&
            (s[2] == 'i' || s[2] == 'I') &&
            (s[3] == 't' || s[3] == 'T') &&
            (s[4] == 'y' || s[4] == 'Y'))
            s += 5;
        if (endptr) *endptr = (char *)s;
        return neg ? -HUGE_VAL : HUGE_VAL;
    }
    if ((s[0] == 'n' || s[0] == 'N') &&
        (s[1] == 'a' || s[1] == 'A') &&
        (s[2] == 'n' || s[2] == 'N')) {
        s += 3;
        /* optional (char-sequence) */
        if (*s == '(') {
            const char *p = s + 1;
            while (*p && *p != ')') p++;
            if (*p == ')') s = p + 1;
        }
        if (endptr) *endptr = (char *)s;
        return __builtin_nan("");
    }

    double result = 0.0;
    int any = 0;

    /* hex float: 0x... */
    if (s[0] == '0' && (s[1] == 'x' || s[1] == 'X')) {
        s += 2;

        /* integer part */
        while (1) {
            int d = _digit_val(*s, 16);
            if (d < 0) break;
            result = result * 16.0 + (double)d;
            any = 1;
            s++;
        }

        /* fractional part */
        if (*s == '.') {
            s++;
            double frac = 1.0;
            while (1) {
                int d = _digit_val(*s, 16);
                if (d < 0) break;
                frac /= 16.0;
                result += (double)d * frac;
                any = 1;
                s++;
            }
        }

        if (!any) {
            if (endptr) *endptr = (char *)nptr;
            return 0.0;
        }

        /* binary exponent p/P */
        if (*s == 'p' || *s == 'P') {
            s++;
            int exp_neg = 0;
            if (*s == '+') s++;
            else if (*s == '-') { exp_neg = 1; s++; }

            int exp_val = 0;
            while (*s >= '0' && *s <= '9') {
                exp_val = exp_val * 10 + (*s - '0');
                s++;
            }
            if (exp_neg) exp_val = -exp_val;

            /* result *= 2^exp_val */
            if (exp_val > 0) {
                while (exp_val >= 30) { result *= (double)(1 << 30); exp_val -= 30; }
                if (exp_val > 0) result *= (double)(1 << exp_val);
            } else {
                exp_val = -exp_val;
                while (exp_val >= 30) { result /= (double)(1 << 30); exp_val -= 30; }
                if (exp_val > 0) result /= (double)(1 << exp_val);
            }
        }
    } else {
        /* decimal float */

        /* integer part */
        while (*s >= '0' && *s <= '9') {
            result = result * 10.0 + (double)(*s - '0');
            any = 1;
            s++;
        }

        /* fractional part */
        if (*s == '.') {
            s++;
            double frac = 1.0;
            while (*s >= '0' && *s <= '9') {
                frac /= 10.0;
                result += (double)(*s - '0') * frac;
                any = 1;
                s++;
            }
        }

        if (!any) {
            if (endptr) *endptr = (char *)nptr;
            return 0.0;
        }

        /* exponent e/E */
        if (*s == 'e' || *s == 'E') {
            s++;
            int exp_neg = 0;
            if (*s == '+') s++;
            else if (*s == '-') { exp_neg = 1; s++; }

            int exp_val = 0;
            while (*s >= '0' && *s <= '9') {
                exp_val = exp_val * 10 + (*s - '0');
                if (exp_val > 400) { exp_val = 400; break; } /* cap to avoid loop */
                s++;
            }
            /* consume remaining exponent digits */
            while (*s >= '0' && *s <= '9') s++;

            /* Apply exponent by repeated multiply/divide.
             * Use a table of powers of 10 for efficiency. */
            static const double pow10_pos[] = {
                1e1, 1e2, 1e4, 1e8, 1e16, 1e32, 1e64, 1e128, 1e256
            };
            static const double pow10_neg[] = {
                1e-1, 1e-2, 1e-4, 1e-8, 1e-16, 1e-32, 1e-64, 1e-128, 1e-256
            };

            if (exp_neg) {
                for (int i = 0; i < 9 && exp_val > 0; i++) {
                    if (exp_val & 1) result *= pow10_neg[i];
                    exp_val >>= 1;
                }
            } else {
                for (int i = 0; i < 9 && exp_val > 0; i++) {
                    if (exp_val & 1) result *= pow10_pos[i];
                    exp_val >>= 1;
                }
            }
        }
    }

    if (endptr) *endptr = (char *)s;
    return neg ? -result : result;
}

float strtof(const char *nptr, char **endptr) {
    return (float)strtod(nptr, endptr);
}

/* ======================================================================== */
/*  Minimal vsnprintf implementation                                        */
/*                                                                          */
/*  Supports: %d %ld %lld %i %li %lli %u %lu %llu %x %lx %llx             */
/*            %o %s %c %p %f %e %g %% %X %n                                */
/*  Flags: -, +, space, 0, #                                                */
/*  Width and precision (including *)                                       */
/*  This is sufficient for Lua's number formatting (luaO_pushfstring,       */
/*  l_sprintf, string.format).                                              */
/* ======================================================================== */

/* Helper: write a character to the buffer */
static inline void _putc(char *buf, size_t size, size_t *pos, char c) {
    if (*pos < size) buf[*pos] = c;
    (*pos)++;
}

/* Helper: write a string to the buffer */
static void _puts(char *buf, size_t size, size_t *pos, const char *s, size_t len) {
    for (size_t i = 0; i < len; i++)
        _putc(buf, size, pos, s[i]);
}

/* Helper: output unsigned integer in given base */
static void _put_uint(char *buf, size_t size, size_t *pos,
                      unsigned long long val, int base, int uppercase,
                      int width, int zero_pad, int left_align,
                      int precision, const char *prefix) {
    char tmp[72];
    const char *digits = uppercase ? "0123456789ABCDEF" : "0123456789abcdef";
    int len = 0;

    if (val == 0 && precision != 0) {
        tmp[len++] = '0';
    } else if (val != 0) {
        while (val) {
            tmp[len++] = digits[val % base];
            val /= base;
        }
    }

    /* precision: minimum digits */
    while (len < precision) tmp[len++] = '0';

    int prefix_len = prefix ? (int)strlen(prefix) : 0;
    int total = len + prefix_len;
    int pad = (width > total) ? width - total : 0;

    if (!left_align && !zero_pad) {
        for (int i = 0; i < pad; i++) _putc(buf, size, pos, ' ');
    }
    if (prefix) _puts(buf, size, pos, prefix, prefix_len);
    if (!left_align && zero_pad) {
        for (int i = 0; i < pad; i++) _putc(buf, size, pos, '0');
    }
    for (int i = len - 1; i >= 0; i--) _putc(buf, size, pos, tmp[i]);
    if (left_align) {
        for (int i = 0; i < pad; i++) _putc(buf, size, pos, ' ');
    }
}

/* Helper: output signed integer */
static void _put_int(char *buf, size_t size, size_t *pos,
                     long long val, int base, int uppercase,
                     int width, int zero_pad, int left_align,
                     int precision, int plus_sign, int space_sign) {
    const char *prefix = NULL;
    char prefix_buf[2];
    unsigned long long uval;

    if (val < 0) {
        prefix_buf[0] = '-'; prefix_buf[1] = '\0';
        prefix = prefix_buf;
        uval = (unsigned long long)(-val);
    } else {
        if (plus_sign) {
            prefix_buf[0] = '+'; prefix_buf[1] = '\0';
            prefix = prefix_buf;
        } else if (space_sign) {
            prefix_buf[0] = ' '; prefix_buf[1] = '\0';
            prefix = prefix_buf;
        }
        uval = (unsigned long long)val;
    }

    _put_uint(buf, size, pos, uval, base, uppercase,
              width, zero_pad, left_align, precision, prefix);
}

/* Forward declaration */
static int _fmt_float(char *buf, size_t size, size_t *pos,
                      double val, char spec, int width,
                      int precision, int has_precision,
                      int zero_pad, int left_align,
                      int plus_sign, int space_sign, int hash);

/* Helper: format a double in %f style into a temp buffer, return length */
static int _dtoa_f(char *out, int outsize, double val, int prec) {
    int len = 0;
    int neg = 0;

    if (val < 0) { neg = 1; val = -val; }

    /* Handle very large values - cap to avoid infinite loops */
    if (val > 1e18) {
        /* Use scientific notation logic won't apply here, just output large */
        if (neg && len < outsize) out[len++] = '-';
        /* Just return "inf"-like for extremely large */
        const char *s = "1e+999";
        for (int i = 0; s[i] && len < outsize; i++) out[len++] = s[i];
        return len;
    }

    /* Split into integer and fractional parts */
    unsigned long long int_part = (unsigned long long)val;
    double frac = val - (double)int_part;

    /* Round fractional part */
    double rounding = 0.5;
    for (int i = 0; i < prec; i++) rounding /= 10.0;
    frac += rounding;
    if (frac >= 1.0) {
        int_part++;
        frac -= 1.0;
    }

    /* Build integer part in reverse */
    char int_buf[24];
    int int_len = 0;
    if (int_part == 0) {
        int_buf[int_len++] = '0';
    } else {
        while (int_part > 0 && int_len < 23) {
            int_buf[int_len++] = '0' + (int)(int_part % 10);
            int_part /= 10;
        }
    }

    if (neg && len < outsize) out[len++] = '-';
    for (int i = int_len - 1; i >= 0 && len < outsize; i--)
        out[len++] = int_buf[i];

    if (prec > 0) {
        if (len < outsize) out[len++] = '.';
        for (int i = 0; i < prec && len < outsize; i++) {
            frac *= 10.0;
            int d = (int)frac;
            if (d > 9) d = 9;
            out[len++] = '0' + d;
            frac -= d;
        }
    }

    return len;
}

/* Helper: format a double in %e style into a temp buffer */
static int _dtoa_e(char *out, int outsize, double val, int prec, int uppercase) {
    int len = 0;
    int neg = 0;

    if (val < 0) { neg = 1; val = -val; }

    if (val == 0.0) {
        if (neg && len < outsize) out[len++] = '-';
        if (len < outsize) out[len++] = '0';
        if (prec > 0) {
            if (len < outsize) out[len++] = '.';
            for (int i = 0; i < prec && len < outsize; i++)
                out[len++] = '0';
        }
        if (len < outsize) out[len++] = uppercase ? 'E' : 'e';
        if (len < outsize) out[len++] = '+';
        if (len < outsize) out[len++] = '0';
        if (len < outsize) out[len++] = '0';
        return len;
    }

    /* Compute exponent */
    int exp10 = 0;
    double v = val;
    if (v >= 10.0) {
        while (v >= 10.0 && exp10 < 400) { v /= 10.0; exp10++; }
    } else if (v < 1.0) {
        while (v < 1.0 && exp10 > -400) { v *= 10.0; exp10--; }
    }

    /* v is now in [1.0, 10.0) */

    /* Round the mantissa */
    double rounding = 0.5;
    for (int i = 0; i < prec; i++) rounding /= 10.0;
    v += rounding;
    if (v >= 10.0) { v /= 10.0; exp10++; }

    if (neg && len < outsize) out[len++] = '-';

    /* First digit */
    int d = (int)v;
    if (d > 9) d = 9;
    if (len < outsize) out[len++] = '0' + d;
    v -= d;

    if (prec > 0) {
        if (len < outsize) out[len++] = '.';
        for (int i = 0; i < prec && len < outsize; i++) {
            v *= 10.0;
            d = (int)v;
            if (d > 9) d = 9;
            out[len++] = '0' + d;
            v -= d;
        }
    }

    if (len < outsize) out[len++] = uppercase ? 'E' : 'e';
    if (exp10 < 0) {
        if (len < outsize) out[len++] = '-';
        exp10 = -exp10;
    } else {
        if (len < outsize) out[len++] = '+';
    }

    /* Exponent: at least 2 digits */
    if (exp10 >= 100) {
        if (len < outsize) out[len++] = '0' + exp10 / 100;
        exp10 %= 100;
    }
    if (len < outsize) out[len++] = '0' + exp10 / 10;
    if (len < outsize) out[len++] = '0' + exp10 % 10;

    return len;
}

static int _fmt_float(char *buf, size_t size, size_t *pos,
                      double val, char spec, int width,
                      int precision, int has_precision,
                      int zero_pad, int left_align,
                      int plus_sign, int space_sign, int hash) {
    (void)hash;
    char tmp[128];
    int len = 0;

    if (!has_precision) precision = 6;

    /* Handle special values */
    if (__builtin_isnan(val)) {
        const char *s = (spec >= 'A' && spec <= 'Z') ? "NAN" : "nan";
        len = 3;
        memcpy(tmp, s, 3);
    } else if (__builtin_isinf(val)) {
        int neg = val < 0;
        const char *s;
        if (neg) {
            s = (spec >= 'A' && spec <= 'Z') ? "-INF" : "-inf";
            len = 4;
        } else if (plus_sign) {
            s = (spec >= 'A' && spec <= 'Z') ? "+INF" : "+inf";
            len = 4;
        } else if (space_sign) {
            s = (spec >= 'A' && spec <= 'Z') ? " INF" : " inf";
            len = 4;
        } else {
            s = (spec >= 'A' && spec <= 'Z') ? "INF" : "inf";
            len = 3;
        }
        memcpy(tmp, s, len);
        zero_pad = 0; /* no zero-padding for inf/nan */
    } else if (spec == 'f' || spec == 'F') {
        len = _dtoa_f(tmp, sizeof(tmp), val, precision);
        /* Add sign prefix if positive */
        if (val >= 0 && !__builtin_signbit(val)) {
            if (plus_sign) { memmove(tmp + 1, tmp, len); tmp[0] = '+'; len++; }
            else if (space_sign) { memmove(tmp + 1, tmp, len); tmp[0] = ' '; len++; }
        }
    } else if (spec == 'e' || spec == 'E') {
        len = _dtoa_e(tmp, sizeof(tmp), val, precision, spec == 'E');
        if (val >= 0 && !__builtin_signbit(val)) {
            if (plus_sign) { memmove(tmp + 1, tmp, len); tmp[0] = '+'; len++; }
            else if (space_sign) { memmove(tmp + 1, tmp, len); tmp[0] = ' '; len++; }
        }
    } else if (spec == 'g' || spec == 'G') {
        /* %g: use %e if exponent < -4 or >= precision, else %f */
        int p = precision;
        if (p == 0) p = 1;

        /* Compute exponent for %g decision */
        double absval = val < 0 ? -val : val;
        int exp10 = 0;
        if (absval != 0.0) {
            double v = absval;
            if (v >= 10.0) {
                while (v >= 10.0) { v /= 10.0; exp10++; }
            } else if (v < 1.0) {
                while (v < 1.0 && exp10 > -400) { v *= 10.0; exp10--; }
            }
        }

        if (exp10 < -4 || exp10 >= p) {
            /* Use %e with precision p-1 */
            len = _dtoa_e(tmp, sizeof(tmp), val, p - 1, spec == 'G');
        } else {
            /* Use %f with precision p - (exp10+1) */
            int fprec = p - (exp10 + 1);
            if (fprec < 0) fprec = 0;
            len = _dtoa_f(tmp, sizeof(tmp), val, fprec);
        }

        /* Remove trailing zeros after decimal point (unless # flag) */
        if (!hash) {
            /* Find the 'e'/'E' if present */
            int e_pos = -1;
            for (int i = 0; i < len; i++) {
                if (tmp[i] == 'e' || tmp[i] == 'E') { e_pos = i; break; }
            }
            int end = (e_pos >= 0) ? e_pos : len;
            /* Find decimal point */
            int dot = -1;
            for (int i = 0; i < end; i++) {
                if (tmp[i] == '.') { dot = i; break; }
            }
            if (dot >= 0) {
                int last_nonzero = dot;
                for (int i = dot + 1; i < end; i++) {
                    if (tmp[i] != '0') last_nonzero = i;
                }
                int new_end = last_nonzero + 1;
                if (new_end == dot + 1) new_end = dot; /* remove dot too */
                if (e_pos >= 0) {
                    memmove(tmp + new_end, tmp + e_pos, len - e_pos);
                    len = new_end + (len - e_pos);
                } else {
                    len = new_end;
                }
            }
        }

        if (val >= 0 && !__builtin_signbit(val)) {
            if (plus_sign) { memmove(tmp + 1, tmp, len); tmp[0] = '+'; len++; }
            else if (space_sign) { memmove(tmp + 1, tmp, len); tmp[0] = ' '; len++; }
        }
    } else if (spec == 'a' || spec == 'A') {
        /* Hex float format: [-]0xh.hhhp[+-]d */
        int neg = 0;
        double v = val;
        if (v < 0) { neg = 1; v = -v; }

        if (v == 0.0) {
            len = 0;
            if (neg) tmp[len++] = '-';
            else if (plus_sign) tmp[len++] = '+';
            else if (space_sign) tmp[len++] = ' ';
            tmp[len++] = '0';
            tmp[len++] = spec == 'A' ? 'X' : 'x';
            tmp[len++] = '0';
            if (precision > 0 || hash) {
                tmp[len++] = '.';
                for (int i = 0; i < precision; i++) tmp[len++] = '0';
            }
            tmp[len++] = spec == 'A' ? 'P' : 'p';
            tmp[len++] = '+';
            tmp[len++] = '0';
        } else {
            /* Normalize: v = m * 2^exp where 1 <= m < 2 */
            int exp2 = 0;
            while (v >= 2.0) { v /= 2.0; exp2++; }
            while (v < 1.0 && v > 0) { v *= 2.0; exp2--; }

            len = 0;
            if (neg) tmp[len++] = '-';
            else if (plus_sign) tmp[len++] = '+';
            else if (space_sign) tmp[len++] = ' ';
            tmp[len++] = '0';
            tmp[len++] = spec == 'A' ? 'X' : 'x';

            /* Integer part of mantissa (always 1 for normalized) */
            int ipart = (int)v;
            tmp[len++] = '0' + ipart;
            v -= ipart;

            int p = has_precision ? precision : 13; /* ~52 bits / 4 = 13 hex digits */
            if (p > 0 || hash) {
                tmp[len++] = '.';
                const char *hd = (spec == 'A') ? "0123456789ABCDEF" : "0123456789abcdef";
                for (int i = 0; i < p; i++) {
                    v *= 16.0;
                    int d = (int)v;
                    if (d > 15) d = 15;
                    tmp[len++] = hd[d];
                    v -= d;
                }
            }

            tmp[len++] = spec == 'A' ? 'P' : 'p';
            if (exp2 < 0) {
                tmp[len++] = '-';
                exp2 = -exp2;
            } else {
                tmp[len++] = '+';
            }
            if (exp2 == 0) {
                tmp[len++] = '0';
            } else {
                char ebuf[12];
                int elen = 0;
                while (exp2 > 0) {
                    ebuf[elen++] = '0' + exp2 % 10;
                    exp2 /= 10;
                }
                for (int i = elen - 1; i >= 0; i--)
                    tmp[len++] = ebuf[i];
            }
        }
    } else {
        /* Unknown float spec, just output the raw char */
        _putc(buf, size, pos, spec);
        return 0;
    }

    /* Pad and output */
    int pad = (width > len) ? width - len : 0;
    if (!left_align && !zero_pad) {
        for (int i = 0; i < pad; i++) _putc(buf, size, pos, ' ');
    }
    if (!left_align && zero_pad) {
        /* Insert zeros after sign/0x prefix */
        int prefix_len = 0;
        if (len > 0 && (tmp[0] == '-' || tmp[0] == '+' || tmp[0] == ' '))
            prefix_len = 1;
        if (prefix_len > 0 && len > prefix_len + 1 && tmp[prefix_len] == '0' &&
            (tmp[prefix_len+1] == 'x' || tmp[prefix_len+1] == 'X'))
            prefix_len += 2;
        else if (len > 1 && tmp[0] == '0' && (tmp[1] == 'x' || tmp[1] == 'X'))
            prefix_len = 2;
        _puts(buf, size, pos, tmp, prefix_len);
        for (int i = 0; i < pad; i++) _putc(buf, size, pos, '0');
        _puts(buf, size, pos, tmp + prefix_len, len - prefix_len);
    } else {
        _puts(buf, size, pos, tmp, len);
    }
    if (left_align) {
        for (int i = 0; i < pad; i++) _putc(buf, size, pos, ' ');
    }

    return 0;
}

int vsnprintf(char *buf, size_t size, const char *fmt, va_list ap) {
    size_t pos = 0;

    while (*fmt) {
        if (*fmt != '%') {
            _putc(buf, size, &pos, *fmt++);
            continue;
        }
        fmt++; /* skip '%' */

        /* Flags */
        int left_align = 0, zero_pad = 0, plus_sign = 0, space_sign = 0, hash = 0;
        while (1) {
            if (*fmt == '-')      { left_align = 1; fmt++; }
            else if (*fmt == '0') { zero_pad = 1; fmt++; }
            else if (*fmt == '+') { plus_sign = 1; fmt++; }
            else if (*fmt == ' ') { space_sign = 1; fmt++; }
            else if (*fmt == '#') { hash = 1; fmt++; }
            else break;
        }
        if (left_align) zero_pad = 0;

        /* Width */
        int width = 0;
        if (*fmt == '*') {
            width = va_arg(ap, int);
            if (width < 0) { left_align = 1; width = -width; }
            fmt++;
        } else {
            while (*fmt >= '0' && *fmt <= '9') {
                width = width * 10 + (*fmt - '0');
                fmt++;
            }
        }

        /* Precision */
        int precision = -1;
        int has_precision = 0;
        if (*fmt == '.') {
            fmt++;
            has_precision = 1;
            precision = 0;
            if (*fmt == '*') {
                precision = va_arg(ap, int);
                if (precision < 0) { precision = -1; has_precision = 0; }
                fmt++;
            } else {
                while (*fmt >= '0' && *fmt <= '9') {
                    precision = precision * 10 + (*fmt - '0');
                    fmt++;
                }
            }
        }

        /* Length modifier */
        int length = 0; /* 0=default, 1=l, 2=ll, 3=h, 4=hh, 5=z, 6=j */
        if (*fmt == 'l') {
            fmt++;
            if (*fmt == 'l') { length = 2; fmt++; }
            else length = 1;
        } else if (*fmt == 'h') {
            fmt++;
            if (*fmt == 'h') { length = 4; fmt++; }
            else length = 3;
        } else if (*fmt == 'z') {
            length = 5; fmt++;
        } else if (*fmt == 'j') {
            length = 6; fmt++;
        }

        /* Conversion specifier */
        char spec = *fmt++;
        switch (spec) {
        case 'd': case 'i': {
            long long val;
            switch (length) {
                case 2:  val = va_arg(ap, long long); break;
                case 1:  val = va_arg(ap, long); break;
                case 5:  val = (long long)(ptrdiff_t)va_arg(ap, ptrdiff_t); break;
                default: val = va_arg(ap, int); break;
            }
            _put_int(buf, size, &pos, val, 10, 0, width, zero_pad, left_align,
                     has_precision ? precision : -1, plus_sign, space_sign);
            break;
        }
        case 'u': case 'x': case 'X': case 'o': {
            unsigned long long val;
            switch (length) {
                case 2:  val = va_arg(ap, unsigned long long); break;
                case 1:  val = va_arg(ap, unsigned long); break;
                case 5:  val = (unsigned long long)va_arg(ap, size_t); break;
                default: val = va_arg(ap, unsigned int); break;
            }
            int base = (spec == 'o') ? 8 : (spec == 'u') ? 10 : 16;
            const char *prefix = NULL;
            if (hash && val != 0) {
                if (spec == 'o') prefix = "0";
                else if (spec == 'x') prefix = "0x";
                else if (spec == 'X') prefix = "0X";
            }
            _put_uint(buf, size, &pos, val, base, (spec == 'X'), width,
                      zero_pad, left_align, has_precision ? precision : -1, prefix);
            break;
        }
        case 'c': {
            char c = (char)va_arg(ap, int);
            int pad = (width > 1) ? width - 1 : 0;
            if (!left_align) for (int i = 0; i < pad; i++) _putc(buf, size, &pos, ' ');
            _putc(buf, size, &pos, c);
            if (left_align)  for (int i = 0; i < pad; i++) _putc(buf, size, &pos, ' ');
            break;
        }
        case 's': {
            const char *s = va_arg(ap, const char *);
            if (!s) s = "(null)";
            size_t slen = strlen(s);
            if (has_precision && precision >= 0 && (size_t)precision < slen)
                slen = (size_t)precision;
            int pad = (width > (int)slen) ? width - (int)slen : 0;
            if (!left_align) for (int i = 0; i < pad; i++) _putc(buf, size, &pos, ' ');
            _puts(buf, size, &pos, s, slen);
            if (left_align)  for (int i = 0; i < pad; i++) _putc(buf, size, &pos, ' ');
            break;
        }
        case 'p': {
            void *ptr = va_arg(ap, void *);
            unsigned long long uval = (unsigned long long)(uintptr_t)ptr;
            _puts(buf, size, &pos, "0x", 2);
            /* Output as hex, minimum 1 digit */
            _put_uint(buf, size, &pos, uval, 16, 0, 0, 0, 0, 1, NULL);
            break;
        }
        case 'f': case 'F':
        case 'e': case 'E':
        case 'g': case 'G':
        case 'a': case 'A': {
            double val = va_arg(ap, double);
            _fmt_float(buf, size, &pos, val, spec, width,
                      has_precision ? precision : 6, has_precision,
                      zero_pad, left_align, plus_sign, space_sign, hash);
            break;
        }
        case 'n': {
            int *np = va_arg(ap, int *);
            if (np) *np = (int)pos;
            break;
        }
        case '%':
            _putc(buf, size, &pos, '%');
            break;
        case '\0':
            fmt--; /* back up so the outer loop's *fmt test sees '\0' */
            break;
        default:
            _putc(buf, size, &pos, '%');
            _putc(buf, size, &pos, spec);
            break;
        }
    }

    /* Null-terminate */
    if (size > 0) {
        if (pos < size) buf[pos] = '\0';
        else buf[size - 1] = '\0';
    }

    return (int)pos;
}

int snprintf(char *buf, size_t size, const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    int ret = vsnprintf(buf, size, fmt, ap);
    va_end(ap);
    return ret;
}

int sprintf(char *buf, const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    int ret = vsnprintf(buf, (size_t)-1, fmt, ap);
    va_end(ap);
    return ret;
}

/* ======================================================================== */
/*  sscanf - minimal implementation for Lua number parsing                  */
/*                                                                          */
/*  Lua uses sscanf primarily for:                                          */
/*    sscanf(s, "%" LUA_NUMBER_SCAN "%c", &n, &dummy)                      */
/*  We only need to support %f/%lf (number scanning) and %c.               */
/* ======================================================================== */

int sscanf(const char *str, const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);

    int count = 0;
    const char *s = str;

    while (*fmt) {
        if (*fmt == '%') {
            fmt++;
            /* Skip flags, width, length modifiers */
            while (*fmt == '*' || *fmt == 'l' || *fmt == 'h' ||
                   (*fmt >= '0' && *fmt <= '9'))
                fmt++;

            switch (*fmt) {
            case 'f': case 'e': case 'g':
            case 'F': case 'E': case 'G': {
                char *end;
                double val = strtod(s, &end);
                if (end == s) goto done;
                double *out = va_arg(ap, double *);
                *out = val;
                s = end;
                count++;
                break;
            }
            case 'd': case 'i': {
                char *end;
                long val = strtol(s, &end, 0);
                if (end == s) goto done;
                int *out = va_arg(ap, int *);
                *out = (int)val;
                s = end;
                count++;
                break;
            }
            case 'u': {
                char *end;
                unsigned long val = strtoul(s, &end, 0);
                if (end == s) goto done;
                unsigned int *out = va_arg(ap, unsigned int *);
                *out = (unsigned int)val;
                s = end;
                count++;
                break;
            }
            case 'c': {
                if (!*s) goto done;
                char *out = va_arg(ap, char *);
                *out = *s++;
                count++;
                break;
            }
            case 's': {
                while (isspace((unsigned char)*s)) s++;
                if (!*s) goto done;
                char *out = va_arg(ap, char *);
                while (*s && !isspace((unsigned char)*s))
                    *out++ = *s++;
                *out = '\0';
                count++;
                break;
            }
            case '%':
                if (*s == '%') s++;
                else goto done;
                break;
            default:
                goto done;
            }
            fmt++;
        } else if (isspace((unsigned char)*fmt)) {
            while (isspace((unsigned char)*s)) s++;
            fmt++;
        } else {
            if (*s == *fmt) { s++; fmt++; }
            else goto done;
        }
    }

done:
    va_end(ap);
    return count;
}

/* ======================================================================== */
/*  stdio stubs                                                             */
/* ======================================================================== */

struct _FILE {
    int dummy;
};

static struct _FILE _stdin_file  = {0};
static struct _FILE _stdout_file = {0};
static struct _FILE _stderr_file = {0};

FILE *stdin  = &_stdin_file;
FILE *stdout = &_stdout_file;
FILE *stderr = &_stderr_file;

int fprintf(FILE *stream, const char *fmt, ...) {
    (void)stream;
    /* For bare-metal, just format into a discard buffer */
    char tmp[256];
    va_list ap;
    va_start(ap, fmt);
    int ret = vsnprintf(tmp, sizeof(tmp), fmt, ap);
    va_end(ap);
    return ret;
}

int printf(const char *fmt, ...) {
    char tmp[256];
    va_list ap;
    va_start(ap, fmt);
    int ret = vsnprintf(tmp, sizeof(tmp), fmt, ap);
    va_end(ap);
    return ret;
}

int puts(const char *s) {
    (void)s;
    return 0;
}

int fflush(FILE *stream) {
    (void)stream;
    return 0;
}

int fclose(FILE *stream) {
    (void)stream;
    return 0;
}

int feof(FILE *stream) {
    (void)stream;
    return 1;
}

int ferror(FILE *stream) {
    (void)stream;
    return 0;
}

int fgetc(FILE *stream) {
    (void)stream;
    return EOF;
}

int ungetc(int c, FILE *stream) {
    (void)c; (void)stream;
    return EOF;
}

char *fgets(char *s, int n, FILE *stream) {
    (void)s; (void)n; (void)stream;
    return NULL;
}

size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    (void)ptr; (void)size; (void)nmemb; (void)stream;
    return 0;
}

size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream) {
    (void)ptr; (void)size; (void)nmemb; (void)stream;
    return nmemb; /* pretend success */
}

FILE *fopen(const char *path, const char *mode) {
    (void)path; (void)mode;
    return NULL;
}

FILE *freopen(const char *path, const char *mode, FILE *stream) {
    (void)path; (void)mode; (void)stream;
    return NULL;
}

int fseek(FILE *stream, long offset, int whence) {
    (void)stream; (void)offset; (void)whence;
    return -1;
}

long ftell(FILE *stream) {
    (void)stream;
    return -1;
}

void clearerr(FILE *stream) {
    (void)stream;
}

int setvbuf(FILE *stream, char *buf, int mode, size_t size) {
    (void)stream; (void)buf; (void)mode; (void)size;
    return 0;
}

int rename(const char *oldpath, const char *newpath) {
    (void)oldpath; (void)newpath;
    return -1;
}

int remove(const char *path) {
    (void)path;
    return -1;
}

char *tmpnam(char *s) {
    (void)s;
    return NULL;
}

FILE *tmpfile(void) {
    return NULL;
}

int getc(FILE *stream) {
    (void)stream;
    return EOF;
}

int putchar(int c) {
    (void)c;
    return c;
}

void perror(const char *s) {
    (void)s;
}

/* ======================================================================== */
/*  locale stubs                                                            */
/* ======================================================================== */

static char _decimal_point[] = ".";
static char _empty_str[] = "";

static struct lconv _lconv = {
    .decimal_point     = _decimal_point,
    .thousands_sep     = _empty_str,
    .grouping          = _empty_str,
    .int_curr_symbol   = _empty_str,
    .currency_symbol   = _empty_str,
    .mon_decimal_point = _empty_str,
    .mon_thousands_sep = _empty_str,
    .mon_grouping      = _empty_str,
    .positive_sign     = _empty_str,
    .negative_sign     = _empty_str,
    .int_frac_digits   = 127,
    .frac_digits       = 127,
    .p_cs_precedes     = 127,
    .p_sep_by_space    = 127,
    .n_cs_precedes     = 127,
    .n_sep_by_space    = 127,
    .p_sign_posn       = 127,
    .n_sign_posn       = 127,
};

struct lconv *localeconv(void) {
    return &_lconv;
}

char *setlocale(int category, const char *locale) {
    (void)category; (void)locale;
    return NULL;
}

/* ======================================================================== */
/*  math functions                                                          */
/*                                                                          */
/*  Lua 5.4 with LUA_32BITS uses float (lua_Number) but some internal code */
/*  still references double variants.  We provide both.                     */
/*  We use GCC/Clang __builtin_ intrinsics which the compiler can lower to */
/*  hardware FPU instructions or soft-float library calls as appropriate.   */
/* ======================================================================== */

double floor(double x) { return __builtin_floor(x); }
double ceil(double x) { return __builtin_ceil(x); }
double fabs(double x) { return __builtin_fabs(x); }
double sqrt(double x) { return __builtin_sqrt(x); }
double sin(double x) { return __builtin_sin(x); }
double cos(double x) { return __builtin_cos(x); }
double tan(double x) { return __builtin_tan(x); }
double asin(double x) { return __builtin_asin(x); }
double acos(double x) { return __builtin_acos(x); }
double atan(double x) { return __builtin_atan(x); }
double atan2(double y, double x) { return __builtin_atan2(y, x); }
double pow(double x, double y) { return __builtin_pow(x, y); }
double fmod(double x, double y) { return __builtin_fmod(x, y); }
double exp(double x) { return __builtin_exp(x); }
double log(double x) { return __builtin_log(x); }
double log2(double x) { return __builtin_log2(x); }
double log10(double x) { return __builtin_log10(x); }
double ldexp(double x, int n) { return __builtin_ldexp(x, n); }
double frexp(double x, int *exp) { return __builtin_frexp(x, exp); }

double modf(double x, double *iptr) {
    double i = __builtin_floor(x);
    *iptr = i;
    return x - i;
}

float floorf(float x) { return __builtin_floorf(x); }
float ceilf(float x) { return __builtin_ceilf(x); }
float fabsf(float x) { return __builtin_fabsf(x); }
float sqrtf(float x) { return __builtin_sqrtf(x); }
float sinf(float x) { return __builtin_sinf(x); }
float cosf(float x) { return __builtin_cosf(x); }
float tanf(float x) { return __builtin_tanf(x); }
float asinf(float x) { return __builtin_asinf(x); }
float acosf(float x) { return __builtin_acosf(x); }
float atanf(float x) { return __builtin_atanf(x); }
float atan2f(float y, float x) { return __builtin_atan2f(y, x); }
float powf(float x, float y) { return __builtin_powf(x, y); }
float fmodf(float x, float y) { return __builtin_fmodf(x, y); }
float expf(float x) { return __builtin_expf(x); }
float logf(float x) { return __builtin_logf(x); }
float log2f(float x) { return __builtin_log2f(x); }
float log10f(float x) { return __builtin_log10f(x); }
float ldexpf(float x, int n) { return __builtin_ldexpf(x, n); }
float frexpf(float x, int *exp) { return __builtin_frexpf(x, exp); }

float modff(float x, float *iptr) {
    float i = __builtin_floorf(x);
    *iptr = i;
    return x - i;
}

/* ======================================================================== */
/*  signal stubs                                                            */
/* ======================================================================== */

_sig_func_ptr signal(int sig, _sig_func_ptr handler) {
    (void)sig; (void)handler;
    return SIG_DFL;
}

int raise(int sig) {
    (void)sig;
    return 0;
}

/* ======================================================================== */
/*  time stubs                                                              */
/* ======================================================================== */

time_t time(time_t *t) {
    if (t) *t = 0;
    return 0;
}

clock_t clock(void) {
    return 0;
}

double difftime(time_t t1, time_t t0) {
    return (double)(t1 - t0);
}

time_t mktime(struct tm *tm) {
    (void)tm;
    return (time_t)-1;
}

static struct tm _tm_buf;

struct tm *localtime(const time_t *timer) {
    (void)timer;
    memset(&_tm_buf, 0, sizeof(_tm_buf));
    return &_tm_buf;
}

struct tm *gmtime(const time_t *timer) {
    (void)timer;
    memset(&_tm_buf, 0, sizeof(_tm_buf));
    return &_tm_buf;
}

size_t strftime(char *s, size_t max, const char *fmt, const struct tm *tm) {
    (void)s; (void)max; (void)fmt; (void)tm;
    if (max > 0) s[0] = '\0';
    return 0;
}

/* ======================================================================== */
/*  setjmp / longjmp  (ARM Cortex-M assembly)                              */
/*                                                                          */
/*  setjmp saves: r4-r11, sp (r13), lr (r14) = 10 registers                */
/*  longjmp restores them and returns the given value.                      */
/*                                                                          */
/*  Note: These are defined as naked functions with inline assembly.        */
/*  For Thumb-2 (Cortex-M), we use .thumb_func.                            */
/* ======================================================================== */

__attribute__((naked))
int setjmp(jmp_buf env) {
    __asm__ volatile (
        /* Save r4-r11, then sp and lr separately (SP not allowed in stmia on ARMv8-M) */
        "stmia r0!, {r4-r11}\n"
        "mov   r2, sp\n"
        "str   r2, [r0, #0]\n"   /* save sp */
        "str   lr, [r0, #4]\n"   /* save lr */
        "movs  r0, #0\n"
        "bx    lr\n"
    );
}

__attribute__((naked, noreturn))
void longjmp(jmp_buf env, int val) {
    __asm__ volatile (
        /* Restore r4-r11, then sp and lr separately */
        "ldmia r0!, {r4-r11}\n"
        "ldr   r2, [r0, #0]\n"   /* restore sp */
        "mov   sp, r2\n"
        "ldr   r2, [r0, #4]\n"   /* restore lr */
        "mov   lr, r2\n"
        "movs  r0, r1\n"
        "it    eq\n"              /* if val == 0, set it to 1 */
        "moveq r0, #1\n"
        "bx    lr\n"
    );
}
