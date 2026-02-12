/*
 * luacompat.h - Bare-metal shim for Lua 5.4 on ARM (no libc/newlib)
 *
 * Provides minimal typedefs, macros, and declarations so Lua compiles
 * without a C standard library.  Real implementations live in luacompat.c.
 *
 * GCC builtins already supply: stdarg.h, stddef.h, stdint.h, limits.h, float.h
 * This file covers everything else Lua references.
 */

#ifndef LUACOMPAT_H
#define LUACOMPAT_H

/* ---- Pull in compiler-provided headers --------------------------------- */
#include <stddef.h>   /* size_t, ptrdiff_t, NULL */
#include <stdarg.h>   /* va_list, va_start, va_end, va_arg */
#include <stdint.h>   /* uint8_t, int32_t, intptr_t, ... */
#include <limits.h>   /* INT_MAX, LLONG_MAX, ... */
#include <float.h>    /* DBL_MAX, FLT_MAX, ... */

/* ======================================================================== */
/*  assert.h                                                                */
/* ======================================================================== */

#ifdef NDEBUG
#define assert(expr) ((void)0)
#else
extern void _assert_fail(const char *expr, const char *file, int line);
#define assert(expr) \
    ((expr) ? ((void)0) : _assert_fail(#expr, __FILE__, __LINE__))
#endif

/* ======================================================================== */
/*  errno.h                                                                 */
/* ======================================================================== */

#ifndef EDOM
#define EDOM   33
#endif
#ifndef ERANGE
#define ERANGE 34
#endif
#ifndef EILSEQ
#define EILSEQ 84
#endif

extern int errno;

/* ======================================================================== */
/*  ctype.h  (inline implementations)                                       */
/* ======================================================================== */

static inline int isdigit(int c)  { return (unsigned)c - '0' < 10; }
static inline int isxdigit(int c) {
    return isdigit(c)
        || ((unsigned)c | 32) - 'a' < 6;
}
static inline int isupper(int c)  { return (unsigned)c - 'A' < 26; }
static inline int islower(int c)  { return (unsigned)c - 'a' < 26; }
static inline int isalpha(int c)  { return ((unsigned)c | 32) - 'a' < 26; }
static inline int isalnum(int c)  { return isalpha(c) || isdigit(c); }
static inline int isspace(int c)  {
    return c == ' ' || (unsigned)c - '\t' < 5;   /* \t \n \v \f \r */
}
static inline int iscntrl(int c)  {
    return (unsigned)c < 0x20 || c == 0x7f;
}
static inline int ispunct(int c)  {
    return ((unsigned)c >= 0x21 && (unsigned)c <= 0x7e)
        && !isalnum(c);
}
static inline int isprint(int c)  {
    return (unsigned)c - 0x20 < 0x5f;
}
static inline int isgraph(int c)  {
    return (unsigned)c - 0x21 < 0x5e;
}
static inline int toupper(int c)  { return islower(c) ? c - 32 : c; }
static inline int tolower(int c)  { return isupper(c) ? c + 32 : c; }

/* ======================================================================== */
/*  locale.h                                                                */
/* ======================================================================== */

#define LC_ALL      0
#define LC_COLLATE  1
#define LC_CTYPE    2
#define LC_MONETARY 3
#define LC_NUMERIC  4
#define LC_TIME     5

struct lconv {
    char *decimal_point;
    char *thousands_sep;
    char *grouping;
    char *int_curr_symbol;
    char *currency_symbol;
    char *mon_decimal_point;
    char *mon_thousands_sep;
    char *mon_grouping;
    char *positive_sign;
    char *negative_sign;
    char  int_frac_digits;
    char  frac_digits;
    char  p_cs_precedes;
    char  p_sep_by_space;
    char  n_cs_precedes;
    char  n_sep_by_space;
    char  p_sign_posn;
    char  n_sign_posn;
};

char        *setlocale(int category, const char *locale);
struct lconv *localeconv(void);

/* ======================================================================== */
/*  math.h                                                                  */
/* ======================================================================== */

#define HUGE_VAL  __builtin_huge_val()
#define HUGE_VALF __builtin_huge_valf()
#ifndef INFINITY
#define INFINITY  __builtin_inff()
#endif
#ifndef NAN
#define NAN       __builtin_nanf("")
#endif

#define isinf(x)     __builtin_isinf(x)
#define isnan(x)     __builtin_isnan(x)
#define isfinite(x)  __builtin_isfinite(x)
#define signbit(x)   __builtin_signbit(x)

/* double variants */
double pow(double, double);
double sin(double);
double cos(double);
double tan(double);
double asin(double);
double acos(double);
double atan(double);
double atan2(double, double);
double sqrt(double);
double floor(double);
double ceil(double);
double fmod(double, double);
double exp(double);
double log(double);
double log2(double);
double log10(double);
double fabs(double);
double ldexp(double, int);
double frexp(double, int *);
double modf(double, double *);
double round(double);
double trunc(double);
double copysign(double, double);
double scalbn(double, int);

/* float variants */
float  powf(float, float);
float  sinf(float);
float  cosf(float);
float  tanf(float);
float  asinf(float);
float  acosf(float);
float  atanf(float);
float  atan2f(float, float);
float  sqrtf(float);
float  floorf(float);
float  ceilf(float);
float  fmodf(float, float);
float  expf(float);
float  logf(float);
float  log2f(float);
float  log10f(float);
float  fabsf(float);
float  ldexpf(float, int);
float  frexpf(float, int *);
float  modff(float, float *);
float  roundf(float);
float  truncf(float);
float  copysignf(float, float);
float  scalbnf(float, int);

/* ======================================================================== */
/*  setjmp.h  (use GCC builtins)                                            */
/* ======================================================================== */

/* ARM Cortex-M needs to save: r4-r11, sp, lr = 10 words minimum.
 * Add extra space for safety. */
typedef int jmp_buf[32];

int  setjmp(jmp_buf env);
void longjmp(jmp_buf env, int val) __attribute__((noreturn));

/* ======================================================================== */
/*  signal.h                                                                */
/* ======================================================================== */

#define SIGABRT  6
#define SIGFPE   8
#define SIGILL   4
#define SIGINT   2
#define SIGSEGV 11
#define SIGTERM 15

#define SIG_DFL  ((void (*)(int))0)
#define SIG_IGN  ((void (*)(int))1)
#define SIG_ERR  ((void (*)(int))-1)

typedef int sig_atomic_t;

typedef void (*_sig_func_ptr)(int);

_sig_func_ptr signal(int sig, _sig_func_ptr handler);
int           raise(int sig);

/* ======================================================================== */
/*  stdio.h  (minimal stubs -- Lua IO lib excluded but headers referenced)  */
/* ======================================================================== */

typedef struct _FILE FILE;

extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;

#ifndef EOF
#define EOF (-1)
#endif

#ifndef BUFSIZ
#define BUFSIZ 256
#endif

#ifndef SEEK_SET
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif

#define _IOFBF 0
#define _IOLBF 1
#define _IONBF 2

#ifndef FILENAME_MAX
#define FILENAME_MAX 256
#endif

#ifndef L_tmpnam
#define L_tmpnam 20
#endif

int  fprintf(FILE *stream, const char *fmt, ...);
int  fflush(FILE *stream);
int  fclose(FILE *stream);
int  feof(FILE *stream);
int  ferror(FILE *stream);
int  fgetc(FILE *stream);
int  ungetc(int c, FILE *stream);
char *fgets(char *s, int n, FILE *stream);
size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream);
size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream);
FILE *fopen(const char *path, const char *mode);
FILE *freopen(const char *path, const char *mode, FILE *stream);
int  fseek(FILE *stream, long offset, int whence);
long ftell(FILE *stream);
void clearerr(FILE *stream);
int  setvbuf(FILE *stream, char *buf, int mode, size_t size);
int  rename(const char *oldpath, const char *newpath);
int  remove(const char *path);
char *tmpnam(char *s);
FILE *tmpfile(void);
int  printf(const char *fmt, ...);
int  puts(const char *s);
int  getc(FILE *stream);
int  putchar(int c);
int  sprintf(char *str, const char *fmt, ...);
int  snprintf(char *str, size_t size, const char *fmt, ...);
int  vsnprintf(char *str, size_t size, const char *fmt, va_list ap);
int  sscanf(const char *str, const char *fmt, ...);
void perror(const char *s);

/* ======================================================================== */
/*  stdlib.h                                                                */
/* ======================================================================== */

#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS 0
#endif
#ifndef EXIT_FAILURE
#define EXIT_FAILURE 1
#endif

void *malloc(size_t size);
void *realloc(void *ptr, size_t size);
void  free(void *ptr);
void *calloc(size_t nmemb, size_t size);

void  abort(void) __attribute__((noreturn));
void  exit(int status) __attribute__((noreturn));
int   atexit(void (*func)(void));

int   atoi(const char *s);
long  atol(const char *s);
int   abs(int x);
long  labs(long x);

double          strtod(const char *nptr, char **endptr);
float           strtof(const char *nptr, char **endptr);
long            strtol(const char *nptr, char **endptr, int base);
unsigned long   strtoul(const char *nptr, char **endptr, int base);
long long       strtoll(const char *nptr, char **endptr, int base);
unsigned long long strtoull(const char *nptr, char **endptr, int base);

char *getenv(const char *name);
int   system(const char *command);

void  qsort(void *base, size_t nmemb, size_t size,
             int (*compar)(const void *, const void *));

/* ======================================================================== */
/*  string.h                                                                */
/* ======================================================================== */

void   *memcpy(void *dest, const void *src, size_t n);
void   *memmove(void *dest, const void *src, size_t n);
void   *memset(void *s, int c, size_t n);
int     memcmp(const void *s1, const void *s2, size_t n);
void   *memchr(const void *s, int c, size_t n);

size_t  strlen(const char *s);
char   *strcpy(char *dest, const char *src);
char   *strncpy(char *dest, const char *src, size_t n);
int     strcmp(const char *s1, const char *s2);
int     strcoll(const char *s1, const char *s2);
int     strncmp(const char *s1, const char *s2, size_t n);
char   *strcat(char *dest, const char *src);
char   *strncat(char *dest, const char *src, size_t n);
char   *strchr(const char *s, int c);
char   *strrchr(const char *s, int c);
char   *strstr(const char *haystack, const char *needle);
size_t  strspn(const char *s, const char *accept);
size_t  strcspn(const char *s, const char *reject);
char   *strerror(int errnum);
char   *strpbrk(const char *s, const char *accept);

/* ======================================================================== */
/*  time.h                                                                  */
/* ======================================================================== */

typedef long time_t;
typedef long clock_t;

#ifndef CLOCKS_PER_SEC
#define CLOCKS_PER_SEC 1000000L
#endif

struct tm {
    int tm_sec;
    int tm_min;
    int tm_hour;
    int tm_mday;
    int tm_mon;
    int tm_year;
    int tm_wday;
    int tm_yday;
    int tm_isdst;
};

time_t  time(time_t *t);
clock_t clock(void);
double  difftime(time_t t1, time_t t0);
time_t  mktime(struct tm *tm);
struct tm *localtime(const time_t *timer);
struct tm *gmtime(const time_t *timer);
size_t  strftime(char *s, size_t max, const char *fmt, const struct tm *tm);

/* ======================================================================== */

#endif /* LUACOMPAT_H */
