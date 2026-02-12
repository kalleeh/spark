/* Minimal stddef.h shim for bare-metal ARM Cortex-M */
#ifndef _COMPAT_STDDEF_H
#define _COMPAT_STDDEF_H

typedef unsigned int size_t;
typedef int          ptrdiff_t;
typedef int          wchar_t;

#ifndef NULL
#define NULL ((void *)0)
#endif

#define offsetof(type, member) __builtin_offsetof(type, member)

#endif /* _COMPAT_STDDEF_H */
