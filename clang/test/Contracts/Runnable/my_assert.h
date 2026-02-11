
#ifdef assert
#undef assert
#endif

#define assert(...) ((__VA_ARGS__) ? (void)0 : __handle_assert(__FILE__, __LINE__, #__VA_ARGS__))

#ifndef MY_ASSERT_H
#define MY_ASSERT_H
#include <stdio.h>
#include <stdlib.h>
inline void __handle_assert(const char* file, int line, const char* expr) {
    fprintf(stderr, "%s:%d: assertion \"%s\" failed\n", file, line, expr);
    __builtin_abort();
}

#endif // MY_ASSERT_H


