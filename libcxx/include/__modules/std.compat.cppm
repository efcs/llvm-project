// -*- C++ -*-
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//


module;

#include <__config>

// The headers of Table 24: C++ library headers [tab:headers.cpp]
// and the headers of Table 25: C++ headers for C library facilities [tab:headers.cpp.c]
#include <cassert>
#include <cctype>
#include <cerrno>
#include <cfenv>
#include <cfloat>
#include <cinttypes>
#include <climits>
#if !defined(_LIBCPP_HAS_NO_LOCALIZATION)
#  include <clocale>
#endif
#include <cmath>
#include <csetjmp>
#include <csignal>
#include <cstdarg>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <cuchar>
#if !defined(_LIBCPP_HAS_NO_WIDE_CHARACTERS)
#  include <cwchar>
#endif
#if !defined(_LIBCPP_HAS_NO_WIDE_CHARACTERS)
#  include <cwctype>
#endif

// *** Headers not yet available ***
//
// TODO(EricWF) Come up with enforcement mechanism that this file is updated
// Ideally we just run the tests under modules.

export module std.compat;
export import std;


export {

//===----------------------------------------------------------------------===//
//                              <cstddef>
//===----------------------------------------------------------------------===//
  using ::max_align_t;
  using ::nullptr_t;
  using ::ptrdiff_t;
  using ::size_t;

  // [support.c.headers]/1
  // ...  placed within the global namespace scope, except for ... the
  // declaration of std::byte ([cstddef.syn]), and the functions and
  // function templates described in [support.types.byteops]. ...

  // [support.types.byteops], byte type operations

//===----------------------------------------------------------------------===//
//                              <cfenv>
//===----------------------------------------------------------------------===//
  // types
  using ::fenv_t;
  using ::fexcept_t;

  // functions
  using ::feclearexcept;
  using ::fegetexceptflag;
  using ::feraiseexcept;
  using ::fesetexceptflag;
  using ::fetestexcept;

  using ::fegetround;
  using ::fesetround;

  using ::fegetenv;
  using ::feholdexcept;
  using ::fesetenv;
  using ::feupdateenv;

//===----------------------------------------------------------------------===//
//                              <ctime>
//===----------------------------------------------------------------------===//
  using ::clock_t;
  using ::size_t;
  using ::time_t;

  using ::timespec;
  using ::tm;

  using ::asctime;
  using ::clock;
  using ::ctime;
  using ::difftime;
  using ::gmtime;
  using ::localtime;
  using ::mktime;
  using ::strftime;
  using ::time;
  using ::timespec_get _LIBCPP_USING_IF_EXISTS;

//===----------------------------------------------------------------------===//
//                              <cstring>
//===----------------------------------------------------------------------===//
  using ::size_t;

  using ::memchr;
  using ::memcmp;
  using ::memcpy;
  using ::memmove;
  using ::memset;
  using ::strcat;
  using ::strchr;
  using ::strcmp;
  using ::strcoll;
  using ::strcpy;
  using ::strcspn;
  using ::strerror;
  using ::strlen;
  using ::strncat;
  using ::strncmp;
  using ::strncpy;
  using ::strpbrk;
  using ::strrchr;
  using ::strspn;
  using ::strstr;
  using ::strtok;
  using ::strxfrm;

//===----------------------------------------------------------------------===//
//                            <climits>
//===----------------------------------------------------------------------===//
  // This module exports nothing.

//===----------------------------------------------------------------------===//
//                              <clocale>
//===----------------------------------------------------------------------===//
#ifndef _LIBCPP_HAS_NO_LOCALIZATION
  using ::lconv;

  using ::localeconv;
  using ::setlocale;
#endif // _LIBCPP_HAS_NO_LOCALIZATION

//===----------------------------------------------------------------------===//
//                              <csignal>
//===----------------------------------------------------------------------===//
  using ::sig_atomic_t;

  // [support.signal], signal handlers
  using ::signal;

  using ::raise;

//===----------------------------------------------------------------------===//
//                              <cstdlib>
//===----------------------------------------------------------------------===//
  using ::div_t;
  using ::ldiv_t;
  using ::lldiv_t;
  using ::size_t;

  // [support.start.term], start and termination
  using ::_Exit;
  using ::abort;
  using ::at_quick_exit _LIBCPP_USING_IF_EXISTS;
  using ::atexit;
  using ::exit;
  using ::quick_exit _LIBCPP_USING_IF_EXISTS;

  using ::getenv;
  using ::system;

  // [c.malloc], C library memory allocation
  using ::aligned_alloc;
  using ::calloc;
  using ::free;
  using ::malloc;
  using ::realloc;

  using ::atof;
  using ::atoi;
  using ::atol;
  using ::atoll;
  using ::strtod;
  using ::strtof;
  using ::strtol;
  using ::strtold;
  using ::strtoll;
  using ::strtoul;
  using ::strtoull;

  // [c.mb.wcs], multibyte / wide string and character conversion functions
  using ::mblen;
#ifndef _LIBCPP_HAS_NO_WIDE_CHARACTERS
  using ::mbstowcs;
  using ::mbtowc;
  using ::wcstombs;
  using ::wctomb;
#endif
  // [alg.c.library], C standard library algorithms
  using ::bsearch;
  using ::qsort;

  // [c.math.rand], low-quality random number generation
  using ::rand;
  using ::srand;

  // [c.math.abs], absolute values
  using ::abs;

  using ::labs;
  using ::llabs;

  using ::div;
  using ::ldiv;
  using ::lldiv;

//===----------------------------------------------------------------------===//
//                              <cwctype>
//===----------------------------------------------------------------------===//
#ifndef _LIBCPP_HAS_NO_WIDE_CHARACTERS
  using ::wctrans_t;
  using ::wctype_t;
  using ::wint_t;

  using ::iswalnum;
  using ::iswalpha;
  using ::iswblank;
  using ::iswcntrl;
  using ::iswctype;
  using ::iswdigit;
  using ::iswgraph;
  using ::iswlower;
  using ::iswprint;
  using ::iswpunct;
  using ::iswspace;
  using ::iswupper;
  using ::iswxdigit;
  using ::towctrans;
  using ::towlower;
  using ::towupper;
  using ::wctrans;
  using ::wctype;
#endif // _LIBCPP_HAS_NO_WIDE_CHARACTERS

//===----------------------------------------------------------------------===//
//                              <csetjmp>
//===----------------------------------------------------------------------===//
  using ::jmp_buf;
  using ::longjmp;

//===----------------------------------------------------------------------===//
//                              <cctype>
//===----------------------------------------------------------------------===//
  using ::isalnum;
  using ::isalpha;
  using ::isblank;
  using ::iscntrl;
  using ::isdigit;
  using ::isgraph;
  using ::islower;
  using ::isprint;
  using ::ispunct;
  using ::isspace;
  using ::isupper;
  using ::isxdigit;
  using ::tolower;
  using ::toupper;

//===----------------------------------------------------------------------===//
//                              <cfloat>
//===----------------------------------------------------------------------===//
// This module exports nothing.

//===----------------------------------------------------------------------===//
//                              <cwchar>
//===----------------------------------------------------------------------===//
#ifndef _LIBCPP_HAS_NO_WIDE_CHARACTERS
  using ::mbstate_t;
  using ::size_t;
  using ::wint_t;

  using ::tm;

  using ::btowc;
  using ::fgetwc;
  using ::fgetws;
  using ::fputwc;
  using ::fputws;
  using ::fwide;
  using ::fwprintf;
  using ::fwscanf;
  using ::getwc;
  using ::getwchar;
  using ::putwc;
  using ::putwchar;
  using ::swprintf;
  using ::swscanf;
  using ::ungetwc;
  using ::vfwprintf;
  using ::vfwscanf;
  using ::vswprintf;
  using ::vswscanf;
  using ::vwprintf;
  using ::vwscanf;
  using ::wcscat;
  using ::wcschr;
  using ::wcscmp;
  using ::wcscoll;
  using ::wcscpy;
  using ::wcscspn;
  using ::wcsftime;
  using ::wcslen;
  using ::wcsncat;
  using ::wcsncmp;
  using ::wcsncpy;
  using ::wcspbrk;
  using ::wcsrchr;
  using ::wcsspn;
  using ::wcsstr;
  using ::wcstod;
  using ::wcstof;
  using ::wcstok;
  using ::wcstol;
  using ::wcstold;
  using ::wcstoll;
  using ::wcstoul;
  using ::wcstoull;
  using ::wcsxfrm;
  using ::wctob;
  using ::wmemchr;
  using ::wmemcmp;
  using ::wmemcpy;
  using ::wmemmove;
  using ::wmemset;
  using ::wprintf;
  using ::wscanf;

  // [c.mb.wcs], multibyte / wide string and character conversion functions
  using ::mbrlen;
  using ::mbrtowc;
  using ::mbsinit;
  using ::mbsrtowcs;
  using ::wcrtomb;
  using ::wcsrtombs;
#endif // _LIBCPP_HAS_NO_WIDE_CHARACTERS

//===----------------------------------------------------------------------===//
//                              <cstdio>
//===----------------------------------------------------------------------===//
  using ::FILE;
  using ::fpos_t;
  using ::size_t;

  using ::clearerr;
  using ::fclose;
  using ::feof;
  using ::ferror;
  using ::fflush;
  using ::fgetc;
  using ::fgetpos;
  using ::fgets;
  using ::fopen;
  using ::fprintf;
  using ::fputc;
  using ::fputs;
  using ::fread;
  using ::freopen;
  using ::fscanf;
  using ::fseek;
  using ::fsetpos;
  using ::ftell;
  using ::fwrite;
  using ::getc;
  using ::getchar;
  using ::perror;
  using ::printf;
  using ::putc;
  using ::putchar;
  using ::puts;
  using ::remove;
  using ::rename;
  using ::rewind;
  using ::scanf;
  using ::setbuf;
  using ::setvbuf;
  using ::snprintf;
  using ::sprintf;
  using ::sscanf;
  using ::tmpfile;
  using ::tmpnam;
  using ::ungetc;
  using ::vfprintf;
  using ::vfscanf;
  using ::vprintf;
  using ::vscanf;
  using ::vsnprintf;
  using ::vsprintf;
  using ::vsscanf;

//===----------------------------------------------------------------------===//
//                             <cstdint>
//===----------------------------------------------------------------------===//
  // signed
  using ::int8_t _LIBCPP_USING_IF_EXISTS;
  using ::int16_t _LIBCPP_USING_IF_EXISTS;
  using ::int32_t _LIBCPP_USING_IF_EXISTS;
  using ::int64_t _LIBCPP_USING_IF_EXISTS;

  using ::int_fast16_t;
  using ::int_fast32_t;
  using ::int_fast64_t;
  using ::int_fast8_t;

  using ::int_least16_t;
  using ::int_least32_t;
  using ::int_least64_t;
  using ::int_least8_t;

  using ::intmax_t;

  using ::intptr_t _LIBCPP_USING_IF_EXISTS;

  // unsigned
  using ::uint8_t _LIBCPP_USING_IF_EXISTS;
  using ::uint16_t _LIBCPP_USING_IF_EXISTS;
  using ::uint32_t _LIBCPP_USING_IF_EXISTS;
  using ::uint64_t _LIBCPP_USING_IF_EXISTS;

  using ::uint_fast16_t;
  using ::uint_fast32_t;
  using ::uint_fast64_t;
  using ::uint_fast8_t;

  using ::uint_least16_t;
  using ::uint_least32_t;
  using ::uint_least64_t;
  using ::uint_least8_t;

  using ::uintmax_t;

  using ::uintptr_t _LIBCPP_USING_IF_EXISTS;

//===----------------------------------------------------------------------===//
//                              <cerrno>
//===----------------------------------------------------------------------===//

// This module exports nothing.

//===----------------------------------------------------------------------===//
//                              <cuchar>
//===----------------------------------------------------------------------===//

#if !defined(_LIBCPP_HAS_NO_C8RTOMB_MBRTOC8)
  using ::mbrtoc8 _LIBCPP_USING_IF_EXISTS;
  using ::c8rtomb _LIBCPP_USING_IF_EXISTS;
#endif
  using ::mbrtoc16 _LIBCPP_USING_IF_EXISTS;
  using ::c16rtomb _LIBCPP_USING_IF_EXISTS;
  using ::mbrtoc32 _LIBCPP_USING_IF_EXISTS;
  using ::c32rtomb _LIBCPP_USING_IF_EXISTS;

//===----------------------------------------------------------------------===//
//                            <cinttypes>
//===----------------------------------------------------------------------===//

  using ::imaxdiv_t;

  using ::imaxabs;
  using ::imaxdiv;
  using ::strtoimax;
  using ::strtoumax;
  using ::wcstoimax;
  using ::wcstoumax;

  // abs is conditionally here, but always present in <cmath>. To avoid
  // conflicting declarations, omit the 'using' directive here.

  // div is conditionally here, but always present in <cstdlib>. To avoid
  // conflicting declarations, omit the 'using' directive here.

//===----------------------------------------------------------------------===//
//                               <cstdarg>
//===----------------------------------------------------------------------===//
  using ::va_list;

//===----------------------------------------------------------------------===//
//                               <cmath>
//===----------------------------------------------------------------------===//

  using ::double_t;
  using ::float_t;

  using ::acos;
  using ::acosf;
  using ::acosl;

  using ::asin;
  using ::asinf;
  using ::asinl;

  using ::atan;
  using ::atanf;
  using ::atanl;

  using ::atan2;
  using ::atan2f;
  using ::atan2l;

  using ::cos;
  using ::cosf;
  using ::cosl;

  using ::sin;
  using ::sinf;
  using ::sinl;

  using ::tan;
  using ::tanf;
  using ::tanl;

  using ::acosh;
  using ::acoshf;
  using ::acoshl;

  using ::asinh;
  using ::asinhf;
  using ::asinhl;

  using ::atanh;
  using ::atanhf;
  using ::atanhl;

  using ::cosh;
  using ::coshf;
  using ::coshl;

  using ::sinh;
  using ::sinhf;
  using ::sinhl;

  using ::tanh;
  using ::tanhf;
  using ::tanhl;

  using ::exp;
  using ::expf;
  using ::expl;

  using ::exp2;
  using ::exp2f;
  using ::exp2l;

  using ::expm1;
  using ::expm1f;
  using ::expm1l;

  using ::frexp;
  using ::frexpf;
  using ::frexpl;

  using ::ilogb;
  using ::ilogbf;
  using ::ilogbl;

  using ::ldexp;
  using ::ldexpf;
  using ::ldexpl;

  using ::log;
  using ::logf;
  using ::logl;

  using ::log10;
  using ::log10f;
  using ::log10l;

  using ::log1p;
  using ::log1pf;
  using ::log1pl;

  using ::log2;
  using ::log2f;
  using ::log2l;

  using ::logb;
  using ::logbf;
  using ::logbl;

  using ::modf;
  using ::modff;
  using ::modfl;

  using ::scalbn;
  using ::scalbnf;
  using ::scalbnl;

  using ::scalbln;
  using ::scalblnf;
  using ::scalblnl;

  using ::cbrt;
  using ::cbrtf;
  using ::cbrtl;

  using ::abs;

  using ::fabs;
  using ::fabsf;
  using ::fabsl;

  using ::hypot;
  using ::hypotf;
  using ::hypotl;

  using ::pow;
  using ::powf;
  using ::powl;

  using ::sqrt;
  using ::sqrtf;
  using ::sqrtl;

  using ::erf;
  using ::erff;
  using ::erfl;

  using ::erfc;
  using ::erfcf;
  using ::erfcl;

  using ::lgamma;
  using ::lgammaf;
  using ::lgammal;

  using ::tgamma;
  using ::tgammaf;
  using ::tgammal;

  using ::ceil;
  using ::ceilf;
  using ::ceill;

  using ::floor;
  using ::floorf;
  using ::floorl;

  using ::nearbyint;
  using ::nearbyintf;
  using ::nearbyintl;

  using ::rint;
  using ::rintf;
  using ::rintl;

  using ::lrint;
  using ::lrintf;
  using ::lrintl;

  using ::llrint;
  using ::llrintf;
  using ::llrintl;

  using ::round;
  using ::roundf;
  using ::roundl;

  using ::lround;
  using ::lroundf;
  using ::lroundl;

  using ::llround;
  using ::llroundf;
  using ::llroundl;

  using ::trunc;
  using ::truncf;
  using ::truncl;

  using ::fmod;
  using ::fmodf;
  using ::fmodl;

  using ::remainder;
  using ::remainderf;
  using ::remainderl;

  using ::remquo;
  using ::remquof;
  using ::remquol;

  using ::copysign;
  using ::copysignf;
  using ::copysignl;

  using ::nan;
  using ::nanf;
  using ::nanl;

  using ::nextafter;
  using ::nextafterf;
  using ::nextafterl;

  using ::nexttoward;
  using ::nexttowardf;
  using ::nexttowardl;

  using ::fdim;
  using ::fdimf;
  using ::fdiml;

  using ::fmax;
  using ::fmaxf;
  using ::fmaxl;

  using ::fmin;
  using ::fminf;
  using ::fminl;

  using ::fma;
  using ::fmaf;
  using ::fmal;

  using ::fpclassify;
  using ::isfinite;
  using ::isgreater;
  using ::isgreaterequal;
  using ::isinf;
  using ::isless;
  using ::islessequal;
  using ::islessgreater;
  using ::isnan;
  using ::isnormal;
  using ::isunordered;
  using ::signbit;

//===----------------------------------------------------------------------===//
// <cassert>
//===----------------------------------------------------------------------===//
// This module exports nothing.
}
