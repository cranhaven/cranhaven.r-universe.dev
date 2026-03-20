// R compatibility header for llamaR
// Redirects C stdio functions to R-safe equivalents
// This header is force-included via -include in Makevars

#ifndef R_LLAMA_COMPAT_H
#define R_LLAMA_COMPAT_H

#include <stdint.h>
#include <R.h>
#include <R_ext/Print.h>

// Override stderr/stdout to prevent direct usage
// R CMD check flags these as non-portable
#ifdef stderr
#undef stderr
#endif
#ifdef stdout
#undef stdout
#endif

// Sentinel file pointers for our fprintf/fputs wrappers below.
// Using a non-zero dummy address avoids -Wnonnull warnings from gcc
// when fprintf(stderr, ...) is expanded.
static FILE *const r_llama_dummy_stream_ = (FILE*)(void*)(intptr_t)1;
#define stderr r_llama_dummy_stream_
#define stdout r_llama_dummy_stream_

// Wrapper for fprintf to stderr -> REprintf
#define fprintf(stream, ...) \
    ((stream == r_llama_dummy_stream_) ? (REprintf(__VA_ARGS__), 0) : fprintf(stream, __VA_ARGS__))

// Wrapper for fputs to stderr -> REprintf
#define fputs(str, stream) \
    ((stream == r_llama_dummy_stream_) ? (REprintf("%s", str), 0) : fputs(str, stream))

// fflush is a no-op for our dummy streams
#define fflush(stream) \
    ((stream == r_llama_dummy_stream_) ? 0 : fflush(stream))

// Override exit/_Exit to prevent process termination (CRAN requirement)
static inline void r_llama_exit(int status) {
    Rf_error("llama: exit called with status %d", status);
    while(1) {} // Rf_error never returns, but silence compiler warnings
}

#undef exit
#define exit(status) r_llama_exit(status)

#undef _Exit
#define _Exit(status) r_llama_exit(status)

#endif // R_LLAMA_COMPAT_H
