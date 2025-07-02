# libdeflate

<!-- badges: start -->
[![R-CMD-check](https://github.com/tylermorganwall/libdeflate/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tylermorganwall/libdeflate/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

High‑performance DEFLATE compression for R, wrapping Eric Biggers’ [`libdeflate`](https://github.com/ebiggers/libdeflate) C library.

The installed package includes a **static** copy of the C library (along with CMake config files) so you can use DEFLATE compression in your package without requiring the user to separately install libdeflate as a system dependency. The package also includes a minimal R API that allows for compression and decompression of raw vectors, which can serve as an implementation reference for any downstream libraries.

---

## Installation

```r
# once released on CRAN
install.packages("libdeflate")

# development version
remotes::install_github("tylermorganwall/libdeflate")
```

No external libraries are required—the libdeflate static library is built and installed during the R package install.

---

## Example


``` r
library(libdeflate)

raw_in  = charToRaw("Example data payload, 123412341234123412341234")

print(raw_in)
```

```
##  [1] 45 78 61 6d 70 6c 65 20 64 61 74 61 20 70 61 79 6c 6f 61 64 2c 20 31 32 33 34 31 32 33 34 31 32 33 34 31 32 33 34
## [39] 31 32 33 34 31 32 33 34
```

``` r
length(raw_in)
```

```
## [1] 46
```

``` r
cmp     = alloc_compressor(level = 6L)      # create compressor @ default level
raw_cmp = deflate_compress(cmp, raw_in)     # compress

print(raw_cmp)
```

```
##  [1] 73 ad 48 cc 2d c8 49 55 48 49 2c 49 54 28 48 ac cc c9 4f 4c d1 51 30 34 32 36 c1 86 01
```

``` r
length(raw_cmp)
```

```
## [1] 29
```

``` r
dcmp    = alloc_decompressor()              # create decompressor
raw_out = deflate_decompress(dcmp, raw_cmp, length(raw_in))

print(raw_out)
```

```
##  [1] 45 78 61 6d 70 6c 65 20 64 61 74 61 20 70 61 79 6c 6f 61 64 2c 20 31 32 33 34 31 32 33 34 31 32 33 34 31 32 33 34
## [39] 31 32 33 34 31 32 33 34
```

``` r
print(rawToChar(raw_out)) # round‑trip successful
```

```
## [1] "Example data payload, 123412341234123412341234"
```

---

## R API

| Function | Purpose | Key arguments |
|----------|---------|---------------|
| `alloc_compressor(level = 6L)` | Allocate a compression context. | `level` ∈ **0…12** *(0 = none, 6 = default, 12 = max)* |
| `deflate_compress(compressor, input)` | Compress a `raw` vector. | `compressor`, `input` *(coerced to raw)* |
| `alloc_decompressor()` | Allocate a decompression context. | – |
| `deflate_decompress(decompressor, input, out_len)` | Inflate a DEFLATE stream. | `decompressor`, `input`, `out_len` *(expected size)* |

All four functions are simple `.Call()` wrappers around the C API; see the `@examples` in their help pages for typical workflows.

---

## Using the bundled static library in your own packages

The package installs

```
lib/<R_ARCH>/libdeflate.a        # static archive
lib/<R_ARCH>/cmake/libdeflate/*  # CMake config files
include/libdeflate.h             # public header
```

`R_ARCH` can be obtained in R via `Sys.info()[["machine"]]`.

### Makevars‑style linkage

```make
## configure
DEFLATE_DIR=$(Rscript -e 'cat(system.file("lib", Sys.info()[["machine"]], package = "libdeflate"))')
CPPFLAGS += -I$(DEFLATE_DIR)/../include
PKG_LIBS += -L$(DEFLATE_DIR) -ldeflate
```

### CMake consumers

Call this R code in your configure step to determine the location of the CMake config files:

```r
DEFLATE_LIB_ARCH = normalizePath(sprintf(
  "%s/%s",
  system.file(
    "lib",
    package = "libdeflate",
    mustWork = TRUE
  ),
  Sys.info()[["machine"]]
))

DEFLATE_CMAKE_CONFIG = file.path(DEFLATE_LIB_ARCH, "cmake", "libdeflate")
```

---

## Minimal C wrapper (see r-api.c in package source)

```c
// File: src/r-api.c
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <libdeflate.h>

SEXP C_alloc_compressor(SEXP level_SEXP) {
    int level = INTEGER(level_SEXP)[0];
    struct libdeflate_compressor *c = libdeflate_alloc_compressor(level);
    if(!c) Rf_error("libdeflate_alloc_compressor(%d) failed", level);
    SEXP ext = PROTECT(R_MakeExternalPtr(c, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(ext, compressor_finalizer, TRUE);
    UNPROTECT(1);
    return ext;
}

```
