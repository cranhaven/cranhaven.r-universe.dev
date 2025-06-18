# libimath

<!-- badges: start -->
[![R-CMD-check](https://github.com/tylermorganwall/libimath/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tylermorganwall/libimath/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Static build of Academy Software Foundation's [`Imath`](https://github.com/AcademySoftwareFoundation/Imath) C++ library for R. `Imath` is a basic, light-weight, and efficient C++ representation of 2D and 3D vectors, matrices, and other mathematical objects, functions, and data types common in computer graphics applications, including the `half` 16-bit floating-point type. It is not a generic linear algebra library (such as Eigen), but rather one specialized for 2D and 3D transformations common in computer graphics.

The installed package includes a **static** copy of the C++ library (along with CMake config files) so you can use Imath functionality in your package without requiring the user to separately install `Imath` as a system dependency. This package does not provide an R API--rather, it simply makes the `Imath` C++ library available to other R packages.

`Imath` is maintained by the OpenEXR project, a part of the Academy Software Foundation (ASWF).

## Features

The `Imath` library provides:

- **Vectors**: 2D, 3D, and 4D vector representations and operations
- **Matrices**: 2x2, 3x3, and 4x4 matrix operations
- **Half Float**: 16-bit floating point type and operations
- **Quaternions**: Quaternion operations for rotations
- **Bounding Boxes**: 2D and 3D bounding box representations
- **Colors**: RGB and RGBA color representations
- **Euler Angles**: Angle representation operations
- **Transformations**: Functions for coordinate transformations

For detailed information on the `Imath` API, please refer to the [Imath documentation](https://imath.readthedocs.io/).

---

## Installation

```r
# once released on CRAN
install.packages("tylermorganwall/libimath")

# development version
remotes::install_github("tylermorganwall/libimath")
```

No external libraries are required—the `Imath` static library is built and installed during the R package install.

---

## Using the bundled static library in your own packages

The package installs:

```
lib/<R_ARCH>/libImath-3_2.a        # static archive (version 3.2)
lib/<R_ARCH>/cmake/Imath/*         # CMake config files
include/Imath/*                    # public headers
```

`R_ARCH` can be obtained in R via `Sys.info()[["machine"]]`.

### Makevars-style linkage

The version (3_2) is appended after the library name: add this to link the static library.

```make
## configure
IMATH_DIR=$(Rscript -e 'cat(system.file("lib", Sys.info()[["machine"]], package = "libimath"))')
CPPFLAGS += -I$(IMATH_DIR)/../include
PKG_LIBS += -L$(IMATH_DIR) -lImath-3_2
```

### CMake consumers

Call this R code in your configure step to determine the location of the CMake config files:

```r
IMATH_LIB_ARCH = normalizePath(sprintf(
  "%s/%s",
  system.file(
    "lib",
    package = "libimath",
    mustWork = TRUE
  ),
  Sys.info()[["machine"]]
))

IMATH_CMAKE_CONFIG = file.path(IMATH_LIB_ARCH, "cmake", "Imath")
```

### Minimal example

Below is a minimal example showing how to use this library with R.

```c++
#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Print.h> 

#include <Imath/ImathMatrix.h>
#include <Imath/ImathVec.h>

/**
 * Rotates a 3D point using Imath's matrix operations
 * 
 * @param point An R numeric vector with 3 elements (x, y, z)
 * @param angles An R numeric vector with 3 elements (rotation angles in radians)
 * @return The rotated point as an R numeric vector
 */
extern "C" SEXP imath_rotate_point(SEXP point, SEXP angles) {
    // Validate inputs
    if (TYPEOF(point) != REALSXP || LENGTH(point) != 3) {
        Rf_error("'point' must be a numeric vector of length 3");
    }
    if (TYPEOF(angles) != REALSXP || LENGTH(angles) != 3) {
        Rf_error("'angles' must be a numeric vector of length 3");
    }
    
    // Extract point coordinates
    double *point_ptr = REAL(point);
    Imath::V3f p((float)point_ptr[0], (float)point_ptr[1], (float)point_ptr[2]);
    
    // Extract rotation angles
    double *angles_ptr = REAL(angles);
    Imath::V3f rot((float)angles_ptr[0], (float)angles_ptr[1], (float)angles_ptr[2]);
    
    // Create identity matrix
    Imath::M44f M;
    M.makeIdentity();
    
    // Create rotation matrix
    Imath::M44f R;
    R.makeIdentity();
    R.rotate(rot);
    
    // Apply rotation
    M = R * M;
    
    // Transform the point
    Imath::V3f result;
    M.multVecMatrix(p, result);
    
    // Create and populate result vector
    SEXP r_result = PROTECT(Rf_allocVector(REALSXP, 3));
    REAL(r_result)[0] = (double)result.x;
    REAL(r_result)[1] = (double)result.y;
    REAL(r_result)[2] = (double)result.z;
    
    // Add names to result vector for R
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 3));
    SET_STRING_ELT(names, 0, Rf_mkChar("x"));
    SET_STRING_ELT(names, 1, Rf_mkChar("y"));
    SET_STRING_ELT(names, 2, Rf_mkChar("z"));
    Rf_setAttrib(r_result, R_NamesSymbol, names);
    
    UNPROTECT(2);
    return r_result;
}

//=== registration ===========================================================

static const R_CallMethodDef CallEntries[] = {
    {"imath_rotate_point", (DL_FUNC) &imath_rotate_point, 2},
    {NULL, NULL, 0}
};

void R_init_libimathwrapper(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

```

And the R code calling this function:

```r
#' Rotate Point
#'
#' @param point A length-3 numeric vector (x, y, z)
#' @param angles A length-3 numeric vector (rotation angles in radians)
#' @return The rotated point as an R numeric vector
#' @export
#' @examples
#' # This rotates a point around an angle.
#' point = c(1.0, 0.0, 0.0)
#' angles = c(0.0, pi/4, 0.0)
#' imath_rotate_point(point, angles)
imath_rotate_point = function(point, angles) {
  rotated = .Call(
    "imath_rotate_point",
    point,
    angles,
    PACKAGE = "libimath"
  )
  return(rotated)
}
```
