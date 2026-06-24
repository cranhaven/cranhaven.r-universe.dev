# eummd


## eummd 0.2.0, October 2024

  - Fixed bug with meammd, and computation of p-value.

## eummd 0.1.8, May 2024

  - Added parameters `allowzeropval` and `alternative` 
    for functions that compute p-values
  - Fixed internal C++ function for merging two sorted vectors

## eummd 0.1.4, October 2023

  - Started `NEWS.md`.

  - Replaced some LaTeX symbols, `\textrm` -> `\textnormal` and 
  `lVert` -> `||` (same for `rVert`), to fix LaTeX compilation issues
  in the `r-oldrel-macos-arm64` when creating the PDF documentation file.
 
  - Added `#include<cmath>` to `medianheuristic.cpp` and switched from 
  `std::abs` to `std::fabs` in that file; otherwise seemed to be ambiguous 
  to compiler for `r-oldrel-macos-x86_64`.

  - Added energy distance method `energydist`, which is computed naively.
