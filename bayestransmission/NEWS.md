# bayestransmission 0.1.0

## CRAN Resubmission Changes

* Changed `\dontrun{}` to `\donttest{}` in documentation examples per CRAN feedback
* Implemented conditional compilation to reduce package size by 6% (36.3 MB → 34 MB)
* Moved development C++ code out of installed package

## New Features

* **Conditional Compilation System**: Three-tier build configuration
  - Default build exposes 8 critical C++ classes (34 MB)
  - Comprehensive testing build adds 8 testing classes (38 MB)  
  - Full build adds 7 additional diagnostic classes (42 MB)
  - Added `getExposureFlags()` to query build configuration at runtime

* **Enhanced Testing**: Comprehensive test suite with conditional skipping
  - Test helpers: `skip_if_not_exposed()` and `skip_if_method_not_available()`
  - 304 tests pass, 38 appropriately skip in minimal build
  - Full coverage of MCMC, likelihood computation, and model creation

## Bug Fixes

* Fixed potential segfaults in SystemHistory container access methods
* Improved memory management with shared_ptr caching for Map/IntMap objects

## Internal Changes

* Reorganized C++ class exposure into logical tiers
* Added stub functions for conditionally compiled exports with informative errors
* Reduced Rcpp template instantiation by ~65%
* Cleaned up package structure (moved original_cpp out of inst/)

# bayestransmission 0.0.0.9000

* Initial CRAN submission.
* Added Bayesian inference methods for infectious disease transmission models.
* Implemented MCMC algorithms for estimating transmission parameters.
* Added support for multiple model types including LogNormal and LinearAbx models.

