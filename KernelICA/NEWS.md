# KernelICA 2.0.0

## Package reinstatement

* This release reinstates **KernelICA** on CRAN after the package was previously archived.
* The issues that led to the archival have been resolved, and the package has been updated to comply with current CRAN policies and checks.
* Package maintenance has been transferred to a new maintainer.
* Updated package metadata to reflect the current authorship and maintainer information.

## Bug fixes

* Fixed an argument-order bug in the Hermite kernel implementation affecting
  `kernel_matrix(..., kernel = "hermite")` and
  `incomplete_cholesky(..., kernel = "hermite")`.
* Fixed validation of user-supplied initialization matrices in
  `kernel_ica()`.
* Fixed selection of the best solution across multiple random starts in
  `kernel_ica()`.
* Improved handling of empty vectors and zero-length loops in several
  functions by replacing `1:n` and `1:length(x)` with `seq_len()` and
  `seq_along()`.
* Added a guard in `MD_distant_matrices()` for cases where no additional
  matrices need to be generated.
* `incomplete_cholesky()` now accepts integer input consistently by coercing
  input to numeric.
* `kernel_matrix()` now validates the `kernel` argument using `match.arg()`.
* Corrected the sign in the exponent of the Gaussian kernel.

## Build system

* Modernized the C++ implementation by replacing manual memory management with
  `std::unique_ptr`.
* Added missing standard library includes required by current compilers.
* Added `-DNDEBUG` to release compilation flags.
* Explicitly specified the package name when loading Rcpp modules.

## Documentation

* Updated package documentation to derive author and maintainer information
  directly from `DESCRIPTION`.
* Removed redundant author annotations from individual help pages.
* Corrected the citation year for the kernel ICA reference.
* Fixed several typographical errors in the documentation.