# ravetools 0.1.3

* Rewrote `band_pass2` to avoid `NA` generated when upper band frequency is `Nyquist`
* Added `Vector3`, `Matrix4`, `Quaternion` for in-place calculation
* Added support for `WASM`
* Fixed issues reported by `CRAN`: "format string is not a string literal (potentially insecure)"

# ravetools 0.1.2

* Compatible with the latest `filearray`
* Exported `grow_volume`
* `mesh_from_volume` no longer throw errors if the mesh does not form a manifold

# ravetools 0.1.1

* Fixed a precision issue that caused test failure on some machine

# ravetools 0.1.0

* Added `fill_surface` to fill in volume based on given surface mesh
* Added `mesh_from_volume` to generate mesh from volume. This function can be used together with `fill_surface` to generate surface envelope
* Added `register_volume` to align two imaging data using linear or non-linear registration
* Added `fftw` on `2D` image and `3D` volume data
* Added convolution for `1D`, `2D`, `3D` data using `FFT` 

# ravetools 0.0.9

* Fixed `pwelch` frequency not starting from zero issue
* Upgraded `TinyThread` using the latest pull-request to `RcppParallel`

# ravetools 0.0.8

* Added `interpolate_stimulation` to detect stimulation signals within the response and interpolate with smooth signals
* The package now imports `splines`
* Added `fast_quantile` and `fast_mvquantile` to improve the quantile/median calculation speed
* Fixed the `plot_signals` plotting range too large when signals have large values (such as stimulation)
* Fixed `TinyThreads` library memory leak issues
* Simplified `diagnose_channel`, avoid duplicated `pwelch` calculation

# ravetools 0.0.7

* Added signal `filter`, `filtfilt` that produce the same results as `Matlab` (with precision error)
* Added two ways to perform band-pass filters
* Allows multiple channels through `pwelch` as a row-major matrix to speed up calculation
* Added `wavelet_cycles_suggest` to provide default calculation of wavelet cycles
* Added internal argument `signature` to wavelet to resolve potential cache conflicts when running in multiple processes. (This allows `RAVE` to run wavelet on multiple subjects at the same time)

# ravetools 0.0.6

* Added decibel average in `pwelch`
* Allowed `pwelch` sampling frequency to be greater than the signal length
* Adjusted parameters diagnostic plot and `pwelch` plot to properly handle graph text, margin, axis
* Added `plot_signals` to plot multiple functional data within the same canvas

# ravetools 0.0.5

* Exposed `C++` code to `inst/includes` so other users can dynamically link to the functions (https://github.com/dipterix/ravetools/issues/5)
* Removed confusing in-place arguments in the `fftw` related code
* Corrected `fftw` plans to respect the flags
* Added `C++` to convert raw binary bytes to `uint`, `int`, `float`, and `string`

# ravetools 0.0.4

Parallel processes might use different temporary directory paths. To improve the performance, it is recommended to set a shared temporary directory, hence this version

* Allows temporary directories to be set via environment variable `RAVETOOLS_TEMPDIR` or option `ravetools.tempdir`. 

# ravetools 0.0.3

This version fixes a memory issue reported by `CRAN` check (`gcc-UBSAN`). 

* There is a potential integer overflow where `NA_INTEGER` is subtracted by one before being converted to `R_xlen_t` type. This update fixes this issue
* Removed `RcppParallel` and copied part of it into `inst/include` folder, with `TBB` removed under the `GPL-3` license framework.


# ravetools 0.0.2

This is an initial version of `ravetools`. Although a bare minimal set of signal processing functions are provided, it is sufficient to perform preprocess pipelines on most `iEEG` signals. Some functions are added from the `dipsaus` package, with considerable performance improvement. The `C++` functions have been tested on all major platforms, with different architectures (`ARM`, `i386`, `x64`).

### Documentation

* Added `README` file to demonstrate basic usage
* Added a `NEWS.md` file to track changes to the package.

### Signal processing functions
* Re-implemented `decimate` with `FIR` filters creating the same results as in `Matlab`
* Added `detrend` function to 
* Added `diagnose_channel` to visually inspect channel signals
* Added `morlet_wavelet` to enable fast and memory efficient wavelet decomposition; the result agrees with existing `Matlab` code with floating errors (`10^-7`)
* Added `multitaper`
* Added `pwelch` (`Welch` periodogram)
* Added `notch_filter` to remove line noise

### High-performance functions

The following functions are implemented in `C++` parallel. They tend to be faster than normal base-R implementations, depending on the number of `CPU` cores used.

* Added `collapse` to collapse arrays
* Added `shift_array` to shift array along certain indices
* Added `fast_cov` to calculate `pearson` covariance matrix in parallel
* Added `baseline_array` to calculate baseline arrays with multiple margins
