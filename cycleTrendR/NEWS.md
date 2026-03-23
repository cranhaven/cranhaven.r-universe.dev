
# cycleTrendR 0.3.0

## Major new features
- Introduced **universal time handling** via the new argument `dates_type`.
- Full support for:
  - `dates_type = "date"` (daily data)
  - `dates_type = "posix"` (sub-daily wearable/physiological data)
  - `dates_type = "numeric"` (neuroscience, simulations, spike trains)
- Internal unified time index (`timenum`) for consistent modeling across formats.

## Enhancements
- Automatic switching between STL, Lomb–Scargle, Fourier, LOESS, GAM, and GAMM.
- Fourier harmonics now operate in **time units** of `timenum`.
- Improved change-point detection compatible with all time formats.
- Updated spectral analysis pipeline for irregular and numeric time series.
- New vignette: *cycleTrendR in practice*.

## Bug fixes
- Removed hard-coded assumptions about Date class.
- Fixed plotting issues related to time axis.
- Improved robustness of bootstrap confidence intervals.

## Documentation
- Updated README with universal examples.
- Added new vignette demonstrating Date, POSIXct, and numeric workflows.


# cycleTrendR 0.2.0

* Major improvements to documentation, imports, and CRAN compliance.
* Added full roxygen2 documentation for all parameters and return values.
* Improved NAMESPACE management with explicit `@importFrom` directives.
* Enhanced vignette stability and reduced computational load in examples.
* Achieved full CRAN compliance: 0 errors, 0 warnings, 0 notes.

# cycleTrendR 0.1.0

* Initial release of **cycleTrendR**.
* Added the main function `adaptive_cycle_trend_analysis()` supporting:
  - LOESS, GAM, and GAMM trend estimation
  - Automatic Fourier harmonic selection (AICc/BIC)
  - Lomb–Scargle periodogram for irregular sampling
  - Bootstrap confidence intervals (IID and MBB)
  - Change-point detection
  - Rolling-origin forecasting
* Added publication-quality ggplot2 visualizations:
  - Trend + CI
  - Periodogram
  - Residual ACF
  - Diagnostics and summary tables
* Added a comprehensive vignette: *cycleTrendR-overview*.
* Added README with installation instructions and examples.
