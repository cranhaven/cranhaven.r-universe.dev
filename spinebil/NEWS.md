# Version 1.0.6

- Added `ppi_noise_grid()` to estimate 95th percentile noise thresholds across sample sizes, indices, and settings, with support for custom noise generators, parallel computation, and CSV export.
- Enhanced `ppi_samplesize_effect()` with flexible sample sizes, optional input data, additional index arguments, and CSV export.
- Improved `ppi_scale()` to support custom noise generators, additional index arguments, and configurable parallel workers.
- Added support for index functions accepting either a two-column matrix or two numeric vectors.
- Updated the `sigma` column in `ppi_scale()` to use `"structure"` and `"noise"` labels instead of `0` and `1`.
- Updated documentation, vignettes, and package dependencies.
- Fixed the vignette rebuilding issue reported in the CRAN MKL check.


# Version 1.0.5

- Added `BugReports` field and improved parallel handling.
- Made all `future::plan()` calls temporary to ensure safer parallel execution.

# Version 1.0.4

- Wrapped the example for `ppi_samplesize_effect()` in `\dontrun{}`.
- All other examples remain tested and run quickly.

# Version 1.0.3

- The full example for `ppi_samplesize_effect()` is wrapped in \dontrun{}
  because it takes >5s. A working demonstration is provided in the vignette.

# Version 1.0.2

- Wrapped the example for `ppi_samplesize_effect()` in `\donttest{}` to comply with CRAN’s runtime policy (<5 seconds).  

# Version 1.0.1

- All tested examples run in <10 seconds.

# Version 1.0.0

- Several existing functions have been renamed for consistency.
- Added multiple new functions expanding the package’s functionality.
- Introduced Quarto vignettes to demonstrate usage and workflows.
- Added a hex sticker for the package.

# Version 0.1.6

- Corrected DOI formatting in DESCRIPTION to meet CRAN requirements.

# Version 0.1.5

- Further reduction in code speed to satisfy CRAN checks

# Version 0.1.4

- Fixes to correct items reported in manual CRAN check
- Added citation into the DESCRIPTION

# Version 0.1.3

- Fixed CRAN errors
- updated citation to published paper

# Version 0.1.2

- Fixed CRAN errors
- removed dependency on binostics

# Version 0.1.1

- Updated to work with new version of dplyr and tibble.
- Removed dependency on mbgraphic (index functions now available in tourr).
