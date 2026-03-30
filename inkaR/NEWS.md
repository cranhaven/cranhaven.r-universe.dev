# inkaR News

## inkaR 0.6.0

### New Features

* **Interactive Selection Wizard**: Calling `inkaR()` without arguments now opens a professional CLI wizard to guide users through selecting an indicator, then an available spatial level, and finally a year or year range.
* **Analytical Multi-Indicator Downloads**: Users can now pass a vector of IDs (e.g., `c("bip", "xbev")`) or enter multiple IDs in the wizard (separated by spaces).
* **Multi-Indicator Joins**: Automatically handles the merging of multiple indicators.
  * `format = "long"` (default) stacks indicators in a tidy format.
  * `format = "wide"` pivots indicators into individual columns for direct comparison and mathematical operations.
* **Fuzzy Search Engine**: Indicator selection now supports error-tolerant search using the `stringdist` package (Jaro-Winkler method).

### CLI Enhancements

* **Professional Table Display**: Rewrote indicator selection and search results using the `cli` package for high-end, paginated, and responsive terminal tables.
* **Usage History & Favorites**: The package now tracks frequently and recently used indicators, highlighting them in green and pinning them to the top of selection lists.
* **Intelligent Year Selection**: Implemented a professional 4-column layout for year selection with validated user input.

### Professional Visualizations

* **Premium Themes**: Introduced `theme_inkaR()`, a high-end ggplot2 theme with dedicated light and dark modes.
* **Upgraded Mapping**: `plot_inkar()` now supports bilingual labels, automatic unit extraction, and professional color palettes (Viridis, Magma).

## inkaR 0.4.4

* FIXED: Added graceful handling for SSL certificate verification issues on specific CRAN builders (e.g., Fedora).
* IMPROVED: Wrapped network-dependent examples in `try()` to ensure CRAN checks pass even with environment-specific network limitations.

## inkaR 0.4.3

* FIXED: Addressed CRAN feedback regarding single quotes in `DESCRIPTION`.
* FIXED: Added missing `\value` tags to exported `.Rd` files (e.g., `clear_inkar_cache()`).
* FIXED: Replaced `\dontrun{}` with `\donttest{}` or `if(interactive())` to ensure proper example testing.
* FIXED: Ensured CSV exporting examples do not write to the user home directory (`tempdir()`).

## inkaR 0.4.2

### Enhancements

* Added persistent disk-caching (`tools::R_user_dir`) to heavily improve `GetGebieteZumRaumbezug` lookup times across sessions.
* Implemented parallel API dispatching (`httr2::req_perform_parallel`) for exploring spatial levels, eliminating the sequential wait time.
* Introduced `plot_inkar()` for native geospatial mapping with `ggplot2` and `sf` by directly downloading and matching GADM polygon data.

### Bug Fixes

* Fixed missing mock payloads in `testthat` by completely regenerating `httptest2` records.
* Avoided decimal conversion errors (`NA`) by implementing robust locale-agnostic numeric parsing.
* Addressed spell checking NOTEs caught by win-builder.
