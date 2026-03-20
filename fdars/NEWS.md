# fdars 0.3.2

## Bug Fixes

- Fixed compiled code WARNING: wrapped `abort`/`exit`/`_exit` symbols using linker `--wrap` flag to convert process termination into R errors
- Reduced test CPU time by limiting Rust thread pool (`RAYON_NUM_THREADS=2`)

# fdars 0.3.1

## Bug Fixes

- Fixed Windows installation failure (missing cargo checksum files)
- Wrapped slow bootstrap examples in `\donttest{}`

# fdars 0.3.0

## Internal

- Upgraded Rust backend (fdars-core) to v0.4.0
- New `FdMatrix` type for safer matrix handling (internal)
- New streaming depth module in core (internal)
- Reduced package size by removing non-essential vendored files
- No user-facing API changes

# fdars 0.2.0

## Test Coverage & Quality

- Improved Rust core test coverage to 84%+
- Improved R package test coverage to 80%+
- Added pre-commit hooks for cargo fmt and clippy

## New Features

### Optimal Cluster Selection
- Added `optim.kmeans.fd()` function to automatically determine the optimal number of clusters for functional k-means
- Three selection criteria available:
  - **Silhouette score**: Measures cluster cohesion vs separation (-1 to 1, higher is better)
  - **Calinski-Harabasz index**: Ratio of between/within cluster variance (higher is better)
  - **Elbow method**: Visual inspection of within-cluster sum of squares
- Added `print()` and `plot()` methods for `optim.kmeans.fd` objects
- Silhouette and Calinski-Harabasz computations implemented in Rust for performance

### k-NN Bandwidth Selection for Nonparametric Regression
- Added k-nearest neighbors support to `fregre.np()` via the `type.S` parameter:
  - `"kNN.gCV"`: Global cross-validation (single k for all observations)
  - `"kNN.lCV"`: Local cross-validation (adaptive k per observation)
- Extended `predict.fregre.np()` to handle k-NN models

### Flexible Metrics in Clustering
- `kmeans.fd()` now accepts both string metrics (`"L2"`, `"L1"`, `"Linf"`) and metric/semimetric functions
- String metrics use fast Rust-only path; function metrics provide flexibility for custom distances

## Improvements

### ggplot2 Visualizations
- All plot methods now use ggplot2 instead of base R graphics:
  - `plot.fdata()`: Functional data curves with minimal theme
  - `plot.kmeans.fd()`: Cluster-colored curves with dashed cluster centers
  - `plot.optim.kmeans.fd()`: Criterion scores with optimal k highlighted
  - `plot.outliers.fdata()`: Outlier/normal curves with color legend

## Documentation

### Vignettes
Added 6 comprehensive vignettes:
- Introduction to fdars
- Functional Depth Functions
- Distance Metrics and Semimetrics
- Functional Regression
- Functional Clustering
- Outlier Detection

### API Documentation
- Complete roxygen2 documentation for all exported functions

## Bug Fixes
- Fixed namespace issues with stats and utils imports
- Fixed ggplot2 `.data` pronoun import for R CMD check compliance

# fdars 0.1.0

- Initial release
- Core functional data class (`fdata`) with 1D and 2D support
- 7 depth functions: FM, mode, RP, RT, FSD, KFSD, RPD
- Distance metrics: Lp, Hausdorff, DTW, KL
- Semimetrics: PCA, derivative, basis, Fourier, hshift
- Functional regression: PC, basis, nonparametric
- K-means clustering with k-means++ initialization
- Outlier detection: depth-based and LRT methods
- Statistical tests: flm.test, fmean.test
- Bootstrap inference and confidence intervals
- High-performance Rust backend with parallel processing
