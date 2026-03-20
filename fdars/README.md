# fdars

[![R-CMD-check](https://github.com/sipemu/fdars-r/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/sipemu/fdars-r/actions/workflows/r-cmd-check.yml)
[![CRAN status](https://www.r-pkg.org/badges/version/fdars)](https://CRAN.R-project.org/package=fdars)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/fdars)](https://cran.r-project.org/package=fdars)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![codecov](https://codecov.io/gh/sipemu/fdars-r/graph/badge.svg)](https://app.codecov.io/gh/sipemu/fdars-r)

**Functional Data Analysis in Rust** - A high-performance R package for functional data analysis with a Rust backend.

## What is Functional Data Analysis?

Functional Data Analysis (FDA) is a branch of statistics that deals with data where each observation is a function, curve, or surface rather than a single number or vector. Examples include:

- **Temperature curves**: Daily temperature recordings over a year for multiple weather stations
- **Growth curves**: Height measurements of children tracked over time
- **Spectroscopy data**: Absorbance spectra measured across wavelengths
- **Financial trajectories**: Stock price movements over trading days
- **Medical signals**: ECG, EEG, or fMRI time series

Traditional statistical methods treat each time point as a separate variable, losing the inherent smoothness and continuity of the data. FDA treats the entire curve as a single observation, enabling more powerful and interpretable analyses.

## Features

fdars is a comprehensive toolkit for functional data analysis with a high-performance Rust backend providing 10-200x speedups over pure R implementations.

### Data Representation
- **1D functional data** - Curves, time series, spectra
- **2D functional data** - Surfaces, images, spatial fields
- **Metadata support** - Attach IDs and covariates to observations
- **Flexible I/O** - Create from matrices, arrays, or data frames

### Depth & Centrality
Measure how "central" or "typical" each curve is:
- Fraiman-Muniz (FM), Band depth (BD), Modified band depth (MBD)
- Modal depth, Random projection (RP, RT, RPD)
- Functional spatial depth (FSD, KFSD)
- Depth-based median, trimmed mean, trimmed variance

### Outlier Detection
Multiple approaches to identify anomalous curves:
- Depth-based trimming and weighting
- Likelihood ratio test (LRT)
- Functional boxplot
- Magnitude-Shape plot (magnitude vs shape outliers)
- Outliergram (MEI vs MBD)

### Distance & Similarity
Quantify differences between curves:
- Lp distances (L1, L2, L∞)
- Hausdorff distance
- Dynamic time warping (DTW)
- PCA-based and derivative-based semimetrics

### Regression
Predict scalar outcomes from functional predictors:
- Principal component regression (`fregre.pc`)
- Basis expansion regression (`fregre.basis`)
- Nonparametric kernel regression (`fregre.np`)
- Cross-validation for model selection

### Clustering
Group similar curves together:
- K-means clustering with K-means++ initialization
- Fuzzy C-means with soft membership
- Automatic selection of optimal k (silhouette, CH, elbow)

### Smoothing & Basis Expansion
- Nadaraya-Watson, local linear/polynomial regression
- B-spline and Fourier basis expansions
- P-splines with automatic smoothing parameter selection
- Cross-validation (GCV, AIC, BIC) for basis selection

### Functional Statistics
- Mean, variance, standard deviation, covariance
- Geometric median (L1 median)
- Bootstrap confidence intervals
- Hypothesis testing for functional means

### Gaussian Process Simulation
Generate synthetic functional data:
- Multiple covariance kernels (Gaussian, Matérn, Exponential, Periodic)
- Kernel composition (addition, multiplication)
- Brownian motion and Ornstein-Uhlenbeck processes

### Group Comparison
- Between-group distance matrices (centroid, Hausdorff, depth-based)
- Permutation tests for significant group differences
- Visualization (heatmaps, dendrograms)

### Visualization
- Curve plots with categorical/continuous coloring
- Group means and confidence intervals
- Functional boxplots
- FPCA component visualization
- Outlier diagnostic plots

## Installation

### Prerequisites

- R (>= 4.0)
- Rust toolchain (install from [rustup.rs](https://rustup.rs/))
- A C compiler (gcc, clang)

### From GitHub

```r
# Install remotes if needed
install.packages("remotes")

# Install fdars (with documentation)
remotes::install_github("sipemu/fdars-r", build_vignettes = TRUE)
```

**Note:** On Windows, you may need [Rtools](https://cran.r-project.org/bin/windows/Rtools/) installed.

### From Binary Release (No Rust Required)

Download the pre-built binary from [GitHub Releases](https://github.com/sipemu/fdars-r/releases):

```r
# macOS
install.packages("path/to/fdars_x.y.z.tgz", repos = NULL, type = "mac.binary")

# Windows
install.packages("path/to/fdars_x.y.z.zip", repos = NULL, type = "win.binary")
```

### From Source

```bash
# Clone the repository
git clone https://github.com/sipemu/fdars-r.git
cd fdars-r

# Build and install
R CMD build .
R CMD INSTALL fdars_*.tar.gz
```

## Quick Start

```r
library(fdars)

# Create functional data from a matrix (rows = observations, cols = time points)
t <- seq(0, 1, length.out = 100)
X <- matrix(0, 20, 100)
for (i in 1:20) {
  X[i, ] <- sin(2 * pi * t) + rnorm(100, sd = 0.1)
}
fd <- fdata(X, argvals = t)

# Compute depth - measures how "central" each curve is
depths <- depth(fd)  # default: FM method
depths <- depth(fd, method = "mode")  # or specify method

# Find the functional median (most central curve)
median_curve <- median(fd)  # default: FM method

# Detect outliers
outliers <- outliers.depth.trim(fd, trim = 0.1)

# Functional regression: predict scalar y from functional X
y <- rowMeans(X) + rnorm(20, sd = 0.1)
model <- fregre.pc(fd, y, ncomp = 3)
predictions <- predict(model, fd)

# Cluster curves into groups
clusters <- cluster.kmeans(fd, ncl = 2)

# Smooth noisy curves
S <- S.NW(t, h = 0.1)  # Nadaraya-Watson smoother
smoothed <- S %*% X[1, ]
```

## Key Concepts

### Functional Data Objects (`fdata`)

The `fdata` class stores functional data as a matrix where rows are observations and columns are evaluation points:

```r
fd <- fdata(data_matrix, argvals = time_points, rangeval = c(0, 1))
```

#### Identifiers and Metadata

You can attach identifiers and metadata (covariates) to functional data objects:

```r
# Create fdata with IDs and metadata
meta <- data.frame(
  group = factor(c("control", "treatment", ...)),
  age = c(25, 32, ...),
  response = c(0.5, 0.8, ...)
)
fd <- fdata(X, id = paste0("patient_", 1:n), metadata = meta)

# Access fields
fd$id              # Character vector of identifiers
fd$metadata$group  # Access metadata columns

# Subsetting preserves metadata
fd_sub <- fd[1:10, ]  # id and metadata are also subsetted

# View metadata info
print(fd)    # Shows metadata columns
summary(fd)  # Shows metadata types and ranges
```

**Note:** If metadata contains an `id` column or has non-default row names, they must match the fdata identifiers. An error is thrown on mismatch.

### Depth Functions

Depth measures how "central" or "typical" a curve is relative to a sample. Higher depth = more central.

Use the unified `depth()` function with a `method` parameter:

```r
depth(fd, method = "FM")     # Fraiman-Muniz depth (default)
depth(fd, method = "BD")     # Band depth
depth(fd, method = "MBD")    # Modified band depth
depth(fd, method = "mode")   # Modal depth (kernel density)
depth(fd, method = "RP")     # Random projection depth
depth(fd, method = "RT")     # Random Tukey depth
depth(fd, method = "FSD")    # Functional spatial depth
depth(fd, method = "KFSD")   # Kernel functional spatial depth
depth(fd, method = "RPD")    # Random projection with derivatives
```

### Functional Regression

Predict a scalar response from functional predictors:

- `fregre.pc` - Principal component regression
- `fregre.basis` - Basis expansion regression
- `fregre.np` - Nonparametric kernel regression

All models support `predict()` for new data.

### Distance Metrics

Measure similarity between curves using `metric()` with a method parameter:

```r
metric(fd, method = "lp")        # Lp distance (default, L2 = Euclidean)
metric(fd, method = "hausdorff") # Hausdorff distance
metric(fd, method = "dtw")       # Dynamic time warping
metric(fd, method = "pca")       # PCA-based semimetric
metric(fd, method = "deriv")     # Derivative-based semimetric
```

Individual functions are also available: `metric.lp`, `metric.hausdorff`, `metric.DTW`, `semimetric.pca`, `semimetric.deriv`.

### Outlier Detection

Identify unusual curves:

- `outliers.depth.trim` - Trimmed depth-based detection
- `outliers.depth.pond` - Weighted depth-based detection
- `outliers.lrt` - Likelihood ratio test
- `outliers.boxplot` - Functional boxplot-based detection
- `magnitudeshape` - Magnitude-Shape outlier detection
- `outliergram` - Outliergram (MEI vs MBD plot)

#### Labeling Outliers by ID or Metadata

Both `magnitudeshape` and `outliergram` support labeling points by ID or metadata columns:

```r
# Create fdata with IDs and metadata
fd <- fdata(X, id = paste0("patient_", 1:n),
            metadata = data.frame(subject_id = paste0("S", 1:n)))

# Outliergram with custom labels
og <- outliergram(fd)
plot(og, label = "id")           # Label outliers with patient IDs
plot(og, label = "subject_id")   # Label with metadata column
plot(og, label_all = TRUE)       # Label ALL points, not just outliers

# magnitudeshape with custom labels
magnitudeshape(fd, label = "id")        # Label outliers with patient IDs
magnitudeshape(fd, label = NULL)        # No labels
```

### Functional Statistics

- `mean(fd)` - Functional mean
- `var(fd)` - Functional variance
- `sd(fd)` - Functional standard deviation
- `cov(fd)` - Functional covariance
- `gmed(fd)` - Geometric median (L1 median via Weiszfeld algorithm)

### Covariance Functions and Gaussian Process Generation

Generate synthetic functional data from Gaussian processes with various covariance kernels:

```r
# Smooth samples with Gaussian (squared exponential) kernel
fd_smooth <- make_gaussian_process(n = 20, t = seq(0, 1, length.out = 100),
                                   cov = kernel_gaussian(length_scale = 0.2))

# Rough samples with Matern kernel
fd_rough <- make_gaussian_process(n = 20, t = seq(0, 1, length.out = 100),
                                  cov = kernel_matern(nu = 1.5))

# Periodic samples
fd_periodic <- make_gaussian_process(n = 10, t = seq(0, 2, length.out = 200),
                                     cov = kernel_periodic(period = 0.5))

# Combine kernels: signal + noise
cov_total <- kernel_add(kernel_gaussian(variance = 1), kernel_whitenoise(variance = 0.1))
fd_noisy <- make_gaussian_process(n = 10, t = seq(0, 1, length.out = 100), cov = cov_total)
```

Available covariance functions:
- `kernel_gaussian` - Squared exponential (RBF) kernel, infinitely smooth
- `kernel_exponential` - Exponential kernel (Matern ν=0.5), rough
- `kernel_matern` - Matern family with smoothness parameter ν
- `kernel_brownian` - Brownian motion covariance (1D only)
- `kernel_linear` - Linear kernel
- `kernel_polynomial` - Polynomial kernel
- `kernel_whitenoise` - Independent noise at each point
- `kernel_periodic` - Periodic kernel (1D only)
- `kernel_add` - Combine kernels by addition
- `kernel_mult` - Combine kernels by multiplication

### Depth-Based Medians and Trimmed Means

Use the unified functions with a `method` parameter:

```r
# Median (curve with maximum depth)
median(fd)                          # default: FM method
median(fd, method = "mode")         # modal depth-based median

# Trimmed mean (mean of deepest curves)
trimmed(fd, trim = 0.1)             # default: FM method
trimmed(fd, trim = 0.1, method = "RP")  # RP depth-based trimmed mean

# Trimmed variance
trimvar(fd, trim = 0.1)             # default: FM method
trimvar(fd, trim = 0.1, method = "mode")
```

### Visualization

- `plot(fd, color = ...)` - Plot curves with coloring by numeric or categorical variables
  - `show.mean = TRUE` - Overlay group mean curves
  - `show.ci = TRUE` - Show confidence interval ribbons per group
- `boxplot.fdata` - Functional boxplot with depth-based envelopes
- `magnitudeshape` - Magnitude-Shape outlier detection and visualization
- `outliergram` - Outliergram for shape outlier detection (MEI vs MBD plot)
- `plot.fdata2pc` - FPCA visualization (components, variance, scores)

### Group Comparison

- `group.distance` - Compute distances between groups (centroid, Hausdorff, depth-based)
- `group.test` - Permutation test for significant group differences
- `plot.group.distance` - Visualize group distances (heatmap, dendrogram)

### Clustering

- `cluster.kmeans` - K-means clustering for functional data
- `cluster.optim` - Optimal k selection using silhouette, CH, or elbow
- `cluster.fcm` - Fuzzy C-means clustering with soft membership
- `cluster.init` - K-means++ center initialization

### Curve Registration

- `register.fd` - Shift registration using cross-correlation

### Feature Extraction

- `localavg.fdata` - Extract local average features from curves

### 2D Functional Data (Surfaces)

fdars supports 2D functional data (surfaces/images). The following functions have full 2D support:

| Category | Functions |
|----------|-----------|
| **Depth** | `depth` (methods: FM, mode, RP, RT, FSD, KFSD) |
| **Distance** | `metric.lp`, `metric.hausdorff`, `semimetric.pca`, `semimetric.deriv` |
| **Statistics** | `mean`, `var`, `sd`, `cov`, `gmed`, `deriv` |
| **Centrality** | `median`, `trimmed`, `trimvar` (all methods except BD, MBD, RPD) |
| **Regression** | `fregre.np` (nonparametric) |
| **Visualization** | `plot` (heatmap + contours) |

**Note:** Band depths (BD, MBD), RPD, and DTW do not support 2D data.

```r
# Create 2D functional data (e.g., 10 surfaces on a 20x30 grid)
n <- 10
m1 <- 20
m2 <- 30
s <- seq(0, 1, length.out = m1)
t <- seq(0, 1, length.out = m2)

# Generate surfaces: f(s,t) = sin(2*pi*s) * cos(2*pi*t) + noise
X <- array(0, dim = c(n, m1, m2))
for (i in 1:n) {
  for (si in 1:m1) {
    for (ti in 1:m2) {
      X[i, si, ti] <- sin(2*pi*s[si]) * cos(2*pi*t[ti]) + rnorm(1, sd = 0.1)
    }
  }
}

fd2d <- fdata(X, argvals = list(s, t), fdata2d = TRUE)

# All these work with 2D data:
mean_surface <- mean(fd2d)           # Mean surface
var_surface <- var(fd2d)             # Pointwise variance
depths <- depth(fd2d)                # Depth values
median_surface <- median(fd2d)       # Depth-based median
gmed_surface <- gmed(fd2d)           # Geometric median

# Plot 2D data (heatmap + contours)
plot(fd2d)
```

#### Converting DataFrames to 2D fdata

Use `df_to_fdata2d()` to convert long-format DataFrames to 2D functional data:

```r
# DataFrame structure: id column, s-index column, t-value columns
df <- data.frame(
  id = rep(c("surf1", "surf2"), each = 5),
  s = rep(1:5, 2),
  t1 = rnorm(10), t2 = rnorm(10), t3 = rnorm(10)
)

# Convert to 2D fdata
fd2d <- df_to_fdata2d(df, id_col = 1, s_col = 2)

# With metadata (must have one row per surface)
meta <- data.frame(group = c("A", "B"), value = c(1.5, 2.3))
fd2d <- df_to_fdata2d(df, id_col = 1, s_col = 2, metadata = meta)
```

## Examples

- **[Wine Quality Analysis with Andrews Curves](https://github.com/sipemu/fdars-r/blob/main/examples/medium-andrews-wine.qmd)** — A comprehensive walkthrough using the UCI Wine dataset (178 wines, 13 chemicals, 3 cultivars) demonstrating outlier detection, clustering, hypothesis testing, FPCA, and process monitoring. Render with `quarto render examples/medium-andrews-wine.qmd`.

- **[Predictive Truck Maintenance with Andrews Curves](https://github.com/sipemu/fdars-r/blob/main/examples/scania-aps-failure.qmd)** — Applying the full FDA pipeline to the Scania APS Failure dataset (76,000 trucks, 170 anonymized sensors, binary failure classification) for fleet health monitoring, outlier triage, and sensor-level diagnostics. Render with `quarto render examples/scania-aps-failure.qmd`.

## License

MIT

## Author

Simon Mueller

## Acknowledgments

- Built with [extendr](https://extendr.rs/) for R-Rust integration
- Uses [rayon](https://github.com/rayon-rs/rayon) for parallelization
