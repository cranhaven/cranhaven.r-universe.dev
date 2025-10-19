
<!-- README.md is generated from README.Rmd. Please edit that file -->

# matrixCorr

<!-- badges: start -->

[![R-CMD-check.yaml](https://github.com/Prof-ThiagoOliveira/matrixCorr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Prof-ThiagoOliveira/matrixCorr/actions/workflows/R-CMD-check.yaml)
[![test-coverage.yaml](https://github.com/Prof-ThiagoOliveira/matrixCorr/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/Prof-ThiagoOliveira/matrixCorr/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

`matrixCorr` computes correlation and related association matrices from
small to high-dimensional data using simple, consistent functions and
sensible defaults. It includes shrinkage and robust options for noisy or
**p ≥ n** settings, plus convenient print/plot methods.
Performance-critical paths are implemented in C++ with BLAS/OpenMP and
memory-aware symmetric updates. The API accepts base matrices and data
frames and returns standard R objects via a consistent S3 interface.

Supported measures include Pearson, Spearman, Kendall, distance
correlation, partial correlation, and robust biweight mid-correlation;
agreement tools cover Bland–Altman (two-method and repeated-measures)
and Lin’s concordance correlation coefficient (including
repeated-measures LMM/REML extensions).

## Features

- High-performance C++ backend using `Rcpp`
- General correlations such as `pearson_corr()`, `spearman_rho()`,
  `kendall_tau()`
- Robust correlation metrics (`biweight_mid_corr()`)
- Distance correlation (`distance_corr()`)
- Partial correlation (`partial_correlation()`)
- Shrinkage for $p >> n$ (`schafer_corr()`)
- Agreement metrics
  - Bland–Altman (two-method `bland_altman()` and repeated-measures
    `bland_altman_repeated()`),
  - Lin’s concordance correlation coefficient (pairwise `ccc()`,
    repeated-measures LMM/REML `ccc_lmm_reml()` and non-parametric
    `ccc_pairwise_u_stat()`)

## Installation

``` r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("Prof-ThiagoOliveira/matrixCorr")
```

## Example

### Correlation matrices (Pearson, Spearman, Kendall)

``` r
library(matrixCorr)

set.seed(1)
X <- as.data.frame(matrix(rnorm(300 * 6), ncol = 6))
names(X) <- paste0("V", 1:6)

R_pear <- pearson_corr(X)
R_spr  <- spearman_rho(X)
R_ken  <- kendall_tau(X)

print(R_pear, digits = 2)
plot(R_spr)   # heatmap
```

### Robust correlation (biweight mid-correlation)

``` r
set.seed(2)
Y <- X
# inject outliers
Y$V1[sample.int(nrow(Y), 8)] <- Y$V1[sample.int(nrow(Y), 8)] + 8

R_bicor <- biweight_mid_corr(Y)
print(R_bicor, digits = 2)
```

### High-dimensional shrinkage correlation ($p >> n$)

``` r
set.seed(3)
n <- 60; p <- 200
Xd <- matrix(rnorm(n * p), n, p)
colnames(Xd) <- paste0("G", seq_len(p))

R_shr <- schafer_corr(Xd)
print(R_shr, digits = 2, max_rows = 6, max_cols = 6)
```

### Partial correlation matrix

``` r
R_part <- partial_correlation(X)
print(R_part, digits = 2)
```

### Distance correlation matrix

``` r
R_dcor <- distance_corr(X)
print(R_dcor, digits = 2)
```

## Agreement analyses

### Two-method Bland–Altman

``` r
set.seed(4)
x <- rnorm(120, 100, 10)
y <- x + 0.5 + rnorm(120, 0, 8)

ba <- bland_altman(x, y)
print(ba)
plot(ba)
```

### Repeated-measures Bland–Altman (pairwise matrix)

``` r
set.seed(5)
S <- 20; Tm <- 6
subj  <- rep(seq_len(S), each = Tm)
time  <- rep(seq_len(Tm), times = S)

true  <- rnorm(S, 50, 6)[subj] + (time - mean(time)) * 0.4
mA    <- true + rnorm(length(true), 0, 2)
mB    <- true + 1.0 + rnorm(length(true), 0, 2.2)
mC    <- 0.95 * true + rnorm(length(true), 0, 2.5)

dat <- rbind(
  data.frame(y = mA, subject = subj, method = "A", time = time),
  data.frame(y = mB, subject = subj, method = "B", time = time),
  data.frame(y = mC, subject = subj, method = "C", time = time)
)
dat$method <- factor(dat$method, levels = c("A","B","C"))

ba_rep <- bland_altman_repeated(
  data = dat, response = "y", subject = "subject",
  method = "method", time = "time",
  include_slope = FALSE, use_ar1 = FALSE
)
summary(ba_rep)
# plot(ba_rep)  # faceted BA scatter by pair
```

### Two-method Lin’s concordance correlation

    # Lin's CCC for x vs y (with CI + heatmap)
    cc2 <- ccc(cbind(x = x, y = y), ci = TRUE)
    print(cc2)
    summary(cc2)
    plot(cc2, title = "Lin's CCC (two methods)")

### Lin’s concordance correlation coefficient (repeated-measures LMM/REML)

``` r
set.seed(6)
S <- 30; Tm <- 8
id     <- factor(rep(seq_len(S), each = 2 * Tm))
method <- factor(rep(rep(c("A","B"), each = Tm), times = S))
time   <- rep(rep(seq_len(Tm), times = 2), times = S)

u  <- rnorm(S, 0, 0.8)[as.integer(id)]
g  <- rnorm(S * Tm, 0, 0.5)
g  <- g[ (as.integer(id) - 1L) * Tm + as.integer(time) ]
y  <- (method == "B") * 0.3 + u + g + rnorm(length(id), 0, 0.7)

dat_ccc <- data.frame(y, id, method, time)

# Using non-parametric approch
ccc_rep_u <- ccc_pairwise_u_stat(
  data = dat_ccc, response = "y", method = "method", time = "time",
  ci = TRUE
)
print(ccc_rep_u)
summary(ccc_rep_u)
plot(ccc_rep_u, title = "Repeated-measures CCC (U-statistic)")

# Using LMM approch
fit_ccc <- ccc_lmm_reml(dat_ccc, response = "y", rind = "id",
                        method = "method", time = "time", ci = TRUE)
summary(fit_ccc)  # overall CCC, variance components, SEs/CI
```

## Contributing

Issues and pull requests are welcome. Please see `CONTRIBUTING.md` for
guidelines and `cran-comments.md`/`DESCRIPTION` for package metadata.

## License

MIT [Thiago de Paula Oliveira](https://orcid.org/0000-0002-4555-2584)

See inst/LICENSE for the full MIT license text.
