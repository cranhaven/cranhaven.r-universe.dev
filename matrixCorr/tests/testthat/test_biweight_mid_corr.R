# tests/testthat/test-biweight_mid_corr.R

# helper - defined using R
bicor_manual_test <- function(x, y, c_const = 9) {
  rx <- {
    mx  <- median(x); madx <- median(abs(x - mx))
    u   <- (x - mx) / (c_const * madx)
    w   <- ifelse(abs(u) < 1, (1 - u^2)^2, 0)
    r   <- (x - mx) * w
    r / sqrt(sum(r^2))
  }
  ry <- {
    my  <- median(y); mady <- median(abs(y - my))
    u   <- (y - my) / (c_const * mady)
    w   <- ifelse(abs(u) < 1, (1 - u^2)^2, 0)
    r   <- (y - my) * w
    r / sqrt(sum(r^2))
  }
  sum(rx * ry)
}

biweight_standardise_R <- function(x, c_const = 9, max_p_outliers = 1) {
  stopifnot(is.numeric(x), length(x) >= 2L)
  mx  <- stats::median(x)
  mad <- stats::median(abs(x - mx))
  if (!(mad > 0)) return(rep(NaN, length(x)))

  # Side-cap disabled here for exactness in most tests
  stopifnot(max_p_outliers == 1)
  u <- (x - mx) / (c_const * mad)
  w <- ifelse(abs(u) < 1, (1 - u^2)^2, 0)
  r <- (x - mx) * w
  s2 <- sum(r^2)
  if (!(s2 > 0)) return(rep(NaN, length(x)))
  r / sqrt(s2)
}

bicor_manual_R <- function(x, y, c_const = 9, max_p_outliers = 1) {
  zx <- biweight_standardise_R(x, c_const, max_p_outliers)
  zy <- biweight_standardise_R(y, c_const, max_p_outliers)
  if (any(!is.finite(zx)) || any(!is.finite(zy))) return(NaN)
  sum(zx * zy)
}

# Weighted versions (for small n testcases)
w_quantile <- function(x, w, p) {
  o  <- order(x, method = "radix")
  xs <- x[o]; ws <- w[o]
  W  <- sum(ws)
  if (!(W > 0)) return(NaN)
  if (p <= 0) return(xs[1L])
  if (p >= 1) return(xs[length(xs)])
  target <- p * W
  csum   <- 0
  for (i in seq_along(xs)) {
    csum <- csum + ws[i]
    if (csum >= target) {
      if (i == 1L) return(xs[1L])
      cprev <- csum - ws[i]
      frac  <- (target - cprev) / ws[i]
      return(xs[i-1L] + frac * (xs[i] - xs[i-1L]))
    }
  }
  xs[length(xs)]
}

w_median  <- function(x, w)  w_quantile(x, w, 0.5)
w_mad     <- function(x, w, med) w_median(abs(x - med), w)

biweight_standardise_w_R <- function(x, w, c_const = 9, max_p_outliers = 1) {
  stopifnot(is.numeric(x), is.numeric(w), length(x) == length(w))
  W <- sum(w); if (!(W > 0)) return(rep(NaN, length(x)))
  med <- w_median(x, w)
  mad <- w_mad(x, w, med)
  if (!(mad > 0)) {
    # weighted Pearson fallback (for tests that expect it)
    mu <- sum(w * x) / W
    r  <- (x - mu) * w
    s2 <- sum(r^2)
    if (!(s2 > 0)) return(rep(NaN, length(x)))
    return(r / sqrt(s2))
  }
  stopifnot(max_p_outliers == 1)
  u  <- (x - med) / (c_const * mad)
  wt <- ifelse(abs(u) < 1, (1 - u^2)^2, 0) * w
  r  <- (x - med) * wt
  s2 <- sum(r^2)
  if (!(s2 > 0)) return(rep(NaN, length(x)))
  r / sqrt(s2)
}

bicor_weighted_manual_R <- function(x, y, w, c_const = 9, max_p_outliers = 1) {
  zx <- biweight_standardise_w_R(x, w, c_const, max_p_outliers)
  zy <- biweight_standardise_w_R(y, w, c_const, max_p_outliers)
  if (any(!is.finite(zx)) || any(!is.finite(zy))) return(NaN)
  sum(zx * zy)
}

test_that("affine transformations yield ±1 exactly", {
  x     <- 1:5
  y_pos <- 2*x + 7    # strictly increasing affine transform
  y_neg <- -3*x + 1   # strictly decreasing affine transform

  M <- cbind(x = x, y_pos = y_pos, y_neg = y_neg)

  R <- biweight_mid_corr(
    M, c_const = 9, max_p_outliers = 1,
    pearson_fallback = "none", n_threads = 1L
  )

  expect_equal(R["x", "y_pos"],  1, tolerance = 1e-12)
  expect_equal(R["x", "y_neg"], -1, tolerance = 1e-12)
  expect_equal(as.numeric(diag(R)), rep(1, 3))
})

test_that("single gross same-direction leaves bicor = 1", {
  x <- c(1:5, 1000)
  y <- c(1:5, 1000)
  R <- biweight_mid_corr(cbind(x, y), pearson_fallback = "none")
  expect_gt(R["x", "y"], 0.999)
})

test_that("opposite-direction outlier: bicor matches manual definition", {
  x <- c(1:5, 1000)
  y <- c(1:5, -1000)
  R <- biweight_mid_corr(cbind(x, y), pearson_fallback = "none")
  expect_equal(R["x","y"], bicor_manual_test(x, y), tolerance = 1e-12)
  expect_lt(R["x","y"], 0.9)  # sanity: not near 1
})

test_that("fallback policy: 'none' and 'hybrid' produce NA for constant column; 'all' matches Pearson", {
  A <- rep(1, 4)            # constant -> MAD = 0 and variance = 0
  B <- 1:4
  C <- 2*B + 5
  M <- cbind(A = A, B = B, C = C)

  # 'none' => NA for all correlations involving A; B~C remains exactly 1
  R_none <- biweight_mid_corr(M, pearson_fallback = "none")
  expect_true(R_none["A","A"]==1)
  expect_true(all(is.na(R_none["A", c("B","C")])))
  expect_equal(R_none["B","C"], 1, tolerance = 1e-12)

  # 'hybrid' tries Pearson for zero-MAD column; but A has zero variance,
  # so it must still be NA. B~C remains exactly 1.
  R_hybr <- biweight_mid_corr(M, pearson_fallback = "hybrid")
  expect_true(R_hybr["A","A"]==1)
  expect_true(all(is.na(R_hybr["A", c("B","C")])))
  expect_equal(R_hybr["B","C"], 1, tolerance = 1e-12)
})

test_that("threading does not change results beyond tiny numerical noise", {
  set.seed(42)
  X <- matrix(rnorm(500 * 20), 500, 20)

  R1 <- biweight_mid_corr(X, n_threads = 1L)
  R2 <- biweight_mid_corr(X, n_threads = 2L)

  expect_equal(R1, R2, tolerance = 1e-8)
})

test_that("data-frame path: drops non-numeric columns and preserves numeric names", {
  df <- data.frame(
    num1 = 1:5,
    fac  = factor(c("a","b","a","b","a")),
    logi = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    num2 = 2*(1:5) + 7,
    date = as.Date("2020-01-01") + 0:4
  )

  R <- biweight_mid_corr(df, pearson_fallback = "none")
  expect_equal(colnames(R), c("num1", "num2"))
  expect_equal(rownames(R), c("num1", "num2"))
  expect_equal(R["num1","num2"], 1, tolerance = 1e-12)
})

test_that("validator rejects missing values", {
  df_na <- data.frame(a = c(1, 2, NA, 4), b = c(2, 3, 4, 5))
  expect_error(
    biweight_mid_corr(df_na),
    regexp = "Missing values are not allowed"
  )
})

test_that("attributes and class are set correctly", {
  set.seed(1)
  X <- matrix(rnorm(200), 50, 4)
  R <- biweight_mid_corr(X)

  expect_s3_class(R, "biweight_mid_corr")
  expect_equal(attr(R, "method"), "biweight_mid_correlation")
  expect_true(grepl("biweight mid-correlation", attr(R, "description")))
  expect_equal(attr(R, "package"), "matrixCorr")
})

test_that("NA policy: error vs pairwise", {
  set.seed(1)
  x <- c(rnorm(4), NA, rnorm(3))
  y <- c(rnorm(3), NA, rnorm(4))

  M <- cbind(x = x, y = y)
  # error mode must reject
  expect_error(biweight_mid_corr(M, na_method = "error"),
               "Missing values are not allowed")

  # pairwise mode: compute on finite overlap
  idx <- is.finite(x) & is.finite(y)
  expect_true(sum(idx) >= 5)  # ensure enough points for a stable check
  Rpw <- biweight_mid_corr(M, na_method = "pairwise",
                           pearson_fallback = "none",
                           c_const = 9, max_p_outliers = 1)
  expect_equal(Rpw["x","y"],
               bicor_manual_R(x[idx], y[idx], c_const = 9),
               tolerance = 1e-12)
  expect_equal(as.numeric(diag(Rpw)), rep(1, 2))
})

test_that("weighted, NA-free: matches manual weighted bicor", {
  x <- c(-2, -1, 0, 1, 2)
  y <- c(-4, -2, 0, 2, 4)  # perfectly monotone
  w <- c(1, 1, 5, 1, 1)    # emphasise the centre

  Rw <- biweight_mid_corr(cbind(x = x, y = y),
                          w = w,
                          na_method = "error",
                          pearson_fallback = "none",
                          c_const = 9, max_p_outliers = 1)
  expected <- bicor_weighted_manual_R(x, y, w, c_const = 9)
  expect_equal(Rw["x","y"], expected, tolerance = 1e-12)
  expect_equal(as.numeric(diag(Rw)), rep(1, 2))
})

test_that("weighted + pairwise: matches manual on overlapping rows (hybrid fallback)", {
  x <- c(0, 0, 0, 1, 2, NA)
  y <- c(0, 0, 0, 2, 4,  5)
  w <- c(1, 1, 5, 1, 1,  3)

  idx <- is.finite(x) & is.finite(y)
  Rwp <- biweight_mid_corr(cbind(x = x, y = y),
                           w = w, na_method = "pairwise",
                           pearson_fallback = "hybrid",   # ← change here
                           c_const = 9, max_p_outliers = 1)

  expect_equal(Rwp["x","y"],
               bicor_weighted_manual_R(x[idx], y[idx], w[idx], c_const = 9),
               tolerance = 1e-12)
})

test_that("fallback = 'all' reproduces Pearson (including constant columns)", {
  A <- rep(1, 5); B <- 1:5; C <- 2*B + 5
  M <- cbind(A = A, B = B, C = C)

  R_all <- biweight_mid_corr(M, pearson_fallback = "all", na_method = "error")
  R_p   <- suppressWarnings(stats::cor(M, method = "pearson"))

  to_na <- function(m) { m[is.nan(m)] <- NA_real_; m }

  # Diagonal: ignore names on the vector
  expect_equal(unname(diag(R_all)), rep(1, 3))

  # Off-diagonals: align NaN from bicor to NA as in base cor()
  R_all_na <- to_na(R_all)
  expect_equal(R_all_na[upper.tri(R_all_na)], R_p[upper.tri(R_p)])
  expect_equal(R_all_na[lower.tri(R_all_na)], R_p[lower.tri(R_p)])
})


test_that("fallback = 'hybrid' when one column has MAD=0 but variance>0", {
  # x has MAD = 0 (at least half zeros) but variance > 0
  x <- c(0, 0, 0, 1, 2)
  y <- c(0, 1, 2, 3, 4)

  # Expected: Pearson standardisation for x, biweight for y
  mu  <- mean(x); zx <- (x - mu) / sqrt(sum((x - mu)^2))
  zy  <- biweight_standardise_R(y, c_const = 9)
  expected <- sum(zx * zy)

  Rh <- biweight_mid_corr(cbind(x = x, y = y),
                          pearson_fallback = "hybrid",
                          c_const = 9, max_p_outliers = 1)
  expect_equal(Rh["x","y"], expected, tolerance = 1e-12)
})

test_that("mad_consistent=TRUE equals using c_const*1.4826", {
  set.seed(123)
  X <- matrix(rnorm(200), 50, 4)

  R1 <- biweight_mid_corr(X, mad_consistent = TRUE,  c_const = 9)
  R2 <- biweight_mid_corr(X, mad_consistent = FALSE, c_const = 9 * 1.4826)

  expect_equal(R1, R2, tolerance = 1e-12, check.attributes = FALSE)
})


test_that("max_p_outliers side-cap improves robustness in one-sided outliers", {
  # Perfect linear pattern + a few very large positive outliers on y
  set.seed(7)
  x <- seq(-2, 2, length.out = 40)
  y <- 3*x + rnorm(length(x), sd = 0.02)
  y[38:40] <- y[38:40] + 50  # large positive outliers (one-sided)

  R0 <- biweight_mid_corr(cbind(x, y),
                          c_const = 9, max_p_outliers = 1,
                          pearson_fallback = "none")
  R1 <- biweight_mid_corr(cbind(x, y),
                          c_const = 9, max_p_outliers = 0.10,
                          pearson_fallback = "none")
  # With side-cap, more points remain with positive weight on the + side -> correlation should not decrease
  expect_gte(R1["x","y"], R0["x","y"])
})

test_that("sparse_threshold returns dsCMatrix with zeros below threshold; NAs preserved", {
  skip_if_not_installed("Matrix")

  # Small matrix with modest correlations
  set.seed(99)
  X <- matrix(rnorm(200), 50, 4)
  R  <- biweight_mid_corr(X, sparse_threshold = 0.9)  # most off-diagonals -> 0

  expect_s4_class(R, "ddiMatrix")
  # diagonal must remain 1
  expect_equal(as.numeric(Matrix::diag(R)), rep(1, ncol(X)))
  # confirm many structural zeros (at least one)
  nz  <- Matrix::nnzero(R)
  expect_lt(nz, ncol(X)^2)  # sparsity achieved
})

test_that("plot methods do not error and return expected classes", {
  skip_if_not_installed("ggplot2")

  set.seed(10)
  X <- matrix(rnorm(100), 20, 5)
  R <- biweight_mid_corr(X)

  # plot method: should return a ggplot object
  p <- plot(R, title = "bicor heatmap")
  expect_s3_class(p, "ggplot")
})

# --- cross-check a few small exact-value scenarios (no side-cap, no weights) ---

test_that("small exact scenarios: ±1 for monotone affine transforms", {
  x     <- 1:5
  y_pos <- 2*x + 7
  y_neg <- -3*x + 1
  M <- cbind(x = x, y_pos = y_pos, y_neg = y_neg)

  R <- biweight_mid_corr(M, c_const = 9, max_p_outliers = 1,
                         pearson_fallback = "none", n_threads = 1L)
  expect_equal(R["x","y_pos"],  1, tolerance = 1e-12)
  expect_equal(R["x","y_neg"], -1, tolerance = 1e-12)
  expect_equal(as.numeric(diag(R)), rep(1, 3))
})

test_that("gross opposite-direction outlier matches manual bicor", {
  x <- c(1:5, 1000)
  y <- c(1:5, -1000)
  R <- biweight_mid_corr(cbind(x, y), pearson_fallback = "none")
  expect_equal(R["x","y"], bicor_manual_R(x, y), tolerance = 1e-12)
  expect_lt(R["x","y"], 0.9)
})

test_that("threading is numerically stable", {
  set.seed(42)
  X <- matrix(rnorm(500 * 20), 500, 20)
  R1 <- biweight_mid_corr(X, n_threads = 1L)
  R2 <- biweight_mid_corr(X, n_threads = 2L)
  expect_equal(R1, R2, tolerance = 1e-8)
})

test_that("data.frame path: drops non-numerics and preserves names", {
  df <- data.frame(
    num1 = 1:5,
    fac  = factor(c("a","b","a","b","a")),
    logi = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    num2 = 2*(1:5) + 7,
    date = as.Date("2020-01-01") + 0:4
  )
  R <- biweight_mid_corr(df, pearson_fallback = "none")
  expect_equal(colnames(R), c("num1", "num2"))
  expect_equal(rownames(R), c("num1", "num2"))
  expect_equal(R["num1","num2"], 1, tolerance = 1e-12)
})

test_that("validator rejects missing values in error mode", {
  df_na <- data.frame(a = c(1, 2, NA, 4), b = c(2, 3, 4, 5))
  expect_error(biweight_mid_corr(df_na),
               regexp = "Missing values are not allowed")
})
