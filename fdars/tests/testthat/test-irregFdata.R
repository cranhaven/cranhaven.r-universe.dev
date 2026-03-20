# Tests for irregFdata class and functions

test_that("irregFdata constructor validates input", {
  # Valid input
  argvals <- list(c(0, 0.5, 1), c(0, 1))
  X <- list(c(1, 2, 3), c(1, 3))
  ifd <- irregFdata(argvals, X)

  expect_s3_class(ifd, "irregFdata")
  expect_equal(ifd$n, 2)
  expect_equal(length(ifd$argvals), 2)
  expect_equal(length(ifd$X), 2)

  # Invalid: mismatched lengths
  expect_error(irregFdata(list(c(0, 1)), list(c(1, 2, 3))))

  # Invalid: argvals and X different lengths
  expect_error(irregFdata(list(c(0, 1)), list()))
})

test_that("irregFdata detects unsorted argvals", {
  expect_error(irregFdata(list(c(0.5, 0, 1)), list(c(1, 2, 3))))
})

test_that("is.irregular works correctly", {
  t <- seq(0, 1, length.out = 50)
  fd <- simFunData(n = 5, argvals = t, M = 3, seed = 42)
  expect_false(is.irregular(fd))

  ifd <- sparsify(fd, minObs = 5, maxObs = 20, seed = 123)
  expect_true(is.irregular(ifd))
})

test_that("sparsify creates valid irregFdata", {
  t <- seq(0, 1, length.out = 100)
  fd <- simFunData(n = 10, argvals = t, M = 5, seed = 42)

  ifd <- sparsify(fd, minObs = 10, maxObs = 30, seed = 123)

  expect_s3_class(ifd, "irregFdata")
  expect_equal(ifd$n, 10)

  # Check obs counts are within range
  obs_counts <- sapply(ifd$X, length)
  expect_true(all(obs_counts >= 10))
  expect_true(all(obs_counts <= 30))
})

test_that("sparsify is reproducible with seed", {
  t <- seq(0, 1, length.out = 50)
  fd <- simFunData(n = 5, argvals = t, M = 3, seed = 42)

  ifd1 <- sparsify(fd, minObs = 5, maxObs = 20, seed = 123)
  ifd2 <- sparsify(fd, minObs = 5, maxObs = 20, seed = 123)

  expect_equal(ifd1$argvals, ifd2$argvals)
  expect_equal(ifd1$X, ifd2$X)
})

test_that("sparsify with probability function works", {
  t <- seq(0, 1, length.out = 100)
  fd <- simFunData(n = 20, argvals = t, M = 5, seed = 42)

  # More points in middle
  prob_middle <- function(t) dnorm(t, mean = 0.5, sd = 0.2)
  ifd <- sparsify(fd, minObs = 20, maxObs = 30, prob = prob_middle, seed = 123)

  expect_s3_class(ifd, "irregFdata")

  # Check that points tend to be more dense around 0.5
  all_t <- unlist(ifd$argvals)
  middle_count <- sum(all_t > 0.3 & all_t < 0.7)
  edge_count <- sum(all_t <= 0.3 | all_t >= 0.7)
  expect_true(middle_count > edge_count)
})

test_that("irregFdata subsetting works", {
  argvals <- list(c(0, 0.5, 1), c(0, 0.3, 0.6, 1), c(0, 1))
  X <- list(c(1, 2, 3), c(1, 2, 3, 4), c(1, 4))
  ifd <- irregFdata(argvals, X, id = c("a", "b", "c"))

  # Positive indices
  sub1 <- ifd[1:2]
  expect_equal(sub1$n, 2)
  expect_equal(sub1$id, c("a", "b"))

  # Single index
  sub2 <- ifd[2]
  expect_equal(sub2$n, 1)
  expect_equal(sub2$id, "b")

  # Negative indices
  sub3 <- ifd[-1]
  expect_equal(sub3$n, 2)
  expect_equal(sub3$id, c("b", "c"))
})

test_that("as.fdata.irregFdata with NA method works", {
  t <- seq(0, 1, length.out = 50)
  fd <- simFunData(n = 5, argvals = t, M = 3, seed = 42)
  ifd <- sparsify(fd, minObs = 10, maxObs = 20, seed = 123)

  fd_back <- as.fdata(ifd, method = "na")

  expect_s3_class(fd_back, "fdata")
  expect_true(any(is.na(fd_back$data)))
})

test_that("as.fdata.irregFdata with linear interpolation works", {
  argvals <- list(c(0, 0.5, 1), c(0, 0.25, 0.75, 1))
  X <- list(c(0, 1, 0), c(0, 0.5, 0.5, 0))
  ifd <- irregFdata(argvals, X)

  target <- seq(0, 1, length.out = 5)
  fd <- as.fdata(ifd, argvals = target, method = "linear")

  expect_s3_class(fd, "fdata")
  expect_equal(ncol(fd$data), 5)
  expect_false(any(is.na(fd$data)))  # No NAs with linear interpolation
})

test_that("int.simpson computes correct integrals for irregFdata", {
  # Constant function = 1 over [0, 1] should integrate to 1
  argvals <- list(c(0, 0.5, 1), c(0, 0.5, 1))
  X <- list(c(1, 1, 1), c(2, 2, 2))
  ifd <- irregFdata(argvals, X)

  integrals <- int.simpson(ifd)

  expect_equal(length(integrals), 2)
  expect_true(abs(integrals[1] - 1) < 0.01)
  expect_true(abs(integrals[2] - 2) < 0.01)
})

test_that("norm computes correct L2 norms for irregFdata", {
  # Constant function = c over [0, 1] has L2 norm = c
  argvals <- list(c(0, 0.5, 1))
  X <- list(c(3, 3, 3))
  ifd <- irregFdata(argvals, X)

  norms <- norm(ifd, p = 2)

  expect_equal(length(norms), 1)
  expect_true(abs(norms[1] - 3) < 0.01)
})

test_that("mean.irregFdata estimates mean function", {
  t <- seq(0, 1, length.out = 100)
  fd <- simFunData(n = 50, argvals = t, M = 3, seed = 42)
  ifd <- sparsify(fd, minObs = 20, maxObs = 40, seed = 123)

  mean_fd <- mean(ifd, bandwidth = 0.1)

  expect_s3_class(mean_fd, "fdata")
  expect_equal(ncol(mean_fd$data), 100)  # Default 100 points

  # Mean should not have NaN if bandwidth is reasonable
  expect_false(any(is.nan(mean_fd$data)))
})

test_that("metric.lp computes distance matrix for irregFdata", {
  argvals <- list(c(0, 0.5, 1), c(0, 0.5, 1), c(0, 0.5, 1))
  X <- list(c(0, 0, 0), c(1, 1, 1), c(0, 0, 0))
  ifd <- irregFdata(argvals, X)

  D <- metric.lp(ifd, p = 2)

  expect_equal(dim(D), c(3, 3))
  expect_true(all(diag(D) < 0.01))  # Diagonal should be ~0
  expect_true(D[1, 3] < 0.01)  # Curves 1 and 3 are identical
  expect_true(D[1, 2] > 0.9)  # Curve 2 is different
  expect_equal(D[1, 2], D[2, 1])  # Symmetric
})

test_that("irregFdata preserves id and metadata", {
  argvals <- list(c(0, 1), c(0, 1))
  X <- list(c(1, 2), c(3, 4))
  meta <- data.frame(group = c("A", "B"))
  ifd <- irregFdata(argvals, X, id = c("obs1", "obs2"), metadata = meta)

  expect_equal(ifd$id, c("obs1", "obs2"))
  expect_equal(ifd$metadata$group, c("A", "B"))

  # Subsetting preserves metadata
  sub <- ifd[1]
  expect_equal(sub$id, "obs1")
  expect_equal(sub$metadata$group, "A")
})

test_that("print.irregFdata works without error", {
  argvals <- list(c(0, 0.5, 1), c(0, 1))
  X <- list(c(1, 2, 3), c(1, 3))
  ifd <- irregFdata(argvals, X)

  expect_output(print(ifd), "Irregular Functional Data")
})

test_that("plot.irregFdata works without error", {
  argvals <- list(c(0, 0.5, 1), c(0, 0.3, 0.7, 1))
  X <- list(c(1, 2, 1), c(0, 1, 1.5, 0.5))
  ifd <- irregFdata(argvals, X)

  # Should not error
  expect_silent(plot(ifd))
})

test_that("fdata2basis works with irregFdata using bspline", {
  # Create irregular data from simulated curves
  t <- seq(0, 1, length.out = 50)
  fd <- simFunData(n = 5, argvals = t, M = 3, seed = 42)
  ifd <- sparsify(fd, minObs = 15, maxObs = 25, seed = 123)

  # Fit B-spline basis
  coefs <- fdata2basis(ifd, nbasis = 8, type = "bspline")

  expect_equal(nrow(coefs), 5)  # 5 curves
  expect_equal(ncol(coefs), 8)  # 8 basis functions
  expect_false(any(is.na(coefs)))
})

test_that("fdata2basis works with irregFdata using fourier", {
  # Create irregular data
  t <- seq(0, 1, length.out = 50)
  fd <- simFunData(n = 5, argvals = t, M = 3, seed = 42)
  ifd <- sparsify(fd, minObs = 15, maxObs = 25, seed = 123)

  # Fit Fourier basis
  coefs <- fdata2basis(ifd, nbasis = 7, type = "fourier")

  expect_equal(nrow(coefs), 5)  # 5 curves
  expect_equal(ncol(coefs), 7)  # 7 basis functions
  expect_false(any(is.na(coefs)))
})

test_that("fdata2basis.irregFdata matches regular fdata2basis approximately", {
  # Create regular data
  set.seed(42)
  t <- seq(0, 1, length.out = 50)
  X <- matrix(sin(2 * pi * t), nrow = 1)
  fd <- fdata(X, argvals = t)

  # Create "irregular" version with same points (should give similar result)
  ifd <- irregFdata(argvals = list(t), X = list(X[1, ]))

  # Fit basis on both
  coefs_regular <- fdata2basis(fd, nbasis = 9, type = "fourier")
  coefs_irreg <- fdata2basis(ifd, nbasis = 9, type = "fourier")

  # They should be very similar
  expect_equal(as.vector(coefs_regular), as.vector(coefs_irreg), tolerance = 1e-6)
})

test_that("fdata2basis.irregFdata can reconstruct curves", {
  # Create a known sine curve
  t <- seq(0, 1, length.out = 100)
  true_values <- sin(2 * pi * t)

  # Sample irregularly
  set.seed(123)
  sample_idx <- sort(sample(1:100, 30))
  ifd <- irregFdata(
    argvals = list(t[sample_idx]),
    X = list(true_values[sample_idx])
  )

  # Fit Fourier basis (which should capture a sine well)
  coefs <- fdata2basis(ifd, nbasis = 5, type = "fourier")

  # Reconstruct on original grid
  fd_recon <- basis2fdata(coefs, argvals = t, type = "fourier")

  # Check reconstruction error is small
  recon_error <- sqrt(mean((fd_recon$data[1, ] - true_values)^2))
  expect_lt(recon_error, 0.1)  # RMSE < 0.1
})

# =============================================================================
# Tests for standardize and scale_minmax
# =============================================================================

test_that("standardize.fdata produces mean 0 and sd 1", {
  t <- seq(0, 1, length.out = 50)
  # Create data with different means and sds
  X <- matrix(0, 5, 50)
  X[1, ] <- rnorm(50, mean = 10, sd = 2)
  X[2, ] <- rnorm(50, mean = -5, sd = 0.5)
  X[3, ] <- rnorm(50, mean = 100, sd = 10)
  X[4, ] <- rnorm(50, mean = 0, sd = 1)
  X[5, ] <- rnorm(50, mean = 50, sd = 5)
  fd <- fdata(X, argvals = t)

  fd_std <- standardize(fd)

  # Check each curve has mean ~0 and sd ~1
  means <- rowMeans(fd_std$data)
  sds <- apply(fd_std$data, 1, sd)

  expect_equal(means, rep(0, 5), tolerance = 1e-10)
  expect_equal(sds, rep(1, 5), tolerance = 1e-10)
})

test_that("standardize.fdata handles constant curves", {
  t <- seq(0, 1, length.out = 20)
  X <- matrix(c(rep(5, 20), rnorm(20)), 2, 20, byrow = TRUE)
  fd <- fdata(X, argvals = t)

  # Should not error on constant curve
  fd_std <- standardize(fd)

  # Constant curve becomes all zeros (mean 0, but sd was 0)
  expect_equal(fd_std$data[1, ], rep(0, 20))
})

test_that("scale_minmax.fdata produces values in [0,1]", {
  t <- seq(0, 1, length.out = 50)
  X <- matrix(rnorm(150) * 10 + 50, 3, 50)
  fd <- fdata(X, argvals = t)

  fd_scaled <- scale_minmax(fd)

  # Check each curve is in [0, 1]
  for (i in 1:3) {
    expect_gte(min(fd_scaled$data[i, ]), 0)
    expect_lte(max(fd_scaled$data[i, ]), 1)
    expect_equal(min(fd_scaled$data[i, ]), 0)
    expect_equal(max(fd_scaled$data[i, ]), 1)
  }
})

test_that("scale_minmax.fdata with custom range", {
  t <- seq(0, 1, length.out = 20)
  X <- matrix(rnorm(40), 2, 20)
  fd <- fdata(X, argvals = t)

  fd_scaled <- scale_minmax(fd, min = -1, max = 1)

  # Check each curve is in [-1, 1]
  for (i in 1:2) {
    expect_equal(min(fd_scaled$data[i, ]), -1)
    expect_equal(max(fd_scaled$data[i, ]), 1)
  }
})

test_that("standardize.irregFdata works correctly", {
  argvals <- list(c(0, 0.5, 1), c(0, 0.25, 0.5, 0.75, 1))
  X <- list(c(10, 20, 30), c(100, 110, 120, 130, 140))
  ifd <- irregFdata(argvals, X)

  ifd_std <- standardize(ifd)

  # Check each curve has mean ~0 and sd ~1
  for (i in 1:2) {
    expect_equal(mean(ifd_std$X[[i]]), 0, tolerance = 1e-10)
    expect_equal(sd(ifd_std$X[[i]]), 1, tolerance = 1e-10)
  }
})

test_that("scale_minmax.irregFdata works correctly", {
  argvals <- list(c(0, 0.5, 1), c(0, 0.25, 0.5, 0.75, 1))
  X <- list(c(10, 20, 30), c(100, 110, 120, 130, 140))
  ifd <- irregFdata(argvals, X)

  ifd_scaled <- scale_minmax(ifd)

  # Check each curve is in [0, 1]
  for (i in 1:2) {
    expect_equal(min(ifd_scaled$X[[i]]), 0)
    expect_equal(max(ifd_scaled$X[[i]]), 1)
  }

  # Check with custom range
  ifd_scaled2 <- scale_minmax(ifd, min = -1, max = 1)
  for (i in 1:2) {
    expect_equal(min(ifd_scaled2$X[[i]]), -1)
    expect_equal(max(ifd_scaled2$X[[i]]), 1)
  }
})

# ==============================================================================
# Additional tests to improve coverage
# ==============================================================================

test_that("print.irregFdata works", {
  argvals <- list(c(0, 0.5, 1), c(0, 0.25, 0.5, 0.75, 1))
  X <- list(c(1, 2, 3), c(4, 5, 6, 7, 8))
  ifd <- irregFdata(argvals, X)

  expect_output(print(ifd), "Irregular Functional Data")
})

test_that("summary.irregFdata works", {
  argvals <- list(c(0, 0.5, 1), c(0, 0.25, 0.5, 0.75, 1))
  X <- list(c(1, 2, 3), c(4, 5, 6, 7, 8))
  ifd <- irregFdata(argvals, X)

  s <- summary(ifd)
  expect_output(print(s), "Irregular Functional Data")
})

test_that("[.irregFdata subsetting works", {
  argvals <- list(c(0, 0.5, 1), c(0, 0.25, 0.5, 0.75, 1), c(0, 1))
  X <- list(c(1, 2, 3), c(4, 5, 6, 7, 8), c(10, 20))
  ifd <- irregFdata(argvals, X)

  ifd_sub <- ifd[1:2]
  expect_s3_class(ifd_sub, "irregFdata")
  expect_equal(length(ifd_sub$X), 2)
})

test_that("as.fdata.irregFdata converts to regular fdata", {
  set.seed(42)
  argvals <- list(
    c(0, 0.25, 0.5, 0.75, 1),
    c(0, 0.2, 0.4, 0.6, 0.8, 1),
    c(0, 0.33, 0.67, 1)
  )
  X <- list(
    sin(2*pi*argvals[[1]]),
    sin(2*pi*argvals[[2]]),
    sin(2*pi*argvals[[3]])
  )
  ifd <- irregFdata(argvals, X)

  # Default uses union of all observation points
  fd <- as.fdata(ifd)
  expect_s3_class(fd, "fdata")
  expect_equal(nrow(fd$data), 3)

  # Can specify custom argvals
  fd2 <- as.fdata(ifd, argvals = seq(0, 1, length.out = 20))
  expect_equal(ncol(fd2$data), 20)
})

test_that("as.fdata.fdata returns same object", {
  t_grid <- seq(0, 1, length.out = 10)
  X <- matrix(rnorm(30), 3, 10)
  fd <- fdata(X, argvals = t_grid)

  fd2 <- as.fdata(fd)
  expect_identical(fd, fd2)
})

test_that("sparsify creates irregular data from fdata", {
  t_grid <- seq(0, 1, length.out = 50)
  X <- matrix(rnorm(100), 2, 50)
  fd <- fdata(X, argvals = t_grid)

  ifd <- sparsify(fd, minObs = 15, maxObs = 25, seed = 42)
  expect_s3_class(ifd, "irregFdata")
  # Each curve should have between 15-25 points
  for (i in 1:2) {
    expect_gte(length(ifd$X[[i]]), 15)
    expect_lte(length(ifd$X[[i]]), 25)
  }
})

test_that("sparsify respects minObs constraint", {
  t_grid <- seq(0, 1, length.out = 50)
  X <- matrix(rnorm(100), 2, 50)
  fd <- fdata(X, argvals = t_grid)

  ifd <- sparsify(fd, minObs = 10, seed = 42)
  expect_s3_class(ifd, "irregFdata")
  # Each curve should have at least minObs points
  for (i in 1:2) {
    expect_gte(length(ifd$X[[i]]), 10)
  }
})

test_that("mean.irregFdata computes mean", {
  argvals <- list(
    c(0, 0.5, 1),
    c(0, 0.25, 0.5, 0.75, 1),
    c(0, 0.33, 0.67, 1)
  )
  X <- list(c(1, 2, 3), c(1, 2, 3, 4, 5), c(1, 2, 3, 4))
  ifd <- irregFdata(argvals, X)

  m <- mean(ifd, n.eval = 10)
  expect_s3_class(m, "fdata")
  expect_equal(nrow(m$data), 1)
})

test_that("plot.irregFdata works", {
  argvals <- list(c(0, 0.5, 1), c(0, 0.25, 0.5, 0.75, 1))
  X <- list(c(1, 2, 3), c(4, 5, 6, 7, 8))
  ifd <- irregFdata(argvals, X)

  expect_no_error(plot(ifd))
})

test_that("autoplot.irregFdata works", {
  skip_if_not_installed("ggplot2")
  argvals <- list(c(0, 0.5, 1), c(0, 0.25, 0.5, 0.75, 1))
  X <- list(c(1, 2, 3), c(4, 5, 6, 7, 8))
  ifd <- irregFdata(argvals, X)

  p <- ggplot2::autoplot(ifd)
  expect_s3_class(p, "ggplot")
})
