# Tests for uncovered depth and metric validation/edge cases

# =============================================================================
# Depth function input validation
# =============================================================================

test_that("depth functions reject non-fdata input", {
  expect_error(.depth.FM(list()), "fdata")
  expect_error(.depth.BD(list()), "fdata")
  expect_error(.depth.MBD(list()), "fdata")
  expect_error(.depth.MEI(list()), "fdata")
  expect_error(.depth.mode(list()), "fdata")
  expect_error(.depth.RP(list()), "fdata")
  expect_error(.depth.RT(list()), "fdata")
  expect_error(.depth.FSD(list()), "fdata")
  expect_error(.depth.KFSD(list()), "fdata")
  expect_error(.depth.RPD(list()), "fdata")
})

test_that("depth functions reject non-fdata fdataori", {
  fd <- fdata(matrix(rnorm(50), 5, 10))
  expect_error(.depth.FM(fd, fdataori = list()), "fdata")
  expect_error(.depth.BD(fd, fdataori = list()), "fdata")
  expect_error(.depth.MBD(fd, fdataori = list()), "fdata")
  expect_error(.depth.MEI(fd, fdataori = list()), "fdata")
  expect_error(.depth.mode(fd, fdataori = list()), "fdata")
  expect_error(.depth.RP(fd, fdataori = list()), "fdata")
  expect_error(.depth.RT(fd, fdataori = list()), "fdata")
  expect_error(.depth.FSD(fd, fdataori = list()), "fdata")
  expect_error(.depth.KFSD(fd, fdataori = list()), "fdata")
  expect_error(.depth.RPD(fd, fdataori = list()), "fdata")
})

test_that("depth functions reject column mismatch", {
  fd_10 <- fdata(matrix(rnorm(50), 5, 10))
  fd_20 <- fdata(matrix(rnorm(100), 5, 20))
  expect_error(.depth.FM(fd_10, fdataori = fd_20))
  expect_error(.depth.BD(fd_10, fdataori = fd_20))
  expect_error(.depth.MBD(fd_10, fdataori = fd_20))
  expect_error(.depth.mode(fd_10, fdataori = fd_20))
  expect_error(.depth.RP(fd_10, fdataori = fd_20))
  expect_error(.depth.RT(fd_10, fdataori = fd_20))
  expect_error(.depth.FSD(fd_10, fdataori = fd_20))
  expect_error(.depth.KFSD(fd_10, fdataori = fd_20))
})

test_that(".depth.BD rejects insufficient reference curves", {
  fd <- fdata(matrix(rnorm(50), 5, 10))
  fd_one <- fdata(matrix(rnorm(10), 1, 10))
  expect_error(.depth.BD(fd, fdataori = fd_one), "at least 2")
})

test_that(".depth.RPD rejects 2D data", {
  fd2d <- fdata(array(rnorm(200), c(5, 4, 10)),
                argvals = list(1:4, 1:10), fdata2d = TRUE)
  expect_error(.depth.RPD(fd2d), "2D")
})

test_that(".depth.BD rejects 2D data", {
  fd2d <- fdata(array(rnorm(200), c(5, 4, 10)),
                argvals = list(1:4, 1:10), fdata2d = TRUE)
  expect_error(.depth.BD(fd2d), "2D")
})

test_that("depth functions detect 1D/2D mismatch", {
  fd1d <- fdata(matrix(rnorm(50), 5, 10))
  fd2d <- fdata(array(rnorm(200), c(5, 4, 10)),
                argvals = list(1:4, 1:10), fdata2d = TRUE)
  expect_error(.depth.FM(fd1d, fdataori = fd2d))
  expect_error(.depth.mode(fd1d, fdataori = fd2d))
})

test_that(".depth.mode works with 2D data", {
  fd2d <- fdata(array(rnorm(200), c(5, 4, 10)),
                argvals = list(1:4, 1:10), fdata2d = TRUE)
  result <- .depth.mode(fd2d)
  expect_length(result, 5)
  expect_true(all(is.finite(result)))
})

test_that(".depth.RP works with 2D data", {
  fd2d <- fdata(array(rnorm(200), c(5, 4, 10)),
                argvals = list(1:4, 1:10), fdata2d = TRUE)
  result <- .depth.RP(fd2d)
  expect_length(result, 5)
})

test_that(".depth.RT works with 2D data", {
  fd2d <- fdata(array(rnorm(200), c(5, 4, 10)),
                argvals = list(1:4, 1:10), fdata2d = TRUE)
  result <- .depth.RT(fd2d)
  expect_length(result, 5)
})

test_that(".depth.FSD works with 2D data", {
  fd2d <- fdata(array(rnorm(200), c(5, 4, 10)),
                argvals = list(1:4, 1:10), fdata2d = TRUE)
  result <- .depth.FSD(fd2d)
  expect_length(result, 5)
})

test_that(".depth.KFSD works with 2D data", {
  fd2d <- fdata(array(rnorm(200), c(5, 4, 10)),
                argvals = list(1:4, 1:10), fdata2d = TRUE)
  result <- .depth.KFSD(fd2d)
  expect_length(result, 5)
})

test_that(".depth.RPD with single derivative", {
  fd <- fdata(matrix(rnorm(100), 10, 10))
  result <- .depth.RPD(fd, deriv = c(0))
  expect_length(result, 10)
})

test_that(".depth.RPD rejects excessive derivative order", {
  fd <- fdata(matrix(rnorm(100), 10, 10))
  expect_error(.depth.RPD(fd, deriv = c(0, 10)))
})

test_that("gmed rejects non-fdata input", {
  expect_error(gmed(list()), "fdata")
})

test_that("gmed works with 2D data", {
  fd2d <- fdata(array(rnorm(200), c(5, 4, 10)),
                argvals = list(1:4, 1:10), fdata2d = TRUE)
  result <- gmed(fd2d)
  expect_s3_class(result, "fdata")
})

test_that("var and sd work with 2D data", {
  fd2d <- fdata(array(rnorm(200), c(5, 4, 10)),
                argvals = list(1:4, 1:10), fdata2d = TRUE)
  v <- var(fd2d)
  s <- sd(fd2d)
  expect_s3_class(v, "fdata")
  expect_s3_class(s, "fdata")
})

# =============================================================================
# Metric function input validation
# =============================================================================

test_that("metric functions reject non-fdata input", {
  expect_error(metric.hausdorff(list()), "fdata")
  expect_error(metric.DTW(list()), "fdata")
  expect_error(semimetric.pca(list()), "fdata")
  expect_error(semimetric.deriv(list()), "fdata")
  expect_error(semimetric.basis(list()), "fdata")
  expect_error(semimetric.fourier(list()), "fdata")
  expect_error(semimetric.hshift(list()), "fdata")
  expect_error(metric.kl(list()), "fdata")
})

test_that("metric functions reject non-fdata fdataref", {
  fd <- fdata(matrix(rnorm(50), 5, 10))
  expect_error(metric.lp(fd, list()), "fdata")
  expect_error(metric.hausdorff(fd, list()), "fdata")
  expect_error(metric.DTW(fd, list()), "fdata")
  expect_error(semimetric.pca(fd, list()), "fdata")
  expect_error(semimetric.deriv(fd, list()), "fdata")
  expect_error(semimetric.basis(fd, list()), "fdata")
  expect_error(semimetric.fourier(fd, list()), "fdata")
  expect_error(semimetric.hshift(fd, list()), "fdata")
  expect_error(metric.kl(fd, list()), "fdata")
})

test_that("metric functions reject 1D/2D mismatch", {
  fd1d <- fdata(matrix(rnorm(50), 5, 10))
  fd2d <- fdata(array(rnorm(200), c(5, 4, 10)),
                argvals = list(1:4, 1:10), fdata2d = TRUE)
  expect_error(metric.lp(fd1d, fd2d))
  expect_error(metric.hausdorff(fd1d, fd2d))
  expect_error(metric.DTW(fd1d, fd2d))
  expect_error(semimetric.pca(fd1d, fd2d))
  expect_error(semimetric.deriv(fd1d, fd2d))
  expect_error(semimetric.fourier(fd1d, fd2d))
  expect_error(semimetric.hshift(fd1d, fd2d))
  expect_error(metric.kl(fd1d, fd2d))
})

test_that("metric functions reject column mismatch", {
  fd_10 <- fdata(matrix(rnorm(50), 5, 10))
  fd_20 <- fdata(matrix(rnorm(100), 5, 20))
  expect_error(metric.lp(fd_10, fd_20))
  expect_error(metric.hausdorff(fd_10, fd_20))
  expect_error(semimetric.hshift(fd_10, fd_20))
  expect_error(metric.kl(fd_10, fd_20))
})

test_that("metric.DTW rejects 2D data", {
  fd2d <- fdata(array(rnorm(200), c(5, 4, 10)),
                argvals = list(1:4, 1:10), fdata2d = TRUE)
  expect_error(metric.DTW(fd2d), "2D")
})

test_that("semimetric.basis rejects 2D data", {
  fd2d <- fdata(array(rnorm(200), c(5, 4, 10)),
                argvals = list(1:4, 1:10), fdata2d = TRUE)
  expect_error(semimetric.basis(fd2d), "2D")
})

test_that("semimetric.fourier rejects 2D data", {
  fd2d <- fdata(array(rnorm(200), c(5, 4, 10)),
                argvals = list(1:4, 1:10), fdata2d = TRUE)
  expect_error(semimetric.fourier(fd2d), "2D")
})

test_that("semimetric.hshift rejects 2D data", {
  fd2d <- fdata(array(rnorm(200), c(5, 4, 10)),
                argvals = list(1:4, 1:10), fdata2d = TRUE)
  expect_error(semimetric.hshift(fd2d), "2D")
})

test_that("metric.kl rejects 2D data", {
  fd2d <- fdata(array(rnorm(200), c(5, 4, 10)),
                argvals = list(1:4, 1:10), fdata2d = TRUE)
  expect_error(metric.kl(fd2d), "2D")
})

test_that("metric.lp weight validation", {
  fd <- fdata(matrix(rnorm(50), 5, 10))
  expect_error(metric.lp(fd, w = rep(1, 11)))
})

test_that("metric functions with rownames set dimnames", {
  X <- matrix(rnorm(50), 5, 10)
  rownames(X) <- paste0("f", 1:5)
  fd <- fdata(X)

  d_lp <- metric.lp(fd)
  expect_true(!is.null(dimnames(d_lp)))

  d_haus <- metric.hausdorff(fd)
  expect_true(!is.null(dimnames(d_haus)))

  d_dtw <- metric.DTW(fd)
  expect_true(!is.null(dimnames(d_dtw)))

  d_four <- semimetric.fourier(fd)
  expect_true(!is.null(dimnames(d_four)))

  d_hs <- semimetric.hshift(fd)
  expect_true(!is.null(dimnames(d_hs)))

  d_kl <- metric.kl(fd)
  expect_true(!is.null(dimnames(d_kl)))
})

test_that("metric.lp 2D self-distance works", {
  fd2d <- fdata(array(rnorm(200), c(5, 4, 10)),
                argvals = list(1:4, 1:10), fdata2d = TRUE)
  d <- metric.lp(fd2d)
  expect_equal(dim(d), c(5, 5))
  expect_true(all(diag(d) < 1e-10))
})

test_that("metric.hausdorff 2D self-distance works", {
  fd2d <- fdata(array(rnorm(200), c(5, 4, 10)),
                argvals = list(1:4, 1:10), fdata2d = TRUE)
  d <- metric.hausdorff(fd2d)
  expect_equal(dim(d), c(5, 5))
  expect_true(all(diag(d) < 1e-10))
})

test_that("semimetric.deriv 2D works", {
  fd2d <- fdata(array(rnorm(500), c(5, 10, 10)),
                argvals = list(1:10, 1:10), fdata2d = TRUE)
  d <- semimetric.deriv(fd2d, nderiv = 1)
  expect_equal(dim(d), c(5, 5))
})

test_that("semimetric.deriv 2D cross-distance rejects mismatched types", {
  fd1d <- fdata(matrix(rnorm(50), 5, 10))
  fd2d <- fdata(array(rnorm(200), c(5, 4, 10)),
                argvals = list(1:4, 1:10), fdata2d = TRUE)
  expect_error(semimetric.deriv(fd2d, fd1d))
})

test_that("semimetric.deriv 1D cross-distance rejects 2D ref", {
  fd1d <- fdata(matrix(rnorm(50), 5, 10))
  fd2d <- fdata(array(rnorm(200), c(5, 4, 10)),
                argvals = list(1:4, 1:10), fdata2d = TRUE)
  expect_error(semimetric.deriv(fd1d, fd2d))
})

test_that("semimetric.pca 2D dimension mismatch", {
  fd2d_a <- fdata(array(rnorm(200), c(5, 4, 10)),
                  argvals = list(1:4, 1:10), fdata2d = TRUE)
  fd2d_b <- fdata(array(rnorm(300), c(5, 5, 12)),
                  argvals = list(1:5, 1:12), fdata2d = TRUE)
  expect_error(semimetric.pca(fd2d_a, fd2d_b))
})
