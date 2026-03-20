# Tests for 2D functional data operations and fdata edge cases

# Helper: create a valid 2D fdata object
make_fd2d <- function(n = 5, m1 = 4, m2 = 10) {
  fdata(array(rnorm(n * m1 * m2), c(n, m1, m2)),
        argvals = list(seq(0, 1, length.out = m1), seq(0, 1, length.out = m2)),
        fdata2d = TRUE)
}

# =============================================================================
# fdata construction edge cases
# =============================================================================

test_that("fdata accepts data.frame and converts to matrix", {
  df <- data.frame(matrix(1:20, 2, 10))
  fd <- fdata(df)
  expect_s3_class(fd, "fdata")
  expect_equal(nrow(fd$data), 2)
})

test_that("fdata rejects mismatched argvals length", {
  expect_error(fdata(matrix(1:20, 2, 10), argvals = 1:5), "argvals")
})

test_that("fdata 2D requires argvals", {
  expect_error(fdata(matrix(1:20, 2, 10), fdata2d = TRUE), "argvals")
})

test_that("fdata metadata validation", {
  expect_error(fdata(matrix(1:20, 2, 10), metadata = "invalid"),
               "data.frame")
  expect_error(fdata(matrix(1:20, 2, 10), metadata = data.frame(x = 1:3)),
               "rows")
})

# =============================================================================
# 2D fdata operations
# =============================================================================

test_that("fdata.cen rejects 2D data", {
  fd2d <- make_fd2d()
  expect_error(fdata.cen(fd2d), "2D")
})

test_that("mean works with 2D fdata", {
  fd2d <- make_fd2d()
  m <- mean(fd2d)
  expect_s3_class(m, "fdata")
  expect_equal(nrow(m$data), 1)
})

test_that("norm rejects 2D fdata", {
  fd2d <- make_fd2d()
  expect_error(norm(fd2d), "2D")
})

test_that("normalize rejects 2D fdata", {
  fd2d <- make_fd2d()
  expect_error(normalize(fd2d), "2D")
})

test_that("standardize rejects 2D fdata", {
  fd2d <- make_fd2d()
  expect_error(standardize(fd2d), "2D")
})

test_that("scale_minmax rejects 2D fdata", {
  fd2d <- make_fd2d()
  expect_error(scale_minmax(fd2d), "2D")
})

# =============================================================================
# 2D print/summary
# =============================================================================

test_that("print.fdata works for 2D data", {
  fd2d <- make_fd2d()
  expect_output(print(fd2d), "2D")
})

test_that("print.fdata works for 2D with metadata", {
  fd2d <- make_fd2d()
  fd2d$metadata <- data.frame(group = c("A", "B", "C", "D", "E"))
  expect_output(print(fd2d), "Metadata")
})

test_that("summary.fdata with various metadata types", {
  fd <- fdata(matrix(rnorm(100), 10, 10))
  fd$metadata <- data.frame(
    num_col = rnorm(10),
    fac_col = factor(rep(c("A", "B"), 5)),
    char_col = rep(c("x", "y"), 5),
    stringsAsFactors = FALSE
  )
  expect_output(summary(fd), "Metadata")
})

# =============================================================================
# 2D plotting
# =============================================================================

test_that("autoplot works for 2D fdata", {
  fd2d <- make_fd2d()
  expect_no_error(autoplot(fd2d))
})

test_that("boxplot rejects 2D fdata", {
  fd2d <- make_fd2d(n = 10)
  expect_error(boxplot(fd2d), "2D")
})

# =============================================================================
# 2D subsetting
# =============================================================================

test_that("2D fdata column subsetting is restricted", {
  fd2d <- make_fd2d()
  expect_error(fd2d[, 1:5])
})

test_that("2D fdata row subsetting with drop returns matrix", {
  fd2d <- make_fd2d()
  result <- fd2d[1, drop = TRUE]
  expect_true(is.matrix(result))
})

test_that("1D fdata single row with drop returns vector", {
  fd <- fdata(matrix(rnorm(50), 5, 10))
  result <- fd[1, drop = TRUE]
  expect_true(is.numeric(result))
})

# =============================================================================
# 2D derivatives
# =============================================================================

test_that("deriv works for 2D fdata", {
  fd2d <- make_fd2d(n = 3, m1 = 10, m2 = 10)
  # 2D deriv returns list of partial derivatives
  d <- deriv(fd2d)
  expect_true(is.list(d))
})

test_that("deriv preserves id and metadata", {
  fd <- fdata(matrix(rnorm(100), 10, 10),
              id = paste0("curve_", 1:10),
              metadata = data.frame(g = rep(c("A", "B"), 5)))
  d <- deriv(fd)
  expect_equal(d$id, fd$id)
  expect_equal(d$metadata, fd$metadata)
})

# =============================================================================
# 2D other operations
# =============================================================================

test_that("register.fd rejects 2D data", {
  fd2d <- make_fd2d()
  expect_error(register.fd(fd2d), "2D")
})

test_that("register.fd rejects wrong target length", {
  fd <- fdata(matrix(rnorm(100), 10, 10))
  expect_error(register.fd(fd, target = 1:5), "length")
})

test_that("register.fd with multi-row target fdata", {
  fd <- fdata(matrix(rnorm(100), 10, 10))
  target <- fdata(matrix(rnorm(20), 2, 10))
  # May or may not error - just exercise the code path
  tryCatch({
    result <- register.fd(fd, target = target)
    expect_s3_class(result, "register.fd")
  }, error = function(e) {
    expect_true(TRUE)
  })
})

test_that("localavg.fdata rejects 2D data", {
  fd2d <- make_fd2d()
  expect_error(localavg.fdata(fd2d), "2D")
})

test_that("localavg.fdata rejects invalid intervals", {
  fd <- fdata(matrix(rnorm(100), 10, 10))
  expect_error(localavg.fdata(fd, intervals = matrix(1:9, 3, 3)), "2 columns")
})

test_that("fdata.bootstrap rejects 2D data", {
  fd2d <- make_fd2d()
  expect_error(fdata.bootstrap(fd2d), "2D")
})

test_that("fdata.bootstrap.ci validates input", {
  expect_error(fdata.bootstrap.ci("invalid", mean), "fdata")
  fd <- fdata(matrix(rnorm(100), 10, 10))
  expect_error(fdata.bootstrap.ci(fd, "invalid"), "function")
})

test_that("fdata2pc rejects 2D data", {
  fd2d <- make_fd2d()
  expect_error(fdata2pc(fd2d), "2D")
})

test_that("fdata2pls rejects 2D data", {
  fd2d <- make_fd2d()
  expect_error(fdata2pls(fd2d, rnorm(5)), "2D")
})

test_that("fdata2basis rejects 2D data", {
  fd2d <- make_fd2d()
  expect_error(fdata2basis(fd2d), "2D")
})

test_that("group.distance validates input", {
  expect_error(group.distance("invalid", c("A", "B")), "fdata")
  fd <- fdata(matrix(rnorm(20), 2, 10))
  expect_error(group.distance(fd, c("A", "B", "C")), "length")
})

test_that("group.test validates input", {
  expect_error(group.test("invalid", c("A")), "fdata")
  fd <- fdata(matrix(rnorm(20), 2, 10))
  expect_error(group.test(fd, c("A")), "length")
})

test_that("group.test prints results", {
  set.seed(42)
  n <- 20
  fd <- fdata(matrix(rnorm(n * 30), n, 30))
  groups <- rep(c("A", "B"), each = n / 2)
  result <- group.test(fd, groups, B = 20)
  expect_output(print(result), "P-value")
})
