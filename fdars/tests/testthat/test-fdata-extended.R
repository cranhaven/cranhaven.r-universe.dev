# Tests for untested fdata functions: df_to_fdata2d, median, fdata2fd

# =============================================================================
# df_to_fdata2d
# =============================================================================

test_that("df_to_fdata2d basic conversion works", {
  df <- data.frame(
    id = rep(c("surf1", "surf2"), each = 3),
    s = rep(1:3, 2),
    t1 = c(1, 2, 3, 4, 5, 6),
    t2 = c(7, 8, 9, 10, 11, 12)
  )

  fd2d <- df_to_fdata2d(df, id_col = 1, s_col = 2)

  expect_s3_class(fd2d, "fdata")
  expect_true(fd2d$fdata2d)
  expect_equal(nrow(fd2d$data), 2)  # 2 surfaces
  # 3 s-values * 2 t-values = 6 columns
  expect_equal(ncol(fd2d$data), 6)
})

test_that("df_to_fdata2d with character column names", {
  df <- data.frame(
    id = rep(c("a", "b"), each = 2),
    s = rep(1:2, 2),
    t1 = 1:4,
    t2 = 5:8
  )

  fd2d <- df_to_fdata2d(df, id_col = "id", s_col = "s")
  expect_s3_class(fd2d, "fdata")
  expect_true(fd2d$fdata2d)
})

test_that("df_to_fdata2d with metadata", {
  df <- data.frame(
    id = rep(c("surf1", "surf2"), each = 3),
    s = rep(1:3, 2),
    t1 = rnorm(6),
    t2 = rnorm(6)
  )
  meta <- data.frame(group = c("A", "B"), value = c(1.5, 2.3))

  fd2d <- df_to_fdata2d(df, id_col = 1, s_col = 2, metadata = meta)
  expect_s3_class(fd2d, "fdata")
  expect_equal(fd2d$metadata$group, c("A", "B"))
})

test_that("df_to_fdata2d errors on non-data.frame", {
  expect_error(df_to_fdata2d(matrix(1:10, 2, 5)), "data frame")
})

test_that("df_to_fdata2d errors on mismatched metadata rows", {
  df <- data.frame(
    id = rep(c("surf1", "surf2"), each = 2),
    s = rep(1:2, 2),
    t1 = 1:4
  )
  meta <- data.frame(group = c("A", "B", "C"))  # 3 rows, but 2 surfaces
  expect_error(df_to_fdata2d(df, id_col = 1, s_col = 2, metadata = meta), "rows")
})

test_that("df_to_fdata2d with explicit t_cols", {
  df <- data.frame(
    id = rep("s1", 2),
    s = 1:2,
    x = c(1, 2),
    y = c(3, 4),
    extra = c(5, 6)
  )

  fd2d <- df_to_fdata2d(df, id_col = 1, s_col = 2, t_cols = c("x", "y"))
  expect_equal(ncol(fd2d$data), 4)  # 2 s * 2 t
})

# =============================================================================
# median (depth-based functional median)
# =============================================================================

test_that("median returns fdata with 1 row", {
  set.seed(42)
  t <- seq(0, 1, length.out = 50)
  X <- matrix(rnorm(20 * 50), 20, 50)
  fd <- fdata(X, argvals = t)

  med <- median(fd)
  expect_s3_class(med, "fdata")
  expect_equal(nrow(med$data), 1)
  expect_equal(ncol(med$data), 50)
})

test_that("median with different methods", {
  set.seed(42)
  t <- seq(0, 1, length.out = 30)
  X <- matrix(rnorm(15 * 30), 15, 30)
  fd <- fdata(X, argvals = t)

  med_fm <- median(fd, method = "FM")
  med_mode <- median(fd, method = "mode")
  med_mbd <- median(fd, method = "MBD")

  expect_s3_class(med_fm, "fdata")
  expect_s3_class(med_mode, "fdata")
  expect_s3_class(med_mbd, "fdata")
})

test_that("median of numeric vector falls through to stats::median", {
  result <- median(c(1, 3, 5))
  expect_equal(result, 3)
})

test_that("median returns one of the original curves", {
  set.seed(42)
  t <- seq(0, 1, length.out = 30)
  X <- matrix(rnorm(10 * 30), 10, 30)
  fd <- fdata(X, argvals = t)

  med <- median(fd)
  # The median curve should be one of the original rows
  found <- FALSE
  for (i in 1:10) {
    if (all(abs(med$data[1, ] - X[i, ]) < 1e-10)) {
      found <- TRUE
      break
    }
  }
  expect_true(found)
})

# =============================================================================
# fdata2fd (requires fda package)
# =============================================================================

test_that("fdata2fd with bspline basis", {
  skip_if_not_installed("fda")

  t <- seq(0, 1, length.out = 50)
  X <- matrix(sin(2 * pi * t), nrow = 1)
  fd <- fdata(X, argvals = t)

  fd_obj <- fdata2fd(fd, nbasis = 10, type = "bspline")
  expect_s3_class(fd_obj, "fd")
})

test_that("fdata2fd with fourier basis", {
  skip_if_not_installed("fda")

  t <- seq(0, 1, length.out = 50)
  X <- matrix(sin(2 * pi * t), nrow = 1)
  fd <- fdata(X, argvals = t)

  fd_obj <- fdata2fd(fd, nbasis = 11, type = "fourier")
  expect_s3_class(fd_obj, "fd")
})

test_that("fdata2fd validates input", {
  skip_if_not_installed("fda")
  expect_error(fdata2fd(matrix(1:10, 2, 5)), "fdata")
})
