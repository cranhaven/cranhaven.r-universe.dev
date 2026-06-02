library(testthat)
library(HealthMarkers)

# ---- normalize_vec direct tests --------------------------------------------

test_that("normalize_vec method='none' returns input unchanged", {
  x <- c(1, 2, 3, NA, 5)
  expect_identical(normalize_vec(x, "none"), x)
})

test_that("normalize_vec method='z' gives mean~0 and sd~1 on non-NA values", {
  skip_on_cran()
  x <- c(1, 2, 3, 4, 5)
  out <- normalize_vec(x, "z")
  expect_equal(length(out), length(x))
  expect_equal(mean(out, na.rm = TRUE), 0, tolerance = 1e-10)
  expect_equal(sd(out, na.rm = TRUE), 1, tolerance = 1e-10)
})

test_that("normalize_vec method='z' preserves NA positions", {
  skip_on_cran()
  x <- c(1, NA, 3, 4, 5)
  out <- normalize_vec(x, "z")
  expect_true(is.na(out[2]))
  expect_false(any(is.na(out[-2])))
})

test_that("normalize_vec method='range' maps to [0,1] by default", {
  skip_on_cran()
  x <- c(1, 2, 3, 4, 5)
  out <- normalize_vec(x, "range")
  expect_equal(min(out, na.rm = TRUE), 0)
  expect_equal(max(out, na.rm = TRUE), 1)
})

test_that("normalize_vec method='range' custom feature_range works", {
  skip_on_cran()
  x <- c(1, 2, 3, 4, 5)
  out <- normalize_vec(x, "range", feature_range = c(-1, 1))
  expect_equal(min(out, na.rm = TRUE), -1)
  expect_equal(max(out, na.rm = TRUE), 1)
})

test_that("normalize_vec method='robust' handles non-zero MAD", {
  skip_on_cran()
  set.seed(42)
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  out <- normalize_vec(x, "robust")
  expect_equal(length(out), length(x))
  expect_type(out, "double")
})

test_that("normalize_vec method='inverse' returns rank-based normal scores", {
  skip_on_cran()
  x <- c(1, 2, 3, 4, 5)
  out <- normalize_vec(x, "inverse")
  expect_equal(length(out), length(x))
  # Inverse-normal output should be monotone increasing for sorted input
  expect_true(all(diff(out) > 0))
})

test_that("normalize_vec method='z' on constant vector returns zeros (warn suppressed)", {
  skip_on_cran()
  x <- rep(3, 5)
  out <- suppressWarnings(normalize_vec(x, "z", warn_constant = FALSE))
  expect_true(all(out == 0))
})

test_that("normalize_vec all-NA vector returns all-NA", {
  skip_on_cran()
  x <- rep(NA_real_, 4)
  out <- suppressWarnings(normalize_vec(x, "z"))
  expect_true(all(is.na(out)))
})

test_that("normalize_vec coerces non-numeric with NAs and warns", {
  skip_on_cran()
  # Provide a vector where coercion introduces NAs to trigger the warning
  x <- c("1", "not_a_number", "3")
  expect_warning(normalize_vec(x, "z"), "coerced to numeric")
})
