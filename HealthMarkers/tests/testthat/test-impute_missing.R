# tests/testthat/test_impute_missing.R

library(testthat)
library(tibble)

# 1) impute_missing()
test_that("impute_missing mean/median/zero/constant work", {
  skip_on_cran()
  df <- tibble(x = c(1, NA, 3), y = c(NA, 2, NA), z = c("a", "b", "c"))
  # default mean
  out1 <- suppressWarnings(impute_missing(df))
  expect_equal(out1$x, c(1, mean(c(1, 3)), 3))
  expect_equal(out1$y, c(mean(2), 2, mean(2)))
  # zero only x
  out2 <- suppressWarnings(impute_missing(df, method = "zero", cols = "x"))
  expect_equal(out2$x, c(1, 0, 3))
  expect_true(is.na(out2$y[1]))
  # constant
  out3 <- suppressWarnings(impute_missing(df, method = "constant", constant = 99))
  expect_equal(out3$x, c(1, 99, 3))
  expect_equal(out3$y, c(99, 2, 99))
})

test_that("impute_missing errors appropriately", {
  skip_on_cran()
  expect_error(impute_missing("not a df"), "must be a data.frame or tibble")
  expect_error(impute_missing(tibble(a = 1), cols = "b"), "Some `cols` not in data")
})

# 2) impute_mice()
test_that("impute_mice basic functionality is silent and imputes", {
  skip_on_cran()
  skip_if_not_installed("mice")
  df <- tibble(a = c(1, NA, 3), b = c(4, 5, NA), c = c("x", "y", "z"))
  expect_silent({
    out <- impute_mice(df, m = 1)
  })
  expect_true(all(!is.na(out$a)))
  expect_true(all(!is.na(out$b)))
  # non-numeric untouched
  expect_identical(out$c, df$c)
})

test_that("impute_mice errors on bad inputs", {
  skip_on_cran()
  skip_if_not_installed("mice")
  expect_error(impute_mice("nope"), "must be a data.frame or tibble")
  expect_error(impute_mice(tibble(x = 1:3), cols = "foo"),
               "Some `cols` not in data")
  # only one numeric column with missing values -> error
  expect_error(
    impute_mice(tibble(a = c(1, NA, 3), b = c(1, 2, 3))),
    "need at least two numeric columns"
  )
})

# 3) impute_missforest()
test_that("impute_missforest fills NAs and preserves non-numeric", {
  skip_on_cran()
  skip_if_not_installed("missForest")
  df <- tibble(
    a = c(2, NA, 8),
    b = c(NA, 6, 9),
    d = factor(c("u", "v", "w"))
  )
  out <- suppressWarnings(impute_missforest(df))
  # numeric columns are now complete
  expect_false(any(is.na(out$a)))
  expect_false(any(is.na(out$b)))
  # non-numeric untouched
  expect_identical(out$d, df$d)
})

test_that("impute_missforest errors on bad inputs", {
  skip_on_cran()
  skip_if_not_installed("missForest")
  expect_error(impute_missforest("nope"), "must be a data.frame or tibble")
  expect_error(impute_missforest(tibble(x = 1:3), cols = "foo"),
               "Some `cols` not in data")
  # only one numeric column with missing values -> error
  expect_error(
    impute_missforest(tibble(a = c(1, NA, 3), b = c(1, 2, 3))),
    "need at least two numeric columns"
  )
})

# Verbose message tests -------------------------------------------------------

test_that("impute_missing verbose emits preparing inputs and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- tibble(x = c(1, NA, 3), y = c(NA, 2, NA))
  expect_message(suppressWarnings(impute_missing(df, verbose = TRUE)), "impute_missing")
  expect_message(suppressWarnings(impute_missing(df, verbose = TRUE)), "results:")
})

test_that("impute_missing verbose double-fire guard: each message fires exactly once", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df   <- tibble(x = c(1, NA, 3), y = c(NA, 2, NA))
  msgs <- testthat::capture_messages(suppressWarnings(impute_missing(df, verbose = TRUE)))
  expect_equal(sum(grepl("preparing inputs", msgs)), 1L)
  expect_equal(sum(grepl("results:",         msgs)), 1L)
})

test_that("impute_mice verbose emits preparing inputs and results messages", {
  skip_on_cran()
  skip_if_not_installed("mice")
  withr::local_options(healthmarkers.verbose = "inform")
  df <- tibble(a = c(1, NA, 3), b = c(4, 5, NA))
  expect_message(impute_mice(df, m = 1, verbose = TRUE), "impute_mice")
  expect_message(impute_mice(df, m = 1, verbose = TRUE), "results:")
})

test_that("impute_mice verbose double-fire guard", {
  skip_on_cran()
  skip_if_not_installed("mice")
  withr::local_options(healthmarkers.verbose = "inform")
  df   <- tibble(a = c(1, NA, 3), b = c(4, 5, NA))
  msgs <- testthat::capture_messages(impute_mice(df, m = 1, verbose = TRUE))
  expect_equal(sum(grepl("preparing inputs", msgs)), 1L)
  expect_equal(sum(grepl("results:",         msgs)), 1L)
})

test_that("impute_missforest verbose emits preparing inputs and results messages", {
  skip_on_cran()
  skip_if_not_installed("missForest")
  withr::local_options(healthmarkers.verbose = "inform")
  df <- tibble(a = c(2, NA, 8), b = c(NA, 6, 9))
  expect_message(suppressWarnings(impute_missforest(df, verbose = TRUE)), "impute_missforest")
  expect_message(suppressWarnings(impute_missforest(df, verbose = TRUE)), "results:")
})

test_that("impute_missforest verbose double-fire guard", {
  skip_on_cran()
  skip_if_not_installed("missForest")
  withr::local_options(healthmarkers.verbose = "inform")
  df   <- tibble(a = c(2, NA, 8), b = c(NA, 6, 9))
  msgs <- testthat::capture_messages(suppressWarnings(impute_missforest(df, verbose = TRUE)))
  expect_equal(sum(grepl("preparing inputs", msgs)), 1L)
  expect_equal(sum(grepl("results:",         msgs)), 1L)
})
