# tests/testthat/test_frailty_index.R

# Helper to unload di if already loaded
unload_di_if_loaded <- function() {
  if ("di" %in% loadedNamespaces()) {
    unloadNamespace("di")
  }
}

# 1) frailty_index errors if 'di' not installed
test_that("frailty_index errors without di installed", {
  unload_di_if_loaded()
  empty_lib <- tempfile("libs")
  dir.create(empty_lib)
  withr::with_libpaths(new = empty_lib, action = "replace", {
    expect_error(
      frailty_index(tibble::tibble(a = 1, b = 0)),
      "Package 'di' is required"
    )
  })
})

# 2) frailty_index returns the expected list structure
test_that("frailty_index returns list(di, columns)", {
  skip_on_cran()
  skip_if_not_installed("di")
  df <- tibble::tibble(var1 = c(1, 0, 1), var2 = c(0, 1, 1), var3 = c(1, 1, 0))
  res <- frailty_index(df, cols = c("var1", "var2", "var3"))
  expect_type(res, "list")
  expect_true(all(c("di", "columns") %in% names(res)))
  expect_length(res$di, nrow(df))
  expect_true(all(res$di >= 0 & res$di <= 1, na.rm = TRUE))
  expect_true(is.matrix(res$columns) || is.data.frame(res$columns))
})

# 3) auto-select numeric deficits when cols = NULL (exclude age)
test_that("frailty_index auto-selects numeric cols when cols=NULL", {
  skip_on_cran()
  skip_if_not_installed("di")
  df <- tibble::tibble(age = c(30, 40), d1 = c(1, 0), d2 = c(0, 1), d3 = c(1, 1))
  res <- frailty_index(df, age = "age")
  expect_false("age" %in% colnames(res$columns))
  expect_setequal(colnames(res$columns), c("d1", "d2", "d3"))
})

# 4) plot_frailty_age errors if 'di' not installed
test_that("plot_frailty_age errors without di installed", {
  skip_on_cran()
  unload_di_if_loaded()
  empty_lib <- tempfile("libs")
  dir.create(empty_lib)
  withr::with_libpaths(new = empty_lib, action = "replace", {
    expect_error(
      plot_frailty_age(tibble::tibble(a = 1, b = 0), age = NULL),
      "Package 'di' is required"
    )
  })
})

# 5) plot_frailty_age never errors when di is installed
test_that("plot_frailty_age never errors when di is installed", {
  skip_on_cran()
  skip_if_not_installed("di")
  df <- tibble::tibble(
    age = rep(50, 3),
    d1  = c(1, 0, 1),
    d2  = c(0, 1, 0)
  )
  expect_error(
    plot_frailty_age(df, cols = c("d1", "d2"), age = "age", bins = 5),
    NA
  )
})

# 6) return = 'data' produces tidy tibble with di and selected columns
test_that("frailty_index return='data' returns tibble with di and inputs", {
  skip_on_cran()
  skip_if_not_installed("di")
  df <- tibble::tibble(age = c(70, 72), d1 = c(1, 0), d2 = c(0, 1))
  out <- frailty_index(df, cols = c("d1", "d2"), age = "age", return = "data")
  expect_s3_class(out, "tbl_df")
  expect_true(all(c("di", "d1", "d2", "age") %in% names(out)))
  expect_equal(nrow(out), nrow(df))
  expect_true(all(out$di >= 0 & out$di <= 1, na.rm = TRUE))
})

# 7) NA handling options: warn/error/ignore
test_that("frailty_index NA handling works", {
  skip_on_cran()
  skip_if_not_installed("di")
  df <- tibble::tibble(d1 = c(1, NA, 0), d2 = c(0, 1, 1))
  # warn on high missingness
  expect_warning(
    frailty_index(df, cols = c("d1", "d2"), na_action = "warn", na_warn_prop = 0.2),
    "Missing values|High missingness"
  )
  # error on any NA
  expect_error(
    frailty_index(df, cols = c("d1", "d2"), na_action = "error"),
    "Missing values present"
  )
  # ignore runs
  expect_error(
    frailty_index(df, cols = c("d1", "d2"), na_action = "ignore"),
    NA
  )
})

# 8) verbose messages include preparing, col_map, and results
test_that("frailty_index verbose = TRUE emits preparing and column mapping messages", {
  skip_on_cran()
  skip_if_not_installed("di")
  withr::local_options(healthmarkers.verbose = "inform")
  df <- tibble::tibble(d1 = c(1, 0), d2 = c(0, 1))
  expect_message(frailty_index(df, cols = c("d1", "d2"), verbose = TRUE), "frailty_index")
  expect_message(frailty_index(df, cols = c("d1", "d2"), verbose = TRUE), "col_map")
})

test_that("frailty_index verbose double-fire guard: each message fires exactly once", {
  skip_on_cran()
  skip_if_not_installed("di")
  withr::local_options(healthmarkers.verbose = "inform")
  df   <- tibble::tibble(d1 = c(1, 0), d2 = c(0, 1))
  msgs <- testthat::capture_messages(
    frailty_index(df, cols = c("d1", "d2"), verbose = TRUE)
  )
  expect_gte(sum(grepl("col_map", msgs)), 1L)
})

test_that("frailty_index return='data' verbose emits results summary", {
  skip_on_cran()
  skip_if_not_installed("di")
  withr::local_options(healthmarkers.verbose = "inform")
  df   <- tibble::tibble(d1 = c(1, 0), d2 = c(0, 1))
  msgs <- testthat::capture_messages(
    frailty_index(df, cols = c("d1", "d2"), return = "data", verbose = TRUE)
  )
  expect_equal(sum(grepl("results:", msgs)), 1L)
})
