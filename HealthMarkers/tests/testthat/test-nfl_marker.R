library(testthat)

cm <- list(nfl = "NfL")

test_that("mapping validation and missing columns error", {
  skip_on_cran()
  df <- data.frame(NfL = c(10, 20))

  # empty list with inferrable data – succeeds via case-insensitive inference
  expect_no_error(nfl_marker(df, list()))

  expect_error(
    nfl_marker(df, list(nfl = "")),
    class = "healthmarkers_nfl_error_bad_map_values"
  )

  expect_error(
    nfl_marker(data.frame(X = 1), list(nfl = "NfL")),
    class = "healthmarkers_nfl_error_missing_columns"
  )
})

test_that("verbose emits col_map and results messages", {
  skip_on_cran()
  df <- data.frame(NfL = c(12, 35))
  withr::local_options(healthmarkers.verbose = "inform")
  expect_message(nfl_marker(df, cm, verbose = TRUE), "nfl_marker")
  expect_message(nfl_marker(df, cm, verbose = TRUE), "col_map")
  expect_message(nfl_marker(df, cm, verbose = TRUE), "results:")
})

test_that("verbose double-fire guard", {
  skip_on_cran()
  df <- data.frame(NfL = c(12, 35))
  withr::local_options(healthmarkers.verbose = "inform")
  msgs <- testthat::capture_messages(nfl_marker(df, cm, verbose = TRUE))
  expect_gte(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})

test_that("numeric coercion warning when non-numeric introduces NAs", {
  skip_on_cran()
  df <- data.frame(NfL = c("12", "oops", "35"))

  expect_warning(
    nfl_marker(df, cm),
    class = "healthmarkers_nfl_warn_na_coercion"
  )
})

test_that("NA policies: keep, omit, error, warn", {
  skip_on_cran()
  df <- data.frame(NfL = c(12, NA, 35))

  out_keep <- nfl_marker(df, cm, na_action = "keep")
  expect_equal(nrow(out_keep), 3L)
  expect_true(is.na(out_keep$nfl_value[2]))

  out_omit <- nfl_marker(df, cm, na_action = "omit")
  expect_equal(nrow(out_omit), 2L)
  expect_false(any(is.na(out_omit$nfl_value)))

  expect_error(
    nfl_marker(df, cm, na_action = "error"),
    class = "healthmarkers_nfl_error_missing_values"
  )

  expect_warning(
    nfl_marker(df, cm, na_action = "warn"),
    class = "healthmarkers_nfl_warn_missing_inputs"
  )
})

test_that("domain warnings: negative values", {
  skip_on_cran()
  df_neg <- data.frame(NfL = c(-5, 15))

  expect_warning(
    nfl_marker(df_neg, cm),
    class = "healthmarkers_nfl_warn_negative_values"
  )
})

test_that("extreme NfL values pass through without error", {
  skip_on_cran()
  df <- data.frame(NfL = c(1e7, 15, 35))
  out <- nfl_marker(df, cm)
  expect_equal(nrow(out), 3L)
  expect_equal(out$nfl_value[1], 1e7)
})

test_that("padding preserved for keep/warn", {
  skip_on_cran()
  df <- data.frame(NfL = c(12, NA, 35))

  out_keep <- nfl_marker(df, cm, na_action = "keep")
  out_warn <- suppressWarnings(nfl_marker(df, cm, na_action = "warn"))

  expect_equal(nrow(out_keep), 3L)
  expect_equal(nrow(out_warn), 3L)
})
