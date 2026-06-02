library(testthat)

cm <- list(vitamin_d = "VitD")

test_that("mapping validation and missing columns error", {
  skip_on_cran()
  df <- data.frame(VitD = c(10, 20))
  # empty list with inferrable data – succeeds via inference
  expect_no_error(vitamin_d_status(df, list()))
  expect_error(vitamin_d_status(df, list(vitamin_d = "")), class = "healthmarkers_vitd_error_bad_map_values")
  expect_error(vitamin_d_status(data.frame(X=1), list(vitamin_d = "VitD")),
               class = "healthmarkers_vitd_error_missing_columns")
})

test_that("verbose emits preparing, column map, and results messages", {
  skip_on_cran()
  df <- data.frame(VitD = c(12, 35))
  withr::local_options(healthmarkers.verbose = "inform")
  expect_message(vitamin_d_status(df, cm, verbose = TRUE), "vitamin_d_status")
  expect_message(vitamin_d_status(df, cm, verbose = TRUE), "col_map")
  expect_message(vitamin_d_status(df, cm, verbose = TRUE), "results:")
})

test_that("verbose double-fire guard", {
  skip_on_cran()
  df <- data.frame(VitD = c(12, 35))
  withr::local_options(healthmarkers.verbose = "inform")
  msgs <- testthat::capture_messages(vitamin_d_status(df, cm, verbose = TRUE))
  expect_gte(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})

test_that("coercion warning when non-numeric introduces NAs", {
  skip_on_cran()
  df <- data.frame(VitD = c("12", "oops", "35"))
  expect_warning(vitamin_d_status(df, cm), class = "healthmarkers_vitd_warn_na_coercion")
})

test_that("NA policies: keep, omit, error, warn", {
  skip_on_cran()
  df <- data.frame(VitD = c(12, NA, 35))
  out_keep <- vitamin_d_status(df, cm, na_action = "keep")
  expect_equal(nrow(out_keep), 3L)
  expect_true(is.na(out_keep$vitamin_d_status[2]))

  out_omit <- vitamin_d_status(df, cm, na_action = "omit")
  expect_equal(nrow(out_omit), 2L)
  expect_false(any(is.na(out_omit$vitamin_d_status)))

  expect_error(vitamin_d_status(df, cm, na_action = "error"),
               class = "healthmarkers_vitd_error_missing_values")

  expect_warning(vitamin_d_status(df, cm, na_action = "warn"),
                 class = "healthmarkers_vitd_warn_missing_inputs")
})

test_that("domain warnings: negative values and suspicious units", {
  skip_on_cran()
  df_neg <- data.frame(VitD = c(-5, 15))
  expect_warning(vitamin_d_status(df_neg, cm),
                 class = "healthmarkers_vitd_warn_negative_values")

  # High median suggests nmol/L units
  df_units <- data.frame(VitD = c(175, 200, 225))
  expect_warning(vitamin_d_status(df_units, cm),
                 class = "healthmarkers_vitd_warn_units_suspicious")
})

test_that("extreme inputs pass through without error (check_extreme removed)", {
  skip_on_cran()
  df <- data.frame(VitD = c(-10, 15, 500, 35))
  out <- suppressWarnings(vitamin_d_status(df, cm))
  expect_s3_class(out, "tbl_df")
  expect_true("vitamin_d_status" %in% names(out))
})

test_that("classification boundaries are correct and ordered factor returned", {
  skip_on_cran()
  df <- data.frame(VitD = c(19.9, 20.0, 29.9, 30.0, NA))
  out <- vitamin_d_status(df, cm)
  expect_identical(levels(out$vitamin_d_status),
                   c("Deficient","Insufficient","Sufficient"))
  expect_true(is.ordered(out$vitamin_d_status))
  expect_equal(as.character(out$vitamin_d_status[1]), "Deficient")
  expect_equal(as.character(out$vitamin_d_status[2]), "Insufficient")
  expect_equal(as.character(out$vitamin_d_status[3]), "Insufficient")
  expect_equal(as.character(out$vitamin_d_status[4]), "Sufficient")
  expect_true(is.na(out$vitamin_d_status[5]))
})

test_that("padding preserved for keep/warn", {
  skip_on_cran()
  df <- data.frame(VitD = c(12, NA, 35))
  out_keep <- vitamin_d_status(df, cm, na_action = "keep")
  out_warn <- suppressWarnings(vitamin_d_status(df, cm, na_action = "warn"))
  expect_equal(nrow(out_keep), 3L)
  expect_equal(nrow(out_warn), 3L)
})
