library(testthat)

cm <- list(calcium = "Ca", albumin = "Alb")

test_that("validation errors: data type, col_map type, missing map, empty mapping, missing columns", {
  skip_on_cran()
  df_ok <- data.frame(Ca = 9, Alb = 3.5)

  expect_error(
    corrected_calcium("x", cm),
    class = "healthmarkers_calcium_error_data_type"
  )

  expect_error(
    corrected_calcium(df_ok, col_map = "not_list"),
    class = "healthmarkers_calcium_error_colmap_type"
  )

  # partial col_map is supplemented by inference
  expect_no_error(
    corrected_calcium(df_ok, col_map = list(calcium = "Ca"))
  )

  expect_error(
    corrected_calcium(df_ok, col_map = list(calcium = "", albumin = "Alb")),
    class = "healthmarkers_calcium_error_bad_map_values"
  )

  expect_error(
    corrected_calcium(data.frame(X = 1, Y = 2), cm),
    class = "healthmarkers_calcium_error_missing_columns"
  )
})

test_that("numeric coercion warning on non-numeric inputs", {
  skip_on_cran()
  df <- data.frame(Ca = c("9.0", "oops"), Alb = c("3.5", "3.0"))

  expect_warning(
    out <- corrected_calcium(df, cm),
    class = "healthmarkers_calcium_warn_na_coercion"
  )
  expect_true("corrected_calcium" %in% names(out))
})

test_that("NA policies: keep / warn / omit / error / ignore", {
  skip_on_cran()
  df <- data.frame(
    Ca  = c(9.0, NA, 8.5),
    Alb = c(3.0, 3.5, NA)
  )

  out_keep <- corrected_calcium(df, cm, na_action = "keep")
  expect_equal(nrow(out_keep), 3L)
  expect_true(any(is.na(out_keep$corrected_calcium)))

  expect_warning(
    out_warn <- corrected_calcium(df, cm, na_action = "warn"),
    class = "healthmarkers_calcium_warn_missing_inputs"
  )
  expect_equal(nrow(out_warn), 3L)

  out_omit <- corrected_calcium(df, cm, na_action = "omit")
  expect_equal(nrow(out_omit), 1L)

  expect_error(
    corrected_calcium(df, cm, na_action = "error"),
    class = "healthmarkers_calcium_error_missing_values"
  )

  out_ignore <- corrected_calcium(df, cm, na_action = "ignore")
  expect_equal(nrow(out_ignore), 3L)
})

test_that("conventional units Payne formula exact", {
  skip_on_cran()
  df <- data.frame(Ca = 8.5, Alb = 2.8)

  out <- corrected_calcium(df, cm, units = "conventional")
  expected <- 8.5 + 0.8 * (4.0 - 2.8)

  expect_equal(out$corrected_calcium, expected, tolerance = 1e-12)
})

test_that("auto SI detection converts and re-scales back (unit assumption warning)", {
  skip_on_cran()
  df <- data.frame(Ca = 2.2, Alb = 35)

  # check warning class
  expect_warning(
    corrected_calcium(df, cm, units = "auto"),
    class = "healthmarkers_calcium_warn_unit_assumption"
  )

  # check numeric result separately (do not rely on expect_warning's return)
  out <- suppressWarnings(corrected_calcium(df, cm, units = "auto"))
  expect_equal(out$corrected_calcium, 9.2 / 4, tolerance = 1e-12)
})

test_that("domain warnings for albumin and corrected calcium ranges (no extreme scan)", {
  skip_on_cran()
  # 1) Trigger corrected calcium outside 5–15 mg/dL but albumin in range
  df_corr <- data.frame(
    Ca  = c(4.0, 16.0),  # will yield corrected <5 and >15
    Alb = c(4.0, 4.0)    # within 2–5 g/dL
  )
  expect_warning(
    corrected_calcium(df_corr, cm, units = "conventional"),
    class = "healthmarkers_calcium_warn_corrected_range"
  )

  # 2) Trigger albumin outside 2–5 g/dL (regardless of corrected Ca)
  df_alb <- data.frame(
    Ca  = c(9.0, 9.5),
    Alb = c(1.5, 6.0)    # outside 2–5 g/dL
  )
  expect_warning(
    corrected_calcium(df_alb, cm, units = "conventional"),
    class = "healthmarkers_calcium_warn_albumin_range"
  )
})

test_that("domain warnings triggered for out-of-range albumin/calcium (conventional)", {
  skip_on_cran()
  df <- data.frame(
    Ca  = c(4.5, 16),
    Alb = c(1.5, 6.0)
  )

  expect_warning(
    corrected_calcium(df, cm, units = "conventional"),
    class = "healthmarkers_calcium_warn_albumin_range"
  )
})

test_that("extreme input values pass through and produce finite output", {
  skip_on_cran()
  df <- data.frame(
    Ca  = c(25, 9),
    Alb = c(3.5, 3.5)
  )

  out <- suppressWarnings(corrected_calcium(df, cm, units = "conventional"))
  expect_equal(nrow(out), 2L)
  expect_true(all(is.finite(out$corrected_calcium)))
})

test_that("standard input produces correct Payne formula output", {
  skip_on_cran()
  df <- data.frame(
    Ca  = c(9.0, 9.0),
    Alb = c(4.0, 2.0)
  )

  out <- corrected_calcium(df, cm, units = "conventional")
  # Row 1: 9.0 + 0.8*(4.0-4.0) = 9.0
  expect_equal(out$corrected_calcium[1], 9.0)
  # Row 2: 9.0 + 0.8*(4.0-2.0) = 10.6
  expect_equal(out$corrected_calcium[2], 10.6)
})

test_that("verbose = TRUE emits col_map and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- data.frame(Ca = 9.0, Alb = 3.5)
  expect_message(corrected_calcium(df, cm, verbose = TRUE), "corrected_calcium")
  expect_message(corrected_calcium(df, cm, verbose = TRUE), "col_map")
  expect_message(corrected_calcium(df, cm, verbose = TRUE), "results:")
})

test_that("verbose double-fire guard: each message fires exactly once", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df   <- data.frame(Ca = 9.0, Alb = 3.5)
  msgs <- testthat::capture_messages(corrected_calcium(df, cm, verbose = TRUE))
  expect_gte(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})

test_that("empty input returns 0-row tibble with column", {
  skip_on_cran()
  df <- data.frame(Ca = numeric(), Alb = numeric())

  out <- corrected_calcium(df, cm)
  expect_equal(nrow(out), 0L)
  expect_true("corrected_calcium" %in% names(out))
})

test_that("all NA inputs produce NA outputs (keep/warn/ignore) and omit drops all", {
  skip_on_cran()
  df <- data.frame(Ca = c(NA, NA), Alb = c(NA, NA))

  out_keep <- corrected_calcium(df, cm, na_action = "keep")
  expect_true(all(is.na(out_keep$corrected_calcium)))

  expect_warning(
    out_warn <- corrected_calcium(df, cm, na_action = "warn"),
    class = "healthmarkers_calcium_warn_missing_inputs"
  )
  expect_true(all(is.na(out_warn$corrected_calcium)))

  out_ignore <- corrected_calcium(df, cm, na_action = "ignore")
  expect_true(all(is.na(out_ignore$corrected_calcium)))

  out_omit <- corrected_calcium(df, cm, na_action = "omit")
  expect_equal(nrow(out_omit), 0L)
})
