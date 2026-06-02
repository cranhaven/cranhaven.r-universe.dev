library(testthat)

cm <- list(alm = "ALM_kg", bmi = "BMI", sex = "Sex")

test_that("mapping validation and missing columns error", {
  skip_on_cran()
  df <- data.frame(ALM_kg = 10, BMI = 25, Sex = "Male")

  # non-list col_map → colmap_type error
  expect_error(
    alm_bmi_index(df, "not_a_list"),
    class = "healthmarkers_alm_bmi_error_colmap_type"
  )

  expect_error(
    alm_bmi_index(df, list(alm = "", bmi = "BMI", sex = "Sex")),
    class = "healthmarkers_alm_bmi_error_bad_map_values"
  )

  expect_error(
    alm_bmi_index(data.frame(X = 1), cm),
    class = "healthmarkers_alm_bmi_error_missing_columns"
  )
})

test_that("verbose = TRUE emits col_map and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- data.frame(ALM_kg = 10, BMI = 25, Sex = "Male")
  expect_message(alm_bmi_index(df, cm, verbose = TRUE), "alm_bmi_index")
  expect_message(alm_bmi_index(df, cm, verbose = TRUE), "col_map")
  expect_message(alm_bmi_index(df, cm, verbose = TRUE), "results:")
})

test_that("verbose double-fire guard: each message fires exactly once", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df   <- data.frame(ALM_kg = 10, BMI = 25, Sex = "Male")
  msgs <- testthat::capture_messages(alm_bmi_index(df, cm, verbose = TRUE))
  expect_gte(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})

test_that("numeric coercion warning when strings introduce NAs", {
  skip_on_cran()
  df <- data.frame(
    ALM_kg = c("10", "oops"),
    BMI    = c("25", "26"),
    Sex    = c("Male", "Female")
  )

  expect_warning(
    alm_bmi_index(df, cm),
    class = "healthmarkers_alm_bmi_warn_na_coercion"
  )
})

test_that("NA policies: keep, omit, error, warn", {
  skip_on_cran()
  df <- data.frame(
    ALM_kg = c(10, NA, 12),
    BMI    = c(25, 26, 27),
    Sex    = c("Male", "Female", NA)
  )

  out_keep <- alm_bmi_index(df, cm, na_action = "keep")
  expect_equal(nrow(out_keep), 3L)
  expect_true(any(is.na(out_keep$alm_bmi_ratio)))

  out_omit <- alm_bmi_index(df, cm, na_action = "omit")
  expect_equal(nrow(out_omit), 1L)

  expect_error(
    alm_bmi_index(df, cm, na_action = "error"),
    class = "healthmarkers_alm_bmi_error_missing_values"
  )

  expect_warning(
    alm_bmi_index(df, cm, na_action = "warn"),
    class = "healthmarkers_alm_bmi_warn_missing_inputs"
  )
})

test_that("domain warnings: BMI and ALM ranges and nonpositive BMI", {
  skip_on_cran()
  # BMI out of [10,60], ALM plausible
  df_bmi <- data.frame(
    ALM_kg = c(15, 20),
    BMI    = c(8, 70),
    Sex    = c("M", "F")
  )
  expect_warning(
    alm_bmi_index(df_bmi, cm),
    class = "healthmarkers_alm_bmi_warn_bmi_range"
  )

  # ALM out of [5,40], BMI plausible
  df_alm <- data.frame(
    ALM_kg = c(3, 50),
    BMI    = c(25, 30),
    Sex    = c("M", "F")
  )
  expect_warning(
    alm_bmi_index(df_alm, cm),
    class = "healthmarkers_alm_bmi_warn_alm_range"
  )

  # Nonpositive BMI, ALM plausible — suppress the BMI_range warning here
  df_np <- data.frame(
    ALM_kg = c(15, 20),
    BMI    = c(0, -5),
    Sex    = c("M", "F")
  )

  suppress_range <- function(expr) {
    withCallingHandlers(
      expr,
      warning = function(w) {
        if (inherits(w, "healthmarkers_alm_bmi_warn_bmi_range")) {
          invokeRestart("muffleWarning")
        }
      }
    )
  }

  expect_warning(
    suppress_range(alm_bmi_index(df_np, cm)),
    class = "healthmarkers_alm_bmi_warn_bmi_nonpositive"
  )
})

test_that("sex normalization and unknown sex warning", {
  skip_on_cran()
  df <- data.frame(
    ALM_kg = c(10, 10, 10),
    BMI    = c(25, 25, 25),
    Sex    = c("M", "female", "X")
  )

  expect_warning(
    alm_bmi_index(df, cm),
    class = "healthmarkers_alm_bmi_warn_sex_unknown"
  )
})

test_that("extreme ALM/BMI values trigger domain warnings", {
  skip_on_cran()
  df <- data.frame(
    ALM_kg = c(2, 18, 45),
    BMI    = c(8, 25, 70),
    Sex    = c("Male", "Female", "Male")
  )
  expect_warning(
    withCallingHandlers(
      alm_bmi_index(df, cm),
      warning = function(w) {
        if (inherits(w, "healthmarkers_alm_bmi_warn_bmi_range")) invokeRestart("muffleWarning")
      }
    ),
    class = "healthmarkers_alm_bmi_warn_alm_range"
  )
  out <- suppressWarnings(alm_bmi_index(df, cm))
  expect_equal(nrow(out), 3L)
  expect_true(all(is.na(out$alm_bmi_ratio) | out$alm_bmi_ratio > 0))
})

test_that("thresholds: low muscle mass flagged correctly by sex", {
  skip_on_cran()
  df <- data.frame(
    ALM_kg = c(15, 20, 10, 12),
    BMI    = c(20, 25, 20, 25),
    Sex    = c("Male", "Male", "Female", "Female")
  )
  # ratios: 0.75, 0.8, 0.5, 0.48
  out <- alm_bmi_index(df, cm)

  expect_equal(round(out$alm_bmi_ratio, 3), c(0.75, 0.8, 0.5, 0.48))
  expect_equal(out$low_muscle_mass, c(TRUE, FALSE, TRUE, TRUE))
})

test_that("padding preserved for keep/warn", {
  skip_on_cran()
  df <- data.frame(
    ALM_kg = c(10, NA, 12),
    BMI    = c(25, 26, 27),
    Sex    = c("Male", "Female", "Male")
  )

  out_keep <- alm_bmi_index(df, cm, na_action = "keep")
  out_warn <- suppressWarnings(alm_bmi_index(df, cm, na_action = "warn"))

  expect_equal(nrow(out_keep), 3L)
  expect_equal(nrow(out_warn), 3L)
})
