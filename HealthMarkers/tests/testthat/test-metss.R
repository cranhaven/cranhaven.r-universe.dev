# tests/testthat/test_metss.R

library(testthat)
library(tibble)
library(HealthMarkers)

# ---- Missing required columns ----
test_that("metss errors if missing required columns", {
  skip_on_cran()
  df <- tibble(
    bp_sys  = 120,
    bp_dia  = 80,
    TG      = 1.5,
    HDL_c   = 1.1,
    glucose = 5.3,
    sex     = 1,
    race    = "NHW"
  )
  expect_error(
    metss(df),
    "metss\\(\\): missing required columns: waist"
  )
})

# ---- Correct computation (default params NHW male) ----
test_that("metss computes correct score for default NHW male", {
  skip_on_cran()
  df <- tibble(
    waist   = 94,
    bp_sys  = 120,
    bp_dia  = 80,
    TG      = 1.5,
    HDL_c   = 1.1,
    glucose = 5.3,
    sex     = 1,
    race    = "NHW"
  )
  out <- metss(df)
  MAP <- (2 * 80 + 120) / 3
  z_MAP <- (MAP - 97) / 11
  expected <- -2.344 + 0.466 * z_MAP
  expect_s3_class(out, "tbl_df")
  expect_named(out, "MetSSS")
  expect_type(out$MetSSS, "double")
  expect_equal(unname(out$MetSSS), expected, tolerance = 1e-6)
})

# ---- Vectorization ----
test_that("metss is vectorized over multiple rows", {
  skip_on_cran()
  df <- tibble(
    waist   = c(94, 100),
    bp_sys  = c(120, 130),
    bp_dia  = c(80, 85),
    TG      = c(1.5, 2.0),
    HDL_c   = c(1.1, 1.2),
    glucose = c(5.3, 6.0),
    sex     = c(1, 1),
    race    = c("NHW", "NHW")
  )
  out <- metss(df)
  expect_equal(nrow(out), 2L)
  single_out <- metss(df[1, ])
  expect_equal(unname(out$MetSSS[1]), unname(single_out$MetSSS), tolerance = 1e-6)
})

# ---- Missing params key ----
test_that("metss errors when params key for sex-race is missing", {
  skip_on_cran()
  df_f <- tibble(
    waist   = 100,
    bp_sys  = 130,
    bp_dia  = 85,
    TG      = 2.0,
    HDL_c   = 1.2,
    glucose = 6.0,
    sex     = 2,
    race    = "NHW"
  )
  # Provide custom params with only NHW_M (no NHW_F) to trigger the missing-key error
  custom_params_m_only <- list(
    NHW_M = list(
      intercept = -2.344,
      waist     = c(mean = 94.0, sd = 12.4, coef = 0.846),
      TG        = c(mean = 1.50, sd = 0.60, coef = 0.701),
      HDL       = c(mean = 1.10, sd = 0.30, coef = -0.663),
      glucose   = c(mean = 5.30, sd = 0.60, coef = 0.658),
      MAP       = c(mean = 97.0, sd = 11.0, coef = 0.466)
    )
  )
  expect_error(
    metss(df_f, params = custom_params_m_only),
    "params missing keys for: NHW_F"
  )
})

# ---- Verbose messages ----
test_that("verbose emits col_map and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- tibble(
    waist   = 94,
    bp_sys  = 120,
    bp_dia  = 80,
    TG      = 1.5,
    HDL_c   = 1.1,
    glucose = 5.3,
    sex     = 1,
    race    = "NHW"
  )
  expect_message(metss(df, verbose = TRUE), "metss")
  expect_message(metss(df, verbose = TRUE), "col_map")
  expect_message(metss(df, verbose = TRUE), "results:")
})

test_that("verbose double-fire guard", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- tibble(
    waist   = 94, bp_sys = 120, bp_dia = 80,
    TG = 1.5, HDL_c = 1.1, glucose = 5.3,
    sex = 1, race = "NHW"
  )
  msgs <- testthat::capture_messages(metss(df, verbose = TRUE))
  expect_gte(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})

# ---- NA handling policies ----
test_that("na_action = error and omit behave as expected", {
  skip_on_cran()
  df <- tibble(
    waist   = c(94, NA_real_),
    bp_sys  = c(120, 120),
    bp_dia  = c(80, 80),
    TG      = c(1.5, 1.5),
    HDL_c   = c(1.1, 1.1),
    glucose = c(5.3, 5.3),
    sex     = c(1, 1),
    race    = c("NHW", "NHW")
  )
  expect_error(
    suppressWarnings(metss(df, na_action = "error")),
    "required inputs contain missing values"
  )
  out_omit <- suppressWarnings(metss(df, na_action = "omit"))
  expect_equal(nrow(out_omit), 1L)
})

# ---- Extreme values pass through unchanged ----
test_that("extreme input values pass through without error", {
  skip_on_cran()
  df_ext <- tibble(
    waist   = 400,
    bp_sys  = 500,
    bp_dia  = 250,
    TG      = 50,
    HDL_c   = 10,
    glucose = 80,
    sex     = 1,
    race    = "NHW"
  )
  out <- suppressWarnings(metss(df_ext, diagnostics = FALSE))
  expect_equal(nrow(out), 1L)
})

# ---- Multiple keys warning ----
test_that("multiple sex/race keys triggers warning and uses first-row key", {
  skip_on_cran()
  df <- tibble(
    waist   = c(94, 100),
    bp_sys  = c(120, 130),
    bp_dia  = c(80, 85),
    TG      = c(1.5, 2.0),
    HDL_c   = c(1.1, 1.2),
    glucose = c(5.3, 6.0),
    sex     = c(1, 2),
    race    = c("NHW", "NHB")
  )
  expect_warning(
    out <- metss(df),
    "multiple sex/race keys detected"
  )
  expect_equal(nrow(out), 2L)
})

# ---- Custom params and computation check ----
test_that("custom params applied correctly", {
  skip_on_cran()
  custom_params <- list(
    NHW_M = list(
      intercept = 0,
      waist     = c(mean = 100, sd = 10, coef = 1),
      TG        = c(mean = 2,   sd = 0.5, coef = 0.5),
      HDL       = c(mean = 1.2, sd = 0.2, coef = -0.7),
      glucose   = c(mean = 6,   sd = 0.5, coef = 0.8),
      MAP       = c(mean = 100, sd = 10,  coef = 0.3)
    )
  )
  df <- tibble(
    waist   = 110,
    bp_sys  = 130,
    bp_dia  = 85,
    TG      = 2.5,
    HDL_c   = 1.0,
    glucose = 6.5,
    sex     = 1,
    race    = "NHW"
  )
  MAP <- (2 * 85 + 130) / 3
  z_waist <- (110 - 100) / 10
  z_TG    <- (2.5 - 2) / 0.5
  z_HDL   <- (1.0 - 1.2) / 0.2
  z_glu   <- (6.5 - 6) / 0.5
  z_MAP   <- (MAP - 100) / 10
  expected <- 0 + 1*z_waist + 0.5*z_TG + (-0.7)*z_HDL + 0.8*z_glu + 0.3*z_MAP
  out <- metss(df, params = custom_params)
  expect_equal(unname(out$MetSSS), expected, tolerance = 1e-6)
})
