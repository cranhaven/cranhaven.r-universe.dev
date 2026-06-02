# tests/testthat/test_tracer_dxa_is.R
library(testthat)
library(tibble)
library(dplyr)

test_that("adipose-only mode returns three indices with correct values", {
  skip_on_cran()
  df <- tibble(
    I0              = 60,    # pmol/L
    rate_palmitate  = 1.5,   # µmol/min
    rate_glycerol   = 2.0,   # µmol/min
    fat_mass        = 20,    # kg
    weight          = 70,    # kg
    HDL_c           = 1.2,   # mmol/L
    bmi             = 24
  )
  col_map_adipose <- list(
    I0             = "I0",
    rate_palmitate = "rate_palmitate",
    rate_glycerol  = "rate_glycerol",
    fat_mass       = "fat_mass",
    weight         = "weight",
    HDL_c          = "HDL_c",
    bmi            = "bmi"
  )
  out <- tracer_dxa_is(df, col_map_adipose, verbose = FALSE)
  expect_named(out, c("LIRI_inv", "Lipo_inv", "ATIRI_inv"))

  # Lipo_inv = - rate_glycerol * (I0/6)
  expect_equal(out$Lipo_inv, -1 * (2.0 * (60 / 6)))
  # ATIRI_inv = - rate_palmitate * (I0/6)
  expect_equal(out$ATIRI_inv, -1 * (1.5 * (60 / 6)))
  # LIRI_inv should be finite given positive inputs
  expect_true(is.finite(out$LIRI_inv))
})

test_that("full mode errors when required columns are missing", {
  skip_on_cran()
  df <- tibble(G0 = 5, I0 = 60)
  full_map <- list(
    G0 = "G0", G30 = "G30", G120 = "G120",
    I0 = "I0", I30 = "I30", I120 = "I120",
    TG = "TG", HDL_c = "HDL_c", FFA = "FFA",
    rate_palmitate = "rate_palmitate",
    rate_glycerol = "rate_glycerol",
    fat_mass = "fat_mass",
    weight = "weight",
    bmi = "bmi"
  )
  expect_error(
    tracer_dxa_is(df, full_map),
    "missing required columns"
  )
})

test_that("full mode returns all seven indices and correct values", {
  skip_on_cran()
  df_full <- tibble(
    G0             = 5,
    G30            = 7,
    G120           = 7,
    I0             = 60,
    I30            = 65,
    I120           = 70,
    TG             = 1.5,
    HDL_c          = 1.2,
    FFA            = 0.5,
    rate_palmitate = 1.5,
    rate_glycerol  = 2.0,
    fat_mass       = 20,
    weight         = 70,
    bmi            = 24
  )
  full_map <- list(
    G0 = "G0", G30 = "G30", G120 = "G120",
    I0 = "I0", I30 = "I30", I120 = "I120",
    TG = "TG", HDL_c = "HDL_c", FFA = "FFA",
    rate_palmitate = "rate_palmitate",
    rate_glycerol = "rate_glycerol",
    fat_mass = "fat_mass",
    weight = "weight",
    bmi = "bmi"
  )
  out <- tracer_dxa_is(df_full, full_map, verbose = FALSE)

  expected_names <- c(
    "I_AUC", "FFA_AUC",
    "tracer_palmitate_SI", "tracer_glycerol_SI",
    "LIRI_inv", "Lipo_inv", "ATIRI_inv"
  )
  expect_named(out, expected_names)

  I0_u <- 60 / 6
  I30_u <- 65 / 6
  I120_u <- 70 / 6

  I_AUC_expected <- 0.5 * ((I0_u + I30_u) * 30 + (I30_u + I120_u) * 90)
  expect_equal(out$I_AUC, I_AUC_expected, tolerance = 1e-8)

  expect_equal(out$FFA_AUC, 0.5 * (0.5 + 0.5) * 120)

  expect_equal(out$tracer_palmitate_SI, 1.5 / 20)
  expect_equal(out$tracer_glycerol_SI, 2.0 / 20)

  expect_equal(out$Lipo_inv, -1 * (2.0 * I0_u))
  expect_equal(out$ATIRI_inv, -1 * (1.5 * I0_u))
})

test_that("vectorized over multiple rows", {
  skip_on_cran()
  base_df <- tibble(
    G0 = 5, G30 = 7, G120 = 7,
    I0 = 60, I30 = 65, I120 = 70,
    TG = 1.5, HDL_c = 1.2, FFA = 0.5,
    rate_palmitate = 1.5, rate_glycerol = 2.0,
    fat_mass = 20, weight = 70, bmi = 24
  )
  df2 <- bind_rows(
    base_df,
    mutate(base_df,
      I0             = 120,
      rate_glycerol  = 4,
      rate_palmitate = 3
    )
  )
  full_map <- list(
    G0 = "G0", G30 = "G30", G120 = "G120",
    I0 = "I0", I30 = "I30", I120 = "I120",
    TG = "TG", HDL_c = "HDL_c", FFA = "FFA",
    rate_palmitate = "rate_palmitate",
    rate_glycerol = "rate_glycerol",
    fat_mass = "fat_mass",
    weight = "weight",
    bmi = "bmi"
  )
  out2 <- tracer_dxa_is(df2, full_map, verbose = FALSE)
  expect_equal(nrow(out2), 2)
})

test_that("verbose emits col_map and results messages", {
  skip_on_cran()
  df_full <- tibble(
    G0 = 5, G30 = 7, G120 = 7,
    I0 = 60, I30 = 65, I120 = 70,
    TG = 1.5, HDL_c = 1.2, FFA = 0.5,
    rate_palmitate = 1.5, rate_glycerol = 2.0,
    fat_mass = 20, weight = 70, bmi = 24
  )
  col_map_adipose <- list(
    I0             = "I0",
    rate_palmitate = "rate_palmitate",
    rate_glycerol  = "rate_glycerol",
    fat_mass       = "fat_mass",
    weight         = "weight",
    HDL_c          = "HDL_c",
    bmi            = "bmi"
  )
  withr::local_options(healthmarkers.verbose = "inform")
  expect_message(tracer_dxa_is(df_full, col_map_adipose, verbose = TRUE), "tracer_dxa_is")
  expect_message(tracer_dxa_is(df_full, col_map_adipose, verbose = TRUE), "col_map")
  expect_message(tracer_dxa_is(df_full, col_map_adipose, verbose = TRUE), "results:")
})

test_that("verbose double-fire guard", {
  skip_on_cran()
  df_full <- tibble(
    G0 = 5, G30 = 7, G120 = 7,
    I0 = 60, I30 = 65, I120 = 70,
    TG = 1.5, HDL_c = 1.2, FFA = 0.5,
    rate_palmitate = 1.5, rate_glycerol = 2.0,
    fat_mass = 20, weight = 70, bmi = 24
  )
  col_map_adipose <- list(
    I0             = "I0",
    rate_palmitate = "rate_palmitate",
    rate_glycerol  = "rate_glycerol",
    fat_mass       = "fat_mass",
    weight         = "weight",
    HDL_c          = "HDL_c",
    bmi            = "bmi"
  )
  withr::local_options(healthmarkers.verbose = "inform")
  msgs <- testthat::capture_messages(tracer_dxa_is(df_full, col_map_adipose, verbose = TRUE))
  expect_gte(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})

test_that("na_action policies: error and omit behave as expected", {
  skip_on_cran()
  col_map_adipose <- list(
    I0             = "I0",
    rate_palmitate = "rate_palmitate",
    rate_glycerol  = "rate_glycerol",
    fat_mass       = "fat_mass",
    weight         = "weight",
    HDL_c          = "HDL_c",
    bmi            = "bmi"
  )
  df_na <- tibble(
    I0 = 60, rate_palmitate = 1.5, rate_glycerol = 2.0,
    fat_mass = NA_real_, weight = 70, HDL_c = 1.2, bmi = 24
  )
  expect_error(
    suppressWarnings(tracer_dxa_is(df_na, col_map_adipose, na_action = "error")),
    "required inputs contain missing values"
  )
  out_omit <- suppressWarnings(tracer_dxa_is(df_na, col_map_adipose, na_action = "omit"))
  expect_equal(nrow(out_omit), 0L)
})

test_that("check_extreme removed: function passes through outlier rows", {
  skip_on_cran()
  df_ext <- tibble(
    G0 = 50, G30 = 60, G120 = 70,
    I0 = 6000, I30 = 6000, I120 = 6000,
    TG = 60, HDL_c = 12, FFA = 6,
    rate_palmitate = 20000, rate_glycerol = 20000,
    fat_mass = 0.05, weight = 700, bmi = 120
  )
  full_map <- list(
    G0 = "G0", G30 = "G30", G120 = "G120",
    I0 = "I0", I30 = "I30", I120 = "I120",
    TG = "TG", HDL_c = "HDL_c", FFA = "FFA",
    rate_palmitate = "rate_palmitate",
    rate_glycerol = "rate_glycerol",
    fat_mass = "fat_mass",
    weight = "weight",
    bmi = "bmi"
  )
  out <- tracer_dxa_is(df_ext, full_map)
  expect_equal(nrow(out), 1L)
})

test_that("zero denominators emit a consolidated warning and yield NA where applicable", {
  skip_on_cran()
  df_zero <- tibble(
    G0 = 5, G30 = 7, G120 = 7,
    I0 = 60, I30 = 65, I120 = 70,
    TG = 1.5, HDL_c = 1.2, FFA = 0.5,
    rate_palmitate = 1.5, rate_glycerol = 2.0,
    fat_mass = 0,        # zero -> tracer SI undefined
    weight = 0,          # zero -> fm/weight undefined for LIRI_inv term
    bmi = 24
  )
  full_map <- list(
    G0 = "G0", G30 = "G30", G120 = "G120",
    I0 = "I0", I30 = "I30", I120 = "I120",
    TG = "TG", HDL_c = "HDL_c", FFA = "FFA",
    rate_palmitate = "rate_palmitate",
    rate_glycerol = "rate_glycerol",
    fat_mass = "fat_mass",
    weight = "weight",
    bmi = "bmi"
  )
  expect_warning(
    out_zero <- tracer_dxa_is(df_zero, full_map),
    "zero denominators detected"
  )
  expect_true(is.na(out_zero$tracer_palmitate_SI))
  expect_true(is.na(out_zero$tracer_glycerol_SI))
  # Lipo_inv unaffected by fm/weight; should still be finite
  expect_true(is.finite(out_zero$Lipo_inv))
})
