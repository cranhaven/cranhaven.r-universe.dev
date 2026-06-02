# tests/testthat/test-nutrient_markers.R

library(testthat)
library(tibble)
library(HealthMarkers)

cm_id <- function(df) { m <- as.list(names(df)); names(m) <- names(df); m }

test_that("nutrient_markers computes all indices correctly", {
  skip_on_cran()
  df <- tibble(
    ferritin         = 100,
    transferrin_sat  = 50,
    albumin          = 40,
    total_protein    = 70,
    EPA              = 1,
    DHA              = 2,
    Mg               = 1.0,
    creatinine       = 50,
    glycated_albumin = 4,
    uric_acid        = 400,
    BUN              = 10,
    phosphate        = 1.2,
    calcium          = 2.2,
    Na               = 140,
    K                = 4,
    Cl               = 100,
    HCO3             = 24,
    Tyr              = 60,
    Phe              = 30
  )
  out <- nutrient_markers(df, col_map = cm_id(df))
  expect_s3_class(out, "tbl_df")
  expect_named(out, c(
    "FerritinTS", "AGR", "Omega3Index", "Mg_Cr_Ratio",
    "GlycatedAlbuminPct", "UA_Cr_Ratio", "BUN_Cr_Ratio",
    "Ca_x_Phosphate", "AnionGap", "Tyr_Phe_Ratio"
  ))
  expect_equal(out$FerritinTS, 100 / 50, tolerance = 1e-12)
  expect_equal(out$AGR, 40 / (70 - 40), tolerance = 1e-8)
  expect_equal(out$Omega3Index, 1 + 2, tolerance = 1e-12)
  expect_equal(out$Mg_Cr_Ratio, 1.0 / 50, tolerance = 1e-8)
  expect_equal(out$GlycatedAlbuminPct, (4 / 40) * 100, tolerance = 1e-8)
  expect_equal(out$UA_Cr_Ratio, 400 / 50, tolerance = 1e-8)
  expect_equal(out$BUN_Cr_Ratio, 10 / 50, tolerance = 1e-8)
  expect_equal(out$Ca_x_Phosphate, 2.2 * 1.2, tolerance = 1e-8)
  expect_equal(out$AnionGap, (140 + 4) - (100 + 24), tolerance = 1e-12)
  expect_equal(out$Tyr_Phe_Ratio, 60 / 30, tolerance = 1e-8)
})

test_that("nutrient_markers is vectorized over rows", {
  skip_on_cran()
  df <- tibble(
    ferritin         = c(80, 120),
    transferrin_sat  = c(40, 60),
    albumin          = c(42, 38),
    total_protein    = c(74, 72)
  )
  out <- nutrient_markers(df, col_map = cm_id(df))
  expect_equal(nrow(out), 2L)
  expect_equal(out$FerritinTS, c(80 / 40, 120 / 60), tolerance = 1e-8)
  expect_equal(out$AGR, c(42 / (74 - 42), 38 / (72 - 38)), tolerance = 1e-8)
  expect_true(all(is.na(out$Omega3Index)))
})

test_that("missing inputs yield NA rather than error", {
  skip_on_cran()
  df <- tibble(
    ferritin        = 90,
    transferrin_sat = 45,
    albumin         = 42
  )
  out <- nutrient_markers(df, col_map = cm_id(df))
  expect_equal(out$FerritinTS, 90 / 45, tolerance = 1e-8)
  expect_true(is.na(out$AGR))
  expect_true(is.na(out$Omega3Index))
  expect_true(is.na(out$Mg_Cr_Ratio))
})

test_that("verbose emits preparing, column map, and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- tibble(ferritin = 50, transferrin_sat = 25)
  expect_message(nutrient_markers(df, col_map = cm_id(df), verbose = TRUE), "nutrient_markers")
  expect_message(nutrient_markers(df, col_map = cm_id(df), verbose = TRUE), "col_map")
  expect_message(nutrient_markers(df, col_map = cm_id(df), verbose = TRUE), "results:")
})

test_that("verbose double-fire guard", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- tibble(ferritin = 50, transferrin_sat = 25)
  msgs <- testthat::capture_messages(
    nutrient_markers(df, col_map = cm_id(df), verbose = TRUE)
  )
  expect_equal(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})

test_that("errors if `data` is not a data.frame", {
  skip_on_cran()
  expect_error(nutrient_markers("not a df", col_map = list()), "data\\.frame|tibble")
})

test_that("na_action='error' aborts when used inputs contain NA", {
  skip_on_cran()
  df <- tibble(
    ferritin        = c(100, 90),
    transferrin_sat = c(50, NA_real_)
  )
  expect_error(
    suppressWarnings(nutrient_markers(df, col_map = cm_id(df), na_action = "error")),
    "used input columns contain missing values"
  )
})

test_that("na_action='omit' drops rows with NA in used inputs", {
  skip_on_cran()
  df <- tibble(
    ferritin        = c(100, 90),
    transferrin_sat = c(50, NA_real_)
  )
  withr::local_options(healthmarkers.verbose = "inform")
  expect_message(
    out <- nutrient_markers(df, col_map = cm_id(df), na_action = "omit", verbose = TRUE),
    "omitting 1 rows with NA in used inputs"
  )
  expect_equal(nrow(out), 1L)
  expect_equal(out$FerritinTS, 100 / 50, tolerance = 1e-8)
})

test_that("extreme values produce range note in verbose and no warnings in silent mode", {
  skip_on_cran()
  df <- tibble(ferritin = 5000, transferrin_sat = 50)
  # No warning emitted in non-verbose mode
  expect_no_warning(
    nutrient_markers(df, col_map = cm_id(df), verbose = FALSE)
  )
  # Verbose mode emits a range note informational message
  withr::local_options(healthmarkers.verbose = "inform")
  expect_message(
    nutrient_markers(df, col_map = cm_id(df), verbose = TRUE),
    "range note"
  )
})

test_that("denominator zero emits a single summary warning and yields NA outputs", {
  skip_on_cran()
  df <- tibble(
    Mg         = 0.9,
    uric_acid  = 300,
    BUN        = 14,
    creatinine = 0
  )
  out <- NULL
  expect_warning(
    out <- nutrient_markers(df, col_map = cm_id(df)),
    "zero denominators detected"
  )
  expect_true(all(is.na(out$Mg_Cr_Ratio)))
  expect_true(all(is.na(out$UA_Cr_Ratio)))
  expect_true(all(is.na(out$BUN_Cr_Ratio)))
})

test_that("col_map with unrecognized keys warns and ignores them", {
  skip_on_cran()
  df <- tibble(ferritin = 100, transferrin_sat = 50)
  cm <- list(ferritin = "ferritin", transferrin_sat = "transferrin_sat", LDL = "LDL")
  expect_warning(
    nutrient_markers(df, col_map = cm),
    "ignoring unrecognized keys in col_map"
  )
})
