test_that("lipid_markers computes core lipid markers", {
  skip_on_cran()
  df <- tibble(
    TC    = 5,
    HDL_c = 1,
    TG    = 1.3,
    LDL_c = 3,
    ApoB  = 1.1,
    ApoA1 = 1.5
  )

  out <- lipid_markers(df, col_map = list(
    TC    = "TC",
    HDL_c = "HDL_c",
    TG    = "TG",
    LDL_c = "LDL_c",
    ApoB  = "ApoB",
    ApoA1 = "ApoA1"
  ), verbose = FALSE)

  expect_named(out, c(
    "non_HDL_c", "remnant_c", "ratio_TC_HDL",
    "ratio_TG_HDL", "ratio_LDL_HDL", "ApoB_ApoA1"
  ))
  expect_equal(out$non_HDL_c, 5 - 1)
  expect_equal(out$remnant_c, 5 - (1 + 3))
  expect_equal(out$ratio_TC_HDL, 5 / 1)
  expect_equal(out$ratio_TG_HDL, 1.3 / 1)
  expect_equal(out$ratio_LDL_HDL, 3 / 1)
  expect_equal(out$ApoB_ApoA1, 1.1 / 1.5)
})

test_that("lipid_markers estimates LDL via Friedewald when LDL_c absent, no warning", {
  skip_on_cran()
  df2 <- tibble(TC = 5, HDL_c = 1, TG = 1.5)
  expect_no_warning(
    out2 <- lipid_markers(df2, col_map = list(TC = "TC", HDL_c = "HDL_c", TG = "TG"), verbose = FALSE)
  )
  # LDL (mmol/L Friedewald) = 5 - 1 - (1.5/2.2)
  expect_equal(out2$ratio_LDL_HDL, (5 - 1 - (1.5 / 2.2)) / 1)
})

test_that("lipid_markers emits informational message for Friedewald LDL", {
  skip_on_cran()
  df2 <- tibble(TC = 5, HDL_c = 1, TG = 1.5)
  msgs <- testthat::capture_messages(
    lipid_markers(df2, col_map = list(TC = "TC", HDL_c = "HDL_c", TG = "TG"), verbose = TRUE)
  )
  expect_true(any(grepl("Friedewald", msgs)))
})

test_that("lipid_markers computes VAI_Men and VAI_Women when waist & BMI provided", {
  skip_on_cran()
  df3 <- tibble(
    TC = 5, HDL_c = 1, TG = 1.3,
    LDL_c = 3, ApoB = 1.1, ApoA1 = 1.5,
    waist = 85, BMI = 26
  )
  out3 <- lipid_markers(df3, col_map = list(
    TC    = "TC",
    HDL_c = "HDL_c",
    TG    = "TG",
    LDL_c = "LDL_c",
    ApoB  = "ApoB",
    ApoA1 = "ApoA1",
    waist = "waist",
    BMI   = "BMI"
  ), verbose = FALSE)
  expect_true(all(c("VAI_Men", "VAI_Women") %in% names(out3)))
  exp_men <- (85 / (39.68 + 1.88 * 26)) * (1.3 / 1.03) * (1.31 / 1)
  exp_wom <- (85 / (36.58 + 1.89 * 26)) * (1.3 / 0.81) * (1.52 / 1)
  expect_equal(out3$VAI_Men, exp_men, tolerance = 1e-8)
  expect_equal(out3$VAI_Women, exp_wom, tolerance = 1e-8)
})

test_that("lipid_markers omits VAI when waist or BMI missing", {
  skip_on_cran()
  df4a <- tibble(TC = 5, HDL_c = 1, TG = 1.3, waist = 85) # BMI missing
  out4a <- lipid_markers(df4a, col_map = list(
    TC    = "TC",
    HDL_c = "HDL_c",
    TG    = "TG",
    waist = "waist"
  ), verbose = FALSE)
  expect_false("VAI_Men" %in% names(out4a))
  expect_false("VAI_Women" %in% names(out4a))

  df4b <- tibble(TC = 5, HDL_c = 1, TG = 1.3, BMI = 26) # waist missing
  out4b <- lipid_markers(df4b, col_map = list(
    TC    = "TC",
    HDL_c = "HDL_c",
    TG    = "TG",
    BMI   = "BMI"
  ), verbose = FALSE)
  expect_false("VAI_Men" %in% names(out4b))
  expect_false("VAI_Women" %in% names(out4b))
})

# ────────────────────────────────────────────────────────────────────────────────
# LAP tests
# ────────────────────────────────────────────────────────────────────────────────

test_that("lipid_markers computes LAP_Men and LAP_Women when waist provided", {
  skip_on_cran()
  df_lap <- tibble(TC = 5, HDL_c = 1, TG = 1.3, waist = 100)
  out_lap <- lipid_markers(df_lap, col_map = list(
    TC    = "TC",
    HDL_c = "HDL_c",
    TG    = "TG",
    waist = "waist"
  ), verbose = FALSE)
  expect_true(all(c("LAP_Men", "LAP_Women") %in% names(out_lap)))

  exp_lap_men <- (100 - 65) * 1.3
  exp_lap_wom <- (100 - 58) * 1.3
  expect_equal(out_lap$LAP_Men, exp_lap_men)
  expect_equal(out_lap$LAP_Women, exp_lap_wom)
})

test_that("lipid_markers omits LAP when waist missing", {
  skip_on_cran()
  df_no_lap <- tibble(TC = 5, HDL_c = 1, TG = 1.3)
  out_no_lap <- lipid_markers(df_no_lap, col_map = list(
    TC    = "TC",
    HDL_c = "HDL_c",
    TG    = "TG"
  ), verbose = FALSE)
  expect_false("LAP_Men" %in% names(out_no_lap))
  expect_false("LAP_Women" %in% names(out_no_lap))
})

# ────────────────────────────────────────────────────────────────────────────────
# HM-CS v2 additions: NA policy, extremes, and coercion
# ────────────────────────────────────────────────────────────────────────────────

test_that("na_action='omit' drops rows with NA in required inputs", {
  skip_on_cran()
  df <- tibble(TC = c(5, NA), HDL_c = c(1, 1), TG = c(1.3, 1.4), LDL_c = c(3.7, 3.8))
  out <- lipid_markers(df, col_map = list(TC="TC", HDL_c="HDL_c", TG="TG", LDL_c="LDL_c"), na_action = "omit", verbose = FALSE)
  expect_equal(nrow(out), 1L)
  expect_equal(out$ratio_TC_HDL, 5/1)
})

test_that("na_action='error' aborts on NA in required inputs", {
  skip_on_cran()
  df <- tibble(TC = c(5, NA), HDL_c = c(1, 1), TG = c(1.3, 1.4))
  expect_error(
    lipid_markers(df, col_map = list(TC="TC", HDL_c="HDL_c", TG="TG"), na_action = "error", verbose = FALSE),
    "missing or non-finite"
  )
})

test_that("extreme values produce no warning/error; range note appears in verbose", {
  skip_on_cran()
  df <- tibble(TC = 5, HDL_c = 1, TG = -3, LDL_c = 3) # negative TG out-of-range
  expect_no_warning(
    out <- lipid_markers(df, col_map = list(TC="TC", HDL_c="HDL_c", TG="TG", LDL_c="LDL_c"),
                         verbose = FALSE)
  )
  # TG = -3 propagates; no alteration
  expect_false(is.na(out$ratio_TG_HDL))
  # Range note appears in verbose output
  msgs <- testthat::capture_messages(
    lipid_markers(df, col_map = list(TC="TC", HDL_c="HDL_c", TG="TG", LDL_c="LDL_c"),
                  verbose = TRUE)
  )
  expect_true(any(grepl("range note", msgs)))
})

test_that("numeric coercion warns when NAs introduced", {
  skip_on_cran()
  df <- tibble(TC = c("5","oops"), HDL_c = c("1","1"), TG = c("1.3","1.4"), LDL_c = c(3, 3))
  expect_warning(
    lipid_markers(df, col_map = list(TC="TC", HDL_c="HDL_c", TG="TG", LDL_c="LDL_c"), verbose = FALSE),
    "coerced to numeric; NAs introduced"
  )
})

test_that("verbose emits col_map, optional inputs, computing markers, and results messages", {
  skip_on_cran()
  df_v <- tibble::tibble(TC = 5, HDL_c = 1, TG = 1.3, LDL_c = 3)
  cm_v <- list(TC = "TC", HDL_c = "HDL_c", TG = "TG", LDL_c = "LDL_c")
  msgs <- testthat::capture_messages(lipid_markers(df_v, cm_v, verbose = TRUE))
  expect_true(any(grepl("col_map", msgs)))
  expect_true(any(grepl("optional inputs", msgs)))
  expect_true(any(grepl("computing markers", msgs)))
  expect_true(any(grepl("results:", msgs)))
})

test_that("verbose double-fire guard", {
  skip_on_cran()
  df_v <- tibble::tibble(TC = 5, HDL_c = 1, TG = 1.3, LDL_c = 3)
  cm_v <- list(TC = "TC", HDL_c = "HDL_c", TG = "TG", LDL_c = "LDL_c")
  msgs <- testthat::capture_messages(
    lipid_markers(df_v, cm_v, verbose = TRUE)
  )
  expect_gte(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",  msgs)), 1L)
  expect_equal(sum(grepl("optional inputs", msgs)), 1L)
  expect_equal(sum(grepl("computing markers", msgs)), 1L)
})

test_that("ID column is prepended to output when detected", {
  skip_on_cran()
  df_id <- tibble::tibble(
    id = 1:3, TC = c(5, 5.5, 6), HDL_c = c(1, 1.1, 1.2),
    TG = c(1.3, 1.4, 1.5), LDL_c = c(3, 3.2, 3.4)
  )
  out <- lipid_markers(df_id, verbose = FALSE)
  expect_equal(names(out)[1L], "id")
  expect_equal(out$id, 1:3)
})

test_that("BMI is pre-computed from weight and height enabling VAI and TyG_BMI", {
  skip_on_cran()
  df_bmi <- tibble::tibble(
    TC = 5, HDL_c = 1, TG = 1.3, LDL_c = 3,
    waist = 85, weight = 70, height = 175, glucose = 5.5
  )
  out <- lipid_markers(df_bmi, verbose = FALSE)
  bmi_expected <- 70 / (1.75) ^ 2
  vai_men_expected <- (85 / (39.68 + 1.88 * bmi_expected)) * (1.3 / 1.03) * (1.31 / 1)
  expect_true("VAI_Men" %in% names(out))
  expect_equal(out$VAI_Men, vai_men_expected, tolerance = 1e-6)
  expect_true("TyG_BMI" %in% names(out))
})

test_that("glucose derived from mapped G0 column via col_map redirect enables TyG_BMI", {
  skip_on_cran()
  # Physical column is "pglu0" — col_map maps G0 -> "pglu0".
  # .hm_build_col_map materializes data[["G0"]], then precompute derives glucose.
  df <- tibble::tibble(
    TC    = 5,
    HDL_c = 1,
    TG    = 1.3,
    BMI   = 25,
    waist = 85,
    pglu0 = 5.5    # non-standard name for fasting glucose
  )
  cm  <- list(TC = "TC", HDL_c = "HDL_c", TG = "TG", BMI = "BMI",
              waist = "waist", G0 = "pglu0")
  out <- lipid_markers(df, col_map = cm, verbose = FALSE)
  expect_true("TyG_BMI" %in% names(out))
  expect_false(is.na(out$TyG_BMI), info = "TyG_BMI requires glucose derived from mapped G0")
})

test_that("BMI derived from mapped weight/height with non-standard names enables VAI", {
  skip_on_cran()
  # Physical columns are "body_weight" and "body_height" — must be resolved via col_map.
  # .hm_build_col_map materializes data[["weight"]] and data[["height"]],
  # then .hm_precompute_from_deps derives BMI, enabling VAI_Men.
  df <- tibble::tibble(
    TC          = 5,
    HDL_c       = 1,
    TG          = 1.3,
    LDL_c       = 3,
    waist       = 85,
    body_weight = 70,
    body_height = 175,
    glucose     = 5.5
  )
  cm  <- list(TC = "TC", HDL_c = "HDL_c", TG = "TG", LDL_c = "LDL_c",
              waist = "waist", glucose = "glucose",
              weight = "body_weight", height = "body_height")
  out <- lipid_markers(df, col_map = cm, verbose = FALSE)
  bmi_expected     <- 70 / (1.75)^2
  vai_men_expected <- (85 / (39.68 + 1.88 * bmi_expected)) * (1.3 / 1.03) * (1.31 / 1)
  expect_true("VAI_Men" %in% names(out))
  expect_equal(out$VAI_Men, vai_men_expected, tolerance = 1e-6,
               info = "VAI_Men should use BMI precomputed from mapped weight/height")
})

test_that("partial col_map is filled from dictionary for remaining keys", {
  skip_on_cran()
  # User provides only G0 mapping; TC, HDL_c, TG should be inferred by dict.
  df <- tibble::tibble(
    chol   = 5,      # maps to TC via synonym dictionary
    hdlc   = 1,
    trig   = 1.3,
    pglu0  = 5.5
  )
  cm  <- list(G0 = "pglu0")
  # hm_col_report should match chol->TC, hdlc->HDL_c, trig->TG
  out <- lipid_markers(df, col_map = cm, verbose = FALSE)
  # If dict inferred TC/HDL_c/TG the result has all expected cols
  expect_true("non_HDL_c" %in% names(out))
})

