library(testthat)
library(tibble)

test_that("errors if missing any of HDL_c, TG, or BMI", {
  skip_on_cran()
  df1 <- tibble(TG = 1, BMI = 24)
  expect_error(
    glycemic_markers(df1, verbose = FALSE),
    "missing required columns: HDL_c"
  )
  df2 <- tibble(HDL_c = 1, BMI = 24)
  expect_error(
    glycemic_markers(df2, verbose = FALSE),
    "missing required columns: TG"
  )
  df3 <- tibble(HDL_c = 1, TG = 1.3)
  expect_error(
    glycemic_markers(df3, verbose = FALSE),
    "missing required columns: BMI"
  )
})

test_that("SPISE is computed correctly", {
  skip_on_cran()
  df <- tibble(HDL_c = 1.0, TG = 1.3, BMI = 24)
  expected <- 600 * 1.0^0.185 / (1.3^0.2 * 24^1.338)
  out <- glycemic_markers(df, verbose = FALSE)
  expect_equal(out$SPISE, expected, tolerance = 1e-8)
})

test_that("METS_IR returns finite when glucose present and HDL_c != 1, NA when denominator is zero", {
  skip_on_cran()
  # Use HDL_c != 1 so log(HDL_c) != 0 and result is finite
  df_with <- tibble(HDL_c = 1.2, TG = 1.3, BMI = 24, glucose = 5.6)
  out1 <- glycemic_markers(df_with, verbose = FALSE)
  expected <- (log(2 * 5.6 + 1.3) * 24) / log(1.2)
  expect_equal(out1$METS_IR, expected, tolerance = 1e-8)

  # Denominator zero case -> safe division returns NA
  df_den0 <- tibble(HDL_c = 1, TG = 1.3, BMI = 24, glucose = 5.6)
  out_den0 <- glycemic_markers(df_den0, verbose = FALSE)
  expect_true(is.na(out_den0$METS_IR))

  # No glucose column -> NA
  df_without <- tibble(HDL_c = 1, TG = 1.3, BMI = 24)
  out2 <- glycemic_markers(df_without, verbose = FALSE)
  expect_true(is.na(out2$METS_IR))
})

test_that("prediabetes and diabetes flags handle HbA1c correctly and propagate NA", {
  skip_on_cran()
  df <- tibble(
    HDL_c = 1, TG = 1.3, BMI = 24,
    HbA1c = c(40, 44, 50)
  )
  out <- glycemic_markers(df, verbose = FALSE)
  expect_equal(out$prediabetes, c(0L, 1L, 1L))
  expect_equal(out$diabetes, c(0L, 0L, 1L))

  df2 <- tibble(HDL_c = 1, TG = 1.3, BMI = 24)
  out2 <- glycemic_markers(df2, verbose = FALSE)
  expect_true(all(is.na(out2$prediabetes)))
  expect_true(all(is.na(out2$diabetes)))
})

test_that("HOMA_CP computed when C_peptide & G0 present, NA otherwise", {
  skip_on_cran()
  df_ok <- tibble(
    HDL_c = 1, TG = 1.3, BMI = 24,
    C_peptide = 300, G0 = 5.5
  )
  expected <- (5.5 * (300 / 6)) / 22.5
  out_ok <- glycemic_markers(df_ok, verbose = FALSE)
  expect_equal(out_ok$HOMA_CP, expected, tolerance = 1e-8)

  df_bad <- tibble(HDL_c = 1, TG = 1.3, BMI = 24, C_peptide = 300)
  out_bad <- glycemic_markers(df_bad, verbose = FALSE)
  expect_true(is.na(out_bad$HOMA_CP))
})

test_that("LAR, ASI, and TyG_index compute correctly when inputs present", {
  skip_on_cran()
  # set up a row where everything is present
  df <- tibble(
    HDL_c = 1, TG = 1.3, BMI = 24,
    leptin = 10, adiponectin = 5,
    I0 = 50,
    glucose = 5.6
  )
  out <- glycemic_markers(df, verbose = FALSE)
  # Leptin/Adiponectin Ratio
  expect_equal(out$LAR, 10 / 5)
  # Adiponectin Sensitivity Index = adiponectin / I0
  expect_equal(out$ASI, 5 / 50)
  # TyG_index: ln((TG*88.57) * (glucose*18) / 2)
  TG_mgdl <- 1.3 * 88.57
  Glu_mgdl <- 5.6 * 18
  expect_equal(out$TyG_index, log(TG_mgdl * Glu_mgdl / 2), tolerance = 1e-8)
})

test_that("LAR, ASI, TyG_index are NA when their inputs are missing", {
  skip_on_cran()
  df1 <- tibble(HDL_c = 1, TG = 1.3, BMI = 24)
  out1 <- glycemic_markers(df1, verbose = FALSE)
  expect_true(is.na(out1$LAR))
  expect_true(is.na(out1$ASI))
  expect_true(is.na(out1$TyG_index))

  df2 <- tibble(
    HDL_c = 1, TG = 1.3, BMI = 24,
    leptin = 10
  )
  out2 <- glycemic_markers(df2, verbose = FALSE)
  expect_true(is.na(out2$LAR))
})

test_that("output is vectorized and contains all expected columns", {
  skip_on_cran()
  df <- tibble(
    HDL_c       = c(1, 1.1),
    TG          = c(1.3, 1.5),
    BMI         = c(24, 26),
    glucose     = c(5.5, NA),
    HbA1c       = c(44, 39),
    C_peptide   = c(300, NA),
    G0          = c(5.5, NA),
    leptin      = c(10, NA),
    adiponectin = c(5, NA),
    I0          = c(50, NA)
  )
  out <- glycemic_markers(df, verbose = FALSE)
  expect_equal(nrow(out), 2)
  expect_named(
    out,
    c(
      "SPISE", "METS_IR", "prediabetes", "diabetes",
      "HOMA_CP", "LAR", "ASI", "TyG_index"
    )
  )
})

test_that("verbose emits col_map and results messages", {
  skip_on_cran()
  df <- tibble(HDL_c = 1, TG = 1.3, BMI = 24)
  expect_message(glycemic_markers(df, verbose = TRUE), "glycemic_markers")
  expect_message(glycemic_markers(df, verbose = TRUE), "col_map")
  expect_message(glycemic_markers(df, verbose = TRUE), "optional inputs")
  expect_message(glycemic_markers(df, verbose = TRUE), "computing markers")
  expect_message(glycemic_markers(df, verbose = TRUE), "results:")
})

test_that("glycemic_markers computes expected columns with minimal inputs", {
  skip_on_cran()
  df <- data.frame(HDL_c = c(1.0, 1.3), TG = c(1.5, 2.0), BMI = c(24, 30))
  res <- glycemic_markers(df, verbose = FALSE)
  expect_s3_class(res, "tbl_df")
  expect_named(res, c("SPISE","METS_IR","prediabetes","diabetes","HOMA_CP","LAR","ASI","TyG_index"))
  # METS_IR, TyG_index depend on glucose; NA by default
  expect_true(all(is.na(res$METS_IR)))
  expect_true(all(is.na(res$TyG_index)))
})

test_that("Optional inputs drive only their outputs; NA handling works", {
  skip_on_cran()
  df <- data.frame(
    HDL_c = c(1.2, 1.3),   # avoid ln(HDL_c) == 0
    TG = c(1.5, 2.0),
    BMI = c(24, 30),
    glucose = c(5.6, NA),
    HbA1c = c(44, 38),
    C_peptide = c(300, 500),
    G0 = c(5.5, 6.2),
    I0 = c(60, 120),
    leptin = c(10, 20),
    adiponectin = c(8, 5)
  )
  res_keep <- glycemic_markers(df, na_action = "keep", verbose = FALSE)
  expect_false(all(is.na(res_keep$METS_IR))) # at least first row computable
  expect_false(all(is.na(res_keep$TyG_index)))
  expect_equal(res_keep$prediabetes, c(1L, 0L))
  expect_equal(res_keep$diabetes, c(0L, 0L))
  # error on NA/non-finite when na_action = "error"
  expect_error(glycemic_markers(df, na_action = "error", verbose = FALSE), "missing or non-finite")
  # omit drops the NA row
  res_omit <- glycemic_markers(df, na_action = "omit", verbose = FALSE)
  expect_equal(nrow(res_omit), 1L)
})

test_that("extreme values produce no warning/error; range note appears in verbose", {
  skip_on_cran()
  df <- data.frame(
    HDL_c = c(1.2, 1.3),
    TG    = c(25, 2.0),    # TG out-of-range (default max 20)
    BMI   = c(24, 30),
    glucose = c(5.6, 7.1)
  )
  # No warning or error — extreme values are noted informally
  expect_no_warning(glycemic_markers(df, verbose = FALSE))
  # SPISE is still computed (values not altered)
  out <- glycemic_markers(df, verbose = FALSE)
  expect_false(any(is.na(out$SPISE)))
  # Range note appears in verbose output
  msgs <- testthat::capture_messages(glycemic_markers(df, verbose = TRUE))
  expect_true(any(grepl("range note", msgs)))
})

test_that("Coercion to numeric warns when NAs introduced", {
  skip_on_cran()
  df <- data.frame(
    HDL_c = c("1.0", "bad"),
    TG = c("1.5", "2.0"),
    BMI = c(24, 30)
  )
  expect_warning(glycemic_markers(df, verbose = FALSE), "coerced to numeric; NAs introduced")
})

test_that("verbose double-fire guard", {
  skip_on_cran()
  df <- tibble(HDL_c = 1, TG = 1.3, BMI = 24)
  msgs <- testthat::capture_messages(glycemic_markers(df, verbose = TRUE))
  expect_equal(sum(grepl("col_map",  msgs)), 1L)
  expect_equal(sum(grepl("results:",    msgs)), 1L)
  expect_equal(sum(grepl("optional inputs", msgs)), 1L)
  expect_equal(sum(grepl("computing markers", msgs)), 1L)
})

test_that("glycemic_markers runs and returns key columns", {
  skip_on_cran()
  df <- tibble::tibble(
    HDL_c  = c(1.0, 1.3),
    TG     = c(1.5, 2.0),
    BMI    = c(24, 30),
    glucose = c(5.6, 7.1)
  )
  out <- glycemic_markers(df, na_action = "keep", verbose = FALSE)
  expect_true(all(c("SPISE", "METS_IR") %in% names(out)))
  expect_equal(nrow(out), nrow(df))
})

test_that("ID column is prepended to output when present in data", {
  skip_on_cran()
  df <- tibble::tibble(
    id    = 1:3,
    HDL_c = c(1.0, 1.2, 1.1),
    TG    = c(1.3, 1.5, 1.4),
    BMI   = c(24, 26, 25)
  )
  out <- glycemic_markers(df, verbose = FALSE)
  expect_equal(names(out)[1L], "id")
  expect_equal(out$id, 1:3)
})

test_that("BMI pre-computed from weight and height when BMI absent", {
  skip_on_cran()
  # Without pre-computation this would error (BMI missing)
  df <- tibble::tibble(
    HDL_c  = 1.0,
    TG     = 1.3,
    weight = 70,    # kg
    height = 175    # cm -> should be converted to m
  )
  out <- glycemic_markers(df, verbose = FALSE)
  expected_bmi <- 70 / (175 / 100)^2
  expected_spise <- 600 * 1.0^0.185 / (1.3^0.2 * expected_bmi^1.338)
  expect_equal(out$SPISE, expected_spise, tolerance = 1e-6)
})

test_that("glucose derived from G0 when glucose absent", {
  skip_on_cran()
  df <- tibble::tibble(
    HDL_c = 1.2,   # avoid log(HDL_c) == 0
    TG    = 1.3,
    BMI   = 24,
    G0    = 5.5    # will alias to glucose
  )
  out <- glycemic_markers(df, verbose = FALSE)
  # METS_IR and TyG_index require glucose; should be non-NA via alias
  expect_false(is.na(out$METS_IR))
  expect_false(is.na(out$TyG_index))
})

test_that("glucose derived from mapped G0 column via col_map redirect", {
  skip_on_cran()
  # Physical column is "pglu0" — col_map maps G0 -> "pglu0".
  # .hm_build_col_map materializes data[["G0"]], then precompute derives glucose.
  # METS_IR and TyG_index should be non-NA.
  df <- tibble::tibble(
    HDL_c = 1.2,
    TG    = 1.3,
    BMI   = 24,
    pglu0 = 5.5    # non-standard name for fasting glucose
  )
  cm  <- list(HDL_c = "HDL_c", TG = "TG", BMI = "BMI", G0 = "pglu0")
  out <- glycemic_markers(df, col_map = cm, verbose = FALSE)
  expect_false(is.na(out$METS_IR),    info = "METS_IR requires glucose derived from mapped G0")
  expect_false(is.na(out$TyG_index),  info = "TyG_index requires glucose derived from mapped G0")
})

test_that("BMI derived from mapped weight/height with non-standard column names", {
  skip_on_cran()
  # Physical columns are "wt_kg" and "ht_cm" — must be resolved via col_map.
  # .hm_build_col_map materializes data[["weight"]] and data[["height"]],
  # then .hm_precompute_from_deps derives BMI and SPISE computes.
  df <- tibble::tibble(
    hdlc   = 1.0,
    trig   = 1.3,
    wt_kg  = 70,
    ht_cm  = 175
  )
  cm  <- list(HDL_c = "hdlc", TG = "trig", weight = "wt_kg", height = "ht_cm")
  out <- glycemic_markers(df, col_map = cm, verbose = FALSE)
  expected_bmi   <- 70 / (175 / 100)^2
  expected_spise <- 600 * 1.0^0.185 / (1.3^0.2 * expected_bmi^1.338)
  expect_equal(out$SPISE, expected_spise, tolerance = 1e-6,
               info = "SPISE should use BMI precomputed from mapped weight/height")
})

test_that("partial col_map is filled from dictionary for remaining keys", {
  skip_on_cran()
  # User provides only G0 mapping; HDL_c, TG, BMI should be inferred from
  # dictionary matching column names (pglu0 -> G0, hdlc -> HDL_c, etc.).
  df <- tibble::tibble(
    hdlc  = 1.2,
    trig  = 1.3,
    bmi   = 24,
    pglu0 = 5.5
  )
  cm  <- list(G0 = "pglu0")  # partial — only G0 explicitly provided
  out <- glycemic_markers(df, col_map = cm, verbose = FALSE)
  expect_false(is.na(out$SPISE),     info = "SPISE should compute: HDL_c/TG/BMI inferred by dict")
  expect_false(is.na(out$METS_IR),   info = "METS_IR should compute: glucose derived from G0")
  expect_false(is.na(out$TyG_index), info = "TyG_index should compute: glucose derived from G0")
})

