# tests/testthat/test_urine_markers.R
library(testthat)
library(tibble)

test_that("errors if any required column is missing", {
  skip_on_cran()
  # Missing urine_albumin
  df1 <- tibble(
    urine_creatinine = 1
  )
  expect_error(
    urine_markers(df1),
    "missing columns: urine_albumin"
  )

  # Missing urine_creatinine
  df2 <- tibble(
    urine_albumin = 30
  )
  expect_error(
    urine_markers(df2),
    "missing columns: urine_creatinine"
  )
})

test_that("computes UACR, UPCR, Na/K ratio, and normalized tubular markers; names match", {
  skip_on_cran()
  df <- tibble(
    urine_albumin    = 30,    # mg/L
    urine_creatinine = 2,     # mg/dL
    urine_protein    = 150,   # mg/L
    urine_Na         = 60,    # mmol/L
    urine_K          = 20,    # mmol/L
    NGAL             = 100,   # mg/L
    KIM1             = 10,
    NAG              = 5,
    beta2_micro      = 2,
    a1_micro         = 3,
    IL18             = 1,
    L_FABP           = 0.5
  )
  out <- urine_markers(df)

  expect_named(
    out,
    c("UACR", "albuminuria_stage", "microalbuminuria",
      "UPCR", "U_Na_K_ratio",
      "NGAL_per_gCr", "KIM1_per_gCr", "NAG_per_gCr",
      "Beta2Micro_per_gCr", "A1Micro_per_gCr", "IL18_per_gCr", "L_FABP_per_gCr")
  )

  # UACR = albumin(mg/L) * 100 / creatinine(mg/dL) = 30 * 100 / 2 = 1500 mg/g
  expect_equal(out$UACR, 1500)

  # UPCR = urine_protein / (urine_creatinine*0.01) = 150/0.02 = 7500
  expect_equal(out$UPCR, 150 / (2 * 0.01))

  # Na/K = 60/20 = 3
  expect_equal(out$U_Na_K_ratio, 60 / 20)

  # Creatinine-normalized markers per g creatinine: denom gCr_den = 2*0.01 = 0.02 g/L
  gCr_den <- 2 * 0.01
  expect_equal(out$NGAL_per_gCr,       100 / gCr_den)
  expect_equal(out$KIM1_per_gCr,        10 / gCr_den)
  expect_equal(out$NAG_per_gCr,          5 / gCr_den)
  expect_equal(out$Beta2Micro_per_gCr,   2 / gCr_den)
  expect_equal(out$A1Micro_per_gCr,      3 / gCr_den)
  expect_equal(out$IL18_per_gCr,         1 / gCr_den)
  expect_equal(out$L_FABP_per_gCr,     0.5 / gCr_den)
})

test_that("albuminuria_stage and microalbuminuria factors have correct levels and values", {
  skip_on_cran()
  # UACR = albumin * 100 / creatinine (mg/L and mg/dL respectively)
  # Row1: 100*1/100  =  1 mg/g -> A1 (< 30)
  # Row2: 100*30/100 = 30 mg/g -> A2 (30-300), micro
  # Row3: 100*400/1  = 40000 mg/g -> A3 (> 300)
  df <- tibble(
    urine_albumin    = c(1, 30, 400),
    urine_creatinine = c(100, 100, 1),
    # optional fields omitted
  )
  out <- urine_markers(df)

  expect_true(is.factor(out$albuminuria_stage))
  expect_equal(levels(out$albuminuria_stage), c("A1","A2","A3"))
  expect_equal(as.character(out$albuminuria_stage), c("A1","A2","A3"))

  expect_true(is.factor(out$microalbuminuria))
  expect_equal(levels(out$microalbuminuria), c("normal","micro"))
  # UACR=1 -> normal; UACR=30 -> micro; UACR=40000 -> normal (> 300, macroalbuminuria not flagged as micro)
  expect_equal(as.character(out$microalbuminuria), c("normal","micro","normal"))
})

test_that("UPCR and U_Na_K_ratio are NA when inputs are missing", {
  skip_on_cran()
  df <- tibble(
    urine_albumin    = 30,
    urine_creatinine = 2
    # no urine_protein, no urine_Na/urine_K
  )
  out <- urine_markers(df)
  expect_true(is.na(out$UPCR))
  expect_true(is.na(out$U_Na_K_ratio))
})

test_that("is vectorized over multiple rows", {
  skip_on_cran()
  df <- tibble(
    urine_albumin    = c(30, 40),
    urine_creatinine = c(2, 4),
    urine_protein    = c(150, NA_real_)
  )
  out <- urine_markers(df)
  expect_equal(nrow(out), 2)
  # second row UPCR NA
  expect_true(is.na(out$UPCR[2]))
})

test_that("na_action policies: error and omit behave as expected", {
  skip_on_cran()
  df_na <- tibble(
    urine_albumin    = c(30, NA_real_),
    urine_creatinine = c(2, 2)
  )
  # error -> abort (suppress high-missingness warning)
  expect_error(
    suppressWarnings(urine_markers(df_na, na_action = "error")),
    "required inputs contain missing values"
  )
  # omit -> drops NA row
  out_omit <- suppressWarnings(urine_markers(df_na, na_action = "omit"))
  expect_equal(nrow(out_omit), 1L)
})

test_that("extreme inputs pass through without error (check_extreme removed)", {
  skip_on_cran()
  df_ext <- tibble(
    urine_albumin    = 1e6,
    urine_creatinine = 1e-6,
    urine_protein    = 1e6
  )
  out <- suppressWarnings(urine_markers(df_ext))
  expect_s3_class(out, "tbl_df")
  expect_true("UACR" %in% names(out))
})

test_that("zero denominators emit a consolidated warning and yield NA in ratios", {
  skip_on_cran()
  df_zero <- tibble(
    urine_albumin    = 30,
    urine_creatinine = 0,     # zero -> denominator for all per-gCr ratios
    urine_protein    = 150,
    urine_Na         = 60,
    urine_K          = 20
  )
  expect_warning(
    out_zero <- urine_markers(df_zero),
    "zero denominators detected"
  )
  expect_true(is.na(out_zero$UACR))
  expect_true(is.na(out_zero$UPCR))
  expect_true(is.na(out_zero$NGAL_per_gCr))
  expect_equal(out_zero$U_Na_K_ratio, 60 / 20)  # unaffected (denominator is K, not creatinine)
})

test_that("verbose emits preparing, column map, and results messages", {
  skip_on_cran()
  df <- tibble(
    urine_albumin    = 30,
    urine_creatinine = 2
  )
  withr::local_options(healthmarkers.verbose = "inform")
  expect_message(urine_markers(df, verbose = TRUE), "urine_markers")
  expect_message(urine_markers(df, verbose = TRUE), "col_map")
  expect_message(urine_markers(df, verbose = TRUE), "results:")
})

test_that("verbose double-fire guard", {
  skip_on_cran()
  df <- tibble(
    urine_albumin    = 30,
    urine_creatinine = 2
  )
  withr::local_options(healthmarkers.verbose = "inform")
  msgs <- testthat::capture_messages(urine_markers(df, verbose = TRUE))
  expect_equal(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})
