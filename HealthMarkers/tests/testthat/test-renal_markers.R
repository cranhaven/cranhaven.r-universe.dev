library(testthat)
library(tibble)

# Minimal required mapping and a helper to build small test data frames
cm_req <- list(
  creatinine = "Cr",
  age        = "Age",
  sex        = "Sex",
  race       = "Race",
  BUN        = "BUN"
)

make_df_req <- function(Cr = 1.0, Age = 40, Sex = 1, Race = "white", BUN = 14) {
  tibble(Cr = Cr, Age = Age, Sex = Sex, Race = Race, BUN = BUN)
}

test_that("verbose emits col_map, optional inputs, computing markers, and results messages", {
  skip_on_cran()
  df <- make_df_req()
  msgs <- testthat::capture_messages(renal_markers(df, cm_req, verbose = TRUE))
  expect_true(any(grepl("col_map", msgs)))
  expect_true(any(grepl("optional inputs", msgs)))
  expect_true(any(grepl("computing markers", msgs)))
  expect_true(any(grepl("results:", msgs)))
})

test_that("verbose double-fire guard", {
  skip_on_cran()
  df <- make_df_req()
  msgs <- testthat::capture_messages(renal_markers(df, cm_req, verbose = TRUE))
  expect_gte(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",        msgs)), 1L)
  expect_equal(sum(grepl("optional inputs", msgs)), 1L)
  expect_equal(sum(grepl("computing markers", msgs)), 1L)
})

test_that("error when mapped column not found in data", {
  skip_on_cran()
  df <- tibble(Age = 40, Sex = 1, Race = "white", BUN = 14) # missing Cr
  expect_error(
    renal_markers(df, cm_req, verbose = FALSE),
    "not found"
  )
})

test_that("na_action='omit' dropping all rows returns empty tibble with expected columns", {
  skip_on_cran()
  df <- make_df_req(Cr = NA_real_)
  out <- renal_markers(df, cm_req, na_action = "omit", verbose = FALSE)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0L)
  expect_true(all(c(
    "eGFR_cr","eGFR_cys","eGFR_combined","BUN_Cr_ratio","FE_Urea",
    "NGAL","KIM1","NAG","Beta2Micro","IL18","L_FABP"
  ) %in% names(out)))
})

test_that("sex mapping: numeric vs string agree; male vs female differ", {
  skip_on_cran()
  df_code <- make_df_req(Cr = 1.0, Age = 50, Sex = 1, Race = "white", BUN = 14)
  df_strm <- make_df_req(Cr = 1.0, Age = 50, Sex = "male", Race = "white", BUN = 14)
  out_code <- renal_markers(df_code, cm_req, verbose = FALSE)
  out_strm <- renal_markers(df_strm, cm_req, verbose = FALSE)
  expect_equal(out_code$eGFR_cr, out_strm$eGFR_cr, tolerance = 1e-8)

  df_f_code <- make_df_req(Cr = 1.0, Age = 50, Sex = 0, Race = "white", BUN = 14)
  df_f_str  <- make_df_req(Cr = 1.0, Age = 50, Sex = "female", Race = "white", BUN = 14)
  out_f_code <- renal_markers(df_f_code, cm_req, verbose = FALSE)
  out_f_str  <- renal_markers(df_f_str, cm_req, verbose = FALSE)
  expect_equal(out_f_code$eGFR_cr, out_f_str$eGFR_cr, tolerance = 1e-8)

  expect_false(isTRUE(all.equal(out_code$eGFR_cr, out_f_code$eGFR_cr)))
})

test_that("race mapping: black vs white scales eGFR_cr by ~1.159 (all else equal)", {
  skip_on_cran()
  df_w <- make_df_req(Cr = 1.0, Age = 50, Sex = 1, Race = "caucasian", BUN = 14)
  df_b <- make_df_req(Cr = 1.0, Age = 50, Sex = 1, Race = "african american", BUN = 14)
  out_w <- renal_markers(df_w, cm_req, verbose = FALSE)
  out_b <- renal_markers(df_b, cm_req, verbose = FALSE)
  expect_gt(out_b$eGFR_cr, 0)
  expect_gt(out_w$eGFR_cr, 0)
  expect_equal(unname(out_b$eGFR_cr / out_w$eGFR_cr), 1.159, tolerance = 1e-6)
})

test_that("extreme values produce no warning/error; range note appears in verbose", {
  skip_on_cran()
  df_ext <- make_df_req(Cr = 10, Age = 90, Sex = 1, Race = "white", BUN = 300)
  expect_no_warning(
    out <- renal_markers(df_ext, cm_req, verbose = FALSE)
  )
  expect_true(is.numeric(out$eGFR_cr))
  # Range note appears in verbose
  msgs <- testthat::capture_messages(renal_markers(df_ext, cm_req, verbose = TRUE))
  expect_true(any(grepl("range note", msgs)))
})

test_that("FE_Urea zero denominators yield consolidated warning and NA", {
  skip_on_cran()
  df1 <- tibble(Cr = 1.0, Age = 40, Sex = 1, Race = "white", BUN = 14,
                U_s = 0, Cr_u = 2, U_u = 80)
  cm1 <- modifyList(cm_req, list(urea_serum = "U_s", creatinine_urine = "Cr_u", urea_urine = "U_u"))
  expect_warning(
    out1 <- renal_markers(df1, cm1, verbose = FALSE),
    "zero denominators detected"
  )
  expect_true(is.na(out1$FE_Urea))

  df2 <- tibble(Cr = 1.0, Age = 40, Sex = 1, Race = "white", BUN = 14,
                U_s = 40, Cr_u = 0, U_u = 80)
  expect_warning(
    out2 <- renal_markers(df2, cm1, verbose = FALSE),
    "zero denominators detected"
  )
  expect_true(is.na(out2$FE_Urea))
})

test_that("ID column is prepended to output when detected", {
  skip_on_cran()
  df_id <- tibble::tibble(
    id = 1:3,
    Cr = c(1.0, 1.2, 0.9), Age = c(40, 50, 35), Sex = 1L, Race = "white", BUN = 14
  )
  out <- renal_markers(df_id, cm_req, verbose = FALSE)
  expect_equal(names(out)[1L], "id")
  expect_equal(out$id, 1:3)
})
