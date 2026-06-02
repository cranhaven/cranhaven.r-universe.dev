library(testthat)
library(tibble)

test_that("kidney_failure_risk returns a tibble with 2 risk estimates between 0 and 1", {
  df <- tibble(
    age  = 60,
    sex  = 1,
    eGFR = 45,
    UACR = 300
  )

  out <- kidney_failure_risk(
    data = df,
    col_map = list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR")
  )

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("KFRE_2yr", "KFRE_5yr"))
  expect_true(all(is.numeric(out$KFRE_2yr)))
  expect_true(all(is.numeric(out$KFRE_5yr)))
  expect_true(all(out$KFRE_2yr >= 0 & out$KFRE_2yr <= 1))
  expect_true(all(out$KFRE_5yr >= 0 & out$KFRE_5yr <= 1))
})

test_that("kidney_failure_risk errors if mapped columns are missing in data", {
  skip_on_cran()
  df_bad <- tibble(
    age  = 60,
    sex  = 1,
    eGFR = 45
    # UACR missing
  )

  expect_error(
    kidney_failure_risk(
      data = df_bad,
      col_map = list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR")
    ),
    "missing required columns: UACR"
  )
})

test_that("kidney_failure_risk errors if required col_map entries are missing", {
  skip_on_cran()
  df <- tibble(
    age  = 60,
    sex  = 1,
    eGFR = 45,
    UACR = 300
  )
  # drop UACR key from col_map - kidney_kfre requires explicit mapping (no inference)
  cm <- list(age = "age", sex = "sex", eGFR = "eGFR")
  expect_error(
    kidney_failure_risk(data = df, col_map = cm),
    "UACR"
  )
})

test_that("kidney_failure_risk respects a custom col_map", {
  skip_on_cran()
  df2 <- tibble(A = 70, SEX = 2, GFR = 90, PROT = 20)

  out2 <- kidney_failure_risk(
    data = df2,
    col_map = list(age = "A", sex = "SEX", eGFR = "GFR", UACR = "PROT")
  )

  expect_named(out2, c("KFRE_2yr", "KFRE_5yr"))
  expect_true(all(out2$KFRE_2yr >= 0 & out2$KFRE_2yr <= 1))
  expect_true(all(out2$KFRE_5yr >= 0 & out2$KFRE_5yr <= 1))
})

test_that("na_action='error' aborts when required inputs contain NA", {
  skip_on_cran()
  df <- tibble(age = 60, sex = 1, eGFR = 45, UACR = NA_real_)
  cm <- list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR")
  expect_error(
    suppressWarnings(kidney_failure_risk(df, col_map = cm, na_action = "error")),
    "required inputs contain missing values"
  )
})

test_that("na_action='omit' drops rows with NA", {
  skip_on_cran()
  df <- tibble(
    age = c(60, 70),
    sex = c(1, 2),
    eGFR = c(45, NA_real_),
    UACR = c(300, 100)
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  out <- suppressWarnings(kidney_failure_risk(df, col_map = cm, na_action = "omit"))
  expect_equal(nrow(out), 1L)
  expect_s3_class(out, "tbl_df")
})

test_that("invalid sex coding errors", {
  skip_on_cran()
  df <- tibble(age = 60, sex = 3, eGFR = 45, UACR = 300)
  cm <- list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR")
  expect_error(
    kidney_failure_risk(df, col_map = cm),
    "sex' must be coded as 1=male or 2=female"
  )
})

test_that("extreme input values pass through without error", {
  skip_on_cran()
  df <- tibble(age = 10, sex = 1, eGFR = 0.5, UACR = 20000)
  cm <- list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR")
  out <- suppressWarnings(kidney_failure_risk(df, col_map = cm))
  expect_true(is.numeric(out$KFRE_2yr))
  expect_true(is.numeric(out$KFRE_5yr))
})

test_that("verbose emits col_map and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- tibble::tibble(age = 60, sex = 1, eGFR = 45, UACR = 300)
  cm <- list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR")
  expect_message(kidney_failure_risk(df, col_map = cm, verbose = TRUE), "kidney_failure_risk")
  expect_message(kidney_failure_risk(df, col_map = cm, verbose = TRUE), "col_map")
  expect_message(kidney_failure_risk(df, col_map = cm, verbose = TRUE), "results:")
})

test_that("verbose double-fire guard", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- tibble::tibble(age = 60, sex = 1, eGFR = 45, UACR = 300)
  cm <- list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR")
  msgs <- testthat::capture_messages(
    kidney_failure_risk(df, col_map = cm, verbose = TRUE)
  )
  expect_gte(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})

test_that("non-positive eGFR triggers log warning", {
  skip_on_cran()
  df <- tibble(age = 60, sex = 1, eGFR = 0, UACR = 1)
  cm <- list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR")
  expect_warning(
    kidney_failure_risk(df, col_map = cm),
    "'eGFR'.*log\\(\\) undefined"
  )
})

test_that("non-positive UACR triggers log warning", {
  skip_on_cran()
  df <- tibble(age = 60, sex = 1, eGFR = 1, UACR = 0)
  cm <- list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR")
  expect_warning(
    kidney_failure_risk(df, col_map = cm),
    "'UACR'.*log\\(\\) undefined"
  )
})

# ---- Multi-row vectorization ------------------------------------------------
test_that("kidney_failure_risk handles multi-row data correctly", {
  skip_on_cran()
  df <- tibble::tibble(
    age  = c(45, 60, 75, 80),
    sex  = c(1, 2, 1, 2),
    eGFR = c(90, 45, 22, 10),
    UACR = c(30, 300, 1000, 3000)
  )
  cm <- list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR")
  out <- kidney_failure_risk(df, col_map = cm)
  expect_equal(nrow(out), 4L)
  expect_true(all(out$KFRE_2yr >= 0 & out$KFRE_2yr <= 1))
  expect_true(all(out$KFRE_5yr >= 0 & out$KFRE_5yr <= 1))
  # Lower eGFR / higher UACR should yield higher risk
  expect_gt(out$KFRE_5yr[4], out$KFRE_5yr[1])
})

# ---- String sex encoding ----------------------------------------------------
test_that("kidney_failure_risk accepts string sex codes M/F", {
  skip_on_cran()
  df_str <- tibble::tibble(
    age  = c(60, 70),
    sex  = c("M", "F"),
    eGFR = c(45, 30),
    UACR = c(300, 500)
  )
  df_int <- tibble::tibble(
    age  = c(60, 70),
    sex  = c(1, 2),
    eGFR = c(45, 30),
    UACR = c(300, 500)
  )
  cm <- list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR")
  out_str <- kidney_failure_risk(df_str, col_map = cm)
  out_int <- kidney_failure_risk(df_int, col_map = cm)
  expect_equal(out_str$KFRE_2yr, out_int$KFRE_2yr)
  expect_equal(out_str$KFRE_5yr, out_int$KFRE_5yr)
})

test_that("kidney_failure_risk accepts 'male'/'female' sex coding", {
  skip_on_cran()
  df <- tibble::tibble(
    age  = 60, sex = "male", eGFR = 45, UACR = 300
  )
  df2 <- tibble::tibble(
    age  = 60, sex = 1L, eGFR = 45, UACR = 300
  )
  cm <- list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR")
  out  <- kidney_failure_risk(df,  col_map = cm)
  out2 <- kidney_failure_risk(df2, col_map = cm)
  expect_equal(out$KFRE_2yr, out2$KFRE_2yr)
})
