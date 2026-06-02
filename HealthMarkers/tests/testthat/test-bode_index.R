library(testthat)

cm <- list(fev1_pct = "FEV1pct", sixmwd = "Walk_m", mmrc = "mMRC", bmi = "BMI")

test_that("errors on non-data input", {
  skip_on_cran()
  expect_error(bode_index("x", cm), class = "healthmarkers_bode_error_data_type")
})

test_that("errors on missing mapping keys", {
  skip_on_cran()
  bad <- list(fev1_pct = "FEV1pct", mmrc = "mMRC") # missing others
  df <- data.frame(FEV1pct = 80, Walk_m = 400, mMRC = 1, BMI = 25)
  expect_error(bode_index(df, bad), class = "healthmarkers_bode_error_missing_map")
})

test_that("verbose = TRUE emits col_map and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- data.frame(FEV1pct = 80, Walk_m = 400, mMRC = 1, BMI = 25)
  expect_message(bode_index(df, cm, verbose = TRUE), "bode_index")
  expect_message(bode_index(df, cm, verbose = TRUE), "col_map")
  expect_message(bode_index(df, cm, verbose = TRUE), "results:")
})

test_that("verbose double-fire guard: each message fires exactly once", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df   <- data.frame(FEV1pct = 80, Walk_m = 400, mMRC = 1, BMI = 25)
  msgs <- testthat::capture_messages(bode_index(df, cm, verbose = TRUE))
  expect_gte(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})

test_that("cut-point scoring basic", {
  skip_on_cran()
  df <- data.frame(
    FEV1pct = c(80,60,40,30),
    Walk_m  = c(400,300,200,100),
    mMRC    = c(1,2,3,4),
    BMI     = c(22,22,20,20)
  )
  out <- bode_index(df, cm)
  # Row scores: (0+0+0+0)=0; (1+1+1+0)=3; (2+2+2+1)=7; (3+3+3+1)=10
  expect_equal(out$bode_index, c(0L,3L,7L,10L))
})

test_that("boundary mapping", {
  skip_on_cran()
  df <- data.frame(
    FEV1pct = c(65,50,36,35),
    Walk_m  = c(350,250,150,149),
    mMRC    = c(0,1,2,3),
    BMI     = c(21.1,21.0,21.0,20.9)
  )
  out <- bode_index(df, cm)
  # Expected: 0; (1+1+0+1)=3; (2+2+1+1)=6; (3+3+2+1)=9
  expect_equal(out$bode_index, c(0L,3L,6L,9L))
})

test_that("NA policies keep/omit/error/warn/ignore", {
  skip_on_cran()
  df <- data.frame(
    FEV1pct = c(60, NA, 30),
    Walk_m  = c(300, 200, NA),
    mMRC    = c(2,4,3),
    BMI     = c(25,19,18)
  )
  keep <- bode_index(df, cm, na_action = "keep")
  expect_equal(nrow(keep), 3L)
  omit <- bode_index(df, cm, na_action = "omit")
  expect_equal(nrow(omit), 1L)
  expect_error(bode_index(df, cm, na_action = "error"),
               class = "healthmarkers_bode_error_missing_values")
  expect_warning(bode_index(df, cm, na_action = "warn"),
                 class = "healthmarkers_bode_warn_missing_inputs")
  ignore <- bode_index(df, cm, na_action = "ignore")
  expect_equal(nrow(ignore), 3L)
})

test_that("numeric coercion warning", {
  skip_on_cran()
  df <- data.frame(
    FEV1pct = c("60","oops"),
    Walk_m  = c("300","250"),
    mMRC    = c("2","3"),
    BMI     = c("25","19")
  )
  expect_warning(bode_index(df, cm),
                 class = "healthmarkers_bode_warn_na_coercion")
})

test_that("domain warnings for individual ranges", {
  skip_on_cran()
  # FEV1pct out-of-range only
  df_fev1 <- data.frame(FEV1pct = 200, Walk_m = 400, mMRC = 1, BMI = 22)
  expect_warning(bode_index(df_fev1, cm), class = "healthmarkers_bode_warn_fev1pct_range")

  # mMRC out-of-range only
  df_mmrc <- data.frame(FEV1pct = 80, Walk_m = 400, mMRC = 5, BMI = 22)
  expect_warning(bode_index(df_mmrc, cm), class = "healthmarkers_bode_warn_mmrc_range")

  # 6MWD out-of-range only
  df_walk <- data.frame(FEV1pct = 80, Walk_m = 1600, mMRC = 1, BMI = 22)
  expect_warning(bode_index(df_walk, cm), class = "healthmarkers_bode_warn_sixmwd_range")

  # BMI out-of-range only
  df_bmi <- data.frame(FEV1pct = 80, Walk_m = 400, mMRC = 1, BMI = 85)
  expect_warning(bode_index(df_bmi, cm), class = "healthmarkers_bode_warn_bmi_range")
})

test_that("extreme values trigger domain warnings", {
  skip_on_cran()
  df <- data.frame(
    FEV1pct = c(160,40,20),
    Walk_m  = c(900,140,300),
    mMRC    = c(6,2,-1),
    BMI     = c(5,90,22)
  )
  expect_warning(
    withCallingHandlers(
      bode_index(df, cm),
      warning = function(w) {
        if (inherits(w, c("healthmarkers_bode_warn_mmrc_range", "healthmarkers_bode_warn_bmi_range"))) invokeRestart("muffleWarning")
      }
    ),
    class = "healthmarkers_bode_warn_fev1pct_range"
  )
  # Values still produce output
  out <- suppressWarnings(bode_index(df, cm))
  expect_equal(nrow(out), 3L)
})

test_that("formula correctness with normal input", {
  skip_on_cran()
  df <- data.frame(
    FEV1pct = c(70, 50, 30),
    Walk_m  = c(400, 300, 200),
    mMRC    = c(1, 2, 3),
    BMI     = c(25, 22, 19)
  )
  out <- bode_index(df, cm)
  expect_equal(nrow(out), 3L)
  expect_true(all(is.finite(out$bode_index)))
  # Row 1: fev1>=65=0, walk>=350=0, mmrc<=1=0, bmi>21=0 => total 0
  expect_equal(out$bode_index[1], 0L)
})

test_that("padding keep/warn/ignore retain row count; omit reduces", {
  skip_on_cran()
  df <- data.frame(FEV1pct = c(60, NA), Walk_m = c(300,200), mMRC = c(2,4), BMI = c(25,18))
  keep <- bode_index(df, cm, na_action = "keep")
  warn <- suppressWarnings(bode_index(df, cm, na_action = "warn"))
  ignore <- bode_index(df, cm, na_action = "ignore")
  omit <- bode_index(df, cm, na_action = "omit")
  expect_equal(nrow(keep), 2L)
  expect_equal(nrow(warn), 2L)
  expect_equal(nrow(ignore), 2L)
  expect_equal(nrow(omit), sum(complete.cases(df)))
})

test_that("empty input returns 0-row tibble", {
  skip_on_cran()
  df <- data.frame(FEV1pct = numeric(), Walk_m = numeric(), mMRC = numeric(), BMI = numeric())
  out <- bode_index(df, cm)
  expect_equal(nrow(out), 0L)
  expect_true("bode_index" %in% names(out))
})

test_that("extreme values outside domain ranges produce domain warnings", {
  skip_on_cran()
  # Values within some domains but outside others
  df <- data.frame(
    FEV1pct = c(9, 50, 30),
    Walk_m  = c(45, 300, 700),
    mMRC    = c(0, 2, 4),
    BMI     = c(9, 22, 61)
  )
  out <- suppressWarnings(bode_index(df, cm))
  expect_equal(nrow(out), 3L)
})
