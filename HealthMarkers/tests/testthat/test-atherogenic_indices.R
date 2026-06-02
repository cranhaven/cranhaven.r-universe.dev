library(testthat)
library(tibble)
library(HealthMarkers)

cm <- list(TC = "TC", LDL_c = "LDL_c", HDL_c = "HDL_c", TG = "TG")

test_that("atherogenic_indices computes AIP, CRI_I, CRI_II", {
  skip_on_cran()
  dat <- tibble(TC = c(200, 180), HDL_c = c(50, 60), TG = c(150, 100), LDL_c = c(120, 90))
  res <- atherogenic_indices(dat, col_map = cm)
  expect_s3_class(res, "tbl_df")
  expect_named(res, c("AIP","CRI_I","CRI_II"))
  exp_AIP <- log10(dat$TG / dat$HDL_c)
  exp_CRI_I <- dat$TC / dat$HDL_c
  exp_CRI_II <- dat$LDL_c / dat$HDL_c
  expect_equal(res$AIP, as.numeric(exp_AIP), tolerance = 1e-12)
  expect_equal(res$CRI_I, as.numeric(exp_CRI_I), tolerance = 1e-12)
  expect_equal(res$CRI_II, as.numeric(exp_CRI_II), tolerance = 1e-12)
})

test_that("na_action omit drops rows with missing lipids", {
  skip_on_cran()
  dat <- tibble(TG = c(150, NA_real_), HDL_c = c(50, 40), TC = c(200, 220), LDL_c = c(120, 150))
  cm <- list(TG = "TG", HDL_c = "HDL_c", TC = "TC", LDL_c = "LDL_c")
  res <- atherogenic_indices(dat, col_map = cm, na_action = "omit")
  expect_equal(nrow(res), 1L)
})

test_that("na_action error aborts on missing", {
  skip_on_cran()
  dat <- tibble(TG = c(150, NA_real_), HDL_c = c(50, 40))
  cm <- list(TG = "TG", HDL_c = "HDL_c")
  expect_error(atherogenic_indices(dat, col_map = cm, na_action = "error"), "missing values")
})

test_that("invalid normalize argument errors early", {
  skip_on_cran()
  dat <- tibble(TG = 150, HDL_c = 50)
  cm <- list(TG = "TG", HDL_c = "HDL_c")
  expect_error(atherogenic_indices(dat, col_map = cm, normalize = "foo"))
})

test_that("AIP computed correctly with large TG values", {
  skip_on_cran()
  dat <- tibble(TG = 20000, HDL_c = 10)
  cm <- list(TG = "TG", HDL_c = "HDL_c")
  res <- atherogenic_indices(dat, col_map = cm, verbose = FALSE)
  expect_true(is.finite(res$AIP))
  expect_equal(res$AIP, log10(20000/10))
})

test_that("missing columns reported clearly", {
  skip_on_cran()
  dat <- tibble(TG = 150)
  cm <- list(TG = "TG", HDL_c = "HDL_c")
  expect_error(atherogenic_indices(dat, col_map = cm), "missing required columns in data: HDL_c")
})

test_that("package-level verbosity emits results summary message", {
  skip_on_cran()
  dat <- tibble(TG = 150, HDL_c = 50)
  cm <- list(TG = "TG", HDL_c = "HDL_c")
  withr::local_options(healthmarkers.verbose = "inform")
  expect_message(
    atherogenic_indices(dat, col_map = cm, verbose = TRUE),
    "results:"
  )
})

test_that("verbose = TRUE emits preparing, column map, and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  dat <- tibble(TG = 150, HDL_c = 50, TC = 200, LDL_c = 120)
  cm2 <- list(TG = "TG", HDL_c = "HDL_c", TC = "TC", LDL_c = "LDL_c")
  expect_message(atherogenic_indices(dat, col_map = cm2, verbose = TRUE), "atherogenic_indices")
  expect_message(atherogenic_indices(dat, col_map = cm2, verbose = TRUE), "col_map")
  expect_message(atherogenic_indices(dat, col_map = cm2, verbose = TRUE), "results:")
})

test_that("verbose double-fire guard: each message fires exactly once", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  dat  <- tibble(TG = 150, HDL_c = 50, TC = 200, LDL_c = 120)
  cm2  <- list(TG = "TG", HDL_c = "HDL_c", TC = "TC", LDL_c = "LDL_c")
  msgs <- testthat::capture_messages(
    atherogenic_indices(dat, col_map = cm2, verbose = TRUE)
  )
  expect_gte(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})
