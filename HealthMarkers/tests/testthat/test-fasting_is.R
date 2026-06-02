# tests/testthat/test_fasting_is.R

test_that("fasting_is errors if missing required columns", {
  skip_on_cran()
  df <- tibble::tibble(G0 = 5.5)
  expect_error(
    fasting_is(df, col_map = list(G0 = "G0", I0 = "I0"))
  )
})

test_that("fasting_is returns 10 indices and computes HOMA_IR_inv correctly", {
  skip_on_cran()
  df <- tibble::tibble(G0 = 5.5, I0 = 60)
  out <- fasting_is(
    df,
    col_map   = list(G0 = "G0", I0 = "I0"),
    normalize = "none",
    verbose   = FALSE
  )

  expected_names <- c(
    "Fasting_inv", "Raynaud", "HOMA_IR_inv", "FIRI", "QUICKI",
    "Belfiore_basal", "Ig_ratio_basal", "Isi_basal",
    "Bennett", "HOMA_IR_rev_inv"
  )
  expect_named(out, expected_names, ignore.order = TRUE)
  expect_equal(ncol(out), length(expected_names))

  # manual HOMA_IR_inv = -((G0*18)*(I0/6)) / 22.5
  expected_homa <- -((5.5 * 18) * (60 / 6)) / 22.5
  expect_equal(out$HOMA_IR_inv, expected_homa)
})

test_that("fasting_is is vectorized over multiple rows", {
  skip_on_cran()
  df <- tibble::tibble(
    G0 = c(5.5, 6),
    I0 = c(60, 80)
  )
  out <- fasting_is(df, col_map = list(G0 = "G0", I0 = "I0"))
  expect_equal(nrow(out), 2)
})

test_that("normalize = 'range' and 'z' behave correctly", {
  skip_on_cran()
  df2 <- tibble::tibble(G0 = c(5.5, 6), I0 = c(60, 80))

  out_r <- fasting_is(df2, col_map = list(G0 = "G0", I0 = "I0"), normalize = "range")
  for (col in names(out_r)) {
    vals <- out_r[[col]]
    v2 <- vals[!is.na(vals)]
    if (length(unique(v2)) > 1) {
      expect_equal(min(v2), 0)
      expect_equal(max(v2), 1)
    } else {
      expect_true(all(is.na(vals)))
    }
  }

  df3 <- tibble::tibble(
    G0 = c(5.5, 6, 7),
    I0 = c(60, 80, 100)
  )
  out_z <- fasting_is(df3, col_map = list(G0 = "G0", I0 = "I0"), normalize = "z")
  for (col in names(out_z)) {
    vals <- out_z[[col]]
    v2 <- vals[!is.na(vals)]
    if (length(unique(v2)) > 1) {
      expect_equal(mean(v2), 0, tolerance = 1e-6)
      expect_equal(sd(v2), 1, tolerance = 1e-6)
    } else {
      expect_true(all(is.na(vals)))
    }
  }
})

test_that("normalize = 'inverse' and 'robust' run silently", {
  skip_on_cran()
  df <- tibble::tibble(G0 = c(5.5, 6), I0 = c(60, 80))
  expect_silent(fasting_is(df, col_map = list(G0 = "G0", I0 = "I0"), normalize = "inverse", verbose = FALSE))
  expect_silent(fasting_is(df, col_map = list(G0 = "G0", I0 = "I0"), normalize = "robust",  verbose = FALSE))
})

test_that("invalid normalize argument errors", {
  skip_on_cran()
  df <- tibble::tibble(G0 = 5.5, I0 = 60)
  expect_error(
    fasting_is(df, col_map = list(G0 = "G0", I0 = "I0"), normalize = "foo"),
    "`normalize` must be one of"
  )
})

test_that("verbose = TRUE emits preparing, column map, and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- tibble::tibble(G0 = 5.5, I0 = 60)
  cm <- list(G0 = "G0", I0 = "I0")
  expect_message(fasting_is(df, col_map = cm, verbose = TRUE), "fasting_is")
  expect_message(fasting_is(df, col_map = cm, verbose = TRUE), "col_map")
  expect_message(fasting_is(df, col_map = cm, verbose = TRUE), "results:")
})

test_that("verbose double-fire guard: each message fires exactly once", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df   <- tibble::tibble(G0 = 5.5, I0 = 60)
  cm   <- list(G0 = "G0", I0 = "I0")
  msgs <- testthat::capture_messages(fasting_is(df, col_map = cm, verbose = TRUE))
  expect_equal(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})
