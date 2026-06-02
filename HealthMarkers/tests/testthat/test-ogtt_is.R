library(testthat)
library(tibble)
library(dplyr)

# helper to run ogtt_is more concisely
run_ogtt <- function(df, normalize = "none", verbose = FALSE) {
  ogtt_is(
    df,
    col_map = list(
      G0     = "G0",     I0      = "I0",
      G30    = "G30",    I30     = "I30",
      G120   = "G120",   I120    = "I120",
      weight = "weight", bmi     = "bmi",
      age    = "age",    sex     = "sex"
    ),
    normalize = normalize,
    verbose = verbose
  )
}

# base single-row dataset
base_df <- tibble(
  G0     = 5.5,
  I0     = 60,
  G30    = 7.8,
  I30    = 90,
  G120   = 6.2,
  I120   = 50,
  weight = 70,
  bmi    = 24,
  age    = 30,
  sex    = 1
)

test_that("returns all expected indices for single row", {
  skip_on_cran()
  out <- run_ogtt(base_df, normalize = "none")
  expected_cols <- c(
    "Isi_120", "Cederholm_index", "Gutt_index",
    "Avignon_Si0", "Avignon_Si120", "Avignon_Sim",
    "Modified_stumvoll", "Stumvoll_Demographics",
    "Matsuda_AUC", "Matsuda_ISI",
    "BigttSi", "Ifc_inv", "HIRI_inv", "Belfiore_isi_gly"
  )
  expect_true(all(expected_cols %in% names(out)))
  expect_true(all(vapply(out, function(x) is.finite(x[1]), logical(1))))
})

test_that("vectorized input: two rows gives two outputs", {
  skip_on_cran()
  df2 <- dplyr::bind_rows(base_df, dplyr::mutate(base_df, G0 = 6))
  out2 <- run_ogtt(df2)
  expect_equal(nrow(out2), 2)
})

test_that("normalize = 'range' scales variable indices into [0,1], constants map to a bound", {
  skip_on_cran()
  df2 <- dplyr::bind_rows(base_df, dplyr::mutate(base_df, G0 = 8))
  out_r <- suppressWarnings(run_ogtt(df2, normalize = "range"))
  for (col in names(out_r)) {
    vals <- out_r[[col]]
    v2 <- vals[!is.na(vals)]
    if (length(unique(v2)) > 1) {
      expect_gte(min(v2), 0 - 1e-12)
      expect_lte(max(v2), 1 + 1e-12)
    } else {
      expect_true(all(is.finite(vals)))
      expect_true(all(vals >= -1e-12 & vals <= 1 + 1e-12))
    }
  }
})

test_that("normalize = 'z' gives mean ~ 0 and sd ~ 1 on variable indices; constants -> zeros", {
  skip_on_cran()
  df3 <- dplyr::bind_rows(
    base_df,
    dplyr::mutate(base_df, G0 = 8),
    dplyr::mutate(base_df, G0 = 10)
  )
  out_z <- suppressWarnings(run_ogtt(df3, normalize = "z"))
  for (col in names(out_z)) {
    vals <- out_z[[col]]
    v2 <- vals[!is.na(vals)]
    if (length(unique(v2)) > 1) {
      expect_equal(mean(v2), 0, tolerance = 1e-6)
      expect_equal(sd(v2), 1, tolerance = 1e-6)
    } else {
      expect_true(all(abs(vals) < 1e-10 | is.na(vals)))
    }
  }
})

test_that("normalize = 'inverse' and 'robust' run and return a tibble", {
  skip_on_cran()
  df2 <- dplyr::bind_rows(base_df, dplyr::mutate(base_df, I0 = 30))
  out_inv <- suppressWarnings(run_ogtt(df2, normalize = "inverse"))
  out_rob <- suppressWarnings(run_ogtt(df2, normalize = "robust"))
  expect_s3_class(out_inv, "tbl_df")
  expect_s3_class(out_rob, "tbl_df")
})

test_that("invalid normalize argument errors via ogtt_is arg check", {
  skip_on_cran()
  expect_error(
    run_ogtt(base_df, normalize = "foo"),
    "`normalize` must be one of"
  )
})

test_that("verbose emits preparing, column map, and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  expect_message(run_ogtt(base_df, verbose = TRUE), "ogtt_is")
  expect_message(run_ogtt(base_df, verbose = TRUE), "col_map")
  expect_message(run_ogtt(base_df, verbose = TRUE), "results:")
})

test_that("verbose double-fire guard", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  msgs <- testthat::capture_messages(run_ogtt(base_df, verbose = TRUE))
  expect_equal(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})

test_that("partial col_map is supplemented by dictionary inference", {
  skip_on_cran()
  bad_map <- list(G0 = "G0", I0 = "I0") # too few entries, rest inferred
  expect_no_error(
    ogtt_is(base_df, col_map = bad_map)
  )
})

test_that("BigttSi accounts for sex = 2 differently", {
  skip_on_cran()
  out_m <- run_ogtt(dplyr::mutate(base_df, sex = 1))
  out_f <- run_ogtt(dplyr::mutate(base_df, sex = 2))
  expect_false(isTRUE(all.equal(out_m$BigttSi, out_f$BigttSi)))
})
