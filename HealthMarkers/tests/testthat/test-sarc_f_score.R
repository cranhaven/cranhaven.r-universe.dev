library(testthat)

cm <- list(strength="Strength", walking="Walking", chair="Chair", stairs="Stairs", falls="Falls")

test_that("mapping validation and missing columns error", {
  skip_on_cran()
  df <- data.frame(Strength=0, Walking=0, Chair=0, Stairs=0, Falls=0)
  # empty list with all columns in data – succeeds via inference
  expect_no_error(sarc_f_score(df, list()))
  expect_error(
    sarc_f_score(df, list(strength="", walking="Walking", chair="Chair", stairs="Stairs", falls="Falls")),
    class = "healthmarkers_sarcf_error_bad_map_values"
  )
  expect_error(
    sarc_f_score(data.frame(A=1,B=1,C=1,D=1,E=1), cm),
    class = "healthmarkers_sarcf_error_missing_columns"
  )
})

test_that("verbose emits col_map and results messages", {
  skip_on_cran()
  df <- data.frame(Strength=0, Walking=0, Chair=0, Stairs=0, Falls=0)
  withr::local_options(healthmarkers.verbose = "inform")
  expect_message(sarc_f_score(df, cm, verbose = TRUE), "sarc_f_score")
  expect_message(sarc_f_score(df, cm, verbose = TRUE), "col_map")
  expect_message(sarc_f_score(df, cm, verbose = TRUE), "results:")
})

test_that("verbose double-fire guard", {
  skip_on_cran()
  df <- data.frame(Strength=0, Walking=0, Chair=0, Stairs=0, Falls=0)
  withr::local_options(healthmarkers.verbose = "inform")
  msgs <- testthat::capture_messages(sarc_f_score(df, cm, verbose = TRUE))
  expect_gte(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})

test_that("numeric coercion warning when strings introduce NAs", {
  skip_on_cran()
  df <- data.frame(
    Strength=c("0","oops"),
    Walking =c("1","1"),
    Chair   =c("1","1"),
    Stairs  =c("0","0"),
    Falls   =c("2","2")
  )
  expect_warning(sarc_f_score(df, cm), class = "healthmarkers_sarcf_warn_na_coercion")
})

test_that("NA policies: keep, omit, error, warn", {
  skip_on_cran()
  df <- data.frame(
    Strength=c(0, NA, 2),
    Walking =c(1, 1, 2),
    Chair   =c(1, 1, NA),
    Stairs  =c(0, 1, 2),
    Falls   =c(0, 1, 2)
  )

  out_keep <- sarc_f_score(df, cm, na_action = "keep")
  expect_equal(nrow(out_keep), 3L)
  expect_true(is.na(out_keep$sarc_f_score[2]) || is.na(out_keep$sarc_f_score[3]))

  out_omit <- sarc_f_score(df, cm, na_action = "omit")
  expect_equal(nrow(out_omit), 1L)

  expect_error(
    sarc_f_score(df, cm, na_action = "error"),
    class = "healthmarkers_sarcf_error_missing_values"
  )

  expect_warning(
    sarc_f_score(df, cm, na_action = "warn"),
    class = "healthmarkers_sarcf_warn_missing_inputs"
  )
})

test_that("domain warnings for values outside 0–2", {
  skip_on_cran()
  df <- data.frame(
    Strength=c(-1,0),
    Walking =c(3,1),
    Chair   =c(1,1),
    Stairs  =c(0,0),
    Falls   =c(0,0)
  )
  expect_warning(
    sarc_f_score(df, cm),
    class = "healthmarkers_sarcf_warn_out_of_range"
  )
})

test_that("out-of-range items (0-2) trigger domain warning and still compute", {
  skip_on_cran()
  df <- data.frame(
    Strength=c(-1, 0, 3),
    Walking =c(0, 2, 1),
    Chair   =c(1, 1, 1),
    Stairs  =c(0, 0, 0),
    Falls   =c(0, 0, 0)
  )
  expect_warning(
    sarc_f_score(df, cm),
    class = "healthmarkers_sarcf_warn_out_of_range"
  )
  out <- suppressWarnings(sarc_f_score(df, cm))
  expect_equal(nrow(out), 3L)
})

test_that("thresholding: high risk is TRUE for score >= 4", {
  skip_on_cran()
  df <- data.frame(
    Strength=c(2,1,0,NA),
    Walking =c(1,1,1,1),
    Chair   =c(1,1,1,1),
    Stairs  =c(0,0,0,0),
    Falls   =c(0,0,0,0)
  )

  out <- sarc_f_score(df, cm)
  expect_equal(out$sarc_f_score[1], 4)
  expect_equal(out$sarc_f_score[2], 3)
  expect_equal(out$sarc_f_score[3], 2)

  expect_true(out$sarc_f_high_risk[1])
  expect_false(out$sarc_f_high_risk[2])
  expect_false(out$sarc_f_high_risk[3])
  expect_true(is.na(out$sarc_f_high_risk[4]))
})

test_that("padding preserved for keep/warn", {
  skip_on_cran()
  df <- data.frame(
    Strength=c(0,NA,2),
    Walking =c(1,1,2),
    Chair   =c(1,1,2),
    Stairs  =c(0,1,2),
    Falls   =c(0,1,2)
  )

  out_keep <- sarc_f_score(df, cm, na_action = "keep")
  out_warn <- suppressWarnings(sarc_f_score(df, cm, na_action = "warn"))

  expect_equal(nrow(out_keep), 3L)
  expect_equal(nrow(out_warn), 3L)
})
