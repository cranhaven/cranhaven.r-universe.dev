# tests/testthat/test-methods-compat.R
#
# Tests for IDE-compatibility methods on survey design objects.
# Source: R/methods-compat.R
#
# Test structure:
#   1. names() — survey_taylor returns @data column names
#   2. names() — survey_replicate returns @data column names
#   3. names() — survey_twophase returns @data column names
#   4. names() — SRS-style survey_taylor returns @data column names
#   5. names() — consistent with names(survey_data(design))

test_that("names() returns @data column names for survey_taylor", {
  df <- make_survey_data(n = 100, seed = 1)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)
  expect_identical(names(d), names(df))
})

test_that("names() returns @data column names for survey_replicate", {
  df <- make_survey_data(n = 100, design = "replicate", seed = 2)
  d <- as_survey_replicate(
    df,
    weights = wt,
    repweights = starts_with("repwt_"),
    type = "BRR"
  )
  expect_identical(names(d), names(df))
})

test_that("names() returns @data column names for survey_twophase", {
  df <- make_survey_data(n = 200, design = "twophase", seed = 3)
  phase1 <- as_survey(df, ids = psu, weights = wt, strata = strata)
  d <- as_survey_twophase(
    phase1,
    probs2 = phase2_prob,
    subset = subset
  )
  expect_identical(names(d), names(df))
})

test_that("names() returns @data column names for SRS-style survey_taylor", {
  df <- data.frame(y = 1:10, grp = letters[1:10], w = rep(1, 10))
  d <- as_survey(df, weights = w)
  expect_identical(names(d), names(df))
})

test_that("names() is consistent with names(survey_data(design))", {
  df <- make_survey_data(n = 100, seed = 4)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)
  expect_identical(names(d), names(survey_data(d)))
})
