# tests/testthat/test-wiring.R
#
# Dispatch spike: verify dplyr::filter wiring end-to-end.
#
# SCOPE: dispatch only — does the right method get called?
# This file does NOT test what filter() / dplyr_reconstruct() actually do.
# Behavioral tests (domain correctness, chaining, error paths, invariants,
# subset(), etc.) live in test-filter.R.
#
# Tests confirm:
#   1. dplyr::filter() dispatches to filter.survey_base()
#   2. The result is still a survey object (not a tibble or data.frame)
#   3. dplyr_reconstruct() preserves the class in complex pipelines
#   4. S7::methods_register() in .onLoad() correctly registers methods

test_that("dplyr::filter() dispatches to filter.survey_base for survey_taylor", {
  skip_if_not_installed("dplyr")
  d <- make_all_designs()$taylor
  result <- dplyr::filter(d, y1 > 0)
  expect_true(S7::S7_inherits(result, surveycore::survey_base))
})

test_that("dplyr::filter() result has same nrow as original (domain, not removal)", {
  skip_if_not_installed("dplyr")
  d <- make_all_designs()$taylor
  result <- dplyr::filter(d, y1 > 0)
  expect_equal(nrow(result@data), nrow(d@data))
})

test_that("dplyr::filter() creates ..surveycore_domain.. column", {
  skip_if_not_installed("dplyr")
  d <- make_all_designs()$taylor
  result <- dplyr::filter(d, y1 > 0)
  expect_true("..surveycore_domain.." %in% names(result@data))
  expect_true(is.logical(result@data[["..surveycore_domain.."]]))
})

test_that("dplyr::filter() dispatches to filter.survey_base for survey_replicate", {
  skip_if_not_installed("dplyr")
  d <- make_all_designs()$replicate
  result <- dplyr::filter(d, y1 > 0)
  expect_true(S7::S7_inherits(result, surveycore::survey_base))
  expect_equal(nrow(result@data), nrow(d@data))
})

test_that("dplyr::filter() dispatches to filter.survey_base for survey_twophase", {
  skip_if_not_installed("dplyr")
  d <- make_all_designs()$twophase
  result <- dplyr::filter(d, y1 > 0)
  expect_true(S7::S7_inherits(result, surveycore::survey_base))
  expect_equal(nrow(result@data), nrow(d@data))
})

test_that("dplyr_reconstruct() preserves survey class", {
  skip_if_not_installed("dplyr")
  d <- make_all_designs()$taylor
  result <- dplyr::dplyr_reconstruct(d@data, d)
  expect_true(S7::S7_inherits(result, surveycore::survey_base))
})
