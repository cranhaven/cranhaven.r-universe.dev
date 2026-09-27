# tests/testthat/test-collection-reexports.R
#
# surveycore re-exports for the survey_collection construction and setter API.
# Spec section: §VI.
#
# `library(surveytidy)` alone must be sufficient to call
# `as_survey_collection()`, `set_collection_id()`,
# `set_collection_if_missing_var()`, `add_survey()`, and `remove_survey()`.

test_that("as_survey_collection is re-exported from surveytidy", {
  expect_true(
    exists(
      "as_survey_collection",
      where = "package:surveytidy",
      inherits = FALSE
    )
  )
  expect_identical(
    surveytidy::as_survey_collection,
    surveycore::as_survey_collection
  )
})

test_that("set_collection_id is re-exported from surveytidy", {
  expect_true(
    exists(
      "set_collection_id",
      where = "package:surveytidy",
      inherits = FALSE
    )
  )
  expect_identical(
    surveytidy::set_collection_id,
    surveycore::set_collection_id
  )
})

test_that("set_collection_if_missing_var is re-exported from surveytidy", {
  expect_true(
    exists(
      "set_collection_if_missing_var",
      where = "package:surveytidy",
      inherits = FALSE
    )
  )
  expect_identical(
    surveytidy::set_collection_if_missing_var,
    surveycore::set_collection_if_missing_var
  )
})

test_that("add_survey is re-exported from surveytidy", {
  expect_true(
    exists(
      "add_survey",
      where = "package:surveytidy",
      inherits = FALSE
    )
  )
  expect_identical(
    surveytidy::add_survey,
    surveycore::add_survey
  )
})

test_that("remove_survey is re-exported from surveytidy", {
  expect_true(
    exists(
      "remove_survey",
      where = "package:surveytidy",
      inherits = FALSE
    )
  )
  expect_identical(
    surveytidy::remove_survey,
    surveycore::remove_survey
  )
})
