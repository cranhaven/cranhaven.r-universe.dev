context("pValueString")

test_that("pValueString", {

  # inputs
  testthat::expect_type(pValueString("0.05"), "character")
  testthat::expect_type(pValueString(0.05), "character")
  testthat::expect_error(pValueString(""))

  # outputs
  testthat::expect_match(pValueString("0.03"), ".03")
  testthat::expect_match(pValueString(0.03), ".03")
  testthat::expect_match(pValueString("0.009"), ".009")
  testthat::expect_match(pValueString(0.009), ".009")
  testthat::expect_match(pValueString("0.0009"), "< .001")
  testthat::expect_match(pValueString(0.000), "< .001")

})
