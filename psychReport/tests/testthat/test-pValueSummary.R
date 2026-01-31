context("pValueSummary")

test_that("pValueSummary", {

  # inputs
  testthat::expect_error(pValueSummary("a"))
  testthat::expect_error(pValueSummary(c(0.1, "a")))

  # outputs
  testthat::expect_equal(pValueSummary(0.03), "*")
  testthat::expect_equal(pValueSummary(0.003), "**")
  testthat::expect_equal(pValueSummary(0.0003), "***")
  testthat::expect_equal(pValueSummary(c(0.1, 0.01)), c("", "*"))
  testthat::expect_equal(pValueSummary(c(0.115, 0.001)), c("", "**"))

})
