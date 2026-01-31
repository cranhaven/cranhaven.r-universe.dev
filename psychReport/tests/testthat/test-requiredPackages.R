context("requiredPackages")

test_that("requiredPackages", {

  testthat::expect_error(requiredPackages(c("ez", "dplyr")), NA)
  testthat::expect_error(requiredPackages(c("ez", "dplyr", "xxx")))

})
