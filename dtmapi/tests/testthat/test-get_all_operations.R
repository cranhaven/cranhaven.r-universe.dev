library(testthat)
library(dtmapi)

test_that("get_all_operations works", {
  skip_on_cran()  # Skip test on CRAN

  # Check if the function returns a data frame
  operations_df <- get_all_operations()
  expect_s3_class(operations_df, "data.frame")
  expect_true(nrow(operations_df) > 0)
})
