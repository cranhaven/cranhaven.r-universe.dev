library(testthat)
library(dtmapi)

test_that("get_all_countries works", {
  skip_on_cran()  # Skip test on CRAN

  # Check if the function returns a data frame
  countries_df <- get_all_countries()
  expect_s3_class(countries_df, "data.frame")
  expect_true(nrow(countries_df) > 0)
})
