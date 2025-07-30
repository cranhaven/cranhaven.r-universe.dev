library(testthat)
library(dtmapi)

test_that("get_idp_admin2_data works", {
  skip_on_cran()  # Skip test on CRAN

  idp_admin2_df <- get_idp_admin2_data(Operation="Displacement due to conflict", CountryName='Lebanon')
  expect_s3_class(idp_admin2_df, "data.frame")
  expect_true(nrow(idp_admin2_df) > 0)
})
