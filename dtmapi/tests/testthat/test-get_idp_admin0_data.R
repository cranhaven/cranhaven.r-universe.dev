library(testthat)
library(dtmapi)

test_that("get_idp_admin0_data works", {
  skip_on_cran()  # Skip test on CRAN

  idp_admin0_df <- get_idp_admin0_data(CountryName='Ethiopia', FromRoundNumber=1, ToRoundNumber=10)
  expect_s3_class(idp_admin0_df, "data.frame")
  expect_true(nrow(idp_admin0_df) > 0)
})
