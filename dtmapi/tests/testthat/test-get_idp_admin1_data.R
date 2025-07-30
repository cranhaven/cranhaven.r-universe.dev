library(testthat)
library(dtmapi)

test_that("get_idp_admin1_data works", {
  skip_on_cran()  # Skip test on CRAN

  idp_admin1_df <- get_idp_admin1_data(CountryName='Sudan', Admin1Name="Blue Nile", FromReportingDate='2020-01-01', ToReportingDate='2024-08-15')
  expect_s3_class(idp_admin1_df, "data.frame")
  expect_true(nrow(idp_admin1_df) > 0)
})
