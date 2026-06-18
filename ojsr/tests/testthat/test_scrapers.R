context("ojs scraper")
library(ojsr)

# we are testing on a failing resource

failing_url <- "http://error.error.error/index.php/error"

test_that("not resolving resource is failing gracefully", {
  expect_message( empty_dataframe <- ojsr::get_issues_from_archive(failing_url) )
})

test_that("not resolving resource return empty dataframe", {
  expect_is(ojsr::get_issues_from_archive( failing_url ),"data.frame")
  expect_equal(nrow(ojsr::get_issues_from_archive( failing_url )), 0)
})


