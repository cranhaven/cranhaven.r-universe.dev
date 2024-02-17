
testthat::test_that("correct number of rows returned", {
  
  load(testthat::test_path("test_data", "nrow_resource_test_dat.rda"))
  
  mockery::stub(nrow_resource, 'httr::RETRY', 
                function(...) nrow_resource_test_dat)
  
  testthat::expect_equal(
    nrow_resource("edee9731-daf7-4e0d-b525-e4c1469b8f69"),19)
})


testthat::test_that("live query runs successfully", {
  
  testthat::skip_on_cran()
  
  testthat::expect_true(is.integer(nrow_resource("edee9731-daf7-4e0d-b525-e4c1469b8f69")))
})
