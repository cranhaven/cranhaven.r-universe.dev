
testthat::test_that('function correctly parses response', {
  
  load(testthat::test_path("test_data", "pck_meta_res.rda"))
  
  mockery::stub(package_metadata, 'httr::RETRY', function(...) pck_meta_res)
  
  testthat::expect_equal(
    digest::digest(package_metadata(package = "standard-populations")),
    "40fc2af794eb612e5788e6bbe2008ca6"
  )
})

testthat::test_that('function correctly provides error response', {
  
  load(testthat::test_path("test_data", "test_res_fail.rda"))
  
  mockery::stub(package_metadata, 'httr::RETRY', function(...) test_res_fail)
  
  testthat::expect_error(package_metadata(resource="standard-populations"))
  
})

testthat::test_that('function returns a list from live query', {
  
  testthat::skip_on_cran()
  
  testthat::expect_true(is.list(
    package_metadata(package = "standard-populations")
  ))
})

testthat::test_that('function returns an error from live query with badly formed resource id', {
  
  testthat::skip_on_cran()
  
  testthat::expect_error(package_metadata(package = "abcd123"))
})

