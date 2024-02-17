
testthat::test_that('function correctly parses response', {
  
  load(testthat::test_path("test_data", "res_meta.rda"))
  
  mockery::stub(resource_metadata, 'httr::RETRY', function(...) res_meta)
  
  testthat::expect_equal(
  digest::digest(resource_metadata(resource="abcdef")),
  "7ee9994a03d3db345b0d0f33209084b8"
  )
})

testthat::test_that('function correctly provides error response', {
  
  load(testthat::test_path("test_data", "res_meta_fail.rda"))
  
  mockery::stub(resource_metadata, 'httr::RETRY', function(...) res_meta_fail)
  
  testthat::expect_error(resource_metadata(resource="abcdef"))
  
})

testthat::test_that('function returns a list from live query', {
  
  testthat::skip_on_cran()
  
  testthat::expect_true(is.list(
    resource_metadata(resource="edee9731-daf7-4e0d-b525-e4c1469b8f69")
    ))
})

testthat::test_that('function returns an error from live query with badly
                    formed resource id', {
  
  testthat::skip_on_cran()
  
  testthat::expect_error(resource_metadata(resource="abcd123"))
})
