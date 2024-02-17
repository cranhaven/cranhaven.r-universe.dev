
testthat::test_that('function correctly parses response', {
  
  load(testthat::test_path("test_data", "test_res_success.rda"))
  
  mockery::stub(resource_data_items, 'httr::RETRY', function(...) test_res_success)
  
  testthat::expect_equal(
  digest::digest(resource_data_items(resource="abcdef")),
  "4df5446e9c60b589f88528350c23e4bd"
  )
})

testthat::test_that('function correctly provides error response', {
  
  load(testthat::test_path("test_data", "test_res_fail.rda"))
  
  mockery::stub(resource_data_items, 'httr::RETRY', function(...) test_res_fail)
  
  testthat::expect_error(resource_data_items(resource="abcdef"))
  
})

testthat::test_that('function returns a data.frame from live query', {
  
  testthat::skip_on_cran()
  
  testthat::expect_true(is.data.frame(
    resource_data_items(resource="edee9731-daf7-4e0d-b525-e4c1469b8f69")
    ))
})

testthat::test_that('function returns an error from live query with badly
                    formed resource id', {
  
  testthat::skip_on_cran()
  
  testthat::expect_error(resource_data_items(resource="abcd123"))
})
