
testthat::test_that('http response produces error', {
  
  load(testthat::test_path("test_data", "test_res_fail.rda"))
  
  testthat::expect_error(detect_error(test_res_fail))
})

testthat::test_that('http response does not produce error', {
  
  load(testthat::test_path("test_data", "test_res_success.rda"))
  
  testthat::expect_error(detect_error(test_res_success), NA)
})
