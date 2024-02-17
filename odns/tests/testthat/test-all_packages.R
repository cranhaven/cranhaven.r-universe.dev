testthat::test_that('no errors and successful return of all packages', {
  
  load(testthat::test_path("test_data", "pck_list_res.rda"))
  
  mockery::stub(all_packages, 'httr::RETRY', 
                function(...) pck_list_res)
  
  testthat::expect_equal(
    digest::digest(all_packages()),
    "c298493c38a4916f9e76a7655a2e52df"
  )
})

testthat::test_that("error when request fails", {
  
  load(testthat::test_path("test_data", "test_res_fail.rda"))
  
  mockery::stub(all_packages, 'httr::RETRY', 
                function(...) test_res_fail)
  
  testthat::expect_error(
    digest::digest(all_packages())
    )
})

testthat::test_that('filter functionality works', {
  
  load(testthat::test_path("test_data", "pck_list_res_filt.rda"))
  
  mockery::stub(all_packages, 'httr::RETRY', 
                function(...) pck_list_res_filt)
  
  testthat::expect_equal(
    digest::digest(all_packages(contains = "standard-populations")),
    "7a1c038182bbce1efc68a7f680774624"
  )
})

testthat::test_that('live query works', {
  
  testthat::skip_on_cran()
  
  testthat::expect_true(
    is.data.frame(all_packages(contains = "standard-populations"))
  )
})
