
testthat::test_that('no errors and successful run', {
  
  load(testthat::test_path("test_data", "pck_res.rda"))
  
  mockery::stub(all_resources, "httr::RETRY", function(...) pck_res)
  
  testthat::expect_equal(
    digest::digest(all_resources(
      package_contains = "standard-populations",
      resource_contains = "European"
    )),
    "9704cf3a6290f115ddfc3aeb364c9146")
})

testthat::test_that('WARNING when arguments leads to 0 row data.frame', {
  
  load(testthat::test_path("test_data", "pck_res.rda"))
  
  mockery::stub(all_resources, "httr::RETRY", function(...) pck_res)
  
  testthat::expect_warning(
    all_resources(
      package_contains = "a_package_that_doesnt_exist",
      resource_contains = "a_resource_that_doesnt_exist"
    ))
})

testthat::test_that('live query returns no errors and successful run', {
  
  testthat::skip_on_cran()
  
  testthat::expect_true(
    is.data.frame(all_resources(
      package_contains = "standard-populations",
      resource_contains = "European"
    )))
})

