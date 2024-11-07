test_that("load polar data", {
 expect_no_warning(data(polar))
})
#> Test passed
#> 
test_that("wrong name", {
  expect_warning(data(Polar))
})
#> Test passed