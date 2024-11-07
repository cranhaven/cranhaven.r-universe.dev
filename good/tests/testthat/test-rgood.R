test_that("negative n", {
  expect_warning(rgood ( n = -100 , z = 0.5 , s = -3 ))
})
#> Test passed

test_that("negative th", {
  expect_warning(rgood ( n = 1 , z = 0.5 , s = -3 , th = -9 ))
})
#> Test passed

test_that("z outside (0, 1)", {
expect_warning(rgood ( n = 2 , z = c( -0.5, 0.5 ) , s = -3 ))
})
#> Test passed

test_that("vector", {
expect_vector(rgood ( n = 100 , z = 0.5 , s = -3 ), size = 100)
})
#> Test passed