test_that("negative q result", {
 expect_equal(pgood ( q = -3 , z = 0.6 , s = -3  ), NaN)
})
#> Test passed

test_that("negative q warning", {
  expect_warning(pgood ( q = -3 , z = 0.6 , s = -3  ))
})
#> Test passed 

test_that("non-integer q result", {
  expect_equal(pgood ( q = 3.4 , z = 0.6 , s = -3 ), pgood ( q = floor(3.4) , z = 0.6 , s = -3 ))
})
#> Test passed

test_that("non-integer q warning", {
  expect_warning(pgood ( q = 3.4 , z = 0.6 , s = -3 ))
})
#> Test passed

test_that("z outside (0, 1) result", {
  expect_equal(pgood ( q = -3 , z = -0.6 , s = -3  ), NaN)
})
#> Test passed

test_that("z outside (0, 1) warning", {
  expect_warning(pgood ( q = -3 , z = -0.6 , s = -3  ))
})
#> Test passed

test_that("lower tail", {
  expect_vector(pgood ( q = 0 : 1 , z = c ( 0.6 , 0.9 ) , s = -3 , lower.tail = FALSE ), size = 2)
})
#> Test passed