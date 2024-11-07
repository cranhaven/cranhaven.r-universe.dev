test_that("p outside (0, 1) result", {
  expect_equal(qgood ( p = -0.6 , z = 0.5 , s = -3 ), NaN)
})
#> Test passed

test_that("p outside (0, 1) warning", {
  expect_warning(qgood ( p = -0.6 , z = 0.5 , s = -3 ))
})
#> Test passed

test_that("z outside (0, 1) result", {
  expect_equal(qgood ( p = 0.6 , z = -0.5 , s = -3 ), NaN)
})
#> Test passed

test_that("z outside (0, 1) warning", {
  expect_warning(qgood ( p = 0.6 , z = 1.3 , s = -3 ))
})
#> Test passed

test_that("lower tail", {
  expect_vector(qgood ( p = c ( 0.025 , 0.5 , 0.975 ) , z = c ( 0.6 , 0.3 , 0.5 ) , s = -3 , lower.tail = FALSE ), size = 3)
})
#> Test passed