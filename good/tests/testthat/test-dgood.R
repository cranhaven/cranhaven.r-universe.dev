test_that("non-positive x result", {
   expect_equal(dgood ( x = -3 , z = 0.6 , s = -3 ), 0)
})
#> Test passed

test_that("non-positive x warning", {
  expect_warning(dgood ( x = -3 , z = 0.6 , s = -3 ))
})
#> Test passed

test_that("z outside (0, 1) x result", {
  expect_equal(dgood ( x = -3 , z = -0.9 , s = -3 ), NaN)
})
#> Test passed

test_that("z outside (0, 1) warning", {
  expect_warning(dgood ( x = 4 , z = -0.9 , s = -3 ))
})
#> Test passed

test_that("approximation", {
  expect_warning(dgood ( x = 330 : 331 , z = c ( 0.6 , 0.5 ) , s = -170 ))
}) 
#> Test passed