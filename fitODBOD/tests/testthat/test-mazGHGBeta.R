context_start_file("mazGHGBeta function")
test_that("NA values are avoided",{
  expect_error(mazGHGBeta(NA,0.1,3,3,2),
               "NA or Infinite or NAN values in the Input")
})
test_that("Moments being negative or zero",{
  expect_error(mazGHGBeta(-3,3,3,3,2),
               "Moments cannot be less than or equal to zero")
})
test_that("shape parameter b",{
  expect_error(mazGHGBeta(0.1,5,1,-4,4),
               "Shape parameters cannot be less than or equal to zero")
})
test_that("Binomial trial value issues",{
  expect_error(mazGHGBeta(3,-4,4,1,0.3),
               "Binomial trial value cannot be less than zero")
})
test_that("run smoothly",{
  expect_no_error(mazGHGBeta(1.9,15,5,6,1))
})
