context_start_file("mazGBeta1 function")
test_that("NA values are avoided",{
  expect_error(mazGBeta1(NA,0.1,3,3),
               "NA or Infinite or NAN values in the Input")
})
test_that("Moments being negative or zero",{
  expect_error(mazGBeta1(-3,3,3,2),
               "Moments cannot be less than or equal to zero")
})
test_that("shape parameter b",{
  expect_error(mazGBeta1(0.1,5,-4,4),
               "Shape parameters cannot be less than or equal to zero")
})
test_that("run smoothly",{
  expect_no_error(mazGBeta1(1,5,1,2))
})
