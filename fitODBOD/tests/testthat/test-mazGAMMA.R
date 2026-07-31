context_start_file("mazGAMMA function")
test_that("NA values are avoided",{
  expect_error(mazGAMMA(NA,0.1,2),
              "NA or Infinite or NAN values in the Input")
})
test_that("shape parameter b",{
  expect_error(mazGAMMA(1,5,-4),
              "Shape parameters cannot be less than or equal to zero")
})
test_that("Moments being negative or zero",{
  expect_error(mazGAMMA(-3,0.3,4),
              "Moments cannot be less than or equal to zero")
})
test_that("run smoothly",{
  expect_no_error(mazGAMMA(1,5,1))
})
