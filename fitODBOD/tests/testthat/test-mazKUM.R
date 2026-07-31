context_start_file("mazKUM function")
test_that("NA values are avoided",{
  expect_error(mazKUM(NA,0.1,2),
              "NA or Infinite or NAN values in the Input")
})
test_that("shape parameter b",{
  expect_error(mazKUM(1,5,-4),
              "Shape parameters cannot be less than or equal to zero")
})
test_that("Moments being negative or zero",{
  expect_error(mazKUM(-3,0.3,4),
              "Moments cannot be less than or equal to zero")
})
test_that("run smoothly",{
  expect_no_error(mazKUM(1,0.3,4))
})
