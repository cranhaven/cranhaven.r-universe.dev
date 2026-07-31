context_start_file("mazUNI function")
test_that("NA values are avoided",{
  expect_error(mazUNI(NA),
              "NA or Infinite or NAN values in the Input")
})
test_that("Moments being negative or zero",{
  expect_error(mazUNI(-3),
              "Moments cannot be less than or equal to zero")
})
test_that("run smoothly",{
  expect_no_error(mazUNI(1))
})
