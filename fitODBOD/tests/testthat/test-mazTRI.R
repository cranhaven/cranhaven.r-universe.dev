context_start_file("mazTRI function")
test_that("NA values are avoided",{
  expect_error(mazTRI(NA,0.1),
              "NA or Infinite or NAN values in the Input")
})
test_that("Mode out of range",{
  expect_error(mazTRI(1,5),
              "Mode cannot be less than zero or greater than one")
})
test_that("Moments being negative or zero",{
  expect_error(mazTRI(-3,0.3),
              "Moments cannot be less than or equal to zero")
})
test_that("run smoothly",{
  expect_no_error(mazTRI(2,0.5))
})
