context_start_file("fitTriBin function")
test_that("NA values are avoided",{
  expect_error(fitTriBin(0:7,c(47,54,43,40,40,41,39,95),NA),
              "NA or Infinite or NAN values in the Input")
})
test_that("Chi-squared approximation issues",{
  expect_message(fitTriBin(200:203,c(1,1,1,1),0.000000009),
              "Chi-squared approximation is not suitable because expected frequency approximates to zero")
})
test_that("Chi-squared approximation issues",{
  expect_message(fitTriBin(0:5,c(2,5,40,40,4,3),0.00000001),
              "Chi-squared approximation may be doubtful because expected frequency is less than 5")
})
test_that("Degree of freedom less than zero",{
  expect_error(fitTriBin(c(0,1),c(110,12),0.111),
              "Degrees of freedom cannot be less than or equal to zero")
})
test_that("runs smoothly",{
  expect_no_error(fitTriBin(0:5,c(2,5,40,40,4,3),0.00000001))
})
