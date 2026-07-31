context_start_file("fitLMBin function")
test_that("NA values are avoided",{
  expect_error(fitLMBin(0:7,c(47,54,43,40,40,41,39,95),NA,0.003),
              "NA or Infinite or NAN values in the Input")
})
test_that("Chi-squared approximation issues",{
  expect_message(fitLMBin(0:7,c(47,54,43,40,40,41,39,95),0.1,0.003),
              "Chi-squared approximation is not suitable because expected frequency approximates to zero")
})
test_that("Chi-squared approximation issues",{
  expect_message(fitLMBin(0:3,c(43,40,40,41),0.54,15.0003),
              "Chi-squared approximation may be doubtful because expected frequency is less than 5")
})
test_that("Degree of freedom less than zero",{
  expect_error(fitLMBin(c(0,1,2),c(11,12,12),0.54,1.3),
              "Degrees of freedom cannot be less than or equal to zero")
})
test_that("runs smoothly",{
  expect_no_error(fitLMBin(0:3,c(43,40,40,41),0.54,15.0003))
})
