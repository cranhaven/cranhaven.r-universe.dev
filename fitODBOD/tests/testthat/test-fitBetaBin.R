context_start_file("fitBetaBin function")
test_that("NA values are avoided",{
  expect_error(fitBetaBin(0:7,c(47,12,43,40,40,41,39,95),NA,3),
              "NA or Infinite or NAN values in the Input")
})
test_that("Chi-squared approximation issues",{
  expect_message(fitBetaBin(0:6,c(2,5,4,40,40,4,3),0.0001,0.0003),
              "Chi-squared approximation is not suitable because expected frequency approximates to zero")
})
test_that("Chi-squared approximation issues",{
  expect_message(fitBetaBin(0:4,c(20,50,10,4,3),0.1,6),
              "Chi-squared approximation may be doubtful because expected frequency is less than 5")
})
test_that("Degree of freedom less than zero",{
  expect_error(fitBetaBin(c(0,1,2),c(11,12,12),0.51,0.03),
              "Degrees of freedom cannot be less than or equal to zero")
})
test_that("runs smoothly",{
  expect_no_error(fitBetaBin(0:7,c(47,12,43,40,40,41,39,95),1,3))
})
