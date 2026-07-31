context_start_file("fitBetaCorrBin function")
test_that("NA values are avoided",{
  expect_error(fitBetaCorrBin(0:7,c(47,54,43,40,40,41,39,95),NA,0.003,1),
              "NA or Infinite or NAN values in the Input")
})
test_that("Chi-squared approximation issues",{
  expect_message(fitBetaCorrBin(0:7,c(7,3,3,4,4,4,5,4),0.030,34.3,5.9),
                 "Chi-squared approximation is not suitable because expected frequency approximates to zero")
})
test_that("Chi-squared approximation issues",{
  expect_message(fitBetaCorrBin(0:7,c(47,54,43,40,40,41,39,95),0.01,30,10),
               "Chi-squared approximation may be doubtful because expected frequency is less than 5")
})
test_that("Degree of freedom less than zero",{
  expect_error(fitBetaCorrBin(c(0,1,2,3),c(10,11,12,34),0.003,0.41,0.23),
              "Degrees of freedom cannot be less than or equal to zero")
})
test_that("runs smoothly",{
  expect_no_error(fitBetaCorrBin(0:7,c(47,12,43,40,40,41,39,95),0.003,0.41,0.23))
})
