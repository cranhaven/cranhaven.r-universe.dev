context_start_file("fitBin")
test_that("NA values are avoided",{
  expect_error(fitBin(0:7,c(47,54,43,40,40,41,39,95),NA),
              "NA or Infinite or NAN values in the Input")
})
test_that("p values",{
  expect_error(fitBin(0:7,c(47,54,43,40,40,41,39,95),1.5),
               "Probability value cannot be less than or equal to zero or greater than or equal to one")
})
test_that("Chi-squared approximation issues",{
  expect_message(fitBin(0:5,c(12,147,94,83,40,8),0),
              "Chi-squared approximation may be doubtful because expected frequency is less than 5")
})
test_that("Degree of freedom less than zero",{
  expect_error(fitBin(c(1,2),c(10,12),0),
              "Degrees of freedom cannot be less than or equal to zero")
})
test_that("runs smoothly",{
  expect_no_error(fitBin(0:7,c(47,42,43,40,40,41,39,45),0))
})
