context_start_file("NegLLBetaBin functions")
test_that("NA values are avoided",{
  expect_error(NegLLBetaBin(NA,4,0.2,3),
               "NA or Infinite or NAN values in the Input")
})
test_that("Negativity Binomial random variable",{
  expect_error(NegLLBetaBin(-3,4,0.2,4),
               "Binomial random variable or frequency values cannot be negative")
})
test_that("Shape parameter a",{
  expect_error(NegLLBetaBin(3,4,-3,4),
               "Shape parameters cannot be less than or equal to zero")
})
test_that("Output value expected",{
  expect_identical(round(NegLLBetaBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,0.7,1),4),
                   500.6763)
})
test_that("Checking class of output",{
  expect_type(NegLLBetaBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,0.7,1),
              "double")
})
