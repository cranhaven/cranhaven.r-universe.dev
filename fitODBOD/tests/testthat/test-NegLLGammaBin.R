context_start_file("NegLLGammaBin function")
test_that("NA values are avoided",{
  expect_error(NegLLGammaBin(NA,4,0.1,3),
              "NA or Infinite or NAN values in the Input")
})
test_that("shape parameter a",{
  expect_error(NegLLGammaBin(2,4,-3,3),
              "Shape parameters cannot be less than or equal to zero")
})
test_that("Negativity Binomial random variable",{
  expect_error(NegLLGammaBin(-3,4,0.2,4),
              "Binomial random variable or frequency values cannot be negative")
})
test_that("Output value expected",{
  expect_identical(round(NegLLGammaBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,0.7,1),4),
                   500.6763)
})
test_that("Checking class of output",{
  expect_type(NegLLGammaBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,0.7,1),
              "double")
})
