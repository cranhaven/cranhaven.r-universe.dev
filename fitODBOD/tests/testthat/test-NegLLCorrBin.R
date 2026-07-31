context_start_file("NegLLCorrBin functions")
test_that("NA values are avoided",{
  expect_error(NegLLCorrBin(NA,4,0.1,.3),
               "NA or Infinite or NAN values in the Input")
})
test_that("Negativity Binomial random variable",{
  expect_error(NegLLCorrBin(-3,4,0.2,0.4),
               "Binomial random variable or frequency values cannot be negative")
})
test_that("Probability issues",{
  expect_error(NegLLCorrBin(3,5,3,0.4),
               "Probability value doesnot satisfy conditions")
})
test_that("Correlation above maximum limit",{
  expect_error(NegLLCorrBin(3,5,0.2,19),
               "Correlation cannot be greater than 1 or Lesser than -1 or it cannot be greater than Maximum Correlation or Lesser than Minimum Correlation")
})
test_that("Output value expected",{
  expect_identical(round(NegLLCorrBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,0.7,0.01),4),
                   466.572)
})
test_that("Checking class of output",{
  expect_type(NegLLCorrBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,0.7,0.01),
              "double")
})
