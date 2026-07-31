context_start_file("NegLLBetaCorrBin functions")
test_that("NA values are avoided",{
  expect_error(NegLLBetaCorrBin(NA,4,0.1,.3,1),
              "NA or Infinite or NAN values in the Input")
})
test_that("Negativity Binomial random variable",{
  expect_error(NegLLBetaCorrBin(-3,4,0.2,0.4,1),
              "Binomial random variable or frequency values cannot be negative")
})
test_that("Correlation above maximum limit",{
  expect_error(NegLLBetaCorrBin(3,5,9,0.2,0.2),
              "Correlation cannot be greater than 1 or Lesser than -1 or it cannot be greater than Maximum Correlation or Lesser than Minimum Correlation")
})
test_that("Output value expected",{
  expect_identical(round(NegLLBetaCorrBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,0.007,0.1,0.2),4),
                   731.9854)
})
test_that("Checking class of output",{
  expect_type(NegLLBetaCorrBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,0.007,0.1,0.2),
              "double")
})
