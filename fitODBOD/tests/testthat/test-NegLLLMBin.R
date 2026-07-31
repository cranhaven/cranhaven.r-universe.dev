context_start_file("NegLLLMBin functions")
test_that("NA values are avoided",{
  expect_error(NegLLLMBin(NA,4,0.1,.3),
              "NA or Infinite or NAN values in the Input")
})
test_that("Negativity Binomial random variable",{
  expect_error(NegLLLMBin(-3,4,0.2,0.4),
              "Binomial random variable or frequency values cannot be negative")
})
test_that("Probability issues",{
  expect_error(NegLLLMBin(3,5,3,0.4),
              "Probability or Phi parameter value doesnot satisfy conditions")
})
test_that("Output value expected",{
  expect_identical(round(NegLLLMBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,0.7,1),4),
                   472.6649)
})
test_that("Checking class of output",{
  expect_type(NegLLLMBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,0.7,1),
              "double")
})
