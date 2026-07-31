context_start_file("NegLLGrassiaIIBin functions")
test_that("NA values are avoided",{
  expect_error(NegLLGrassiaIIBin(NA,4,0.1,3),
              "NA or Infinite or NAN values in the Input")
})
test_that("Negativity Binomial random variable",{
  expect_error(NegLLGrassiaIIBin(-3,4,0.2,4),
              "Binomial random variable or frequency values cannot be negative")
})
test_that("Output value expected",{
  expect_identical(round(NegLLGrassiaIIBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,0.7,1),4),
                   511.3372)
})
test_that("Checking class of output",{
  expect_type(NegLLGrassiaIIBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,0.7,1),
              "double")
})
