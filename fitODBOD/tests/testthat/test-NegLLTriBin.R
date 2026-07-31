context_start_file("NegLLTriBin function")
test_that("NA values are avoided",{
  expect_error(NegLLTriBin(NA,4,0.2),
              "NA or Infinite or NAN values in the Input")
})
test_that("Negativity Binomial random variable",{
  expect_error(NegLLTriBin(-3,4,0.2),
              "Binomial random variable or frequency values cannot be negative")
})
test_that("Greater than 1",{
  expect_error(NegLLTriBin(3,4,3),
              "Mode cannot be less than zero or greater than one")
})
test_that("Output value expected",{
  expect_identical(round(NegLLTriBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,0.7),4),
                   440.6691)
})
test_that("Checking class of output",{
  expect_type(NegLLTriBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,0.7),
              "double")
})
