context_start_file("NegLLMcGBB functions")
test_that("NA values are avoided",{
  expect_error(NegLLMcGBB(NA,4,0.1,3,3),
              "NA or Infinite or NAN values in the Input")
})
test_that("shape parameter a",{
  expect_error(NegLLMcGBB(2,4,-3,3,6),
              "Shape parameters cannot be less than or equal to zero")
})
test_that("Negativity Binomial random variable",{
  expect_error(NegLLMcGBB(-3,4,0.2,4,3),
              "Binomial random variable or frequency values cannot be negative")
})
test_that("Output value expected",{
  expect_identical(round(NegLLMcGBB(Chromosome_data$No.of.Asso,Chromosome_data$fre,0.7,1,2),4),
                   453.457)
})
test_that("Checking class of output",{
  expect_type(NegLLMcGBB(Chromosome_data$No.of.Asso,Chromosome_data$fre,0.7,1,2),
              "double")
})
