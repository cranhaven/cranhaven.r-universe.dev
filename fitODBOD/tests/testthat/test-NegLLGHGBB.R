context_start_file("NegLLGHGBB functions")
test_that("NA values are avoided",{
  expect_error(NegLLGHGBB(NA,4,0.1,3,3),
              "NA or Infinite or NAN values in the Input")
})
test_that("shape parameter a",{
  expect_error(NegLLGHGBB(2,4,-3,3,6),
              "Shape parameters cannot be less than or equal to zero")
})
test_that("Negativity Binomial random variable",{
  expect_error(NegLLGHGBB(-3,4,0.2,4,3),
              "Binomial random variable or frequency values cannot be negative")
})
test_that("Output value expected",{
  expect_identical(round(NegLLGHGBB(Chromosome_data$No.of.Asso,Chromosome_data$fre,0.7,12,2),4),
                   900.9297)
})
test_that("Checking class of output",{
  expect_type(NegLLGHGBB(Chromosome_data$No.of.Asso,Chromosome_data$fre,0.7,11,2),
              "double")
})
