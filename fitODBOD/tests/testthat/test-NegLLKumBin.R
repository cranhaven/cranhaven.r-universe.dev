context_start_file("NegLLKumBin functions")
test_that("NA values are avoided",{
  expect_error(NegLLKumBin(NA,4,0.2,3),
              "NA or Infinite or NAN values in the Input")
})
test_that("Negativity Binomial random variable",{
  expect_error(NegLLKumBin(-3,4,0.2,4),
              "Binomial random variable or frequency values cannot be negative")
})
test_that("Shape parameter a",{
  expect_error(NegLLKumBin(3,4,-3,4),
              "Shape parameters cannot be less than or equal to zero")
})
test_that("Shape parameter b",{
  expect_error(NegLLKumBin(3,4,1,-3),
              "Shape parameters cannot be less than or equal to zero")
})
test_that("Less than one",{
  expect_error(NegLLKumBin(2,4,0.4,0.5,-1),
              "Number of iterations cannot be less than one")
})
test_that("Output value expected",{
  expect_identical(round(NegLLKumBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,1,2),4),
                   533.8887)
})
test_that("Checking class of output",{
  expect_type(NegLLKumBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,1,2),
              "double")
})
