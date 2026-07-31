context_start_file("NegLLAddBin functions")
test_that("NA values are avoided",{
  expect_error(NegLLAddBin(NA,4,0.1,.3),"NA or Infinite or NAN values in the Input")
})
test_that("Negativity Binomial random variable",{
  expect_error(NegLLAddBin(-3,4,0.2,0.4),"Binomial random variable or frequency values cannot be negative")
})
test_that("Alpha or probability value issues",{
  expect_error(NegLLAddBin(3,5,3,0.4),"Probability or alpha value doesnot satisfy conditions")
})
test_that("Alpha value greater than right side",{
  expect_error(NegLLAddBin(5,1,0.2,0.4),"alpha parameter doesnot satisfy the conditions")
})
test_that("Output value expected",{
  expect_identical(round(NegLLAddBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,0.7,0.1),4),463.4881)
})
test_that("Checking class of output",{
  expect_type(NegLLAddBin(Chromosome_data$No.of.Asso,Chromosome_data$fre,0.7,0.1),"double")
})
