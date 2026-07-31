context_start_file("dLMBin function errors")
test_that("checking class",{
  expect_type(dLMBin(0:10,10,.58,10.022)$pdf,"double")
})
Temp<-dLMBin(0:10,10,.58,10.022)
test_that("checking length of output",{
  expect_type(Temp,"list")
})
test_that("NA values are avoided",{
  expect_error(dLMBin(NA,4,0.1,.3),
              "NA or Infinite or NAN values in the Input")
})
test_that("Random variable higher than Trial value",{
  expect_error(dLMBin(5,4,0.2,0.4),"Binomial random variable cannot be greater than binomial trial value")
})
test_that("Negativity",{
  expect_error(dLMBin(-3,4,0.2,0.4),"Binomial random variable or binomial trial value cannot be negative")
})
test_that("probability value issues",{
  expect_error(dLMBin(3,5,3,0.4),"Probability value doesnot satisfy conditions")
})
test_that("Phi value issues",{
  expect_error(dLMBin(1,10,0.2,-0.4),"Phi parameter value cannot be zero or less than zero")
})
