context_start_file("pLMBin function")
test_that("checking value",{
  expect_identical(round(pLMBin(2,4,0.5,0.4),4),0.5288)
})
test_that("checking class",{
  expect_type(pLMBin(2,4,0.5,0.1),"double")
})
test_that("checking length of output",{
  expect_equal(length(pLMBin(1:2,4,0.5,1)),2)
})
test_that("NA values are avoided",{
  expect_error(pLMBin(1,4,NA,.3),
               "NA or Infinite or NAN values in the Input")
})
test_that("Random variable higher than Trial value",{
  expect_error(pLMBin(5,4,0.2,0.4),"Binomial random variable cannot be greater than binomial trial value")
})
test_that("Negativity",{
  expect_error(pLMBin(-3,4,0.2,0.4),"Binomial random variable or binomial trial value cannot be negative")
})
test_that("probability value issues",{
  expect_error(pLMBin(3,5,3,0.4),"Probability value doesnot satisfy conditions")
})
test_that("Phi value issues",{
  expect_error(pLMBin(1,10,0.2,-0.4),"Phi parameter value cannot be zero or less than zero")
})
