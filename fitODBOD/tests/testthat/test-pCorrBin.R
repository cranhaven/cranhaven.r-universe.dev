context_start_file("pCorrBin function")
test_that("checking value",{
  expect_identical(round(pCorrBin(2,4,0.5,0.004),4),0.6815)
})
test_that("checking class",{
  expect_type(pCorrBin(2,4,0.5,0.0001),"double")
})
test_that("checking length of output",{
  expect_equal(length(pCorrBin(1:2,4,0.5,0.0001)),2)
})
test_that("NA values are avoided",{
  expect_error(pCorrBin(1,4,NA,.3),
               "NA or Infinite or NAN values in the Input")
})
test_that("Random variable higher than Trial value",{
  expect_error(pCorrBin(5,4,0.2,0.4),
               "Binomial random variable cannot be greater than binomial trial value")
})
test_that("Negativity",{
  expect_error(pCorrBin(-3,4,0.2,0.4),
               "Binomial random variable or binomial trial value cannot be negative")
})
test_that("Probability issues",{
  expect_error(pCorrBin(3,5,3,0.4),
               "Probability value doesnot satisfy conditions")
})
test_that("Correlation above maximum limit",{
  expect_error(pCorrBin(3,5,0.2,19),
               "Correlation cannot be greater than 1 or Lesser than -1 or it cannot be greater than Maximum Correlation or Lesser than Minimum Correlation")
})
