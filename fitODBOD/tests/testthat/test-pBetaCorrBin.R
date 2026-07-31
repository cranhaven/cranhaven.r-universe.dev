context_start_file("pBetaCorrBin function")
test_that("checking value",{
  expect_identical(round(pBetaCorrBin(2,8,0.0005,0.4,1.1),4),
                   0.6501)
})
test_that("checking class",{
  expect_type(pBetaCorrBin(2,8,0.0005,0.4,1.1),"double")
})
test_that("checking length of output",{
  expect_equal(length(pBetaCorrBin(1:2,8,0.0005,0.4,1.1)),2)
})
test_that("NA values are avoided",{
  expect_error(pBetaCorrBin(1,4,NA,.3,1),
               "NA or Infinite or NAN values in the Input")
})
test_that("Random variable higher than Trial value",{
  expect_error(pBetaCorrBin(5,4,0.2,0.4,1),
               "Binomial random variable cannot be greater than binomial trial value")
})
test_that("Negativity",{
  expect_error(pBetaCorrBin(-3,4,0.2,0.4,1),
               "Binomial random variable or binomial trial value cannot be negative")
})
test_that("Correlation above maximum limit",{
  expect_error(pBetaCorrBin(3,5,9,0.2,0.2),
               "Correlation cannot be greater than 1 or Lesser than -1 or it cannot be greater than Maximum Correlation or Lesser than Minimum Correlation")
})
