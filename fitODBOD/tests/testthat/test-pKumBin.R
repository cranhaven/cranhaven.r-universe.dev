context_start_file("pKumBin function")
test_that("checking value",{
  expect_identical(round(pKumBin(1,8,1.05,1.04),4),0.2149)
})
test_that("checking class",{
  expect_type(pKumBin(1,8,1.05,1.04),"double")
})
test_that("checking length of output",{
  expect_equal(length(pKumBin(1:2,8,1.05,1.04)),2)
})
test_that("NA values are avoided",{
  expect_error(pKumBin(1,4,NA,3),
               "NA or Infinite or NAN values in the Input")
})
test_that("Random variable higher than Trial value",{
  expect_error(pKumBin(5,4,0.2,3),
               "Binomial random variable cannot be greater than binomial trial value")
})
test_that("Negativity",{
  expect_error(pKumBin(-3,4,0.2,3),
               "Binomial random variable or binomial trial value cannot be negative")
})
test_that("shape parameter a",{
  expect_error(pKumBin(2,4,-3,3),
               "Shape parameters cannot be less than or equal to zero")
})
test_that("Less than one",{
  expect_error(pKumBin(2,4,0.4,0.5,-1),
               "Number of iterations cannot be less than one")
})
