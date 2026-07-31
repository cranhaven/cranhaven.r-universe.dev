context_start_file("pCOMPBin function")
test_that("checking value",{
  expect_identical(round(pCOMPBin(2,4,0.5,0.4),4),
                   0.636)
})
test_that("checking class",{
  expect_type(pCOMPBin(2,4,0.5,0.1),"double")
})
test_that("checking length of output",{
  expect_equal(length(pCOMPBin(1:2,4,0.5,1)),2)
})
test_that("NA values are avoided",{
  expect_error(pCOMPBin(1,4,NA,.3),
               "NA or Infinite or NAN values in the Input")
})
test_that("Random variable higher than Trial value",{
  expect_error(pCOMPBin(5,4,0.2,0.4),
               "Binomial random variable cannot be greater than binomial trial value")
})
test_that("Negativity",{
  expect_error(pCOMPBin(-3,4,0.2,0.4),
               "Binomial random variable or binomial trial value cannot be negative")
})
test_that("Probability issues",{
  expect_error(pCOMPBin(3,5,3,0.4),
               "Probability value doesnot satisfy conditions")
})
