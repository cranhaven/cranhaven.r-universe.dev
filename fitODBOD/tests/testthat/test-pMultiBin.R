context_start_file("pMultiBin function")
test_that("checking value",{
  expect_identical(round(pMultiBin(2,4,0.5,0.4),4),0.5288)
})
test_that("checking class",{
  expect_type(pMultiBin(2,4,0.5,0.1),"double")
})
test_that("checking length of output",{
  expect_equal(length(pMultiBin(1:2,4,0.5,1)),2)
})
test_that("NA values are avoided",{
  expect_error(dMultiBin(1,4,NA,.3),
               "NA or Infinite or NAN values in the Input")
})
test_that("Random variable higher than Trial value",{
  expect_error(dMultiBin(5,4,0.2,0.4),
               "Binomial random variable cannot be greater than binomial trial value")
})
test_that("Negativity",{
  expect_error(dMultiBin(-3,4,0.2,0.4),
               "Binomial random variable or binomial trial value cannot be negative")
})
test_that("Probability issues",{
  expect_error(dMultiBin(3,5,3,0.4),
               "Probability value doesnot satisfy conditions")
})
test_that("Theta issues",{
  expect_error(dMultiBin(3,5,0.1,-3),
               "Theta parameter value cannot be zero or less than zero")
})
