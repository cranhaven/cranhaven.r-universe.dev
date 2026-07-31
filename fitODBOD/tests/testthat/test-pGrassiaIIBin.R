context_start_file("pGrassiaIIBin function")
test_that("checking value",{
  expect_identical(round(pGrassiaIIBin(2,4,0.5,0.4),4),0.9391)
})
test_that("checking class",{
  expect_type(pGrassiaIIBin(2,4,0.5,0.1),"double")
})
test_that("checking length of output",{
  expect_equal(length(pGrassiaIIBin(1:2,4,0.5,1)),2)
})
test_that("NA values are avoided",{
  expect_error(pGrassiaIIBin(1,4,NA,3),
               "NA or Infinite or NAN values in the Input")
})
test_that("Random variable higher than Trial value",{
  expect_error(pGrassiaIIBin(5,4,0.2,3),
               "Binomial random variable cannot be greater than binomial trial value")
})
test_that("Negativity",{
  expect_error(pGrassiaIIBin(-3,4,0.2,3),
               "Binomial random variable or binomial trial value cannot be negative")
})
test_that("shape parameter a",{
  expect_error(pGrassiaIIBin(2,4,-3,3),
               "Shape parameters cannot be less than or equal to zero")
})
