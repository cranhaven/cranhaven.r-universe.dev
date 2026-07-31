context_start_file("pGammaBin function")
test_that("checking value",{
  expect_identical(round(pGammaBin(2,4,0.5,0.4),4),0.4091)
})
test_that("checking class",{
  expect_type(pGammaBin(2,4,0.5,0.1),"double")
})
test_that("checking length of output",{
  expect_equal(length(pGammaBin(1:2,4,0.5,1)),2)
})
test_that("NA values are avoided",{
  expect_error(pGammaBin(1,4,NA,3),
               "NA or Infinite or NAN values in the Input")
})
test_that("Random variable higher than Trial value",{
  expect_error(pGammaBin(5,4,0.2,3),
               "Binomial random variable cannot be greater than binomial trial value")
})
test_that("Negativity",{
  expect_error(pGammaBin(-3,4,0.2,3),
               "Binomial random variable or binomial trial value cannot be negative")
})
test_that("shape parameter a",{
  expect_error(pGammaBin(2,4,-3,3),
               "Shape parameters cannot be less than or equal to zero")
})

