context_start_file("pAddBin function")
test_that("checking value",{
  expect_identical(round(pAddBin(2,4,0.5,0.4),4),0.5375)
})
test_that("checking class",{
  expect_type(pAddBin(2,4,0.5,0.1),"double")
})
test_that("checking length of output",{
  expect_equal(length(pAddBin(1:2,4,0.5,0.1)),2)
})
test_that("NA values are avoided",{
  expect_error(pAddBin(1:4,4,NA,.3),"NA or Infinite or NAN values in the Input")
})
test_that("Random variable higher than Trial value",{
  expect_error(pAddBin(5,4,0.2,0.4),"Binomial random variable cannot be greater than binomial trial value")
})
test_that("Negativity",{
  expect_error(pAddBin(-3,4,0.2,0.4),
               "Binomial random variable or binomial trial value cannot be negative")
})
test_that("Probability issues",{
  expect_error(pAddBin(3,5,3,0.4),"Probability or alpha value doesnot satisfy conditions")
})
test_that("Alpha being greater than right side",{
  expect_error(pAddBin(1,10,0.2,0.4),"alpha parameter doesnot satisfy the conditions")
})
