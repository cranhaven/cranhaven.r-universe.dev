context_start_file("pTriBin function")
test_that("checking value",{
  expect_identical(round(pTriBin(2,4,0.5),4),0.6375)
})
test_that("checking class",{
  expect_type(pTriBin(2,4,0.5),"double")
})
test_that("checking length of output",{
  expect_equal(length(pTriBin(1:2,4,0.5)),2)
})
test_that("NA values are avoided",{
  expect_error(dTriBin(1,4,NA),
               "NA or Infinite or NAN values in the Input")
})
test_that("Greater than 1",{
  expect_error(dTriBin(2,4,3),
               "Mode cannot be less than or equal to zero or greater than or equal to one")
})
test_that("Random variable higher than Trial value",{
  expect_error(dTriBin(5,4,0.2),
               "Binomial random variable cannot be greater than binomial trial value")
})
test_that("Negativity",{
  expect_error(dTriBin(-3,4,0.2),
               "Binomial random variable or binomial trial value cannot be negative")
})
