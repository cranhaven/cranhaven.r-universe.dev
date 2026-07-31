context_start_file("pUniBin function")
test_that("checking value",{
  expect_identical(round(pUniBin(2,4),1),0.6)
})
test_that("checking class",{
  expect_that(pUniBin(2,4),is_a("numeric"))
})
test_that("checking length of output",{
  expect_equal(length(pUniBin(1:2,4)),2)
})
test_that("NA values are avoided",{
  expect_error(pUniBin(1,NA),
               "NA or Infinite or NAN values in the Input")
})
test_that("Random variable higher than Trial value",{
  expect_error(pUniBin(5,4),
               "Binomial random variable cannot be greater than binomial trial value")
})
test_that("Negativity",{
  expect_error(pUniBin(-3,4),
               "Binomial random variable or binomial trial value cannot be negative")
})
