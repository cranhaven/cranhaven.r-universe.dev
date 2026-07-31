context_start_file("dUniBin function errors")
test_that("checking class",{
  expect_type(dUniBin(0:300,300)$pdf,"double")
})
Temp<-dUniBin(0:300,300)
test_that("checking length of output",{
  expect_type(Temp,"list")
})
test_that("NA values are avoided",{
  expect_error(dUniBin(NA,4),
              "NA or Infinite or NAN values in the Input")
})
test_that("Random variable higher than Trial value",{
  expect_error(dUniBin(5,4),
              "Binomial random variable cannot be greater than binomial trial value")
})
test_that("Negativity",{
  expect_error(dUniBin(-3,4),
              "Binomial random variable or binomial trial value cannot be negative")
})

