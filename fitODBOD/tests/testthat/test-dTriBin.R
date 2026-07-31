context_start_file("dTriBin function errors")
test_that("checking class",{
  expect_type(dTriBin(0:10,10,.4)$pdf,"double")
})
Temp<-dTriBin(0:10,10,.4)
test_that("checking length of output",{
  expect_type(Temp,"list")
})
test_that("NA values are avoided",{
  expect_error(dTriBin(NA,4,0.1),
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

