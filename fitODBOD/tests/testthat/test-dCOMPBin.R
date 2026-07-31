context_start_file("dCOMPBin function errors")
test_that("checking class",{
  expect_type(dCOMPBin(0:10,10,0.58,0.022)$pdf,"double")
})
Temp<-dCOMPBin(0:10,10,0.58,0.022)
test_that("checking length of output",{
  expect_type(Temp,"list")
})
test_that("NA values are avoided",{
  expect_error(dCOMPBin(NA,4,0.1,.3),
              "NA or Infinite or NAN values in the Input")
})
test_that("Random variable higher than Trial value",{
  expect_error(dCOMPBin(5,4,0.2,0.4),
              "Binomial random variable cannot be greater than binomial trial value")
})
test_that("Negativity",{
  expect_error(dCOMPBin(-3,4,0.2,0.4),
              "Binomial random variable or binomial trial value cannot be negative")
})
test_that("Probability issues",{
  expect_error(dCOMPBin(3,5,3,0.4),
              "Probability value doesnot satisfy conditions")
})
