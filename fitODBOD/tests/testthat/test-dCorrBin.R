context_start_file("dCorrBin function errors")
test_that("checking class",{
  expect_type(dCorrBin(0:10,10,0.58,0.022)$pdf,"double")
})
Temp<-dCorrBin(0:10,10,0.58,0.022)
test_that("checking length of output",{
  expect_type(Temp,"list")
})
test_that("NA values are avoided",{
  expect_error(dCorrBin(NA,4,0.1,.3),
              "NA or Infinite or NAN values in the Input")
})
test_that("Random variable higher than Trial value",{
  expect_error(dCorrBin(5,4,0.2,0.4),
              "Binomial random variable cannot be greater than binomial trial value")
})
test_that("Negativity",{
  expect_error(dCorrBin(-3,4,0.2,0.4),
              "Binomial random variable or binomial trial value cannot be negative")
})
test_that("Probability issues",{
  expect_error(dCorrBin(3,5,3,0.4),
              "Probability value doesnot satisfy conditions")
})
test_that("Correlation above maximum limit",{
  expect_error(dCorrBin(3,5,0.2,19),
              "Correlation cannot be greater than 1 or Lesser than -1 or it cannot be greater than Maximum Correlation or Lesser than Minimum Correlation")
})

