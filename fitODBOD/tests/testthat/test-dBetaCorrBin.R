context_start_file("dBetaCorrBin function errors")
test_that("checking class",{
  expect_type(dBetaCorrBin(1,10,0.001,10,13)$pdf,"double")
})
Temp<-dBetaCorrBin(0:10,10,0.001,10,13)
test_that("checking length of output",{
  expect_type(Temp,"list")
})
test_that("NA values are avoided",{
  expect_error(dBetaCorrBin(NA,4,0.1,.3,1),
              "NA or Infinite or NAN values in the Input")
})
test_that("Random variable higher than Trial value",{
  expect_error(dBetaCorrBin(5,4,0.2,0.4,1),
              "Binomial random variable cannot be greater than binomial trial value")
})
test_that("Negativity",{
  expect_error(dBetaCorrBin(-3,4,0.2,0.4,1),
              "Binomial random variable or binomial trial value cannot be negative")
})
test_that("Correlation above maximum limit",{
  expect_error(dBetaCorrBin(3,5,9,0.2,0.2),
              "Correlation cannot be greater than 1 or Lesser than -1 or it cannot be greater than Maximum Correlation or Lesser than Minimum Correlation")
})

