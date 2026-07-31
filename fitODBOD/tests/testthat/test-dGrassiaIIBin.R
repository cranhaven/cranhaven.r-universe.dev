context_start_file("dGrassiaIIBin function errors")
test_that("checking class",{
  expect_type(dGrassiaIIBin(0:10,10,4,.2)$pdf,"double")
})
Temp<-dGrassiaIIBin(0:10,10,4,.2)
test_that("checking length of output",{
  expect_type(Temp,"list")
})
test_that("NA values are avoided",{
  expect_error(dGrassiaIIBin(NA,4,0.1,3),
              "NA or Infinite or NAN values in the Input")
})
test_that("Random variable higher than Trial value",{
  expect_error(dGrassiaIIBin(5,4,0.2,3),
              "Binomial random variable cannot be greater than binomial trial value")
})
test_that("Negativity",{
  expect_error(dGrassiaIIBin(-3,4,0.2,3),
              "Binomial random variable or binomial trial value cannot be negative")
})
test_that("shape parameter a",{
  expect_error(dGrassiaIIBin(2,4,-3,3),
              "Shape parameters cannot be less than or equal to zero")
})

