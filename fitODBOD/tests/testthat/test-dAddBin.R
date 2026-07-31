context_start_file("dAddBin function errors")
test_that("checking class",{
  expect_type(dAddBin(2,4,0.5,0.1)$pdf,"double")
})
Temp<-dAddBin(1:2,4,0.5,0.1)
test_that("checking length of output",{
  expect_type(Temp,"list")
})
test_that("NA values are avoided",{
  expect_error(dAddBin(NA,4,0.1,.3),"NA or Infinite or NAN values in the Input")
})
test_that("Random variable higher than Trial value",{
  expect_error(dAddBin(5,4,0.2,0.4),"Binomial random variable cannot be greater than binomial trial value")
})
test_that("Negativity",{
  expect_error(dAddBin(-3,4,0.2,0.4),"Binomial random variable or binomial trial value cannot be negative")
})
test_that("Alpha or probability value issues",{
  expect_error(dAddBin(3,5,3,0.4),"Probability or alpha value doesnot satisfy conditions")
})
test_that("Alpha being greater than right side",{
  expect_error(dAddBin(1,10,0.2,0.4),"alpha parameter doesnot satisfy the conditions")
})
