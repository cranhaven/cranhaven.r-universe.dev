context_start_file("dGHGBB function errors")
test_that("checking class",{
  expect_type(dGHGBB(0:7,7,1.3,0.3,1.3)$pdf,"double")
})
Temp<-dGHGBB(0:7,7,1.3,0.3,1.3)
test_that("checking length of output",{
  expect_type(Temp,"list")
})
test_that("NA values are avoided",{
  expect_error(dGHGBB(NA,4,0.1,3,3),
              "NA or Infinite or NAN values in the Input")
})
test_that("Random variable higher than Trial value",{
  expect_error(dGHGBB(5,4,0.2,3,3),
              "Binomial random variable cannot be greater than binomial trial value")
})
test_that("Negativity",{
  expect_error(dGHGBB(-3,4,0.2,3,3),
              "Binomial random variable or binomial trial value cannot be negative")
})
test_that("shape parameter a",{
  expect_error(dGHGBB(2,4,-3,3,6),
              "Shape parameters cannot be less than or equal to zero")
})

