context_start_file("pGHGBeta function")
test_that("checking class",{
  expect_type(pGHGBeta(1,5,3,3,2),"double")
})
Temp<-pGHGBeta(1,5,3,3,2)
test_that("checking length of output",{
  expect_type(Temp,"double")
})
test_that("NA values are avoided",{
  expect_error(pGHGBeta(NA,0.1,3,3,2),
              "NA or Infinite or NAN values in the Input")
})
test_that("Greater than 1",{
  expect_error(pGHGBeta(5,8,3,3,2),
              "Invalid values in the input")
})
test_that("Lesser than zero or begin negative",{
  expect_error(pGHGBeta(3,-1,3,3,2),
              "Binomial trial value cannot be less than zero")
})
test_that("shape parameter b",{
  expect_error(pGHGBeta(0.1,5,1,-4,4),
              "Shape parameters cannot be less than or equal to zero")
})

