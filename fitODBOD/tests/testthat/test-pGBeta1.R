context_start_file("pGBeta1 function")
test_that("checking class",{
  expect_type(pGBeta1(0.2,1,0.1,2),"double")
})
Temp<-pGBeta1(0.5,0.1,1,3)
test_that("checking length of output",{
  expect_type(Temp,"double")
})
test_that("NA values are avoided",{
  expect_error(pGBeta1(NA,0.1,3,3),
              "NA or Infinite or NAN values in the Input")
})
test_that("Greater than 1",{
  expect_error(pGBeta1(5,8,3,3),
              "Invalid values in the input")
})
test_that("shape parameter b",{
  expect_error(pGBeta1(0.1,5,-4,1),
              "Shape parameters cannot be less than or equal to zero")
})

