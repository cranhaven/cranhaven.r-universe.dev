context_start_file("dGHGBeta function erros")
test_that("checking class",{
  expect_type(dGHGBeta(seq(0,1,by=0.01),7,1.6312,0.3913,0.6659)$pdf,"double")
})
Temp<-dGHGBeta(seq(0,1,by=0.01),7,1.6312,0.3913,0.6659)
test_that("checking length of output",{
  expect_type(Temp,"list")
})
test_that("NA values are avoided",{
  expect_error(dGHGBeta(NA,0.1,3,3,2),
              "NA or Infinite or NAN values in the Input")
})
test_that("Greater than 1",{
  expect_error(dGHGBeta(5,8,3,3,2),
              "Invalid values in the input")
})
test_that("Lesser than zero or begin negative",{
  expect_error(dGHGBeta(3,-1,3,3,2),
              "Binomial trial value cannot be less than zero")
})
test_that("shape parameter b",{
  expect_error(dGHGBeta(0.1,5,1,-4,4),
              "Shape parameters cannot be less than or equal to zero")
})
