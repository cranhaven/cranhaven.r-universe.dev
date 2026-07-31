context_start_file("dKUM function errors")
test_that("checking class",{
  expect_type(dKUM(seq(0,1,by=0.01),2,3)$pdf,"double")
})
Temp<-dKUM(seq(0,1,by=0.01),2,3)
test_that("checking length of output",{
  expect_type(Temp,"list")
})
test_that("NA values are avoided",{
  expect_error(dKUM(NA,0.1,3),
              "NA or Infinite or NAN values in the Input")
})
test_that("Greater than 1",{
  expect_error(dKUM(3,0.1,3),
              "Invalid values in the input")
})
test_that("shape parameter b",{
  expect_error(dKUM(0.1,5,-4),
              "Shape parameters cannot be less than or equal to zero")
})
test_that("shape parameter a",{
  expect_error(dKUM(0.1,-5,4),
              "Shape parameters cannot be less than or equal to zero")
})
