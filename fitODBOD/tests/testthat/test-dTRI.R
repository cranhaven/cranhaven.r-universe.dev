context_start_file("dTRI function errors")
test_that("checking class",{
  expect_type(dTRI(seq(0,1,by=0.05),0.3)$pdf,"double")
})
Temp<-dTRI(seq(0,1,by=0.05),0.3)
test_that("checking length of output",{
  expect_type(Temp,"list")
})
test_that("NA values are avoided",{
  expect_error(dTRI(NA,0.1),
              "NA or Infinite or NAN values in the Input")
})
test_that("Greater than 1",{
  expect_error(dTRI(3,0.1),
              "Invalid values in the input")
})
test_that("Mode out of range",{
  expect_error(dTRI(0.1,5),
              "Mode cannot be less than zero or greater than one")
})

