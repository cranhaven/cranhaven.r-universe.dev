context_start_file("dUNI function errors")
test_that("checking class",{
  expect_type(dUNI(seq(0,1,by=0.05))$pdf,"double")
})
Temp<-dUNI(seq(0,1,by=0.05))
test_that("checking length of output",{
  expect_type(Temp,"list")
})
test_that("NA values are avoided",{
  expect_error(dUNI(NA),
              "NA or Infinite or NAN values in the Input")
})
test_that("Greater than 1",{
  expect_error(dUNI(3),
              "Invalid values in the input")
})

