context_start_file("pUNI function")
test_that("checking class",{
  expect_type(pUNI(0.3),"double")
})
Temp<-pUNI(0.3)
test_that("checking length of output",{
  expect_type(Temp,"double")
})
test_that("NA values are avoided",{
  expect_error(pUNI(NA),
              "NA or Infinite or NAN values in the Input")
})
test_that("Greater than 1",{
  expect_error(pUNI(3),
              "Invalid values in the input")
})

