context_start_file("pTRI function")
test_that("checking class",{
  expect_type(pTRI(0.2,0.1),"double")
})
Temp<-pTRI(0.5,0.1)
test_that("checking length of output",{
  expect_type(Temp,"double")
})
test_that("NA values are avoided",{
  expect_error(pTRI(NA,0.1),
              "NA or Infinite or NAN values in the Input")
})
test_that("Greater than 1",{
  expect_error(pTRI(3,0.1),
              "Invalid values in the input")
})
test_that("Mode out of range",{
  expect_error(pTRI(0.1,5),
              "Mode cannot be less than zero or greater than one")
})

