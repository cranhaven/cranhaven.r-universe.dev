context_start_file("pKUM function")
test_that("checking class",{
  expect_type(pKUM(0.2,1,0.1),"double")
})
Temp<-pKUM(0.5,0.1,1)
test_that("checking length of output",{
  expect_type(Temp,"double")
})
test_that("NA values are avoided",{
  expect_error(pKUM(NA,0.1,3),
              "NA or Infinite or NAN values in the Input")
})
test_that("Greater than 1",{
  expect_error(pKUM(3,0.1,3),
              "Invalid values in the input")
})
test_that("shape parameter b",{
  expect_error(pKUM(0.1,5,-4),
              "Shape parameters cannot be less than or equal to zero")
})

