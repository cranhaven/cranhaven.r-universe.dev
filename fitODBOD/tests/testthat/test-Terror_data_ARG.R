context_start_file("Checking the Terror Data ARG")
test_that("Comparing values of Incidents",{
  expect_equal(Terror_data_ARG$Incidents,seq(0,6))
})
test_that("Comparing values of frequency",{
  expect_equal(Terror_data_ARG$fre,c(46,15,5,3,5,1,1))
})
test_that("the class of data",{
  expect_s3_class(Terror_data_ARG,"data.frame")
})
test_that("values in binoial random variable",{
  expect_gte(min(Terror_data_ARG$Incidents),0)
})
test_that("values in frequencies",{
  expect_gte(min(Terror_data_ARG[,-1]),0)
})
