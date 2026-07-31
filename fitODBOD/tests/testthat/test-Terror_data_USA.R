context_start_file("Checking the Terror Data USA ")
test_that("Comparing values of Incidents",{
  expect_equal(Terror_data_USA$Incidents,seq(0,5))
})
test_that("Comparing values of frequency",{
  expect_equal(Terror_data_USA$fre,c(38,26,8,2,1,1))
})
test_that("the class of data",{
  expect_s3_class(Terror_data_USA,"data.frame")
})
test_that("values in binoial random variable",{
  expect_gte(min(Terror_data_USA$Incidents),0)
})
test_that("values in frequencies",{
  expect_gte(min(Terror_data_USA[,-1]),0)
})
