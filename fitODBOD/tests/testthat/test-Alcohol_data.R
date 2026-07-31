context_start_file("Checking the Alcohol Data")
test_that("Comparing values of Days",{
  expect_equal(Alcohol_data$Days,seq(0,7))
})
test_that("Comparing values of week1",{
  expect_equal(Alcohol_data$week1,c(47,54,43,40,40,41,39,95))
})
test_that("Comparing values of week2",{
  expect_equal(sum(Alcohol_data$week2),399)
})
test_that("the class of data",{
  expect_s3_class(Alcohol_data,"data.frame")
})
test_that("values in binoial random variable",{
  expect_gte(min(Alcohol_data$Days),0)
})
test_that("values in frequencies",{
  expect_gte(min(Alcohol_data[,-1]),0)
})
