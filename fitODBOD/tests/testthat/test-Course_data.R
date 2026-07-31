context_start_file("Checking the Course Data")
test_that("Comparing values of Subjects passed",{
  expect_equal(Course_data$sub.pass,seq(0,8))
})
test_that("Comparing values of frequency",{
  expect_equal(Course_data$fre,c(1,4,4,8,9,6,8,12,13))
})
test_that("the class of data",{
  expect_s3_class(Course_data,"data.frame")
})
test_that("values in binoial random variable",{
  expect_gte(min(Course_data$sub.pass),0)
})
test_that("values in frequencies",{
  expect_gte(min(Course_data[,-1]),0)
})
