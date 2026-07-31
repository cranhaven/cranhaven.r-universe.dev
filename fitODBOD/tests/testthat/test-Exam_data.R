context_start_file("Checking the Exam Data")
test_that("Comparing values of No of Alpha",{
  expect_equal(Exam_data$No.of.alpha,seq(0,9))
})
test_that("Comparing values of Frequency",{
  expect_equal(Exam_data$fre,c(63,67,34,18,11,8,4,3,1,0))
})
test_that("the class of data",{
  expect_s3_class(Exam_data,"data.frame")
})
test_that("values in binoial random variable",{
  expect_gte(min(Exam_data$No.of.alpha),0)
})
test_that("values in frequencies",{
  expect_gte(min(Exam_data[,-1]),0)
})
