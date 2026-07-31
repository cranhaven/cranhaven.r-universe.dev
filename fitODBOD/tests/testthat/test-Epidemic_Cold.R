context_start_file("Checking the Epidemic Cold")
test_that("Comparing values of Cases",{
  expect_equal(Epidemic_Cold$Cases,seq(0,4))
})
test_that("Comparing values of Frequency",{
  expect_equal(Epidemic_Cold$Families,c(423,199,39,3,0))
})
test_that("the class of data",{
  expect_s3_class(Epidemic_Cold,"data.frame")
})
test_that("values in binoial random variable",{
  expect_gte(min(Epidemic_Cold$Cases),0)
})
test_that("values in frequencies",{
  expect_gte(min(Epidemic_Cold[,-1]),0)
})
