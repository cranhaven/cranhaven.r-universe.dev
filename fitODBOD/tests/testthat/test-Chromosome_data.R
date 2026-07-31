context_start_file("Checking the Chromosome Data")
test_that("Comparing values of No fo association",{
  expect_equal(Chromosome_data$No.of.Asso,seq(0,3))
})
test_that("Comparing values of frequency",{
  expect_equal(Chromosome_data$fre,c(32,103,122,80))
})
test_that("the class of data",{
  expect_s3_class(Chromosome_data,"data.frame")
})
test_that("values in binoial random variable",{
  expect_gte(min(Chromosome_data$No.of.Asso),0)
})
test_that("values in frequencies",{
  expect_gte(min(Chromosome_data[,-1]),0)
})
