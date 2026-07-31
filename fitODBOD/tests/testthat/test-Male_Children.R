context_start_file("Checking the Male Children Data")
test_that("Comparing values of No of Males",{
  expect_equal(Male_Children$No_of_Males,seq(0,12))
})
test_that("Comparing values of frequency",{
  expect_equal(Male_Children$freq,c(3,24,104,286,670,1033,1343,1112,829,478,181,45,7))
})
test_that("the class of data",{
  expect_s3_class(Male_Children,"data.frame")
})
test_that("values in binoial random variable",{
  expect_gte(min(Male_Children$No_of_Males),0)
})
test_that("values in frequencies",{
  expect_gte(min(Male_Children[,-1]),0)
})
