context_start_file("Checking the Plant Disease Data")
test_that("Comparing values of Disease plant",{
  expect_equal(Plant_DiseaseData$Dis.plant,seq(0,9))
})
test_that("Comparing values of Frequency",{
  expect_equal(Plant_DiseaseData$fre,c(36,48,38,23,10,3,1,1,0,0))
})
test_that("the class of data",{
  expect_s3_class(Plant_DiseaseData,"data.frame")
})
test_that("values in binoial random variable",{
  expect_gte(min(Plant_DiseaseData$Dis.plant),0)
})
test_that("values in frequencies",{
  expect_gte(min(Plant_DiseaseData[,-1]),0)
})
