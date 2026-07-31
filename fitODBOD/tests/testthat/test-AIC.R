estimate <- EstMGFBetaBin(Chromosome_data$No.of.Asso,Chromosome_data$fre)
AIC_Temp<-AIC(estimate)
context_start_file("AIC function")
test_that("Output value expected",{
  expect_identical(round(AIC_Temp,4),877.6263)
})
test_that("Checking type of output",{
  expect_type(AIC_Temp,"double")
})
test_that("length of the output",{
  expect_equal(length(AIC_Temp),1)
})
